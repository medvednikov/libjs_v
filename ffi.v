module main

import math

// ============================================================================
// FFI bridge between the codegen and C++ runtime.
//
// Translated from bytecode/ffi.rs.
//
// Key operations:
// - create_executable()  -- packages assembled bytecode, tables, and
//   metadata into a C++ Bytecode::Executable
// - create_shared_function_data()  -- creates a C++ SharedFunctionInstanceData
//   for a parsed function
// - compile_regex()  -- delegates regex compilation to the C++ regex engine
//
// FFI struct types match their C++ counterparts in BytecodeFactory.h.
// ============================================================================

// Opaque pointer type for C++ handles.
// In V, voidptr is used for all opaque C/C++ pointers.

// ============================================================================
// FFI struct types (matching BytecodeFactory.h layout)
// ============================================================================

// Exception handler range (C++ BytecodeFactory::ExceptionHandlerData).
struct FFIExceptionHandler {
	start_offset   u32
	end_offset     u32
	handler_offset u32
}

// Source map entry mapping bytecode offset to source range.
struct FFISourceMapEntry {
	bytecode_offset u32
	source_start    u32
	source_end      u32
}

// A borrowed UTF-16 string slice for passing across FFI.
struct FFIUtf16Slice {
	ptr &u16 = unsafe { nil }
	len int
}

fn FFIUtf16Slice.from_slice(s Utf16String) FFIUtf16Slice {
	return FFIUtf16Slice{
		ptr: s.data.data
		len: s.data.len
	}
}

fn FFIUtf16Slice.from_raw(s []u16) FFIUtf16Slice {
	if s.len == 0 {
		return FFIUtf16Slice{
			ptr: unsafe { nil }
			len: 0
		}
	}
	return FFIUtf16Slice{
		ptr: unsafe { &s[0] }
		len: s.len
	}
}

// C-compatible Optional<u32>.
struct FFIOptionalU32 {
	value     u32
	has_value bool
}

fn FFIOptionalU32.none_val() FFIOptionalU32 {
	return FFIOptionalU32{
		value:     0
		has_value: false
	}
}

fn FFIOptionalU32.some_val(value u32) FFIOptionalU32 {
	return FFIOptionalU32{
		value:     value
		has_value: true
	}
}

// Class element descriptor for ClassBlueprint creation
// (C++ BytecodeFactory::ClassElementData).
struct FFIClassElement {
	kind                       u8 // ClassElementKind
	is_static                  bool
	is_private                 bool
	private_identifier         &u16 = unsafe { nil }
	private_identifier_len     int
	shared_function_data_index FFIOptionalU32
	has_initializer            bool
	literal_value_kind         u8 // LiteralValueKind
	literal_value_number       f64
	literal_value_string       &u16 = unsafe { nil }
	literal_value_string_len   int
}

// Data for creating a C++ SharedFunctionInstanceData.
struct FFISharedFunctionData {
	name                       voidptr
	name_len                   int
	function_kind              u8
	function_length            int
	formal_parameter_count     u32
	strict                     bool
	is_arrow                   bool
	has_simple_parameter_list  bool
	parameter_names            voidptr
	parameter_name_count       int
	source_text_offset         int
	source_text_length         int
	rust_function_ast          voidptr
	uses_this                  bool
	uses_this_from_environment bool
}

// All data needed to create a C++ Bytecode::Executable.
struct FFIExecutableData {
	bytecode                    voidptr
	bytecode_length             int
	identifier_table            voidptr
	identifier_count            int
	property_key_table          voidptr
	property_key_count          int
	string_table                voidptr
	string_count                int
	constants_data              voidptr
	constants_data_length       int
	constants_count             int
	exception_handlers          voidptr
	exception_handler_count     int
	source_map                  voidptr
	source_map_count            int
	basic_block_offsets         voidptr
	basic_block_count           int
	local_variable_names        voidptr
	local_variable_count        int
	property_lookup_cache_count u32
	global_variable_cache_count u32
	template_object_cache_count u32
	object_shape_cache_count    u32
	number_of_registers         u32
	is_strict                   bool
	length_identifier           FFIOptionalU32
	shared_function_data        voidptr
	shared_function_data_count  int
	class_blueprints            voidptr
	class_blueprint_count       int
	compiled_regexes            voidptr
	regex_count                 int
}

// ============================================================================
// Constant encoding
// ============================================================================

// Constant tags for the FFI constant buffer (ABI-compatible with BytecodeFactory).
const ffi_constant_tag_number = u8(0)
const ffi_constant_tag_boolean_true = u8(1)
const ffi_constant_tag_boolean_false = u8(2)
const ffi_constant_tag_null = u8(3)
const ffi_constant_tag_undefined = u8(4)
const ffi_constant_tag_empty = u8(5)
const ffi_constant_tag_string = u8(6)
const ffi_constant_tag_bigint = u8(7)
const ffi_constant_tag_raw_value = u8(8)

// Encode constants into a tagged byte buffer for FFI.
fn encode_constants(constants []ConstantValue) []u8 {
	mut buffer := []u8{}
	for c in constants {
		match c.tag {
			.number {
				buffer << ffi_constant_tag_number
				bits := math.f64_bits(c.number_value)
				buffer << u8(bits)
				buffer << u8(bits >> 8)
				buffer << u8(bits >> 16)
				buffer << u8(bits >> 24)
				buffer << u8(bits >> 32)
				buffer << u8(bits >> 40)
				buffer << u8(bits >> 48)
				buffer << u8(bits >> 56)
			}
			.boolean {
				if c.bool_value {
					buffer << ffi_constant_tag_boolean_true
				} else {
					buffer << ffi_constant_tag_boolean_false
				}
			}
			.null_val {
				buffer << ffi_constant_tag_null
			}
			.undefined_val {
				buffer << ffi_constant_tag_undefined
			}
			.empty_val {
				buffer << ffi_constant_tag_empty
			}
			.string_val {
				buffer << ffi_constant_tag_string
				str_len := u32(c.string_value.len())
				buffer << u8(str_len)
				buffer << u8(str_len >> 8)
				buffer << u8(str_len >> 16)
				buffer << u8(str_len >> 24)
				for code_unit in c.string_value.data {
					buffer << u8(code_unit)
					buffer << u8(code_unit >> 8)
				}
			}
			.bigint_val {
				buffer << ffi_constant_tag_bigint
				bi_len := u32(c.bigint_value.len)
				buffer << u8(bi_len)
				buffer << u8(bi_len >> 8)
				buffer << u8(bi_len >> 16)
				buffer << u8(bi_len >> 24)
				for ch in c.bigint_value.bytes() {
					buffer << ch
				}
			}
			.raw_value {
				buffer << ffi_constant_tag_raw_value
				encoded := c.raw_u64_value
				buffer << u8(encoded)
				buffer << u8(encoded >> 8)
				buffer << u8(encoded >> 16)
				buffer << u8(encoded >> 24)
				buffer << u8(encoded >> 32)
				buffer << u8(encoded >> 40)
				buffer << u8(encoded >> 48)
				buffer << u8(encoded >> 56)
			}
		}
	}
	return buffer
}

// ============================================================================
// create_executable
// ============================================================================

// Create a C++ Executable from the generator's assembled output.
fn create_executable(gen Generator, assembled AssembledBytecode, vm_ptr voidptr, source_code_ptr voidptr) voidptr {
	// Build FFI slices for identifier table
	mut ident_slices := []FFIUtf16Slice{cap: gen.identifier_table.len}
	for s in gen.identifier_table {
		ident_slices << FFIUtf16Slice.from_slice(s)
	}

	// Build FFI slices for property key table
	mut property_key_slices := []FFIUtf16Slice{cap: gen.property_key_table.len}
	for s in gen.property_key_table {
		property_key_slices << FFIUtf16Slice.from_slice(s)
	}

	// Build FFI slices for string table
	mut string_slices := []FFIUtf16Slice{cap: gen.string_table.len}
	for s in gen.string_table {
		string_slices << FFIUtf16Slice.from_slice(s)
	}

	// Encode constants
	constants_buffer := encode_constants(gen.constants)

	// Build FFI exception handlers
	mut ffi_handlers := []FFIExceptionHandler{cap: assembled.exception_handlers.len}
	for h in assembled.exception_handlers {
		ffi_handlers << FFIExceptionHandler{
			start_offset:   h.start_offset
			end_offset:     h.end_offset
			handler_offset: h.handler_offset
		}
	}

	// Build FFI source map
	mut ffi_source_map := []FFISourceMapEntry{cap: assembled.source_map.len}
	for e in assembled.source_map {
		ffi_source_map << FFISourceMapEntry{
			bytecode_offset: e.bytecode_offset
			source_start:    e.source_start
			source_end:      e.source_end
		}
	}

	// Build local variable name slices
	mut local_var_slices := []FFIUtf16Slice{cap: gen.local_variables.len}
	for v in gen.local_variables {
		local_var_slices << FFIUtf16Slice.from_slice(v.name)
	}

	// Collect shared function data pointers
	sfd_ptrs := gen.shared_function_data

	// Class blueprint pointers
	bp_ptrs := gen.class_blueprints

	// Length identifier
	length_id := if li := gen.length_identifier {
		FFIOptionalU32.some_val(li.value)
	} else {
		FFIOptionalU32.none_val()
	}

	// Pre-compute all array pointers as voidptr to avoid type mismatches.
	mut bytecode_ptr := unsafe { nil }
	if assembled.bytecode.len > 0 {
		bytecode_ptr = unsafe { voidptr(&assembled.bytecode[0]) }
	}
	mut ident_ptr := unsafe { nil }
	if ident_slices.len > 0 {
		ident_ptr = unsafe { voidptr(&ident_slices[0]) }
	}
	mut pkey_ptr := unsafe { nil }
	if property_key_slices.len > 0 {
		pkey_ptr = unsafe { voidptr(&property_key_slices[0]) }
	}
	mut str_ptr := unsafe { nil }
	if string_slices.len > 0 {
		str_ptr = unsafe { voidptr(&string_slices[0]) }
	}
	mut const_ptr := unsafe { nil }
	if constants_buffer.len > 0 {
		const_ptr = unsafe { voidptr(&constants_buffer[0]) }
	}
	mut handler_ptr := unsafe { nil }
	if ffi_handlers.len > 0 {
		handler_ptr = unsafe { voidptr(&ffi_handlers[0]) }
	}
	mut srcmap_ptr := unsafe { nil }
	if ffi_source_map.len > 0 {
		srcmap_ptr = unsafe { voidptr(&ffi_source_map[0]) }
	}
	mut bb_ptr := unsafe { nil }
	if assembled.basic_block_start_offsets.len > 0 {
		bb_ptr = unsafe { voidptr(&assembled.basic_block_start_offsets[0]) }
	}
	mut localvar_ptr := unsafe { nil }
	if local_var_slices.len > 0 {
		localvar_ptr = unsafe { voidptr(&local_var_slices[0]) }
	}
	mut sfd_data_ptr := unsafe { nil }
	if sfd_ptrs.len > 0 {
		sfd_data_ptr = unsafe { voidptr(&sfd_ptrs[0]) }
	}
	mut bp_ptr := unsafe { nil }
	if bp_ptrs.len > 0 {
		bp_ptr = unsafe { voidptr(&bp_ptrs[0]) }
	}
	mut regex_ptr := unsafe { nil }
	if gen.compiled_regexes.len > 0 {
		regex_ptr = unsafe { voidptr(&gen.compiled_regexes[0]) }
	}

	_ = FFIExecutableData{
		bytecode:                    bytecode_ptr
		bytecode_length:             assembled.bytecode.len
		identifier_table:            ident_ptr
		identifier_count:            ident_slices.len
		property_key_table:          pkey_ptr
		property_key_count:          property_key_slices.len
		string_table:                str_ptr
		string_count:                string_slices.len
		constants_data:              const_ptr
		constants_data_length:       constants_buffer.len
		constants_count:             gen.constants.len
		exception_handlers:          handler_ptr
		exception_handler_count:     ffi_handlers.len
		source_map:                  srcmap_ptr
		source_map_count:            ffi_source_map.len
		basic_block_offsets:         bb_ptr
		basic_block_count:           assembled.basic_block_start_offsets.len
		local_variable_names:        localvar_ptr
		local_variable_count:        local_var_slices.len
		property_lookup_cache_count: gen.next_property_lookup_cache
		global_variable_cache_count: gen.next_global_variable_cache
		template_object_cache_count: gen.next_template_object_cache
		object_shape_cache_count:    gen.next_object_shape_cache
		number_of_registers:         assembled.number_of_registers
		is_strict:                   gen.strict
		length_identifier:           length_id
		shared_function_data:        sfd_data_ptr
		shared_function_data_count:  sfd_ptrs.len
		class_blueprints:            bp_ptr
		class_blueprint_count:       bp_ptrs.len
		compiled_regexes:            regex_ptr
		regex_count:                 gen.compiled_regexes.len
	}

	// TODO: Call C++ rust_create_executable(vm_ptr, source_code_ptr, &ffi_data)
	_ = vm_ptr
	_ = source_code_ptr
	return unsafe { nil }
}

// ============================================================================
// create_shared_function_data / create_sfd_for_gdi
// ============================================================================

// Create a SharedFunctionInstanceData from a JsFunctionData.
// Computes has_simple_parameter_list, builds parameter name slices,
// and calls into C++ to create the SFD object.
fn ffi_create_shared_function_data(vm voidptr, source_code voidptr, source_len int, data &JsFunctionData, strict bool) voidptr {
	source_start := int(data.source_text_start)
	source_end := int(data.source_text_end)
	source_text_len := source_end - source_start

	mut name_ptr := unsafe { nil }
	mut name_len := 0
	if id := data.name {
		name_ptr = unsafe { voidptr(&id.name.data[0]) }
		name_len = id.name.data.len
	}

	has_simple_parameter_list := data.parameters.all(fn (p FunctionParameter) bool {
		if p.is_rest {
			return false
		}
		if p.default_value != none {
			return false
		}
		return p.binding is IdentifierBinding
	})

	mut parameter_name_slices := []FFIUtf16Slice{}
	if has_simple_parameter_list {
		for p in data.parameters {
			match p.binding {
				IdentifierBinding {
					parameter_name_slices << FFIUtf16Slice.from_slice(p.binding.identifier.name)
				}
				BindingPatternBinding {}
			}
		}
	}

	function_kind := u8(data.kind)
	is_strict := data.is_strict_mode || strict

	mut param_names_ptr := unsafe { nil }
	if parameter_name_slices.len > 0 {
		param_names_ptr = unsafe { voidptr(&parameter_name_slices[0]) }
	}

	_ = FFISharedFunctionData{
		name:                       name_ptr
		name_len:                   name_len
		function_kind:              function_kind
		function_length:            data.function_length
		formal_parameter_count:     u32(data.parameters.len)
		strict:                     is_strict
		is_arrow:                   data.is_arrow_function
		has_simple_parameter_list:  has_simple_parameter_list
		parameter_names:            param_names_ptr
		parameter_name_count:       parameter_name_slices.len
		source_text_offset:         source_start
		source_text_length:         source_text_len
		rust_function_ast:          unsafe { nil }
		uses_this:                  data.parsing_insights.uses_this
		uses_this_from_environment: data.parsing_insights.uses_this_from_environment
	}

	// TODO: Call C++ rust_create_sfd(vm, source_code, &ffi_data)
	_ = vm
	_ = source_code
	_ = source_len
	return unsafe { nil }
}

// Create a SharedFunctionInstanceData for GDI use (no name override).
fn create_sfd_for_gdi(function_data JsFunctionData, function_table FunctionTable, vm_ptr voidptr, source_code_ptr voidptr, is_strict bool) voidptr {
	_ = function_table
	return ffi_create_shared_function_data(vm_ptr, source_code_ptr, 0, &function_data,
		is_strict)
}

// ============================================================================
// Class blueprint creation
// ============================================================================

fn ffi_create_class_blueprint(vm voidptr, name_str []u16, has_super bool, constructor_sfd voidptr, static_init_sfd voidptr, field_initializer_sfds []voidptr, elements_count int) voidptr {
	// TODO: Call C++ rust_create_class_blueprint(...)
	_ = vm
	_ = name_str
	_ = has_super
	_ = constructor_sfd
	_ = static_init_sfd
	_ = field_initializer_sfds
	_ = elements_count
	return unsafe { nil }
}

// ============================================================================
// Number to UTF-16 conversion
// ============================================================================

// Convert a JS number to its UTF-16 string representation using the
// ECMA-262 Number::toString algorithm.
fn ffi_js_number_to_utf16(n f64) Utf16String {
	// Simplified V implementation. The Rust version uses C++ FFI for
	// exact ECMA-262 Number::toString formatting.
	s := '${n}'
	return Utf16String.from_slice(utf16(s))
}

// ============================================================================
// Regex compilation
// ============================================================================

// Compile a regex pattern+flags using the C++ regex engine.
fn ffi_compile_regex(pattern []u16, flags []u16) voidptr {
	// TODO: Call C++ rust_compile_regex(pattern, flags, &error)
	_ = pattern
	_ = flags
	return unsafe { nil }
}

// ============================================================================
// SFD metadata / name setters
// ============================================================================

fn module_sfd_set_name(sfd_ptr voidptr, name &u16, name_len int) {
	// TODO: Call C++ to set SFD display name
	_ = sfd_ptr
	_ = name
	_ = name_len
}

fn rust_sfd_set_metadata(sfd_ptr voidptr, uses_this bool, function_environment_needed bool, function_environment_bindings_count int, might_need_arguments_object bool, contains_direct_call_to_eval bool) {
	// TODO: Call C++ to set SFD metadata
	_ = sfd_ptr
	_ = uses_this
	_ = function_environment_needed
	_ = function_environment_bindings_count
	_ = might_need_arguments_object
	_ = contains_direct_call_to_eval
}

fn rust_sfd_set_class_field_initializer_name(sfd_ptr voidptr, name &u16, name_len int, is_private bool) {
	// TODO: Call C++ to set class field initializer name
	_ = sfd_ptr
	_ = name
	_ = name_len
	_ = is_private
}

// ============================================================================
// Script GDI callbacks
// ============================================================================

fn script_gdi_push_lexical_name(ctx voidptr, name &u16, name_len int) {
	_ = ctx
	_ = name
	_ = name_len
}

fn script_gdi_push_var_name(ctx voidptr, name &u16, name_len int) {
	_ = ctx
	_ = name
	_ = name_len
}

fn script_gdi_push_function(ctx voidptr, sfd_ptr voidptr, name &u16, name_len int) {
	_ = ctx
	_ = sfd_ptr
	_ = name
	_ = name_len
}

fn script_gdi_push_var_scoped_name(ctx voidptr, name &u16, name_len int) {
	_ = ctx
	_ = name
	_ = name_len
}

fn script_gdi_push_annex_b_name(ctx voidptr, name &u16, name_len int) {
	_ = ctx
	_ = name
	_ = name_len
}

fn script_gdi_push_lexical_binding(ctx voidptr, name &u16, name_len int, is_constant bool) {
	_ = ctx
	_ = name
	_ = name_len
	_ = is_constant
}

// ============================================================================
// Eval GDI callbacks
// ============================================================================

fn eval_gdi_set_strict(ctx voidptr, is_strict bool) {
	_ = ctx
	_ = is_strict
}

fn eval_gdi_push_var_name(ctx voidptr, name &u16, name_len int) {
	_ = ctx
	_ = name
	_ = name_len
}

fn eval_gdi_push_function(ctx voidptr, sfd_ptr voidptr, name &u16, name_len int) {
	_ = ctx
	_ = sfd_ptr
	_ = name
	_ = name_len
}

fn eval_gdi_push_var_scoped_name(ctx voidptr, name &u16, name_len int) {
	_ = ctx
	_ = name
	_ = name_len
}

fn eval_gdi_push_annex_b_name(ctx voidptr, name &u16, name_len int) {
	_ = ctx
	_ = name
	_ = name_len
}

fn eval_gdi_push_lexical_binding(ctx voidptr, name &u16, name_len int, is_constant bool) {
	_ = ctx
	_ = name
	_ = name_len
	_ = is_constant
}

// ============================================================================
// Well-known symbols and abstract operations
// ============================================================================

fn ffi_get_well_known_symbol(vm_ptr voidptr, symbol_id u32) u64 {
	// TODO: Call C++ get_well_known_symbol(vm_ptr, symbol_id)
	_ = vm_ptr
	_ = symbol_id
	return 0
}

fn ffi_get_abstract_operation_function(vm_ptr voidptr, name []u16) u64 {
	// TODO: Call C++ get_abstract_operation_function(vm_ptr, name, name_len)
	_ = vm_ptr
	_ = name
	return 0
}
