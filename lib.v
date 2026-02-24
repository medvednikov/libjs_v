module main

// lib.v — Entry points and utility functions.
// Translated from lib.rs. Contains the compilation pipeline,
// GDI/EDI metadata extraction, SFD metadata computation,
// and AST utility functions.
//
// FFI entry points from Rust are translated as regular V functions
// with voidptr for C pointers. The callback types use V function types.

// =============================================================================
// Callback types (FFI function pointer equivalents)
// =============================================================================

type ParseErrorCallback = fn (ctx voidptr, message &u8, message_len int, line u32, column u32)

type ModuleBoolCallback = fn (ctx voidptr, value bool)

type ModuleNameCallback = fn (ctx voidptr, name &u16, name_len int)

type ModuleImportEntryCallback = fn (ctx voidptr, import_name &u16, import_name_len int, is_namespace bool, local_name &u16, local_name_len int, module_specifier &u16, specifier_len int, attribute_keys voidptr, attribute_values voidptr, attribute_count int)

type ModuleExportEntryCallback = fn (ctx voidptr, kind u8, export_name &u16, export_name_len int, local_or_import_name &u16, local_or_import_name_len int, module_specifier &u16, specifier_len int, attribute_keys voidptr, attribute_values voidptr, attribute_count int)

type ModuleRequestedModuleCallback = fn (ctx voidptr, specifier &u16, specifier_len int, attribute_keys voidptr, attribute_values voidptr, attribute_count int)

type ModuleFunctionCallback = fn (ctx voidptr, sfd_ptr voidptr, name &u16, name_len int)

type ModuleLexicalBindingCallback = fn (ctx voidptr, name &u16, name_len int, is_constant bool, function_index i32)

type BuiltinFunctionCallback = fn (ctx voidptr, sfd_ptr voidptr, name &u16, name_len int)

// Module callback table passed from C++ to avoid many function pointer parameters.
struct ModuleCallbacks {
	set_has_top_level_await    ModuleBoolCallback            = unsafe { nil }
	push_import_entry          ModuleImportEntryCallback     = unsafe { nil }
	push_local_export          ModuleExportEntryCallback     = unsafe { nil }
	push_indirect_export       ModuleExportEntryCallback     = unsafe { nil }
	push_star_export           ModuleExportEntryCallback     = unsafe { nil }
	push_requested_module      ModuleRequestedModuleCallback = unsafe { nil }
	set_default_export_binding ModuleNameCallback            = unsafe { nil }
	push_var_name              ModuleNameCallback            = unsafe { nil }
	push_function              ModuleFunctionCallback        = unsafe { nil }
	push_lexical_binding       ModuleLexicalBindingCallback  = unsafe { nil }
}

// FFIUtf16Slice is now defined in ffi.v

// =============================================================================
// Internal helpers
// =============================================================================

// Log parser and scope collector errors, returning true if any were found.
fn check_errors(mut parser Parser) bool {
	return check_errors_with_callback(mut parser, unsafe { nil }, ParseErrorCallback(unsafe { nil }))
}

// Check for errors, optionally reporting them via a callback.
fn check_errors_with_callback(mut parser Parser, error_context voidptr, error_callback ParseErrorCallback) bool {
	if parser.has_errors() {
		if error_callback != ParseErrorCallback(unsafe { nil }) {
			for err in parser.errors_ref() {
				unsafe {
					error_callback(error_context, err.message.str, err.message.len, err.line,
						err.column)
				}
			}
		}
		return true
	}
	if parser.scope_collector.has_errors() {
		if error_callback != ParseErrorCallback(unsafe { nil }) {
			for err in parser.scope_collector.drain_errors() {
				unsafe {
					error_callback(error_context, err.message.str, err.message.len, err.line,
						err.column)
				}
			}
		}
		return true
	}
	return false
}

// Convert scope local variables to generator GeneratorLocalVariable format.
fn convert_local_variables(scope &ScopeData) []GeneratorLocalVariable {
	mut result := []GeneratorLocalVariable{cap: scope.local_variables.len}
	for lv in scope.local_variables {
		result << GeneratorLocalVariable{
			name:                                            lv.name.clone()
			is_lexically_declared:                           lv.kind == .let_or_const
			is_initialized_during_declaration_instantiation: false
		}
	}
	return result
}

// Create a Generator configured for program-level compilation.
fn new_program_generator(strict bool, vm_ptr voidptr, source_code_ptr voidptr, source_len int) Generator {
	mut gen := Generator.new()
	gen.strict = strict
	gen.must_propagate_completion = true
	gen.vm_ptr = vm_ptr
	gen.source_code_ptr = source_code_ptr
	gen.source_len = source_len
	return gen
}

// Shared compilation pipeline: local variable setup → codegen → assemble → create Executable.
fn compile_program_body(mut gen Generator, program &Statement, scope &ScopeData, vm_ptr voidptr, source_code_ptr voidptr) voidptr {
	gen.local_variables = convert_local_variables(scope)

	entry_block := gen.make_block()
	gen.switch_to_basic_block(entry_block)

	env_reg := gen.scoped_operand(Operand.register(register_saved_lexical_environment))
	gen.emit(Instruction{
		tag: .get_lexical_environment
		dst: env_reg.operand
	})
	gen.lexical_environment_register_stack << env_reg

	result := generate_statement(program, mut gen)

	if !gen.is_current_block_terminated() {
		if res := result {
			gen.emit(Instruction{
				tag:   .end_op
				value: res.operand
			})
		}
	}

	assembled := gen.assemble()
	return create_executable(gen, assembled, vm_ptr, source_code_ptr)
}

// =============================================================================
// Entry points: program compilation
// =============================================================================

// Compile a JavaScript program.
fn compile_program(source []u16, vm_ptr voidptr, source_code_ptr voidptr, program_type_num u8, starts_in_strict_mode bool, initiated_by_eval bool, in_eval_function_context bool, allow_super_property_lookup bool, allow_super_constructor_call bool, in_class_field_initializer bool) voidptr {
	pt := match program_type_num {
		0 { ProgramType.script }
		1 { ProgramType.module }
		else { return unsafe { nil } }
	}
	mut parser := Parser.new(source, pt)
	if initiated_by_eval {
		parser.initiated_by_eval = true
		parser.in_eval_function_context = in_eval_function_context
		parser.flags.allow_super_property_lookup = allow_super_property_lookup
		parser.flags.allow_super_constructor_call = allow_super_constructor_call
		parser.flags.in_class_field_initializer = in_class_field_initializer
	}

	program := parser.parse_program(starts_in_strict_mode)

	if check_errors(mut parser) {
		return unsafe { nil }
	}

	parser.scope_collector.analyze(initiated_by_eval)

	if program.kind.tag != .program {
		return unsafe { nil }
	}
	program_data := program.kind.program_data or { return unsafe { nil } }
	scope := program_data.scope

	mut gen := new_program_generator(starts_in_strict_mode, vm_ptr, source_code_ptr, source.len)
	gen.function_table = parser.function_table
	return compile_program_body(mut gen, &program, scope, vm_ptr, source_code_ptr)
}

// Compile a script and extract GDI (GlobalDeclarationInstantiation) metadata.
fn compile_script(source []u16, vm_ptr voidptr, source_code_ptr voidptr, gdi_context voidptr, error_context voidptr, error_callback ParseErrorCallback, initial_line_number int) voidptr {
	mut parser := Parser.new_with_line_offset(source, .script, u32(initial_line_number))

	program := parser.parse_program(false)

	if check_errors_with_callback(mut parser, error_context, error_callback) {
		return unsafe { nil }
	}

	parser.scope_collector.analyze(false)

	if program.kind.tag != .program {
		return unsafe { nil }
	}
	program_data := program.kind.program_data or { return unsafe { nil } }
	scope := program_data.scope
	is_strict := program_data.is_strict_mode

	mut gen := new_program_generator(is_strict, vm_ptr, source_code_ptr, source.len)
	gen.function_table = parser.function_table
	exec_ptr := compile_program_body(mut gen, &program, scope, vm_ptr, source_code_ptr)
	if exec_ptr == unsafe { nil } {
		return unsafe { nil }
	}

	extract_script_gdi(scope, is_strict, vm_ptr, source_code_ptr, gdi_context, mut gen.function_table)

	return exec_ptr
}

// Compile an eval script and extract EDI (EvalDeclarationInstantiation) metadata.
fn compile_eval(source []u16, vm_ptr voidptr, source_code_ptr voidptr, gdi_context voidptr, starts_in_strict_mode bool, in_eval_function_context bool, allow_super_property_lookup bool, allow_super_constructor_call bool, in_class_field_initializer bool, error_context voidptr, error_callback ParseErrorCallback) voidptr {
	mut parser := Parser.new(source, .script)
	parser.initiated_by_eval = true
	parser.in_eval_function_context = in_eval_function_context
	parser.flags.allow_super_property_lookup = allow_super_property_lookup
	parser.flags.allow_super_constructor_call = allow_super_constructor_call
	parser.flags.in_class_field_initializer = in_class_field_initializer

	program := parser.parse_program(starts_in_strict_mode)

	if check_errors_with_callback(mut parser, error_context, error_callback) {
		return unsafe { nil }
	}

	parser.scope_collector.analyze(true)

	if program.kind.tag != .program {
		return unsafe { nil }
	}
	program_data := program.kind.program_data or { return unsafe { nil } }
	scope := program_data.scope
	is_strict := program_data.is_strict_mode

	mut gen := new_program_generator(is_strict, vm_ptr, source_code_ptr, source.len)
	gen.function_table = parser.function_table
	exec_ptr := compile_program_body(mut gen, &program, scope, vm_ptr, source_code_ptr)
	if exec_ptr == unsafe { nil } {
		return unsafe { nil }
	}

	extract_eval_gdi(scope, is_strict, vm_ptr, source_code_ptr, gdi_context, mut gen.function_table)

	return exec_ptr
}

// =============================================================================
// Entry point: dynamic function (new Function())
// =============================================================================

// Compile a dynamically-created function (new Function()).
fn compile_dynamic_function(full_source []u16, parameters_source []u16, body_source []u16, vm_ptr voidptr, source_code_ptr voidptr, function_kind_num u8, error_context voidptr, error_callback ParseErrorCallback) voidptr {
	kind := match function_kind_num {
		0 { FunctionKind.normal }
		1 { FunctionKind.generator }
		2 { FunctionKind.async_kind }
		3 { FunctionKind.async_generator }
		else { return unsafe { nil } }
	}

	// Validate parameters standalone.
	{
		mut lexer := Lexer.new(parameters_source, 1, 0)
		for {
			token := lexer.next()
			if token.token_type == .eof {
				break
			}
			if token.token_type == .invalid {
				msg := token.message or { 'Unexpected token ${token.token_type.name()}' }
				if error_callback != ParseErrorCallback(unsafe { nil }) {
					unsafe {
						error_callback(error_context, msg.str, msg.len, token.line_number,
							token.line_column)
					}
				}
				return unsafe { nil }
			}
		}
	}

	// Wrap in a function for syntactic validation.
	{
		mut validate_src := []u16{}
		match kind {
			.generator { validate_src << utf16('function* test(') }
			.async_kind { validate_src << utf16('async function test(') }
			.async_generator { validate_src << utf16('async function* test(') }
			.normal { validate_src << utf16('function test(') }
		}
		validate_src << parameters_source
		validate_src << utf16('\n) {}')
		mut parser := Parser.new(validate_src, .script)
		parser.parse_program(false)
		if check_errors_with_callback(mut parser, error_context, error_callback) {
			return unsafe { nil }
		}
	}

	// Validate body standalone.
	{
		mut parser := Parser.new(body_source, .script)
		parser.flags.in_function_context = true
		match kind {
			.async_kind, .async_generator {
				parser.flags.await_expression_is_valid = true
			}
			else {}
		}
		match kind {
			.generator, .async_generator {
				parser.flags.in_generator_function_context = true
			}
			else {}
		}
		parser.parse_program(false)
		if check_errors_with_callback(mut parser, error_context, error_callback) {
			return unsafe { nil }
		}
	}

	mut parser := Parser.new(full_source, .script)
	program := parser.parse_program(false)

	if check_errors_with_callback(mut parser, error_context, error_callback) {
		return unsafe { nil }
	}

	parser.scope_collector.analyze_as_dynamic_function()

	if parser.scope_collector.has_errors() {
		if error_callback != ParseErrorCallback(unsafe { nil }) {
			for err in parser.scope_collector.drain_errors() {
				unsafe {
					error_callback(error_context, err.message.str, err.message.len, err.line,
						err.column)
				}
			}
		}
		return unsafe { nil }
	}

	if program.kind.tag != .program {
		return unsafe { nil }
	}
	program_data := program.kind.program_data or { return unsafe { nil } }
	scope := program_data.scope

	// Extract the FunctionExpression from the program.
	mut function_id := ?FunctionId(none)
	for child in scope.children {
		if child.kind.tag == .function_declaration {
			function_id = child.kind.function_id
			break
		}
		if child.kind.tag == .expression_stmt {
			if expr := child.kind.expression {
				if expr.kind.tag == .function_expr {
					function_id = expr.kind.function_id
					break
				}
			}
		}
	}

	fid := function_id or {
		if error_callback != ParseErrorCallback(unsafe { nil }) {
			msg := 'Failed to parse dynamic function'
			unsafe {
				error_callback(error_context, msg.str, msg.len, 0, 0)
			}
		}
		return unsafe { nil }
	}

	mut function_data := parser.function_table.take(fid)

	// Dynamic functions always need an arguments object.
	function_data.parsing_insights.might_need_arguments_object = true

	is_strict := function_data.is_strict_mode
	subtable := parser.function_table.extract_reachable(&function_data)

	return create_sfd_for_gdi(function_data, subtable, vm_ptr, source_code_ptr, is_strict)
}

// =============================================================================
// Entry point: builtin file compilation
// =============================================================================

// Parse a builtin JS file in strict mode, extract top-level function
// declarations, and create SharedFunctionInstanceData for each.
fn compile_builtin_file(source []u16, vm_ptr voidptr, source_code_ptr voidptr, ctx voidptr, push_function BuiltinFunctionCallback) {
	mut parser := Parser.new(source, .script)
	program := parser.parse_program(true) // strict mode

	if parser.has_errors() {
		mut errors_str := []string{}
		for e in parser.errors_ref() {
			errors_str << '${e.line}:${e.column}: ${e.message}'
		}
		panic('Parse errors in builtin file: ${errors_str.join('; ')}')
	}

	parser.scope_collector.analyze(false)

	if program.kind.tag != .program {
		return
	}
	program_data := program.kind.program_data or { return }
	scope := program_data.scope

	for child in scope.children {
		if child.kind.tag == .function_declaration {
			fid := child.kind.function_id
			function_data := parser.function_table.take(fid)
			subtable := parser.function_table.extract_reachable(&function_data)
			sfd_ptr := create_sfd_for_gdi(function_data, subtable, vm_ptr, source_code_ptr,
				true)
			if sfd_ptr != unsafe { nil } {
				if name := child.kind.func_name {
					unsafe {
						push_function(ctx, sfd_ptr, name.name.data.data, name.name.data.len)
					}
				}
			}
		}
	}
}

// =============================================================================
// Entry point: module compilation
// =============================================================================

// Compile an ES module using the parser and bytecode generator.
fn compile_module(source []u16, vm_ptr voidptr, source_code_ptr voidptr, module_context voidptr, callbacks &ModuleCallbacks, error_context voidptr, error_callback ParseErrorCallback) CompileModuleResult {
	mut parser := Parser.new(source, .module)
	program := parser.parse_program(false)

	if check_errors_with_callback(mut parser, error_context, error_callback) {
		return CompileModuleResult{}
	}

	parser.scope_collector.analyze(false)

	if program.kind.tag != .program {
		return CompileModuleResult{}
	}
	program_data := program.kind.program_data or { return CompileModuleResult{} }
	scope := program_data.scope
	has_top_level_await := program_data.has_top_level_await

	mut function_table := parser.function_table

	// Report has_top_level_await.
	unsafe {
		callbacks.set_has_top_level_await(module_context, has_top_level_await)
	}

	// Process imports and exports.
	extract_module_metadata(scope, module_context, callbacks)

	// Extract var declared names and lexical bindings.
	extract_module_declarations(scope, vm_ptr, source_code_ptr, module_context, callbacks, mut
		function_table)

	// Compute requested modules (sorted by source offset).
	extract_requested_modules(scope, module_context, callbacks)

	// Compile module body.
	if has_top_level_await {
		tla_exec := compile_module_as_async(&program, scope, vm_ptr, source_code_ptr,
			source.len, function_table)
		return CompileModuleResult{
			exec_ptr:     unsafe { nil }
			tla_exec_ptr: tla_exec
		}
	} else {
		mut gen := new_program_generator(true, vm_ptr, source_code_ptr, source.len)
		gen.function_table = function_table
		exec := compile_program_body(mut gen, &program, scope, vm_ptr, source_code_ptr)
		return CompileModuleResult{
			exec_ptr:     exec
			tla_exec_ptr: unsafe { nil }
		}
	}
}

struct CompileModuleResult {
	exec_ptr     voidptr
	tla_exec_ptr voidptr
}

// Extract import/export metadata from a module's scope and call callbacks.
fn extract_module_metadata(scope &ScopeData, ctx voidptr, cb &ModuleCallbacks) {
	// Collect all import entries with their module requests.
	mut all_import_entries := []ImportEntryWithRequest{}

	for child in scope.children {
		if child.kind.tag == .import_stmt {
			import_data := child.kind.import_data or { continue }
			for entry in import_data.entries {
				mut in_ptr := unsafe { &u16(nil) }
				mut in_len := 0
				mut is_ns := true
				if in_name := entry.import_name {
					in_ptr = in_name.data.data
					in_len = in_name.data.len
					is_ns = false
				}
				keys, values := build_attribute_slices(import_data.module_request.attributes)
				unsafe {
					cb.push_import_entry(ctx, in_ptr, in_len, is_ns, entry.local_name.data.data,
						entry.local_name.data.len, import_data.module_request.module_specifier.data.data,
						import_data.module_request.module_specifier.data.len, keys.data,
						values.data, keys.len)
				}

				all_import_entries << ImportEntryWithRequest{
					import_name:    entry.import_name
					local_name:     entry.local_name
					module_request: import_data.module_request
				}
			}
		}
	}

	// Process export entries.
	for child in scope.children {
		if child.kind.tag != .export_stmt {
			continue
		}
		export_data := child.kind.export_data or { continue }

		// Handle default export binding name.
		if export_data.is_default_export && export_data.entries.len == 1 {
			entry := export_data.entries[0]
			mut is_declaration := false
			if stmt := export_data.statement {
				if stmt.kind.tag == .function_declaration || stmt.kind.tag == .class_declaration {
					is_declaration = true
				}
			}
			if !is_declaration {
				if lin := entry.local_or_import_name {
					unsafe {
						cb.set_default_export_binding(ctx, lin.data.data, lin.data.len)
					}
				}
			}
		}

		for entry in export_data.entries {
			if entry.kind == .empty_named_export {
				break
			}

			has_module_request := export_data.module_request != none

			if !has_module_request {
				// No module request: check against import entries.
				mut matching_import := ?ImportEntryWithRequest(none)
				if lin := entry.local_or_import_name {
					for ie in all_import_entries {
						if lin == ie.local_name {
							matching_import = ie
							break
						}
					}
				}

				if mie := matching_import {
					if mie.import_name == none {
						// Namespace re-export → local export.
						call_export_callback(cb.push_local_export, ctx, u8(entry.kind),
							entry.export_name, entry.local_or_import_name, ?ModuleRequest(none))
					} else {
						// Re-export of a specific binding → indirect export.
						mr := mie.module_request
						call_export_callback(cb.push_indirect_export, ctx, u8(ExportEntryKind.named_export),
							entry.export_name, mie.import_name, mr)
					}
				} else {
					// Direct local export.
					call_export_callback(cb.push_local_export, ctx, u8(entry.kind), entry.export_name,
						entry.local_or_import_name, ?ModuleRequest(none))
				}
			} else if entry.kind == .module_request_all_but_default {
				// export * from "module"
				call_export_callback(cb.push_star_export, ctx, u8(entry.kind), entry.export_name,
					entry.local_or_import_name, export_data.module_request)
			} else {
				// export { x } from "module" or export { x as y } from "module"
				call_export_callback(cb.push_indirect_export, ctx, u8(entry.kind), entry.export_name,
					entry.local_or_import_name, export_data.module_request)
			}
		}
	}
}

struct ImportEntryWithRequest {
	import_name    ?Utf16String
	local_name     Utf16String
	module_request ModuleRequest
}

// Helper to build FFI attribute arrays from import attributes.
fn build_attribute_slices(attributes []ImportAttribute) ([]FFIUtf16Slice, []FFIUtf16Slice) {
	mut keys := []FFIUtf16Slice{cap: attributes.len}
	mut values := []FFIUtf16Slice{cap: attributes.len}
	for a in attributes {
		keys << FFIUtf16Slice.from_slice(a.key)
		values << FFIUtf16Slice.from_slice(a.value)
	}
	return keys, values
}

// Helper to call an export entry callback with optional module request.
fn call_export_callback(callback ModuleExportEntryCallback, ctx voidptr, kind u8, export_name ?Utf16String, local_or_import_name ?Utf16String, module_request ?ModuleRequest) {
	mut en_ptr := unsafe { &u16(nil) }
	mut en_len := 0
	if en := export_name {
		en_ptr = en.data.data
		en_len = en.data.len
	}
	mut lin_ptr := unsafe { &u16(nil) }
	mut lin_len := 0
	if lin := local_or_import_name {
		lin_ptr = lin.data.data
		lin_len = lin.data.len
	}

	if mr := module_request {
		keys, values := build_attribute_slices(mr.attributes)
		unsafe {
			callback(ctx, kind, en_ptr, en_len, lin_ptr, lin_len, mr.module_specifier.data.data,
				mr.module_specifier.data.len, keys.data, values.data, keys.len)
		}
	} else {
		unsafe {
			callback(ctx, kind, en_ptr, en_len, lin_ptr, lin_len, &u16(nil), 0, nil, nil,
				0)
		}
	}
}

// Extract var declared names and lexical bindings from a module scope.
fn extract_module_declarations(scope &ScopeData, vm_ptr voidptr, source_code_ptr voidptr, ctx voidptr, cb &ModuleCallbacks, mut function_table FunctionTable) {
	default_name := Utf16String.from_slice(utf16('*default*'))

	// Var declared names (walk all nesting levels).
	for child in scope.children {
		mut var_names := []Utf16String{}
		collect_module_var_names_to_array(child.kind, mut var_names)
		for name in var_names {
			unsafe {
				cb.push_var_name(ctx, name.data.data, name.data.len)
			}
		}
	}

	// Lexical bindings and functions to initialize.
	mut function_count := i32(0)
	for ci in 0 .. scope.children.len {
		mut decl_tag := scope.children[ci].kind.tag
		mut is_exported := false
		mut export_stmt := ?&Statement(none)
		if decl_tag == .export_stmt {
			if ed := scope.children[ci].kind.export_data {
				if stmt := ed.statement {
					decl_tag = stmt.kind.tag
					is_exported = true
					export_stmt = stmt
				} else {
					continue
				}
			} else {
				continue
			}
		}

		match decl_tag {
			.function_declaration {
				mut fid := FunctionId{}
				mut func_name_ident := ?&Identifier(none)
				if es := export_stmt {
					fid = es.kind.function_id
					func_name_ident = es.kind.func_name
				} else {
					fid = scope.children[ci].kind.function_id
					func_name_ident = scope.children[ci].kind.func_name
				}
				is_default := is_exported
					&& if n := func_name_ident { n.name == default_name } else { false }

				function_data := function_table.take(fid)
				subtable := function_table.extract_reachable(&function_data)
				sfd_ptr := create_sfd_for_gdi(function_data, subtable, vm_ptr, source_code_ptr,
					true)
				if sfd_ptr == unsafe { nil } {
					continue
				}

				mut binding_name := Utf16String{}
				if n := func_name_ident {
					binding_name = n.name.clone()
				} else {
					continue
				}

				mut sfd_name := binding_name.clone()
				if is_default {
					sfd_display_name := Utf16String.from_slice(utf16('default'))
					module_sfd_set_name(sfd_ptr, sfd_display_name.data.data, sfd_display_name.data.len)
					sfd_name = sfd_display_name
				}

				function_index := function_count
				unsafe {
					cb.push_function(ctx, sfd_ptr, sfd_name.data.data, sfd_name.data.len)
				}
				function_count += 1

				unsafe {
					cb.push_lexical_binding(ctx, binding_name.data.data, binding_name.data.len,
						false, function_index)
				}
			}
			.class_declaration {
				mut cd_opt := ?&ClassData(none)
				if es := export_stmt {
					cd_opt = es.kind.class_data
				} else {
					cd_opt = scope.children[ci].kind.class_data
				}
				if cd := cd_opt {
					if name_ident := cd.name {
						unsafe {
							cb.push_lexical_binding(ctx, name_ident.name.data.data, name_ident.name.data.len,
								false, -1)
						}
					}
				}
			}
			.variable_declaration {
				mut decl_kind := DeclarationKind.var_kind
				mut declarations := []VariableDeclarator{}
				if es := export_stmt {
					decl_kind = es.kind.decl_kind
					declarations = es.kind.declarations.clone()
				} else {
					decl_kind = scope.children[ci].kind.decl_kind
					declarations = scope.children[ci].kind.declarations.clone()
				}
				if decl_kind != .var_kind {
					is_constant := decl_kind == .const_kind
					for decl in declarations {
						mut names := []Utf16String{}
						collect_bound_names(decl.target, mut names)
						for name in names {
							unsafe {
								cb.push_lexical_binding(ctx, name.data.data, name.data.len,
									is_constant, -1)
							}
						}
					}
				}
			}
			.using_declaration {
				mut declarations := []VariableDeclarator{}
				if es := export_stmt {
					declarations = es.kind.declarations.clone()
				} else {
					declarations = scope.children[ci].kind.declarations.clone()
				}
				for decl in declarations {
					mut names := []Utf16String{}
					collect_bound_names(decl.target, mut names)
					for name in names {
						unsafe {
							cb.push_lexical_binding(ctx, name.data.data, name.data.len,
								false, -1)
						}
					}
				}
			}
			else {}
		}
	}
}

// Recursively collect var declared names for module scope into an array.
fn collect_module_var_names_to_array(statement StatementKind, mut result []Utf16String) {
	if statement.tag == .variable_declaration && statement.decl_kind == .var_kind {
		for decl in statement.declarations {
			collect_bound_names(decl.target, mut result)
		}
		return
	}
	if statement.tag == .export_stmt {
		if ed := statement.export_data {
			if stmt := ed.statement {
				collect_module_var_names_to_array(stmt.kind, mut result)
			}
		}
		return
	}
	lib_for_each_child_statement(statement, fn [mut result] (child &StatementKind) {
		collect_module_var_names_to_array(child, mut result)
	})
}

// Extract requested modules sorted by source offset.
fn extract_requested_modules(scope &ScopeData, ctx voidptr, cb &ModuleCallbacks) {
	mut modules := []RequestedModule{}

	for child in scope.children {
		if child.kind.tag == .import_stmt {
			if import_data := child.kind.import_data {
				modules << RequestedModule{
					source_offset: child.range.start.offset
					specifier:     import_data.module_request.module_specifier.clone()
					attributes:    import_data.module_request.attributes.clone()
				}
			}
		} else if child.kind.tag == .export_stmt {
			if export_data := child.kind.export_data {
				if mr := export_data.module_request {
					modules << RequestedModule{
						source_offset: child.range.start.offset
						specifier:     mr.module_specifier.clone()
						attributes:    mr.attributes.clone()
					}
				}
			}
		}
	}

	// Sort by source offset (spec requirement).
	modules.sort(a.source_offset < b.source_offset)

	for m in modules {
		keys, values := build_attribute_slices(m.attributes)
		unsafe {
			cb.push_requested_module(ctx, m.specifier.data.data, m.specifier.data.len,
				keys.data, values.data, keys.len)
		}
	}
}

struct RequestedModule {
	source_offset u32
	specifier     Utf16String
	attributes    []ImportAttribute
}

// Compile a module body as an async function (for TLA modules).
fn compile_module_as_async(program &Statement, scope &ScopeData, vm_ptr voidptr, source_code_ptr voidptr, source_len int, function_table FunctionTable) voidptr {
	mut gen := Generator.new()
	gen.strict = true
	gen.function_table = function_table
	gen.vm_ptr = vm_ptr
	gen.source_code_ptr = source_code_ptr
	gen.source_len = source_len
	gen.enclosing_function_kind = .async_kind

	gen.local_variables = convert_local_variables(scope)

	entry_block := gen.make_block()
	gen.switch_to_basic_block(entry_block)

	// Async function start: emit initial Yield before GetLexicalEnvironment.
	start_block := gen.make_block()
	undef := gen.add_constant_undefined()
	gen.emit(Instruction{
		tag:                .yield_op
		continuation_label: start_block
		value:              undef.operand
	})
	gen.switch_to_basic_block(start_block)

	// Get lexical environment.
	env_reg := gen.scoped_operand(Operand.register(register_saved_lexical_environment))
	gen.emit(Instruction{
		tag: .get_lexical_environment
		dst: env_reg.operand
	})
	gen.lexical_environment_register_stack << env_reg

	// Generate module body statements.
	_ = generate_statement(program, mut gen)

	// Async function end: emit final Yield (no continuation = done).
	if !gen.is_current_block_terminated() {
		undef2 := gen.add_constant_undefined()
		gen.emit(Instruction{
			tag:   .yield_op
			value: undef2.operand
		})
	}

	gen.terminate_unterminated_blocks_with_yield()

	assembled := gen.assemble()
	return create_executable(gen, assembled, vm_ptr, source_code_ptr)
}

// =============================================================================
// GDI/EDI metadata extraction
// =============================================================================

// Recursively collect var-declared names from a statement and all nested
// statements, excluding function/class bodies (which create new var scopes).
fn collect_var_names_recursive(statement &StatementKind, mut result []Utf16String) {
	if statement.tag == .variable_declaration && statement.decl_kind == .var_kind {
		for decl in statement.declarations {
			collect_bound_names(decl.target, mut result)
		}
		return
	}
	lib_for_each_child_statement(statement, fn [mut result] (child &StatementKind) {
		collect_var_names_recursive(child, mut result)
	})
}

// Collect bound names from a target into an array.
fn collect_bound_names(target VariableDeclaratorTarget, mut result []Utf16String) {
	match target {
		IdentifierVarTarget {
			result << target.identifier.name.clone()
		}
		BindingPatternVarTarget {
			collect_bound_names_in_pattern(target.pattern, mut result)
		}
	}
}

fn collect_bound_names_in_pattern(pattern BindingPattern, mut result []Utf16String) {
	for entry in pattern.entries {
		match entry.alias.kind {
			.none_kind {
				if entry.name.kind == .identifier_kind {
					if id := entry.name.identifier {
						result << id.name.clone()
					}
				}
			}
			.identifier_kind {
				if id := entry.alias.identifier {
					result << id.name.clone()
				}
			}
			.binding_pattern_kind {
				if inner := entry.alias.binding_pattern {
					collect_bound_names_in_pattern(inner, mut result)
				}
			}
			.member_expression_kind {}
		}
	}
}

// Collected GDI data for processing by callers.
struct GdiData {
mut:
	var_names         []Utf16String
	functions_to_init []FunctionToInitEntry
	var_scoped_names  []Utf16String
	annexb_names      []Utf16String
	lexical_bindings  []LexicalBindingEntry
}

struct FunctionToInitEntry {
mut:
	function_id FunctionId
	name        Utf16String
	sfd_ptr     voidptr
}

struct LexicalBindingEntry {
	name        Utf16String
	is_constant bool
}

// Collect var names + function declaration names, deduplicated function
// initializations, var-scoped names, annex B names, and lexical bindings.
fn extract_gdi_common(scope &ScopeData, vm_ptr voidptr, source_code_ptr voidptr, is_strict bool, mut function_table FunctionTable) GdiData {
	mut data := GdiData{}

	// Var names (var declarations at any nesting level + top-level function declarations)
	for child in scope.children {
		collect_var_names_recursive(&child.kind, mut data.var_names)
		if child.kind.tag == .function_declaration {
			if name := child.kind.func_name {
				data.var_names << name.name.clone()
			}
		}
	}

	// Functions to initialize (reverse order, deduplicated by name).
	mut seen_names := map[string]bool{}
	for i := scope.children.len - 1; i >= 0; i-- {
		child := scope.children[i]
		if child.kind.tag == .function_declaration {
			if name := child.kind.func_name {
				key := name.name.str()
				if key !in seen_names {
					seen_names[key] = true
					function_data := function_table.take(child.kind.function_id)
					subtable := function_table.extract_reachable(&function_data)
					sfd_ptr := create_sfd_for_gdi(function_data, subtable, vm_ptr, source_code_ptr,
						is_strict)
					data.functions_to_init << FunctionToInitEntry{
						function_id: child.kind.function_id
						name:        name.name.clone()
						sfd_ptr:     sfd_ptr
					}
				}
			}
		}
	}

	// Var-scoped names (var VariableDeclaration names, excluding function declarations)
	for child in scope.children {
		collect_var_names_recursive(&child.kind, mut data.var_scoped_names)
	}

	for name in scope.annexb_function_names {
		data.annexb_names << name.clone()
	}

	for child in scope.children {
		match child.kind.tag {
			.variable_declaration {
				if child.kind.decl_kind != .var_kind {
					is_constant := child.kind.decl_kind == .const_kind
					for decl in child.kind.declarations {
						collect_bound_names_as_lexical(decl.target, is_constant, mut data.lexical_bindings)
					}
				}
			}
			.using_declaration {
				for decl in child.kind.declarations {
					collect_bound_names_as_lexical(decl.target, false, mut data.lexical_bindings)
				}
			}
			.class_declaration {
				if cd := child.kind.class_data {
					if name := cd.name {
						data.lexical_bindings << LexicalBindingEntry{
							name:        name.name.clone()
							is_constant: false
						}
					}
				}
			}
			else {}
		}
	}

	return data
}

fn collect_bound_names_as_lexical(target VariableDeclaratorTarget, is_constant bool, mut result []LexicalBindingEntry) {
	match target {
		IdentifierVarTarget {
			result << LexicalBindingEntry{
				name:        target.identifier.name.clone()
				is_constant: is_constant
			}
		}
		BindingPatternVarTarget {
			collect_bound_names_in_pattern_as_lexical(target.pattern, is_constant, mut
				result)
		}
	}
}

fn collect_bound_names_in_pattern_as_lexical(pattern BindingPattern, is_constant bool, mut result []LexicalBindingEntry) {
	for entry in pattern.entries {
		match entry.alias.kind {
			.none_kind {
				if entry.name.kind == .identifier_kind {
					if id := entry.name.identifier {
						result << LexicalBindingEntry{
							name:        id.name.clone()
							is_constant: is_constant
						}
					}
				}
			}
			.identifier_kind {
				if id := entry.alias.identifier {
					result << LexicalBindingEntry{
						name:        id.name.clone()
						is_constant: is_constant
					}
				}
			}
			.binding_pattern_kind {
				if inner := entry.alias.binding_pattern {
					collect_bound_names_in_pattern_as_lexical(inner, is_constant, mut
						result)
				}
			}
			.member_expression_kind {}
		}
	}
}

// Extract EDI metadata from a program-level ScopeData.
fn extract_eval_gdi(scope &ScopeData, is_strict bool, vm_ptr voidptr, source_code_ptr voidptr, ctx voidptr, mut function_table FunctionTable) {
	eval_gdi_set_strict(ctx, is_strict)

	gdi := extract_gdi_common(scope, vm_ptr, source_code_ptr, is_strict, mut function_table)

	for name in gdi.var_names {
		eval_gdi_push_var_name(ctx, name.data.data, name.data.len)
	}
	for fti in gdi.functions_to_init {
		eval_gdi_push_function(ctx, fti.sfd_ptr, fti.name.data.data, fti.name.data.len)
	}
	for name in gdi.var_scoped_names {
		eval_gdi_push_var_scoped_name(ctx, name.data.data, name.data.len)
	}
	for name in gdi.annexb_names {
		eval_gdi_push_annex_b_name(ctx, name.data.data, name.data.len)
	}
	for lb in gdi.lexical_bindings {
		eval_gdi_push_lexical_binding(ctx, lb.name.data.data, lb.name.data.len, lb.is_constant)
	}
}

// Extract GDI metadata from a program-level ScopeData.
fn extract_script_gdi(scope &ScopeData, is_strict bool, vm_ptr voidptr, source_code_ptr voidptr, ctx voidptr, mut function_table FunctionTable) {
	// Lexical names (let/const/using/class at top level) — script-only step.
	for child in scope.children {
		match child.kind.tag {
			.variable_declaration {
				if child.kind.decl_kind != .var_kind {
					for decl in child.kind.declarations {
						mut names := []Utf16String{}
						collect_bound_names(decl.target, mut names)
						for name in names {
							script_gdi_push_lexical_name(ctx, name.data.data, name.data.len)
						}
					}
				}
			}
			.using_declaration {
				for decl in child.kind.declarations {
					mut names := []Utf16String{}
					collect_bound_names(decl.target, mut names)
					for name in names {
						script_gdi_push_lexical_name(ctx, name.data.data, name.data.len)
					}
				}
			}
			.class_declaration {
				if cd := child.kind.class_data {
					if name := cd.name {
						script_gdi_push_lexical_name(ctx, name.name.data.data, name.name.data.len)
					}
				}
			}
			else {}
		}
	}

	gdi := extract_gdi_common(scope, vm_ptr, source_code_ptr, is_strict, mut function_table)

	for name in gdi.var_names {
		script_gdi_push_var_name(ctx, name.data.data, name.data.len)
	}
	for fti in gdi.functions_to_init {
		script_gdi_push_function(ctx, fti.sfd_ptr, fti.name.data.data, fti.name.data.len)
	}
	for name in gdi.var_scoped_names {
		script_gdi_push_var_scoped_name(ctx, name.data.data, name.data.len)
	}
	for name in gdi.annexb_names {
		script_gdi_push_annex_b_name(ctx, name.data.data, name.data.len)
	}
	for lb in gdi.lexical_bindings {
		script_gdi_push_lexical_binding(ctx, lb.name.data.data, lb.name.data.len, lb.is_constant)
	}
}

// =============================================================================
// AST walking utilities
// =============================================================================

// Visit each child statement of a statement, excluding function/class bodies
// (which create new var scopes).
fn lib_for_each_child_statement(statement &StatementKind, f fn (&StatementKind)) {
	match statement.tag {
		.block {
			if sc := statement.scope {
				for child in sc.children {
					f(&child.kind)
				}
			}
		}
		.if_stmt {
			if cons := statement.consequent {
				f(&cons.kind)
			}
			if alt := statement.alternate {
				f(&alt.kind)
			}
		}
		.while_stmt, .do_while, .with_stmt {
			if b := statement.body {
				f(&b.kind)
			}
		}
		.for_stmt {
			if fi := statement.for_init {
				match fi {
					ForInitDeclaration {
						f(&fi.statement.kind)
					}
					ForInitExpression {}
				}
			}
			if b := statement.body {
				f(&b.kind)
			}
		}
		.for_in_of {
			if lhs := statement.for_in_of_lhs {
				match lhs {
					ForInOfLhsDeclaration {
						f(&lhs.statement.kind)
					}
					ForInOfLhsExpression {}
					ForInOfLhsPattern {}
				}
			}
			if b := statement.body {
				f(&b.kind)
			}
		}
		.switch_stmt {
			if sd := statement.switch_data {
				for case_ in sd.cases {
					for child in case_.scope.children {
						f(&child.kind)
					}
				}
			}
		}
		.try_stmt {
			if td := statement.try_data {
				f(&td.block.kind)
				if handler := td.handler {
					f(&handler.body.kind)
				}
				if finalizer := td.finalizer {
					f(&finalizer.kind)
				}
			}
		}
		.labelled {
			if item := statement.labelled_item {
				f(&item.kind)
			}
		}
		else {}
	}
}

fn lib_for_each_bound_name(target VariableDeclaratorTarget, f fn ([]u16)) {
	match target {
		IdentifierVarTarget {
			f(target.identifier.name.data)
		}
		BindingPatternVarTarget {
			lib_for_each_bound_name_in_pattern(target.pattern, f)
		}
	}
}

fn lib_for_each_bound_name_in_pattern(pattern BindingPattern, f fn ([]u16)) {
	for entry in pattern.entries {
		match entry.alias.kind {
			.none_kind {
				if entry.name.kind == .identifier_kind {
					if id := entry.name.identifier {
						f(id.name.data)
					}
				}
			}
			.identifier_kind {
				if id := entry.alias.identifier {
					f(id.name.data)
				}
			}
			.binding_pattern_kind {
				if inner := entry.alias.binding_pattern {
					lib_for_each_bound_name_in_pattern(inner, f)
				}
			}
			.member_expression_kind {}
		}
	}
}

// Collect all identifiers from a binding pattern into an array.
fn collect_binding_pattern_identifiers(pattern BindingPattern, mut result []&Identifier) {
	for entry in pattern.entries {
		match entry.alias.kind {
			.identifier_kind {
				if id := entry.alias.identifier {
					result << id
				}
			}
			.binding_pattern_kind {
				if sub := entry.alias.binding_pattern {
					collect_binding_pattern_identifiers(sub, mut result)
				}
			}
			.none_kind {
				if entry.name.kind == .identifier_kind {
					if id := entry.name.identifier {
						result << id
					}
				}
			}
			.member_expression_kind {}
		}
	}
}

// =============================================================================
// FFI entry point: function compilation
// =============================================================================

// Compile a function body.
fn compile_function(vm_ptr voidptr, source_code_ptr voidptr, source_len int, sfd_ptr voidptr, payload FunctionPayload, builtin_abstract_operations_enabled bool) voidptr {
	function_data := payload.data

	mut body_scope := ?&ScopeData(none)
	if function_data.body.kind.tag == .function_body {
		body_scope = function_data.body.kind.scope
	} else if function_data.body.kind.tag == .block {
		body_scope = function_data.body.kind.scope
	}

	// Compute SFD metadata before codegen.
	sfd_metadata := compute_sfd_metadata(function_data)

	mut gen := Generator.new()
	gen.strict = function_data.is_strict_mode
	gen.function_environment_needed = sfd_metadata.function_environment_needed
	gen.builtin_abstract_operations_enabled = builtin_abstract_operations_enabled
	gen.function_table = payload.function_table
	gen.vm_ptr = vm_ptr
	gen.source_code_ptr = source_code_ptr
	gen.source_len = source_len
	gen.enclosing_function_kind = function_data.kind

	if bs := body_scope {
		gen.local_variables = convert_local_variables(bs)
	}

	entry_block := gen.make_block()
	gen.switch_to_basic_block(entry_block)

	// For async (non-generator) functions, emit the initial Yield BEFORE
	// GetLexicalEnvironment.
	if gen.is_in_async_function() && !gen.is_in_generator_function() {
		start_block := gen.make_block()
		undef := gen.add_constant_undefined()
		gen.emit(Instruction{
			tag:                .yield_op
			continuation_label: start_block
			value:              undef.operand
		})
		gen.switch_to_basic_block(start_block)
	}

	env_reg := gen.scoped_operand(Operand.register(register_saved_lexical_environment))
	gen.emit(Instruction{
		tag: .get_lexical_environment
		dst: env_reg.operand
	})
	gen.lexical_environment_register_stack << env_reg

	if bs := body_scope {
		emit_function_declaration_instantiation(mut gen, bs, &function_data, function_data.is_strict_mode)
	}

	// For generator functions, emit the initial Yield AFTER FDI.
	if gen.is_in_generator_function() {
		start_block := gen.make_block()
		undef := gen.add_constant_undefined()
		gen.emit(Instruction{
			tag:                .yield_op
			continuation_label: start_block
			value:              undef.operand
		})
		gen.switch_to_basic_block(start_block)
	}

	result := generate_statement(&function_data.body, mut gen)

	if !gen.is_current_block_terminated() {
		if gen.is_in_generator_or_async_function() {
			undef := gen.add_constant_undefined()
			gen.emit(Instruction{
				tag:   .yield_op
				value: undef.operand
			})
		} else if res := result {
			gen.emit(Instruction{
				tag:   .end_op
				value: res.operand
			})
		}
	}

	if gen.is_in_generator_or_async_function() {
		gen.terminate_unterminated_blocks_with_yield()
	}

	assembled := gen.assemble()

	write_sfd_metadata(sfd_ptr, sfd_metadata)

	return create_executable(gen, assembled, vm_ptr, source_code_ptr)
}

// =============================================================================
// SFD metadata computation (ECMA-262 section 10.2.11)
// =============================================================================

// Metadata computed from scope analysis for a SharedFunctionInstanceData.
struct SfdMetadata {
	uses_this                           bool
	function_environment_needed         bool
	function_environment_bindings_count int
	might_need_arguments                bool
	contains_eval                       bool
}

// Intermediate scope analysis data extracted from the function body scope.
struct BodyScopeInfo {
	uses_this                                     bool
	contains_eval                                 bool
	uses_this_from_env                            bool
	might_need_arguments                          bool
	has_function_named_arguments                  bool
	has_lexically_declared_arguments              bool
	non_local_var_count                           int
	non_local_var_count_for_parameter_expressions int
	var_names                                     []Utf16String
	annexb_function_names                         []Utf16String
	has_arguments_object_local                    bool
}

// Compute FDI runtime metadata.
fn compute_sfd_metadata(function_data JsFunctionData) SfdMetadata {
	mut body_scope := ?&ScopeData(none)
	if function_data.body.kind.tag == .function_body {
		body_scope = function_data.body.kind.scope
	}

	strict := function_data.is_strict_mode
	is_arrow := function_data.is_arrow_function

	// Extract all scope analysis data.
	bsi := if bs := body_scope {
		fsd := bs.function_scope_data
		BodyScopeInfo{
			uses_this:                                     bs.uses_this
				|| function_data.parsing_insights.uses_this
			contains_eval:                                 bs.contains_direct_call_to_eval
			uses_this_from_env:                            bs.uses_this_from_environment
				|| function_data.parsing_insights.uses_this_from_environment
			might_need_arguments:                          function_data.parsing_insights.might_need_arguments_object
			has_function_named_arguments:                  if f := fsd {
				f.has_function_named_arguments
			} else {
				false
			}
			has_lexically_declared_arguments:              if f := fsd {
				f.has_lexically_declared_arguments
			} else {
				false
			}
			non_local_var_count:                           if f := fsd {
				f.non_local_var_count
			} else {
				0
			}
			non_local_var_count_for_parameter_expressions: if f := fsd {
				f.non_local_var_count_for_parameter_expressions
			} else {
				0
			}
			var_names:                                     if f := fsd {
				f.var_names.clone()
			} else {
				[]Utf16String{}
			}
			annexb_function_names:                         bs.annexb_function_names.clone()
			has_arguments_object_local:                    bs.local_variables.any(it.kind == .arguments_object)
		}
	} else {
		BodyScopeInfo{
			uses_this:                                     function_data.parsing_insights.uses_this
			contains_eval:                                 function_data.parsing_insights.contains_direct_call_to_eval
			uses_this_from_env:                            function_data.parsing_insights.uses_this_from_environment
			might_need_arguments:                          function_data.parsing_insights.might_need_arguments_object
			has_function_named_arguments:                  false
			has_lexically_declared_arguments:              false
			non_local_var_count:                           0
			non_local_var_count_for_parameter_expressions: 0
			var_names:                                     []Utf16String{}
			annexb_function_names:                         []Utf16String{}
			has_arguments_object_local:                    false
		}
	}

	// §10.2.11 step 4: check for parameter expressions.
	mut has_parameter_expressions := false
	for p in function_data.parameters {
		if p.default_value != none {
			has_parameter_expressions = true
			break
		}
		match p.binding {
			BindingPatternBinding {
				has_parameter_expressions = true
				break
			}
			IdentifierBinding {}
		}
	}

	// §10.2.11 steps 5-8: count non-local unique parameter names.
	mut parameter_names := map[string]bool{}
	mut parameters_in_environment := 0
	arguments_str := Utf16String.from_slice(utf16('arguments'))
	mut has_arguments_parameter := false
	for parameter in function_data.parameters {
		match parameter.binding {
			IdentifierBinding {
				key := parameter.binding.identifier.name.str()
				if key !in parameter_names {
					parameter_names[key] = true
					if !parameter.binding.identifier.is_local() {
						parameters_in_environment += 1
					}
					if parameter.binding.identifier.name == arguments_str {
						has_arguments_parameter = true
					}
				}
			}
			BindingPatternBinding {
				mut idents := []&Identifier{}
				collect_binding_pattern_identifiers(parameter.binding.pattern, mut idents)
				for ident in idents {
					key := ident.name.str()
					if key !in parameter_names {
						parameter_names[key] = true
						if !ident.is_local() {
							parameters_in_environment += 1
						}
					}
				}
			}
		}
	}

	// §10.2.11 steps 15-18: determine if arguments object is needed.
	arguments_object_needed := bsi.might_need_arguments && !is_arrow && !has_arguments_parameter
		&& body_scope != none && (has_parameter_expressions || !bsi.has_function_named_arguments)
		&& (has_parameter_expressions || !bsi.has_lexically_declared_arguments)

	// Arguments object needs an environment binding if it's not a local variable.
	arguments_object_needs_binding := arguments_object_needed && !bsi.has_arguments_object_local

	mut function_environment_bindings_count := 0
	mut var_environment_bindings_count := 0
	mut lex_environment_bindings_count := 0

	// §10.2.11 step 19: route parameter bindings.
	env_is_function_env := strict || !has_parameter_expressions
	if env_is_function_env {
		function_environment_bindings_count += parameters_in_environment
	}

	// §10.2.11 step 22: arguments binding.
	if arguments_object_needs_binding && env_is_function_env {
		function_environment_bindings_count += 1
	}

	if bs := body_scope {
		if !has_parameter_expressions {
			// §10.2.11 step 27: var env shares function env.
			function_environment_bindings_count += bsi.non_local_var_count

			// Annex B: function names hoisted from blocks that aren't already vars.
			if !strict {
				for name in bsi.annexb_function_names {
					if name !in bsi.var_names {
						function_environment_bindings_count += 1
					}
				}
			}

			// §10.2.11 step 30: lexical environment.
			non_local_lex_count := count_non_local_lex_declarations(bs)
			if strict {
				function_environment_bindings_count += non_local_lex_count
			} else {
				can_elide := !bsi.contains_eval && non_local_lex_count == 0
				if !can_elide {
					lex_environment_bindings_count += non_local_lex_count
				}
			}
		} else {
			// §10.2.11 step 28: separate var environment.
			var_environment_bindings_count += bsi.non_local_var_count_for_parameter_expressions

			if !strict {
				for name in bsi.annexb_function_names {
					if name !in bsi.var_names {
						var_environment_bindings_count += 1
					}
				}
			}

			non_local_lex_count := count_non_local_lex_declarations(bs)
			if strict {
				var_environment_bindings_count += non_local_lex_count
			} else {
				can_elide := !bsi.contains_eval && non_local_lex_count == 0
				if !can_elide {
					lex_environment_bindings_count += non_local_lex_count
				}
			}
		}
	}

	function_environment_needed := arguments_object_needs_binding
		|| function_environment_bindings_count > 0 || var_environment_bindings_count > 0
		|| lex_environment_bindings_count > 0 || bsi.uses_this_from_env || bsi.contains_eval

	return SfdMetadata{
		uses_this:                           bsi.uses_this
		function_environment_needed:         function_environment_needed
		function_environment_bindings_count: function_environment_bindings_count
		might_need_arguments:                bsi.might_need_arguments
		contains_eval:                       bsi.contains_eval
	}
}

// Write precomputed SFD metadata to a SharedFunctionInstanceData via FFI.
fn write_sfd_metadata(sfd_ptr voidptr, metadata SfdMetadata) {
	rust_sfd_set_metadata(sfd_ptr, metadata.uses_this, metadata.function_environment_needed,
		metadata.function_environment_bindings_count, metadata.might_need_arguments, metadata.contains_eval)
}

// Count non-local lexically-declared identifiers in a function body scope.
fn count_non_local_lex_declarations(scope &ScopeData) int {
	mut count := 0
	for child in scope.children {
		match child.kind.tag {
			.variable_declaration {
				if child.kind.decl_kind == .let_kind || child.kind.decl_kind == .const_kind {
					for decl in child.kind.declarations {
						count_non_local_names_in_target(decl.target, mut &count)
					}
				}
			}
			.using_declaration {
				for decl in child.kind.declarations {
					count_non_local_names_in_target(decl.target, mut &count)
				}
			}
			.class_declaration {
				if cd := child.kind.class_data {
					if name_ident := cd.name {
						if !name_ident.is_local() {
							count += 1
						}
					}
				}
			}
			else {}
		}
	}
	return count
}

fn count_non_local_names_in_target(target VariableDeclaratorTarget, mut count &int) {
	match target {
		IdentifierVarTarget {
			if !target.identifier.is_local() {
				count += 1
			}
		}
		BindingPatternVarTarget {
			count_non_local_names_in_binding_pattern(target.pattern, mut count)
		}
	}
}

fn count_non_local_names_in_binding_pattern(pattern BindingPattern, mut count &int) {
	for entry in pattern.entries {
		match entry.alias.kind {
			.identifier_kind {
				if id := entry.alias.identifier {
					if !id.is_local() {
						count += 1
					}
				}
			}
			.binding_pattern_kind {
				if sub := entry.alias.binding_pattern {
					count_non_local_names_in_binding_pattern(sub, mut count)
				}
			}
			.none_kind {
				if entry.name.kind == .identifier_kind {
					if id := entry.name.identifier {
						if !id.is_local() {
							count += 1
						}
					}
				}
			}
			.member_expression_kind {}
		}
	}
}

// FFI functions are now in ffi.v
