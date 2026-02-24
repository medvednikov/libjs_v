module main

import math

// =============================================================================
// Codegen enums and types
// =============================================================================

enum CompletionType {
	normal    = 1
	return_ct = 4
	throw_ct  = 5
}

enum EnvironmentMode {
	lexical = 0
	var_env = 1
}

enum ArgumentsKind {
	mapped   = 0
	unmapped = 1
}

enum IteratorHint {
	sync_iter  = 0
	async_iter = 1
}

enum BindingMode {
	initialize_lexical
	set_binding
}

enum PutKind {
	own
	getter
	setter
}

// EvaluatedReference represents a reference for assignment targets.
struct EvaluatedReference {
mut:
	kind         EvaluatedReferenceKind
	base         ScopedOperand
	this_value   ScopedOperand
	identifier   IdentifierTableIndex
	property_key PropertyKeyTableIndex
	property_op  ScopedOperand
	cache        EnvironmentCoordinate
	cache_index  u32
	super_base   ScopedOperand
	priv_id      IdentifierTableIndex
	has_this     bool
}

enum EvaluatedReferenceKind {
	by_id
	by_id_with_this
	by_value
	by_value_with_this
	private_by_id
	super_by_value
	variable
}

// FdiParameterName is used during function declaration instantiation.
struct FdiParameterName {
mut:
	name     Utf16String
	is_local bool
}

// =============================================================================
// Builtin IDs matching JS_ENUMERATE_BUILTINS in Builtins.h
// =============================================================================

const builtin_math_abs = u8(0)
const builtin_math_log = u8(1)
const builtin_math_pow = u8(2)
const builtin_math_exp = u8(3)
const builtin_math_ceil = u8(4)
const builtin_math_floor = u8(5)
const builtin_math_imul = u8(6)
const builtin_math_random = u8(7)
const builtin_math_round = u8(8)
const builtin_math_sqrt = u8(9)
const builtin_math_sin = u8(10)
const builtin_math_cos = u8(11)
const builtin_math_tan = u8(12)
const builtin_regexp_prototype_exec = u8(13)
const builtin_regexp_prototype_replace = u8(14)
const builtin_regexp_prototype_split = u8(15)
const builtin_ordinary_has_instance = u8(16)
const builtin_array_iterator_prototype_next = u8(17)
const builtin_map_iterator_prototype_next = u8(18)
const builtin_set_iterator_prototype_next = u8(19)
const builtin_string_iterator_prototype_next = u8(20)

// =============================================================================
// NaN-boxing constants
// =============================================================================

const nanbox_tag_shift = u64(48)
const nanbox_base_tag = u64(0x7FF8)
const nanbox_int32_tag = u64(0b010) | nanbox_base_tag
const nanbox_boolean_tag = u64(0b001) | nanbox_base_tag
const nanbox_null_tag = u64(0b111) | nanbox_base_tag
const nanbox_empty_tag = u64(0b011) | nanbox_base_tag
const negative_zero_bits = u64(1) << 63

// FFI functions are now in ffi.v

// =============================================================================
// NaN-boxing helpers
// =============================================================================

fn nanboxed_number(value f64) u64 {
	is_negative_zero := math.f64_bits(value) == negative_zero_bits
	if value >= f64(min_i32) && value <= f64(max_i32) && math.trunc(value) == value
		&& !is_negative_zero {
		return (nanbox_int32_tag << nanbox_tag_shift) | u64(u32(i32(value)))
	} else if math.is_nan(value) {
		return u64(0x7FF8_0000_0000_0000)
	} else {
		return math.f64_bits(value)
	}
}

fn nanboxed_boolean(value bool) u64 {
	return (nanbox_boolean_tag << nanbox_tag_shift) | u64(if value {
		1
	} else {
		0
	})
}

fn nanboxed_null() u64 {
	return nanbox_null_tag << nanbox_tag_shift
}

fn nanboxed_empty() u64 {
	return nanbox_empty_tag << nanbox_tag_shift
}

// =============================================================================
// Numeric conversion helpers
// =============================================================================

const min_i32 = i32(-2147483648)
const max_i32 = i32(2147483647)

fn js_to_int32(n f64) i32 {
	if math.is_nan(n) || math.is_inf(n, 0) || n == 0.0 {
		return 0
	}
	int_val := math.copysign(math.floor(math.abs(n)), n)
	int32bit_ := math.fmod(int_val, 4294967296.0)
	int32bit := if int32bit_ < 0.0 { int32bit_ + 4294967296.0 } else { int32bit_ }
	if int32bit >= 2147483648.0 {
		return i32(int32bit - 4294967296.0)
	} else {
		return i32(int32bit)
	}
}

fn js_to_u32(n f64) u32 {
	if math.is_nan(n) || math.is_inf(n, 0) || n == 0.0 {
		return 0
	}
	int_val := math.copysign(math.floor(math.abs(n)), n)
	int32bit := math.fmod(int_val, 4294967296.0)
	if int32bit < 0.0 {
		return u32(int32bit + 4294967296.0)
	} else {
		return u32(int32bit)
	}
}

fn js_exponentiate(base f64, exponent f64) f64 {
	if math.is_inf(exponent, 0) && math.abs(base) == 1.0 {
		return math.nan()
	}
	return math.pow(base, exponent)
}

fn is_numeric_index_key(key []u16) bool {
	if key.len == 0 {
		return false
	}
	if key[0] == ch(`0`) && key.len > 1 {
		return false
	}
	for c in key {
		if c < ch(`0`) || c > ch(`9`) {
			return false
		}
	}
	mut n := u64(0)
	for c in key {
		n = n * 10 + u64(c - ch(`0`))
		if n > u64(0xFFFFFFFF) {
			return false
		}
	}
	return u32(n) < u32(0xFFFFFFFF)
}

// =============================================================================
// Constant conversion helpers
// =============================================================================

fn constant_to_number(val ConstantValue) ?f64 {
	match val.tag {
		.number { return val.number_value }
		.boolean { return if val.bool_value { 1.0 } else { 0.0 } }
		.null_val { return 0.0 }
		.undefined_val { return math.nan() }
		.string_val { return string_to_number(val.string_value) }
		else { return none }
	}
}

fn constant_to_string(val ConstantValue) ?Utf16String {
	match val.tag {
		.string_val {
			return val.string_value
		}
		.number {
			return ffi_js_number_to_utf16(val.number_value)
		}
		.boolean {
			return if val.bool_value {
				Utf16String.from_slice(utf16('true'))
			} else {
				Utf16String.from_slice(utf16('false'))
			}
		}
		.null_val {
			return Utf16String.from_slice(utf16('null'))
		}
		.undefined_val {
			return Utf16String.from_slice(utf16('undefined'))
		}
		.bigint_val {
			return Utf16String.from_slice(utf16(val.bigint_value))
		}
		else {
			return none
		}
	}
}

fn string_to_number(s Utf16String) f64 {
	text := s.str()
	trimmed := text.trim_space()
	if trimmed.len == 0 {
		return 0.0
	}
	if trimmed == 'Infinity' || trimmed == '+Infinity' {
		return math.inf(1)
	}
	if trimmed == '-Infinity' {
		return math.inf(-1)
	}
	return trimmed.f64()
}

// =============================================================================
// Constant folding
// =============================================================================

fn try_constant_fold_unary(mut gen Generator, op UnaryOp, operand ScopedOperand) ?ScopedOperand {
	cv := gen.get_constant(operand) or { return none }
	match op {
		.minus {
			n := constant_to_number(cv) or { return none }
			return gen.add_constant_number(-n)
		}
		.plus {
			n := constant_to_number(cv) or { return none }
			return gen.add_constant_number(n)
		}
		.bitwise_not {
			if cv.tag == .bigint_val {
				return none
			}
			n := constant_to_number(cv) or { return none }
			return gen.add_constant_number(f64(~js_to_int32(n)))
		}
		.not_op {
			as_bool := constant_to_boolean(cv) or { return none }
			return gen.add_constant_boolean(!as_bool)
		}
		else {
			return none
		}
	}
}

fn try_constant_fold_to_boolean(mut gen Generator, operand ScopedOperand) ?ScopedOperand {
	cv := gen.get_constant(operand) or { return none }
	as_bool := constant_to_boolean(cv) or { return none }
	return gen.add_constant_boolean(as_bool)
}

fn try_constant_fold_binary(mut gen Generator, op BinaryOp, lhs_op ScopedOperand, rhs_op ScopedOperand) ?ScopedOperand {
	lhs_const := gen.get_constant(lhs_op) or { return none }
	rhs_const := gen.get_constant(rhs_op) or { return none }

	match op {
		.addition {
			if lhs_const.tag == .string_val || rhs_const.tag == .string_val {
				a := constant_to_string(lhs_const) or { return none }
				b := constant_to_string(rhs_const) or { return none }
				mut result := a.clone()
				result.data << b.data
				return gen.add_constant_string(result)
			}
			a := constant_to_number(lhs_const) or { return none }
			b := constant_to_number(rhs_const) or { return none }
			return gen.add_constant_number(a + b)
		}
		.subtraction {
			a := constant_to_number(lhs_const) or { return none }
			b := constant_to_number(rhs_const) or { return none }
			return gen.add_constant_number(a - b)
		}
		.multiplication {
			a := constant_to_number(lhs_const) or { return none }
			b := constant_to_number(rhs_const) or { return none }
			return gen.add_constant_number(a * b)
		}
		.division {
			a := constant_to_number(lhs_const) or { return none }
			b := constant_to_number(rhs_const) or { return none }
			return gen.add_constant_number(a / b)
		}
		.modulo {
			a := constant_to_number(lhs_const) or { return none }
			b := constant_to_number(rhs_const) or { return none }
			return gen.add_constant_number(math.fmod(a, b))
		}
		.exponentiation {
			a := constant_to_number(lhs_const) or { return none }
			b := constant_to_number(rhs_const) or { return none }
			return gen.add_constant_number(js_exponentiate(a, b))
		}
		.strictly_equals, .strictly_inequals {
			equal := match lhs_const.tag {
				.number {
					if rhs_const.tag == .number {
						lhs_const.number_value == rhs_const.number_value
					} else {
						false
					}
				}
				.string_val {
					if rhs_const.tag == .string_val {
						lhs_const.string_value == rhs_const.string_value
					} else {
						false
					}
				}
				.boolean {
					if rhs_const.tag == .boolean {
						lhs_const.bool_value == rhs_const.bool_value
					} else {
						false
					}
				}
				.null_val {
					rhs_const.tag == .null_val
				}
				.undefined_val {
					rhs_const.tag == .undefined_val
				}
				else {
					false
				}
			}
			result := if op == .strictly_inequals { !equal } else { equal }
			return gen.add_constant_boolean(result)
		}
		.greater_than, .greater_than_equals, .less_than, .less_than_equals {
			a := constant_to_number(lhs_const) or { return none }
			b := constant_to_number(rhs_const) or { return none }
			result := match op {
				.greater_than { a > b }
				.greater_than_equals { a >= b }
				.less_than { a < b }
				.less_than_equals { a <= b }
				else { false }
			}
			return gen.add_constant_boolean(result)
		}
		.bitwise_and {
			a := constant_to_number(lhs_const) or { return none }
			b := constant_to_number(rhs_const) or { return none }
			return gen.add_constant_number(f64(js_to_int32(a) & js_to_int32(b)))
		}
		.bitwise_or {
			a := constant_to_number(lhs_const) or { return none }
			b := constant_to_number(rhs_const) or { return none }
			return gen.add_constant_number(f64(js_to_int32(a) | js_to_int32(b)))
		}
		.bitwise_xor {
			a := constant_to_number(lhs_const) or { return none }
			b := constant_to_number(rhs_const) or { return none }
			return gen.add_constant_number(f64(js_to_int32(a) ^ js_to_int32(b)))
		}
		.left_shift {
			a := constant_to_number(lhs_const) or { return none }
			b := constant_to_number(rhs_const) or { return none }
			return gen.add_constant_number(f64(js_to_int32(a) << (js_to_u32(b) & 0x1f)))
		}
		.right_shift {
			a := constant_to_number(lhs_const) or { return none }
			b := constant_to_number(rhs_const) or { return none }
			return gen.add_constant_number(f64(js_to_int32(a) >> (js_to_u32(b) & 0x1f)))
		}
		.unsigned_right_shift {
			a := constant_to_number(lhs_const) or { return none }
			b := constant_to_number(rhs_const) or { return none }
			return gen.add_constant_number(f64(js_to_u32(a) >> (js_to_u32(b) & 0x1f)))
		}
		else {
			return none
		}
	}
}

// =============================================================================
// String/identifier helpers
// =============================================================================

fn intern_base_identifier(mut gen Generator, base &Expression) ?IdentifierTableIndex {
	name := expression_identifier(base) or { return none }
	return gen.intern_identifier(name.as_slice())
}

fn expression_identifier(expression &Expression) ?Utf16String {
	match expression.kind.tag {
		.identifier_expr {
			if ident := expression.kind.identifier {
				return ident.name.clone()
			}
			return none
		}
		.string_literal {
			mut result := Utf16String.from_slice(utf16("'"))
			result.data << expression.kind.string_value.data
			result.data << utf16("'")
			return result
		}
		.numeric_literal {
			return ffi_js_number_to_utf16(expression.kind.numeric_value)
		}
		.this_expr {
			return Utf16String.from_slice(utf16('this'))
		}
		.member {
			mut s := Utf16String.new()
			if obj := expression.kind.object {
				if obj_id := expression_identifier(obj) {
					s.data << obj_id.data
				}
			}
			if prop := expression.kind.property {
				if property_id := expression_identifier(prop) {
					if expression.kind.computed {
						s.data << utf16('[')
						s.data << property_id.data
						s.data << utf16(']')
					} else {
						s.data << utf16('.')
						s.data << property_id.data
					}
				}
			}
			return s
		}
		else {
			return none
		}
	}
}

fn expression_string_approximation(expression &Expression) ?Utf16String {
	match expression.kind.tag {
		.identifier_expr {
			if ident := expression.kind.identifier {
				return ident.name.clone()
			}
			return none
		}
		.member {
			return member_to_string_approximation(expression)
		}
		else {
			return none
		}
	}
}

fn member_to_string_approximation(expression &Expression) Utf16String {
	match expression.kind.tag {
		.identifier_expr {
			if ident := expression.kind.identifier {
				return ident.name.clone()
			}
			return Utf16String.from_slice(utf16('<object>'))
		}
		.member {
			mut s := Utf16String.new()
			if obj := expression.kind.object {
				s = member_to_string_approximation(obj)
			}
			if prop := expression.kind.property {
				property_str := member_to_string_approximation(prop)
				if expression.kind.computed {
					s.data << utf16('[')
					s.data << property_str.data
					s.data << utf16(']')
				} else {
					s.data << utf16('.')
					s.data << property_str.data
				}
			}
			return s
		}
		.string_literal {
			mut result := Utf16String.from_slice(utf16("'"))
			result.data << expression.kind.string_value.data
			result.data << utf16("'")
			return result
		}
		.this_expr {
			return Utf16String.from_slice(utf16('this'))
		}
		else {
			return Utf16String.from_slice(utf16('<object>'))
		}
	}
}

// =============================================================================
// Builtin detection
// =============================================================================

fn get_builtin(callee &Expression) ?u8 {
	if callee.kind.tag != .member || callee.kind.computed {
		return none
	}
	obj := callee.kind.object or { return none }
	prop := callee.kind.property or { return none }
	if obj.kind.tag != .identifier_expr || prop.kind.tag != .identifier_expr {
		return none
	}
	base_ident := obj.kind.identifier or { return none }
	property_ident := prop.kind.identifier or { return none }
	base_name := base_ident.name.str()
	prop_name := property_ident.name.str()
	builtins := [
		['Math', 'abs', '0'],
		['Math', 'log', '1'],
		['Math', 'pow', '2'],
		['Math', 'exp', '3'],
		['Math', 'ceil', '4'],
		['Math', 'floor', '5'],
		['Math', 'imul', '6'],
		['Math', 'random', '7'],
		['Math', 'round', '8'],
		['Math', 'sqrt', '9'],
		['Math', 'sin', '10'],
		['Math', 'cos', '11'],
		['Math', 'tan', '12'],
	]
	for b in builtins {
		if base_name == b[0] && prop_name == b[1] {
			return u8(b[2].int())
		}
	}
	return none
}

fn builtin_argument_count(builtin_id_ u8) int {
	return match builtin_id_ {
		0 { 1 } // abs
		1 { 1 } // log
		2 { 2 } // pow
		3 { 1 } // exp
		4 { 1 } // ceil
		5 { 1 } // floor
		6 { 2 } // imul
		7 { 0 } // random
		8 { 1 } // round
		9 { 1 } // sqrt
		10 { 1 } // sin
		11 { 1 } // cos
		12 { 1 } // tan
		else { 0 }
	}
}

// =============================================================================
// Main entry: generate_expression
// =============================================================================

fn generate_expression(expression &Expression, mut gen Generator, preferred_dst ?ScopedOperand) ?ScopedOperand {
	saved_source_start := gen.current_source_start
	saved_source_end := gen.current_source_end
	gen.current_source_start = expression.range.start.offset
	gen.current_source_end = expression.range.end_.offset

	result := generate_expression_inner(expression, mut gen, preferred_dst)

	gen.current_source_start = saved_source_start
	gen.current_source_end = saved_source_end
	return result
}

fn generate_expression_inner(expression &Expression, mut gen Generator, preferred_dst ?ScopedOperand) ?ScopedOperand {
	// NamedEvaluation: clear pending_lhs_name for non-function/class expressions.
	if expression.kind.tag != .function_expr && expression.kind.tag != .class_expr {
		gen.pending_lhs_name = none
	}

	match expression.kind.tag {
		// === Literals ===
		.numeric_literal {
			return gen.add_constant_number(expression.kind.numeric_value)
		}
		.boolean_literal {
			return gen.add_constant_boolean(expression.kind.bool_value)
		}
		.null_literal {
			return gen.add_constant_null()
		}
		.string_literal {
			return gen.add_constant_string(expression.kind.string_value.clone())
		}
		.big_int_literal {
			digits := expression.kind.big_int_value.trim_right('n')
			return gen.add_constant_bigint(digits)
		}
		.regexp_literal {
			if rd := expression.kind.regexp_data {
				source_index := gen.intern_string(rd.pattern.as_slice())
				flags_index := gen.intern_string(rd.flags.as_slice())
				compiled := ffi_compile_regex(rd.pattern.as_slice(), rd.flags.as_slice())
				regex_index := gen.intern_regex(compiled)
				dst := choose_dst(mut gen, preferred_dst)
				gen.emit(Instruction{
					tag:           .new_regexp
					dst:           dst.operand
					string_index:  source_index
					string_index2: flags_index
					regexp_index:  regex_index
				})
				return dst
			}
			return none
		}
		// === Identifiers ===
		.identifier_expr {
			if ident := expression.kind.identifier {
				return generate_identifier(ident, mut gen, preferred_dst)
			}
			return none
		}
		// === This ===
		.this_expr {
			if gen.function_environment_needed {
				emit_resolve_this_if_needed(mut gen)
			}
			return gen.this_value()
		}
		// === Unary ===
		.unary {
			if operand := expression.kind.operand {
				return generate_unary_expression(mut gen, expression.kind.unary_op, operand,
					preferred_dst)
			}
			return none
		}
		// === Binary ===
		.binary {
			if lhs := expression.kind.lhs {
				if rhs := expression.kind.rhs {
					return generate_binary_expression(mut gen, expression.kind.binary_op,
						lhs, rhs, preferred_dst)
				}
			}
			return none
		}
		// === Logical ===
		.logical {
			if lhs := expression.kind.lhs {
				if rhs := expression.kind.rhs {
					return generate_logical(mut gen, expression.kind.logical_op, lhs,
						rhs, preferred_dst)
				}
			}
			return none
		}
		// === Conditional ===
		.conditional {
			if test := expression.kind.test {
				if consequent := expression.kind.consequent {
					if alternate := expression.kind.alternate_expr {
						return generate_conditional(mut gen, test, consequent, alternate,
							preferred_dst)
					}
				}
			}
			return none
		}
		// === Sequence ===
		.sequence {
			mut last := ?ScopedOperand(none)
			for expr in expression.kind.expressions {
				last = generate_expression(&expr, mut gen, none)
				if gen.is_current_block_terminated() {
					break
				}
			}
			return last
		}
		// === Function expression ===
		.function_expr {
			return generate_function_expression(mut gen, expression.kind.function_id,
				preferred_dst)
		}
		// === Array ===
		.array_expr {
			return generate_array_expression(mut gen, expression.kind.array_elements,
				preferred_dst)
		}
		// === Member access ===
		.member {
			if obj := expression.kind.object {
				if prop := expression.kind.property {
					return generate_member_expression(mut gen, obj, prop, expression.kind.computed,
						preferred_dst)
				}
			}
			return none
		}
		// === Call ===
		.call {
			if cd := expression.kind.call_data {
				return generate_call_expression(mut gen, cd, preferred_dst, false)
			}
			return none
		}
		// === New ===
		.new_expr {
			if cd := expression.kind.call_data {
				return generate_call_expression(mut gen, cd, preferred_dst, true)
			}
			return none
		}
		// === Spread ===
		.spread {
			if inner := expression.kind.operand {
				return generate_expression_or_undefined(inner, mut gen, preferred_dst)
			}
			return gen.add_constant_undefined()
		}
		// === Yield ===
		.yield_expr {
			return generate_yield_expression(mut gen, expression.kind.argument, expression.kind.is_yield_from)
		}
		// === Await ===
		.await_expr {
			if inner := expression.kind.operand {
				value := generate_expression_or_undefined(inner, mut gen, none)
				received_completion := gen.allocate_register()
				received_completion_type := gen.allocate_register()
				received_completion_value := gen.allocate_register()
				acc := gen.accumulator()
				gen.emit_mov(received_completion, acc)
				return generate_await_with_completions(mut gen, value, received_completion,
					received_completion_type, received_completion_value)
			}
			return none
		}
		// === MetaProperty ===
		.meta_property {
			dst := choose_dst(mut gen, preferred_dst)
			if expression.kind.meta_property_type == .new_target {
				gen.emit(Instruction{
					tag: .get_new_target
					dst: dst.operand
				})
			} else {
				gen.emit(Instruction{
					tag: .get_import_meta
					dst: dst.operand
				})
			}
			return dst
		}
		// === ImportCall ===
		.import_call {
			if spec := expression.kind.specifier {
				spec_val := generate_expression(spec, mut gen, none) or { return none }
				opts_val := if opts := expression.kind.options {
					generate_expression(opts, mut gen, none) or { return none }
				} else {
					gen.add_constant_undefined()
				}
				dst := choose_dst(mut gen, preferred_dst)
				gen.emit(Instruction{
					tag:       .import_call_op
					dst:       dst.operand
					src:       spec_val.operand
					condition: opts_val.operand
				})
				return dst
			}
			return none
		}
		// === Update (++/--) ===
		.update {
			if arg := expression.kind.argument {
				return generate_update_expression(mut gen, expression.kind.update_op,
					arg, expression.kind.prefixed, preferred_dst)
			}
			return none
		}
		// === Assignment ===
		.assignment {
			return generate_assignment_expression_from_kind(mut gen, expression, preferred_dst)
		}
		// === Template literal ===
		.template_literal {
			if td := expression.kind.template_data {
				return generate_template_literal(mut gen, td, preferred_dst)
			}
			return none
		}
		// === Tagged template literal ===
		.tagged_template_literal {
			return generate_tagged_template_literal_expr(mut gen, expression, preferred_dst)
		}
		// === Object ===
		.object_expr {
			return generate_object_expression(mut gen, expression.kind.object_properties,
				preferred_dst)
		}
		// === Optional chain ===
		.optional_chain {
			if base := expression.kind.base {
				current_base := gen.allocate_register()
				current_value := choose_dst(mut gen, preferred_dst)
				undef := gen.add_constant_undefined()
				gen.emit_mov(current_base, undef)
				generate_optional_chain_inner(mut gen, base, expression.kind.references,
					current_value, current_base)
				return current_value
			}
			return none
		}
		// === Super call ===
		.super_call {
			if sd := expression.kind.super_call_data {
				arguments := if sd.is_synthetic && sd.arguments.len == 1 {
					generate_expression_or_undefined(&sd.arguments[0].value, mut gen,
						none)
				} else {
					generate_arguments_array(mut gen, sd.arguments)
				}
				dst := choose_dst(mut gen, preferred_dst)
				gen.emit(Instruction{
					tag:               .super_call_with_argument_array
					dst:               dst.operand
					src:               arguments.operand
					is_synthetic_flag: sd.is_synthetic
				})
				return dst
			}
			return none
		}
		// === Super ===
		.super_expr {
			dst := choose_dst(mut gen, preferred_dst)
			gen.emit(Instruction{
				tag: .resolve_super_base
				dst: dst.operand
			})
			return dst
		}
		// === Class expression ===
		.class_expr {
			if cd := expression.kind.class_data {
				return generate_class_expression(mut gen, cd, preferred_dst)
			}
			return none
		}
		// === Private identifier ===
		.private_identifier_expr {
			return none
		}
		// === Error ===
		.error {
			return none
		}
	}
}

// =============================================================================
// Expression generators
// =============================================================================

fn generate_expression_or_undefined(expression &Expression, mut gen Generator, preferred_dst ?ScopedOperand) ScopedOperand {
	if result := generate_expression(expression, mut gen, preferred_dst) {
		return result
	}
	return gen.add_constant_undefined()
}

fn generate_identifier(ident &Identifier, mut gen Generator, preferred_dst ?ScopedOperand) ?ScopedOperand {
	if ident.is_local() {
		local_index := ident.local_index
		if ident.local_type == 1 {
			// Argument
			if !gen.is_argument_initialized(local_index) {
				return gen.scoped_operand(Operand.argument(local_index))
			}
			return gen.scoped_operand(Operand.argument(local_index))
		} else {
			// Variable
			if !gen.is_local_initialized(local_index) {
				dst := choose_dst(mut gen, preferred_dst)
				gen.emit(Instruction{
					tag:        .throw_if_tdz
					src:        Operand.local(local_index)
					identifier: gen.intern_identifier(ident.name.as_slice())
				})
				gen.emit_mov(dst, gen.local(local_index))
				return dst
			}
			return gen.local(local_index)
		}
	}
	if ident.is_global {
		dst := choose_dst(mut gen, preferred_dst)
		id := gen.intern_identifier(ident.name.as_slice())
		gen.emit(Instruction{
			tag:         .get_global
			dst:         dst.operand
			identifier:  id
			cache_index: gen.next_global_variable_cache_()
		})
		return dst
	}
	dst := choose_dst(mut gen, preferred_dst)
	id := gen.intern_identifier(ident.name.as_slice())
	gen.emit(Instruction{
		tag:        .get_binding
		dst:        dst.operand
		identifier: id
		cache:      EnvironmentCoordinate.empty()
	})
	return dst
}

fn generate_unary_expression(mut gen Generator, op UnaryOp, operand &Expression, preferred_dst ?ScopedOperand) ?ScopedOperand {
	if op == .typeof_op {
		if operand.kind.tag == .identifier_expr {
			if ident := operand.kind.identifier {
				if !ident.is_local() {
					dst := choose_dst(mut gen, preferred_dst)
					id := gen.intern_identifier(ident.name.as_slice())
					gen.emit(Instruction{
						tag:        .typeof_binding
						dst:        dst.operand
						identifier: id
						cache:      EnvironmentCoordinate.empty()
					})
					return dst
				}
			}
		}
		dst := choose_dst(mut gen, preferred_dst)
		value := generate_expression(operand, mut gen, none) or { return none }
		gen.emit(Instruction{
			tag: .typeof_op
			dst: dst.operand
			src: value.operand
		})
		return dst
	}
	if op == .delete_op {
		return emit_delete_reference(mut gen, operand)
	}
	if op == .not_op {
		dst := choose_dst(mut gen, preferred_dst)
		if operand.kind.tag == .unary && operand.kind.unary_op == .not_op {
			if inner := operand.kind.operand {
				value := generate_expression(inner, mut gen, none) or { return none }
				if folded := try_constant_fold_to_boolean(mut gen, value) {
					return folded
				}
				gen.emit(Instruction{
					tag:   .to_boolean
					dst:   dst.operand
					value: value.operand
				})
				return dst
			}
		}
		value := generate_expression(operand, mut gen, none) or { return none }
		if folded := try_constant_fold_unary(mut gen, op, value) {
			return folded
		}
		gen.emit(Instruction{
			tag: .not_op
			dst: dst.operand
			src: value.operand
		})
		return dst
	}
	value := generate_expression(operand, mut gen, none) or { return none }
	if folded := try_constant_fold_unary(mut gen, op, value) {
		return folded
	}
	dst := choose_dst(mut gen, preferred_dst)
	match op {
		.bitwise_not {
			gen.emit(Instruction{
				tag: .bitwise_not
				dst: dst.operand
				src: value.operand
			})
		}
		.plus {
			gen.emit(Instruction{
				tag: .unary_plus
				dst: dst.operand
				src: value.operand
			})
		}
		.minus {
			gen.emit(Instruction{
				tag: .unary_minus
				dst: dst.operand
				src: value.operand
			})
		}
		.void_op {
			return gen.add_constant_undefined()
		}
		else {}
	}
	return dst
}

fn generate_binary_expression(mut gen Generator, op BinaryOp, lhs &Expression, rhs &Expression, preferred_dst ?ScopedOperand) ?ScopedOperand {
	if op == .in_op {
		if lhs.kind.tag == .private_identifier_expr {
			if priv_ident := lhs.kind.private_identifier {
				base := generate_expression(rhs, mut gen, none) or { return none }
				dst := choose_dst(mut gen, preferred_dst)
				id := gen.intern_identifier(priv_ident.name.as_slice())
				gen.emit(Instruction{
					tag:        .has_private_id
					dst:        dst.operand
					base:       base.operand
					identifier: id
				})
				return dst
			}
		}
	}
	lhs_val := match op {
		.bitwise_and, .bitwise_or, .bitwise_xor, .left_shift, .right_shift, .unsigned_right_shift {
			if lhs.kind.tag == .numeric_literal {
				gen.add_constant_number(f64(js_to_int32(lhs.kind.numeric_value)))
			} else {
				generate_expression(lhs, mut gen, none) or { return none }
			}
		}
		else {
			generate_expression(lhs, mut gen, none) or { return none }
		}
	}
	rhs_val := match op {
		.bitwise_and, .bitwise_or, .bitwise_xor {
			if rhs.kind.tag == .numeric_literal {
				gen.add_constant_number(f64(js_to_int32(rhs.kind.numeric_value)))
			} else {
				generate_expression(rhs, mut gen, none) or { return none }
			}
		}
		.left_shift, .right_shift, .unsigned_right_shift {
			if rhs.kind.tag == .numeric_literal {
				gen.add_constant_number(f64(js_to_u32(rhs.kind.numeric_value)))
			} else {
				generate_expression(rhs, mut gen, none) or { return none }
			}
		}
		else {
			generate_expression(rhs, mut gen, none) or { return none }
		}
	}
	if folded := try_constant_fold_binary(mut gen, op, lhs_val, rhs_val) {
		return folded
	}
	dst := choose_dst(mut gen, preferred_dst)
	emit_binary_op(mut gen, op, dst, lhs_val, rhs_val)
	return dst
}

fn emit_binary_op(mut gen Generator, op BinaryOp, dst ScopedOperand, lhs ScopedOperand, rhs ScopedOperand) {
	tag := match op {
		.addition { InstructionTag.add }
		.subtraction { InstructionTag.sub }
		.multiplication { InstructionTag.mul }
		.division { InstructionTag.div }
		.modulo { InstructionTag.mod_op }
		.exponentiation { InstructionTag.exp }
		.strictly_equals { InstructionTag.strictly_equals }
		.strictly_inequals { InstructionTag.strictly_inequals }
		.loosely_equals { InstructionTag.loosely_equals }
		.loosely_inequals { InstructionTag.loosely_inequals }
		.greater_than { InstructionTag.greater_than }
		.greater_than_equals { InstructionTag.greater_than_equals }
		.less_than { InstructionTag.less_than }
		.less_than_equals { InstructionTag.less_than_equals }
		.bitwise_and { InstructionTag.bitwise_and }
		.bitwise_or { InstructionTag.bitwise_or }
		.bitwise_xor { InstructionTag.bitwise_xor }
		.left_shift { InstructionTag.left_shift }
		.right_shift { InstructionTag.right_shift }
		.unsigned_right_shift { InstructionTag.unsigned_right_shift }
		.in_op { InstructionTag.in_op }
		.instance_of { InstructionTag.instance_of }
	}
	gen.emit(Instruction{
		tag: tag
		dst: dst.operand
		lhs: lhs.operand
		rhs: rhs.operand
	})
}

fn generate_logical(mut gen Generator, op LogicalOp, lhs &Expression, rhs &Expression, preferred_dst ?ScopedOperand) ?ScopedOperand {
	dst := choose_dst(mut gen, preferred_dst)
	lhs_val := generate_expression(lhs, mut gen, dst) or { return none }
	gen.emit_mov(dst, lhs_val)
	rhs_block := gen.make_block()
	end_block := gen.make_block()
	emit_logical_jump(mut gen, op, dst, rhs_block, end_block)
	gen.switch_to_basic_block(rhs_block)
	rhs_val := generate_expression(rhs, mut gen, dst) or { return none }
	gen.emit_mov(dst, rhs_val)
	gen.emit(Instruction.jump(end_block))
	gen.switch_to_basic_block(end_block)
	return dst
}

fn emit_logical_jump(mut gen Generator, op LogicalOp, condition ScopedOperand, true_block Label, false_block Label) {
	match op {
		.and_op {
			gen.emit_jump_if(condition, true_block, false_block)
		}
		.or_op {
			gen.emit_jump_if(condition, false_block, true_block)
		}
		.nullish_coalescing {
			gen.emit(Instruction{
				tag:          .jump_nullish
				condition:    condition.operand
				true_target:  true_block
				false_target: false_block
			})
		}
	}
}

fn generate_conditional(mut gen Generator, test &Expression, consequent &Expression, alternate &Expression, preferred_dst ?ScopedOperand) ?ScopedOperand {
	dst := choose_dst(mut gen, preferred_dst)
	condition := generate_expression(test, mut gen, none) or { return none }
	then_block := gen.make_block()
	else_block := gen.make_block()
	end_block := gen.make_block()
	gen.emit_jump_if(condition, then_block, else_block)
	gen.switch_to_basic_block(then_block)
	then_val := generate_expression(consequent, mut gen, dst) or { return none }
	gen.emit_mov(dst, then_val)
	gen.emit(Instruction.jump(end_block))
	gen.switch_to_basic_block(else_block)
	else_val := generate_expression(alternate, mut gen, dst) or { return none }
	gen.emit_mov(dst, else_val)
	gen.emit(Instruction.jump(end_block))
	gen.switch_to_basic_block(end_block)
	return dst
}

fn generate_function_expression(mut gen Generator, function_id FunctionId, preferred_dst ?ScopedOperand) ?ScopedOperand {
	data := gen.function_table.take(function_id)
	sfd_index := emit_new_function(mut gen, data, none)
	dst := choose_dst(mut gen, preferred_dst)
	lhs_name := gen.pending_lhs_name
	gen.pending_lhs_name = none
	gen.emit(Instruction{
		tag:          .new_function
		dst:          dst.operand
		sfd_index:    sfd_index
		has_lhs_name: lhs_name != none
		lhs_name_id:  if lid := lhs_name { lid } else { IdentifierTableIndex{} }
	})
	return dst
}

fn generate_array_expression(mut gen Generator, elements []?Expression, preferred_dst ?ScopedOperand) ?ScopedOperand {
	dst := choose_dst(mut gen, preferred_dst)
	gen.emit(Instruction{
		tag: .new_array
		dst: dst.operand
	})
	for elem_opt in elements {
		if elem := elem_opt {
			if elem.kind.tag == .spread {
				if inner := elem.kind.operand {
					spread_val := generate_expression(inner, mut gen, none) or { continue }
					gen.emit(Instruction{
						tag:   .array_append
						dst:   dst.operand
						value: spread_val.operand
					})
					continue
				}
			}
			value := generate_expression(&elem, mut gen, none) or { continue }
			gen.emit(Instruction{
				tag:   .array_append
				dst:   dst.operand
				value: value.operand
			})
		} else {
			empty := gen.add_constant_empty()
			gen.emit(Instruction{
				tag:   .array_append
				dst:   dst.operand
				value: empty.operand
			})
		}
	}
	return dst
}

fn generate_member_expression(mut gen Generator, object &Expression, property &Expression, computed bool, preferred_dst ?ScopedOperand) ?ScopedOperand {
	base := generate_expression(object, mut gen, none) or { return none }
	if !computed {
		if property.kind.tag == .identifier_expr {
			if ident := property.kind.identifier {
				return emit_get_by_id(mut gen, base, ident, object, preferred_dst)
			}
		}
		if property.kind.tag == .private_identifier_expr {
			if priv := property.kind.private_identifier {
				dst := choose_dst(mut gen, preferred_dst)
				id := gen.intern_identifier(priv.name.as_slice())
				gen.emit(Instruction{
					tag:        .get_private_by_id
					dst:        dst.operand
					base:       base.operand
					identifier: id
				})
				return dst
			}
		}
	}
	prop_val := generate_expression(property, mut gen, none) or { return none }
	return emit_get_by_value(mut gen, base, prop_val, object, preferred_dst)
}

fn emit_get_by_id(mut gen Generator, base ScopedOperand, property &Identifier, base_expr &Expression, preferred_dst ?ScopedOperand) ?ScopedOperand {
	dst := choose_dst(mut gen, preferred_dst)
	prop_key := gen.intern_property_key(property.name.as_slice())
	base_id := intern_base_identifier(mut gen, base_expr)
	gen.emit(Instruction{
		tag:                 .get_by_id
		dst:                 dst.operand
		base:                base.operand
		property_key:        prop_key
		cache_index:         gen.next_property_lookup_cache_()
		has_base_identifier: base_id != none
		base_identifier_id:  if bid := base_id { bid } else { IdentifierTableIndex{} }
	})
	return dst
}

fn emit_get_by_value(mut gen Generator, base ScopedOperand, property ScopedOperand, base_expr &Expression, preferred_dst ?ScopedOperand) ?ScopedOperand {
	if pk := gen.try_constant_string_to_property_key(property) {
		dst := choose_dst(mut gen, preferred_dst)
		base_id := intern_base_identifier(mut gen, base_expr)
		gen.emit(Instruction{
			tag:                 .get_by_id
			dst:                 dst.operand
			base:                base.operand
			property_key:        pk
			cache_index:         gen.next_property_lookup_cache_()
			has_base_identifier: base_id != none
			base_identifier_id:  if bid := base_id { bid } else { IdentifierTableIndex{} }
		})
		return dst
	}
	dst := choose_dst(mut gen, preferred_dst)
	gen.emit(Instruction{
		tag:  .get_by_value
		dst:  dst.operand
		base: base.operand
		src:  property.operand
	})
	return dst
}

fn generate_yield_expression(mut gen Generator, argument ?&Expression, is_yield_from bool) ?ScopedOperand {
	if is_yield_from {
		if arg := argument {
			return generate_yield_from(mut gen, arg)
		}
		return none
	}
	value := if arg := argument {
		generate_expression_or_undefined(arg, mut gen, none)
	} else {
		gen.add_constant_undefined()
	}
	return generate_yield(mut gen, value)
}

fn generate_yield(mut gen Generator, value ScopedOperand) ?ScopedOperand {
	continuation_block := gen.make_block()
	if gen.is_in_async_generator_function() {
		// Async generator: await the value before yielding.
		_ := gen.allocate_register() // received_completion
		received_completion_type := gen.allocate_register()
		received_completion_value := gen.allocate_register()
		gen.emit(Instruction.yield_instr(continuation_block, value.operand))
		gen.switch_to_basic_block(continuation_block)
		gen.emit(Instruction{
			tag:        .get_completion_fields
			callee_dst: received_completion_type.operand
			this_dst:   received_completion_value.operand
		})
		return received_completion_value
	}
	gen.emit(Instruction.yield_instr(continuation_block, value.operand))
	gen.switch_to_basic_block(continuation_block)
	received_completion_type := gen.allocate_register()
	received_completion_value := gen.allocate_register()
	gen.emit(Instruction{
		tag:        .get_completion_fields
		callee_dst: received_completion_type.operand
		this_dst:   received_completion_value.operand
	})
	throw_block := gen.make_block()
	normal_block := gen.make_block()
	throw_const := gen.add_constant_i32(i32(CompletionType.throw_ct))
	gen.emit(Instruction{
		tag:          .jump_strictly_equals
		lhs:          received_completion_type.operand
		rhs:          throw_const.operand
		true_target:  throw_block
		false_target: normal_block
	})
	gen.switch_to_basic_block(throw_block)
	gen.emit(Instruction{
		tag: .throw_op
		src: received_completion_value.operand
	})
	gen.switch_to_basic_block(normal_block)
	return received_completion_value
}

fn generate_yield_from(mut gen Generator, iterable &Expression) ?ScopedOperand {
	// Stub: simplified yield* implementation
	value := generate_expression(iterable, mut gen, none) or { return none }
	return generate_yield(mut gen, value)
}

fn generate_await_with_completions(mut gen Generator, value ScopedOperand, received_completion ScopedOperand, received_completion_type ScopedOperand, received_completion_value ScopedOperand) ScopedOperand {
	continuation_block := gen.make_block()
	gen.emit(Instruction{
		tag:                .await_op
		dst:                received_completion.operand
		src:                value.operand
		continuation_label: continuation_block
	})
	gen.switch_to_basic_block(continuation_block)
	gen.emit(Instruction{
		tag:        .get_completion_fields
		callee_dst: received_completion_type.operand
		this_dst:   received_completion_value.operand
	})
	throw_block := gen.make_block()
	normal_block := gen.make_block()
	throw_const := gen.add_constant_i32(i32(CompletionType.throw_ct))
	gen.emit(Instruction{
		tag:          .jump_strictly_equals
		lhs:          received_completion_type.operand
		rhs:          throw_const.operand
		true_target:  throw_block
		false_target: normal_block
	})
	gen.switch_to_basic_block(throw_block)
	gen.emit(Instruction{
		tag: .throw_op
		src: received_completion_value.operand
	})
	gen.switch_to_basic_block(normal_block)
	return received_completion_value
}

fn emit_resolve_this_if_needed(mut gen Generator) {
	block := gen.basic_blocks[gen.current_block_index.basic_block_index()]
	if !block.resolved_this {
		gen.emit(Instruction{
			tag: .resolve_this_binding
		})
		gen.basic_blocks[gen.current_block_index.basic_block_index()].resolved_this = true
	}
}
