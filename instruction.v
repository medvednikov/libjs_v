module main

// Instruction types, translated from instruction.rs.
// The real Instruction enum is generated from Bytecode.def by build.rs.
// This provides all instruction tags needed by the codegen.

enum InstructionTag {
	mov
	jump
	jump_if
	jump_true
	jump_false
	jump_less_than
	jump_less_than_equals
	jump_greater_than
	jump_greater_than_equals
	jump_loosely_equals
	jump_loosely_inequals
	jump_strictly_equals
	jump_strictly_inequals
	jump_nullish
	jump_undefined
	less_than
	less_than_equals
	greater_than
	greater_than_equals
	loosely_equals
	loosely_inequals
	strictly_equals
	strictly_inequals
	return_op
	end_op
	yield_op
	set_lexical_environment
	create_lexical_environment
	// Unary operations
	new_regexp
	typeof_binding
	typeof_op
	has_private_id
	not_op
	to_boolean
	bitwise_not
	unary_plus
	unary_minus
	// Binary arithmetic operations
	add
	sub
	mul
	div
	mod_op
	exp
	bitwise_and
	bitwise_or
	bitwise_xor
	left_shift
	right_shift
	unsigned_right_shift
	in_op
	instance_of
	// Variable/binding operations
	create_variable
	new_function
	initialize_lexical_binding
	initialize_variable_binding
	get_binding
	set_variable_binding
	get_initialized_binding
	set_lexical_binding
	enter_object_environment
	// Error constructors
	new_type_error
	new_reference_error
	// Meta operations
	get_new_target
	get_import_meta
	import_call_op
	// Control flow
	throw_op
	await_op
	// Completion
	get_completion_fields
	set_completion_type
	// Call operations
	call_op
	call_construct
	call_direct_eval
	call_builtin
	call_with_argument_array
	call_direct_eval_with_argument_array
	call_construct_with_argument_array
	// Type checks
	throw_if_not_object
	throw_if_nullish
	throw_if_tdz
	throw_const_assignment
	is_callable
	is_constructor
	// Iterators
	get_iterator
	iterator_next_unpack
	iterator_next
	iterator_to_array
	iterator_close
	create_async_from_sync_iterator
	get_method
	get_object_property_iterator
	// Property access
	get_by_id
	get_by_id_with_this
	get_by_value
	get_by_value_with_this
	get_length
	get_private_by_id
	// Property mutation
	put_normal_by_id
	put_normal_by_id_with_this
	put_normal_by_value
	put_normal_by_value_with_this
	put_own_by_id
	put_own_by_value
	put_getter_by_id
	put_getter_by_value
	put_setter_by_id
	put_setter_by_value
	put_prototype_by_id
	put_private_by_id
	put_by_spread
	// Delete operations
	delete_variable
	delete_by_id
	delete_by_value
	// Global access
	get_global
	set_global
	// Arithmetic updates
	increment
	decrement
	postfix_increment
	postfix_decrement
	// This/super
	resolve_this_binding
	resolve_super_base
	get_callee_and_this_from_environment
	// Object/array creation
	new_array
	new_primitive_array
	array_append
	new_object
	new_object_with_no_prototype
	new_array_with_length
	cache_object_shape
	init_object_literal_property
	// Conversions
	to_primitive_with_string_hint
	to_string_op
	concat_string
	to_object
	to_length
	to_int32_op
	// Template literals
	get_template_object
	// Super call
	super_call_with_argument_array
	// Private environment
	create_private_environment
	add_private_name
	leave_private_environment
	// Class
	new_class
	// Binding creation
	create_immutable_binding
	create_mutable_binding
	// Data property
	create_data_property_or_throw
	// Copy/spread
	copy_object_excluding_properties
	// Catch
	catch_op
	// Arguments
	create_arguments
	create_rest_params
	// Variable environment
	create_variable_environment
	get_lexical_environment
}

struct Instruction {
mut:
	tag InstructionTag
	// Primary operand fields.
	dst       Operand
	src       Operand
	lhs       Operand
	rhs       Operand
	value     Operand
	condition Operand
	// Label fields for jumps.
	target             ?Label
	true_target        ?Label
	false_target       ?Label
	continuation_label ?Label
	// Environment-specific fields.
	environment Operand
	parent      Operand
	capacity    u32
	// Identifier/binding fields.
	identifier IdentifierTableIndex
	cache      EnvironmentCoordinate
	// Property access fields.
	base         Operand
	property_key PropertyKeyTableIndex
	cache_index  u32
	// Call fields.
	callee    Operand
	this_op   Operand
	first_arg Operand
	argc      u32
	// Various indices.
	string_index  StringTableIndex
	string_index2 StringTableIndex
	regexp_index  RegexTableIndex
	sfd_index     u32
	blueprint_idx u32
	element_count u32
	builtin_id    u8
	// Mode/kind/hint u32 values.
	mode_val       u32
	kind_val       u32
	hint_val       u32
	rest_index_val u32
	// Boolean flags.
	is_immutable_flag     bool
	is_global_flag        bool
	is_strict_flag        bool
	is_synthetic_flag     bool
	has_home_object       bool
	has_lhs_name          bool
	has_base_identifier   bool
	has_expression_string bool
	has_super_class       bool
	// Extra operands.
	home_object_op Operand
	super_class_op Operand
	callee_dst     Operand
	this_dst       Operand
	// Extra identifiers.
	lhs_name_id          IdentifierTableIndex
	base_identifier_id   IdentifierTableIndex
	expression_string_id IdentifierTableIndex
	excluded_count       u32
}

fn Instruction.mov(dst Operand, src Operand) Instruction {
	return Instruction{
		tag: .mov
		dst: dst
		src: src
	}
}

fn Instruction.jump(target Label) Instruction {
	return Instruction{
		tag:    .jump
		target: target
	}
}

fn Instruction.jump_if(condition Operand, true_target Label, false_target Label) Instruction {
	return Instruction{
		tag:          .jump_if
		condition:    condition
		true_target:  true_target
		false_target: false_target
	}
}

fn Instruction.jump_true(condition Operand, target Label) Instruction {
	return Instruction{
		tag:       .jump_true
		condition: condition
		target:    target
	}
}

fn Instruction.jump_false(condition Operand, target Label) Instruction {
	return Instruction{
		tag:       .jump_false
		condition: condition
		target:    target
	}
}

fn Instruction.jump_less_than(lhs Operand, rhs Operand, true_target Label, false_target Label) Instruction {
	return Instruction{
		tag:          .jump_less_than
		lhs:          lhs
		rhs:          rhs
		true_target:  true_target
		false_target: false_target
	}
}

fn Instruction.jump_less_than_equals(lhs Operand, rhs Operand, true_target Label, false_target Label) Instruction {
	return Instruction{
		tag:          .jump_less_than_equals
		lhs:          lhs
		rhs:          rhs
		true_target:  true_target
		false_target: false_target
	}
}

fn Instruction.jump_greater_than(lhs Operand, rhs Operand, true_target Label, false_target Label) Instruction {
	return Instruction{
		tag:          .jump_greater_than
		lhs:          lhs
		rhs:          rhs
		true_target:  true_target
		false_target: false_target
	}
}

fn Instruction.jump_greater_than_equals(lhs Operand, rhs Operand, true_target Label, false_target Label) Instruction {
	return Instruction{
		tag:          .jump_greater_than_equals
		lhs:          lhs
		rhs:          rhs
		true_target:  true_target
		false_target: false_target
	}
}

fn Instruction.jump_loosely_equals(lhs Operand, rhs Operand, true_target Label, false_target Label) Instruction {
	return Instruction{
		tag:          .jump_loosely_equals
		lhs:          lhs
		rhs:          rhs
		true_target:  true_target
		false_target: false_target
	}
}

fn Instruction.jump_loosely_inequals(lhs Operand, rhs Operand, true_target Label, false_target Label) Instruction {
	return Instruction{
		tag:          .jump_loosely_inequals
		lhs:          lhs
		rhs:          rhs
		true_target:  true_target
		false_target: false_target
	}
}

fn Instruction.jump_strictly_equals(lhs Operand, rhs Operand, true_target Label, false_target Label) Instruction {
	return Instruction{
		tag:          .jump_strictly_equals
		lhs:          lhs
		rhs:          rhs
		true_target:  true_target
		false_target: false_target
	}
}

fn Instruction.jump_strictly_inequals(lhs Operand, rhs Operand, true_target Label, false_target Label) Instruction {
	return Instruction{
		tag:          .jump_strictly_inequals
		lhs:          lhs
		rhs:          rhs
		true_target:  true_target
		false_target: false_target
	}
}

fn Instruction.return_instr(value Operand) Instruction {
	return Instruction{
		tag:   .return_op
		value: value
	}
}

fn Instruction.end_instr(value Operand) Instruction {
	return Instruction{
		tag:   .end_op
		value: value
	}
}

fn Instruction.yield_instr(continuation_label ?Label, value Operand) Instruction {
	return Instruction{
		tag:                .yield_op
		continuation_label: continuation_label
		value:              value
	}
}

fn Instruction.set_lexical_environment(environment Operand) Instruction {
	return Instruction{
		tag:         .set_lexical_environment
		environment: environment
	}
}

fn Instruction.create_lexical_environment(dst Operand, parent Operand, capacity u32) Instruction {
	return Instruction{
		tag:      .create_lexical_environment
		dst:      dst
		parent:   parent
		capacity: capacity
	}
}

// is_terminator returns true if this instruction terminates its basic block.
fn (i &Instruction) is_terminator() bool {
	return match i.tag {
		.jump, .jump_if, .jump_true, .jump_false, .jump_less_than, .jump_less_than_equals,
		.jump_greater_than, .jump_greater_than_equals, .jump_loosely_equals,
		.jump_loosely_inequals, .jump_strictly_equals, .jump_strictly_inequals, .jump_nullish,
		.jump_undefined, .return_op, .end_op, .yield_op, .throw_op, .await_op {
			true
		}
		else {
			false
		}
	}
}

// encoded_size returns the byte size of this instruction when encoded.
// Stub: returns a fixed size for now.
fn (i &Instruction) encoded_size() int {
	return match i.tag {
		.mov {
			12
		}
		.jump {
			8
		}
		.jump_if {
			16
		}
		.jump_true, .jump_false {
			12
		}
		.jump_less_than, .jump_less_than_equals, .jump_greater_than, .jump_greater_than_equals,
		.jump_loosely_equals, .jump_loosely_inequals, .jump_strictly_equals,
		.jump_strictly_inequals {
			20
		}
		.less_than, .less_than_equals, .greater_than, .greater_than_equals, .loosely_equals,
		.loosely_inequals, .strictly_equals, .strictly_inequals {
			16
		}
		.return_op, .end_op {
			8
		}
		.yield_op {
			12
		}
		.set_lexical_environment {
			8
		}
		.create_lexical_environment {
			16
		}
		else {
			// Stub: default size for all other instruction types.
			16
		}
	}
}

// encode writes the instruction bytes to the output buffer.
// Stub: writes placeholder bytes.
fn (i &Instruction) encode(strict bool, mut output []u8) {
	size := i.encoded_size()
	for _ in 0 .. size {
		output << u8(0)
	}
}

// rewrite_operand applies the offset to a single operand based on its type.
fn rewrite_operand(mut op Operand, num_registers u32, num_locals u32, num_constants u32) {
	if op.is_invalid() {
		return
	}
	match op.operand_type() {
		.register {}
		.local { op.offset_index_by(num_registers) }
		.constant { op.offset_index_by(num_registers + num_locals) }
		.argument { op.offset_index_by(num_registers + num_locals + num_constants) }
	}
}

// rewrite_operands rewrites all operands in this instruction, offsetting indices
// for the runtime layout [registers | locals | constants | arguments].
fn (mut i Instruction) rewrite_operands(num_registers u32, num_locals u32, num_constants u32) {
	match i.tag {
		.mov {
			rewrite_operand(mut &i.dst, num_registers, num_locals, num_constants)
			rewrite_operand(mut &i.src, num_registers, num_locals, num_constants)
		}
		.jump {}
		.jump_if {
			rewrite_operand(mut &i.condition, num_registers, num_locals, num_constants)
		}
		.jump_true, .jump_false {
			rewrite_operand(mut &i.condition, num_registers, num_locals, num_constants)
		}
		.jump_less_than, .jump_less_than_equals, .jump_greater_than, .jump_greater_than_equals,
		.jump_loosely_equals, .jump_loosely_inequals, .jump_strictly_equals,
		.jump_strictly_inequals {
			rewrite_operand(mut &i.lhs, num_registers, num_locals, num_constants)
			rewrite_operand(mut &i.rhs, num_registers, num_locals, num_constants)
		}
		.less_than, .less_than_equals, .greater_than, .greater_than_equals, .loosely_equals,
		.loosely_inequals, .strictly_equals, .strictly_inequals {
			rewrite_operand(mut &i.dst, num_registers, num_locals, num_constants)
			rewrite_operand(mut &i.lhs, num_registers, num_locals, num_constants)
			rewrite_operand(mut &i.rhs, num_registers, num_locals, num_constants)
		}
		.return_op, .end_op {
			rewrite_operand(mut &i.value, num_registers, num_locals, num_constants)
		}
		.yield_op {
			rewrite_operand(mut &i.value, num_registers, num_locals, num_constants)
		}
		.set_lexical_environment {
			rewrite_operand(mut &i.environment, num_registers, num_locals, num_constants)
		}
		.create_lexical_environment {
			rewrite_operand(mut &i.dst, num_registers, num_locals, num_constants)
			rewrite_operand(mut &i.parent, num_registers, num_locals, num_constants)
		}
		else {
			// Generic rewrite for all other instruction types:
			// rewrite every operand field. Unused fields default to register 0,
			// which is a no-op in rewrite_operand.
			rewrite_operand(mut &i.dst, num_registers, num_locals, num_constants)
			rewrite_operand(mut &i.src, num_registers, num_locals, num_constants)
			rewrite_operand(mut &i.lhs, num_registers, num_locals, num_constants)
			rewrite_operand(mut &i.rhs, num_registers, num_locals, num_constants)
			rewrite_operand(mut &i.value, num_registers, num_locals, num_constants)
			rewrite_operand(mut &i.condition, num_registers, num_locals, num_constants)
			rewrite_operand(mut &i.base, num_registers, num_locals, num_constants)
			rewrite_operand(mut &i.callee, num_registers, num_locals, num_constants)
			rewrite_operand(mut &i.this_op, num_registers, num_locals, num_constants)
			rewrite_operand(mut &i.first_arg, num_registers, num_locals, num_constants)
			rewrite_operand(mut &i.home_object_op, num_registers, num_locals, num_constants)
			rewrite_operand(mut &i.super_class_op, num_registers, num_locals, num_constants)
			rewrite_operand(mut &i.callee_dst, num_registers, num_locals, num_constants)
			rewrite_operand(mut &i.this_dst, num_registers, num_locals, num_constants)
			rewrite_operand(mut &i.environment, num_registers, num_locals, num_constants)
			rewrite_operand(mut &i.parent, num_registers, num_locals, num_constants)
		}
	}
}

// patch_label converts a single label from block index to byte offset.
fn patch_label(mut label Label, block_offsets []int) {
	block_index := int(label.value)
	if block_index < block_offsets.len {
		label.value = u32(block_offsets[block_index])
	}
}

// patch_labels converts all labels in this instruction from block indices to byte offsets.
fn (mut i Instruction) patch_labels(block_offsets []int) {
	match i.tag {
		.jump {
			if mut t := i.target {
				patch_label(mut &t, block_offsets)
				i.target = t
			}
		}
		.jump_if, .jump_nullish, .jump_undefined {
			if mut tt := i.true_target {
				patch_label(mut &tt, block_offsets)
				i.true_target = tt
			}
			if mut ft := i.false_target {
				patch_label(mut &ft, block_offsets)
				i.false_target = ft
			}
		}
		.jump_true, .jump_false {
			if mut t := i.target {
				patch_label(mut &t, block_offsets)
				i.target = t
			}
		}
		.jump_less_than, .jump_less_than_equals, .jump_greater_than, .jump_greater_than_equals,
		.jump_loosely_equals, .jump_loosely_inequals, .jump_strictly_equals,
		.jump_strictly_inequals {
			if mut tt := i.true_target {
				patch_label(mut &tt, block_offsets)
				i.true_target = tt
			}
			if mut ft := i.false_target {
				patch_label(mut &ft, block_offsets)
				i.false_target = ft
			}
		}
		.yield_op, .await_op {
			if mut cl := i.continuation_label {
				patch_label(mut &cl, block_offsets)
				i.continuation_label = cl
			}
		}
		else {}
	}
}
