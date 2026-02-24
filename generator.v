module main

// Bytecode generator, translated from generator.rs.
//
// This module contains the Generator struct which manages all state
// needed for bytecode generation from the AST.
import math

// ScopedOperand wraps an Operand. In Rust this uses Rc for auto-freeing
// registers; in V we use a simpler approach with explicit free.
struct ScopedOperand {
mut:
	operand          Operand
	is_register_type bool
	register_index   u32
}

fn ScopedOperand.from_operand(op Operand) ScopedOperand {
	return ScopedOperand{
		operand:          op
		is_register_type: op.is_register()
		register_index:   if op.is_register() { op.index() } else { u32(0) }
	}
}

fn (a ScopedOperand) == (b ScopedOperand) bool {
	return a.operand == b.operand
}

// Block boundary types for unwind tracking.
enum BlockBoundaryType {
	break_boundary
	continue_boundary
	return_to_finally
	leave_finally
	leave_lexical_environment
}

// A break/continue scope with its target label and language labels.
struct LabelableScope {
mut:
	bytecode_target     Label
	language_label_set  []Utf16String
	completion_register ?ScopedOperand
}

// Codegen-time state for a try/finally scope.
struct FinallyContext {
mut:
	completion_type              ScopedOperand
	completion_value             ScopedOperand
	finally_body                 Label
	exception_preamble           Label
	parent_index                 int // -1 means no parent
	registered_jumps             []FinallyJump
	next_jump_index              i32
	lexical_environment_at_entry ?ScopedOperand
}

const finally_normal = i32(0)
const finally_throw = i32(1)
const finally_return = i32(2)
const finally_first_jump_index = i32(3)

// A break/continue target registered with a FinallyContext.
struct FinallyJump {
mut:
	index  i32
	target Label
}

// A local variable name with metadata (for the generator, not the AST).
struct GeneratorLocalVariable {
mut:
	name                                            Utf16String
	is_lexically_declared                           bool
	is_initialized_during_declaration_instantiation bool
}

// The bytecode generator.
// Manages all state needed for compiling an AST into bytecode.
struct Generator {
mut:
	// Basic block management
	basic_blocks        []BasicBlock
	current_block_index Label

	// Register allocation
	next_register      u32
	free_register_pool []Register

	// Constant pool
	constants []ConstantValue

	// Cached constants for deduplication
	true_constant      ?ScopedOperand
	false_constant     ?ScopedOperand
	null_constant      ?ScopedOperand
	undefined_constant ?ScopedOperand
	empty_constant     ?ScopedOperand
	int32_constants    map[i32]ScopedOperand
	string_constants   map[string]ScopedOperand

	// String/identifier/property tables (with deduplication)
	string_table             []Utf16String
	string_table_index       map[string]StringTableIndex
	identifier_table         []Utf16String
	identifier_table_index   map[string]IdentifierTableIndex
	property_key_table       []Utf16String
	property_key_table_index map[string]PropertyKeyTableIndex
	compiled_regexes         []voidptr

	// Scope/unwind state
	boundaries                         []BlockBoundaryType
	continuable_scopes                 []LabelableScope
	breakable_scopes                   []LabelableScope
	pending_labels                     []Utf16String
	lexical_environment_register_stack []ScopedOperand
	home_objects                       []ScopedOperand

	// Finally context
	finally_contexts        []FinallyContext
	current_finally_context int // -1 means none

	// Various counters
	next_property_lookup_cache u32
	next_global_variable_cache u32
	next_template_object_cache u32
	next_object_shape_cache    u32

	// Codegen state
	strict                      bool
	function_environment_needed bool
	enclosing_function_kind     FunctionKind
	local_variables             []GeneratorLocalVariable
	initialized_locals          []bool
	initialized_arguments       []bool
	pending_lhs_name            ?IdentifierTableIndex

	// Source location tracking
	current_source_start u32
	current_source_end   u32

	// Completion register
	current_completion_register ?ScopedOperand
	must_propagate_completion   bool

	// Accumulator and this
	accumulator ScopedOperand
	this_value_ ScopedOperand

	// Shared function data (opaque pointers)
	shared_function_data []voidptr

	// Class blueprints (opaque pointers)
	class_blueprints []voidptr

	// Length identifier cache
	length_identifier ?PropertyKeyTableIndex

	// Unwind context
	current_unwind_handler ?Label

	// AnnexB function names
	annexb_function_names []Utf16String

	// Builtin abstract operations
	builtin_abstract_operations_enabled bool

	// FFI context
	vm_ptr          voidptr
	source_code_ptr voidptr
	source_len      int

	// Function table
	function_table FunctionTable
}

fn Generator.new() Generator {
	acc := ScopedOperand.from_operand(Operand.register(register_accumulator))
	this_val := ScopedOperand.from_operand(Operand.register(register_this_value))
	return Generator{
		current_block_index:         Label.new(0)
		next_register:               register_reserved_count
		current_finally_context:     -1
		function_environment_needed: true
		enclosing_function_kind:     .normal
		accumulator:                 acc
		this_value_:                 this_val
	}
}

// --- Function kind queries ---

fn (g &Generator) is_in_generator_function() bool {
	return g.enclosing_function_kind == .generator || g.enclosing_function_kind == .async_generator
}

fn (g &Generator) is_in_async_function() bool {
	return g.enclosing_function_kind == .async_kind || g.enclosing_function_kind == .async_generator
}

fn (g &Generator) is_in_async_generator_function() bool {
	return g.enclosing_function_kind == .async_generator
}

fn (g &Generator) is_in_generator_or_async_function() bool {
	return g.enclosing_function_kind != .normal
}

fn (g &Generator) is_in_finalizer() bool {
	for b in g.boundaries {
		if b == .leave_finally {
			return true
		}
	}
	return false
}

// --- Register management ---

// allocate_register allocates a new register (or reuses a freed one).
// Always picks the lowest-numbered free register for deterministic allocation.
fn (mut g Generator) allocate_register() ScopedOperand {
	if g.free_register_pool.len == 0 {
		reg := Register.new(g.next_register)
		g.next_register += 1
		return g.scoped_operand(Operand.register(reg))
	}
	mut min_idx := 0
	mut min_val := g.free_register_pool[0].value
	for i, r in g.free_register_pool {
		if r.value < min_val {
			min_idx = i
			min_val = r.value
		}
	}
	reg := g.free_register_pool[min_idx]
	g.free_register_pool.delete(min_idx)
	return g.scoped_operand(Operand.register(reg))
}

// free_register returns a register to the free pool for reuse.
fn (mut g Generator) free_register(op ScopedOperand) {
	if op.is_register_type && op.register_index >= register_reserved_count {
		g.free_register_pool << Register.new(op.register_index)
	}
}

// local gets a ScopedOperand for a local variable.
fn (mut g Generator) local(index u32) ScopedOperand {
	return g.scoped_operand(Operand.local(index))
}

// resolve_local resolves a local binding (argument or variable) to a ScopedOperand.
fn (mut g Generator) resolve_local(index u32, local_type LocalType) ScopedOperand {
	return match local_type {
		.argument { g.scoped_operand(Operand.argument(index)) }
		.variable { g.local(index) }
	}
}

// accumulator gets the accumulator register.
fn (g &Generator) accumulator() ScopedOperand {
	return g.accumulator
}

// this_value gets the this_value register.
fn (g &Generator) this_value() ScopedOperand {
	return g.this_value_
}

// exception_operand gets the exception register as a raw Operand.
fn (g &Generator) exception_operand() Operand {
	return Operand.register(register_exception)
}

// copy_if_needed_to_preserve_evaluation_order copies a local variable into a
// fresh register to prevent later side effects from changing its value.
fn (mut g Generator) copy_if_needed_to_preserve_evaluation_order(operand ScopedOperand) ScopedOperand {
	if operand.operand.is_local() {
		reg := g.allocate_register()
		g.emit_mov(reg, operand)
		return reg
	}
	return operand
}

fn (mut g Generator) scoped_operand(operand Operand) ScopedOperand {
	return ScopedOperand.from_operand(operand)
}

// --- Constant pool ---

fn (mut g Generator) append_constant(value ConstantValue) ScopedOperand {
	index := u32(g.constants.len)
	g.constants << value
	return g.scoped_operand(Operand.constant(index))
}

fn (mut g Generator) add_constant_number(value f64) ScopedOperand {
	// Deduplicate i32 values (but not -0.0)
	if math.fmod(value, 1.0) == 0.0 && value >= f64(min_i32) && value <= f64(max_i32)
		&& math.f64_bits(value) != math.f64_bits(-0.0) {
		as_i32 := i32(value)
		if as_i32 in g.int32_constants {
			return g.int32_constants[as_i32]
		}
		op := g.append_constant(ConstantValue{ tag: .number, number_value: value })
		g.int32_constants[as_i32] = op
		return op
	}
	return g.append_constant(ConstantValue{ tag: .number, number_value: value })
}

fn (mut g Generator) add_constant_boolean(value bool) ScopedOperand {
	if value {
		if op := g.true_constant {
			return op
		}
		op := g.append_constant(ConstantValue{ tag: .boolean, bool_value: true })
		g.true_constant = op
		return op
	} else {
		if op := g.false_constant {
			return op
		}
		op := g.append_constant(ConstantValue{ tag: .boolean, bool_value: false })
		g.false_constant = op
		return op
	}
}

fn (mut g Generator) add_constant_null() ScopedOperand {
	if op := g.null_constant {
		return op
	}
	op := g.append_constant(ConstantValue{ tag: .null_val })
	g.null_constant = op
	return op
}

fn (mut g Generator) add_constant_undefined() ScopedOperand {
	if op := g.undefined_constant {
		return op
	}
	op := g.append_constant(ConstantValue{ tag: .undefined_val })
	g.undefined_constant = op
	return op
}

fn (mut g Generator) add_constant_empty() ScopedOperand {
	if op := g.empty_constant {
		return op
	}
	op := g.append_constant(ConstantValue{ tag: .empty_val })
	g.empty_constant = op
	return op
}

fn (mut g Generator) add_constant_string(value Utf16String) ScopedOperand {
	key := value.str()
	if key in g.string_constants {
		return g.string_constants[key]
	}
	op := g.append_constant(ConstantValue{ tag: .string_val, string_value: value })
	g.string_constants[key] = op
	return op
}

fn (mut g Generator) add_constant_bigint(value string) ScopedOperand {
	return g.append_constant(ConstantValue{ tag: .bigint_val, bigint_value: value })
}

fn (mut g Generator) add_constant_raw_value(value u64) ScopedOperand {
	return g.append_constant(ConstantValue{ tag: .raw_value, raw_u64_value: value })
}

fn (mut g Generator) add_constant_i32(val i32) ScopedOperand {
	return g.add_constant_number(f64(val))
}

// get_constant gets the constant value for a constant operand.
fn (g &Generator) get_constant(operand ScopedOperand) ?ConstantValue {
	if operand.operand.is_constant() {
		idx := int(operand.operand.index())
		if idx < g.constants.len {
			return g.constants[idx]
		}
	}
	return none
}

// --- Table interning ---

fn (mut g Generator) intern_string(s []u16) StringTableIndex {
	key := string_from_utf16_lossy(s)
	if key in g.string_table_index {
		return g.string_table_index[key]
	}
	index := StringTableIndex{
		value: u32(g.string_table.len)
	}
	utf16_key := Utf16String.from_slice(s)
	g.string_table << utf16_key
	g.string_table_index[key] = index
	return index
}

fn (mut g Generator) intern_identifier(s []u16) IdentifierTableIndex {
	key := string_from_utf16_lossy(s)
	if key in g.identifier_table_index {
		return g.identifier_table_index[key]
	}
	index := IdentifierTableIndex{
		value: u32(g.identifier_table.len)
	}
	utf16_key := Utf16String.from_slice(s)
	g.identifier_table << utf16_key
	g.identifier_table_index[key] = index
	return index
}

fn (mut g Generator) intern_property_key(s []u16) PropertyKeyTableIndex {
	key := string_from_utf16_lossy(s)
	if key in g.property_key_table_index {
		return g.property_key_table_index[key]
	}
	index := PropertyKeyTableIndex{
		value: u32(g.property_key_table.len)
	}
	utf16_key := Utf16String.from_slice(s)
	g.property_key_table << utf16_key
	g.property_key_table_index[key] = index
	return index
}

// try_constant_string_to_property_key converts a constant string operand to
// a property key index if it is not an array index.
fn (mut g Generator) try_constant_string_to_property_key(operand ScopedOperand) ?PropertyKeyTableIndex {
	if !operand.operand.is_constant() {
		return none
	}
	idx := int(operand.operand.index())
	if idx >= g.constants.len {
		return none
	}
	cv := g.constants[idx]
	if cv.tag != .string_val {
		return none
	}
	s := cv.string_value.as_slice()
	if is_array_index(s) {
		return none
	}
	key := string_from_utf16_lossy(s)
	if key in g.property_key_table_index {
		return g.property_key_table_index[key]
	}
	// Cold path: not yet interned
	owned := Utf16String.from_slice(s)
	key_index := PropertyKeyTableIndex{
		value: u32(g.property_key_table.len)
	}
	g.property_key_table << owned
	g.property_key_table_index[key] = key_index
	return key_index
}

// register_shared_function_data registers a SharedFunctionInstanceData and returns its index.
fn (mut g Generator) register_shared_function_data(ptr voidptr) u32 {
	index := u32(g.shared_function_data.len)
	g.shared_function_data << ptr
	return index
}

// register_class_blueprint registers a ClassBlueprint and returns its index.
fn (mut g Generator) register_class_blueprint(ptr voidptr) u32 {
	index := u32(g.class_blueprints.len)
	g.class_blueprints << ptr
	return index
}

fn (mut g Generator) intern_regex(compiled voidptr) RegexTableIndex {
	index := u32(g.compiled_regexes.len)
	g.compiled_regexes << compiled
	return RegexTableIndex{
		value: index
	}
}

// --- Basic block management ---

// make_block creates a new basic block and returns its label.
fn (mut g Generator) make_block() Label {
	index := g.basic_blocks.len
	mut block := BasicBlock.new(u32(index))
	if handler := g.current_unwind_handler {
		block.handler = handler
	}
	g.basic_blocks << block
	return Label.new(u32(index))
}

// switch_to_basic_block switches emission to the given basic block.
fn (mut g Generator) switch_to_basic_block(label Label) {
	g.current_block_index = label
}

// current_block_index_ gets the current basic block's label.
fn (g &Generator) current_block_index_() Label {
	return g.current_block_index
}

// is_current_block_terminated checks if the current block is terminated.
fn (g &Generator) is_current_block_terminated() bool {
	return g.basic_blocks[g.current_block_index.basic_block_index()].terminated
}

// basic_block_count returns the number of basic blocks.
fn (g &Generator) basic_block_count() int {
	return g.basic_blocks.len
}

// terminate_unterminated_blocks_with_yield terminates all unterminated blocks
// with Yield (no continuation). Used for generator and async functions.
fn (mut g Generator) terminate_unterminated_blocks_with_yield() {
	block_count := g.basic_block_count()
	for i in 0 .. block_count {
		label := Label.new(u32(i))
		if g.is_block_terminated(label) {
			continue
		}
		g.switch_to_basic_block(label)
		undef := g.add_constant_undefined()
		g.emit(Instruction.yield_instr(none, undef.operand))
	}
}

// is_block_terminated checks if a specific block is terminated.
fn (g &Generator) is_block_terminated(label Label) bool {
	return g.basic_blocks[label.basic_block_index()].terminated
}

// --- Instruction emission ---

// emit emits an instruction to the current basic block.
fn (mut g Generator) emit(instruction Instruction) {
	if g.is_current_block_terminated() {
		return
	}
	source_map := SourceMapEntry{
		bytecode_offset: 0
		source_start:    g.current_source_start
		source_end:      g.current_source_end
	}
	g.basic_blocks[g.current_block_index.basic_block_index()].append(instruction, source_map)
}

// emit_mov emits a Mov instruction (optimized away if src == dst).
fn (mut g Generator) emit_mov(dst ScopedOperand, src ScopedOperand) {
	if dst != src {
		g.emit(Instruction.mov(dst.operand, src.operand))
	}
}

fn (mut g Generator) emit_mov_raw(dst Operand, src Operand) {
	g.emit(Instruction.mov(dst, src))
}

// emit_jump_if emits a conditional jump, with comparison fusion and constant folding.
fn (mut g Generator) emit_jump_if(condition ScopedOperand, true_target Label, false_target Label) {
	// OPTIMIZATION: If condition is a constant, emit an unconditional jump.
	if cv := g.get_constant(condition) {
		if is_truthy := constant_to_boolean(cv) {
			if is_truthy {
				g.emit(Instruction.jump(true_target))
			} else {
				g.emit(Instruction.jump(false_target))
			}
			return
		}
	}

	// OPTIMIZATION: If the condition is a register, try to fuse with the last
	// comparison instruction.
	if condition.operand.is_register() {
		block := &g.basic_blocks[g.current_block_index.basic_block_index()]
		if block.instructions.len > 0 {
			last := block.instructions.last()
			fused := try_fuse_comparison(last.instruction, condition.operand, true_target,
				false_target)
			if fused_instr := fused {
				// Remove the comparison and emit the fused jump.
				g.basic_blocks[g.current_block_index.basic_block_index()].instructions.pop()
				g.emit(fused_instr)
				return
			}
		}
	}

	g.emit(Instruction.jump_if(condition.operand, true_target, false_target))
}

// try_fuse_comparison attempts to fuse a comparison instruction with a conditional jump.
fn try_fuse_comparison(instr Instruction, cond_operand Operand, true_target Label, false_target Label) ?Instruction {
	match instr.tag {
		.less_than {
			if instr.dst == cond_operand {
				return Instruction.jump_less_than(instr.lhs, instr.rhs, true_target, false_target)
			}
		}
		.less_than_equals {
			if instr.dst == cond_operand {
				return Instruction.jump_less_than_equals(instr.lhs, instr.rhs, true_target,
					false_target)
			}
		}
		.greater_than {
			if instr.dst == cond_operand {
				return Instruction.jump_greater_than(instr.lhs, instr.rhs, true_target,
					false_target)
			}
		}
		.greater_than_equals {
			if instr.dst == cond_operand {
				return Instruction.jump_greater_than_equals(instr.lhs, instr.rhs, true_target,
					false_target)
			}
		}
		.loosely_equals {
			if instr.dst == cond_operand {
				return Instruction.jump_loosely_equals(instr.lhs, instr.rhs, true_target,
					false_target)
			}
		}
		.loosely_inequals {
			if instr.dst == cond_operand {
				return Instruction.jump_loosely_inequals(instr.lhs, instr.rhs, true_target,
					false_target)
			}
		}
		.strictly_equals {
			if instr.dst == cond_operand {
				return Instruction.jump_strictly_equals(instr.lhs, instr.rhs, true_target,
					false_target)
			}
		}
		.strictly_inequals {
			if instr.dst == cond_operand {
				return Instruction.jump_strictly_inequals(instr.lhs, instr.rhs, true_target,
					false_target)
			}
		}
		else {}
	}
	return none
}

// --- Cache index allocation ---

fn (mut g Generator) next_property_lookup_cache_() u32 {
	index := g.next_property_lookup_cache
	g.next_property_lookup_cache += 1
	return index
}

fn (mut g Generator) next_global_variable_cache_() u32 {
	index := g.next_global_variable_cache
	g.next_global_variable_cache += 1
	return index
}

fn (mut g Generator) next_template_object_cache_() u32 {
	index := g.next_template_object_cache
	g.next_template_object_cache += 1
	return index
}

fn (mut g Generator) next_object_shape_cache_() u32 {
	index := g.next_object_shape_cache
	g.next_object_shape_cache += 1
	return index
}

// --- Lexical environment helpers ---

fn (mut g Generator) current_lexical_environment() ScopedOperand {
	if g.lexical_environment_register_stack.len > 0 {
		return g.lexical_environment_register_stack.last()
	}
	return g.scoped_operand(Operand.register(register_saved_lexical_environment))
}

fn (mut g Generator) end_variable_scope() {
	g.end_boundary(.leave_lexical_environment)
	g.lexical_environment_register_stack.pop()
	if !g.is_current_block_terminated() {
		parent := g.current_lexical_environment()
		g.emit(Instruction.set_lexical_environment(parent.operand))
	}
}

fn (mut g Generator) allocate_completion_register() ?ScopedOperand {
	if g.must_propagate_completion {
		reg := g.allocate_register()
		undef := g.add_constant_undefined()
		g.emit_mov(reg, undef)
		return reg
	}
	return none
}

fn (mut g Generator) push_new_lexical_environment(capacity u32) ScopedOperand {
	parent := g.current_lexical_environment()
	new_env := g.allocate_register()
	g.emit(Instruction.create_lexical_environment(new_env.operand, parent.operand, capacity))
	g.lexical_environment_register_stack << new_env
	return new_env
}

// --- Boundary management ---

fn (mut g Generator) start_boundary(ty BlockBoundaryType) {
	g.boundaries << ty
}

fn (mut g Generator) end_boundary(ty BlockBoundaryType) {
	if g.boundaries.len > 0 && g.boundaries.last() == ty {
		g.boundaries.pop()
	}
}

// --- Break/continue scope management ---

fn (mut g Generator) begin_breakable_scope(target Label, label_set []Utf16String, completion ?ScopedOperand) {
	g.breakable_scopes << LabelableScope{
		bytecode_target:     target
		language_label_set:  label_set
		completion_register: completion
	}
	g.start_boundary(.break_boundary)
}

fn (mut g Generator) end_breakable_scope() {
	g.end_boundary(.break_boundary)
	g.breakable_scopes.pop()
}

fn (mut g Generator) begin_continuable_scope(target Label, label_set []Utf16String, completion ?ScopedOperand) {
	g.continuable_scopes << LabelableScope{
		bytecode_target:     target
		language_label_set:  label_set
		completion_register: completion
	}
	g.start_boundary(.continue_boundary)
}

fn (mut g Generator) end_continuable_scope() {
	g.end_boundary(.continue_boundary)
	g.continuable_scopes.pop()
}

fn (mut g Generator) set_current_breakable_scope_completion_register(completion ScopedOperand) {
	if g.breakable_scopes.len == 0 {
		panic('no active breakable scope')
	}
	g.breakable_scopes[g.breakable_scopes.len - 1].completion_register = completion
}

fn (g &Generator) find_breakable_scope(label ?[]u16) ?&LabelableScope {
	if lbl := label {
		for i := g.breakable_scopes.len - 1; i >= 0; i-- {
			for l in g.breakable_scopes[i].language_label_set {
				if utf16_equals(l.data, lbl) {
					return &g.breakable_scopes[i]
				}
			}
		}
		return none
	}
	if g.breakable_scopes.len > 0 {
		return &g.breakable_scopes[g.breakable_scopes.len - 1]
	}
	return none
}

fn (g &Generator) find_continuable_scope(label ?[]u16) ?&LabelableScope {
	if lbl := label {
		for i := g.continuable_scopes.len - 1; i >= 0; i-- {
			for l in g.continuable_scopes[i].language_label_set {
				if utf16_equals(l.data, lbl) {
					return &g.continuable_scopes[i]
				}
			}
		}
		return none
	}
	if g.continuable_scopes.len > 0 {
		return &g.continuable_scopes[g.continuable_scopes.len - 1]
	}
	return none
}

// --- FinallyContext support ---

// push_finally_context pushes a new FinallyContext and sets it as current.
fn (mut g Generator) push_finally_context(ctx FinallyContext) int {
	index := g.finally_contexts.len
	g.finally_contexts << ctx
	g.current_finally_context = index
	return index
}

// has_outer_finally_before_target checks if there is an outer ReturnToFinally
// boundary between boundary_index and the matching break/continue boundary.
fn (g &Generator) has_outer_finally_before_target(is_break bool, boundary_index int) bool {
	mut j := boundary_index - 2
	for j >= 0 {
		inner := g.boundaries[j]
		if is_break && inner == .break_boundary {
			return false
		}
		if !is_break && inner == .continue_boundary {
			return false
		}
		if inner == .return_to_finally {
			return true
		}
		j--
	}
	return false
}

// register_jump_in_finally_context registers a jump target with the current
// FinallyContext. Assigns a unique completion_type index and emits code to
// set it and jump to finally.
fn (mut g Generator) register_jump_in_finally_context(target Label) {
	index := g.current_finally_context
	if index < 0 {
		panic('no active finally context')
	}
	jump_index := g.finally_contexts[index].next_jump_index
	g.finally_contexts[index].next_jump_index += 1
	g.finally_contexts[index].registered_jumps << FinallyJump{
		index:  jump_index
		target: target
	}
	completion_type := g.finally_contexts[index].completion_type
	finally_body := g.finally_contexts[index].finally_body
	index_const := g.add_constant_i32(jump_index)
	g.emit_mov(completion_type, index_const)
	g.emit(Instruction.jump(finally_body))
}

// emit_trampoline_through_finally creates a trampoline block for
// break/continue through nested finally.
fn (mut g Generator) emit_trampoline_through_finally() {
	trampoline_block := g.make_block()
	g.register_jump_in_finally_context(trampoline_block)
	g.switch_to_basic_block(trampoline_block)
	index := g.current_finally_context
	if index < 0 {
		panic('no active finally context')
	}
	g.current_finally_context = g.finally_contexts[index].parent_index
}

// generate_break generates a break, walking boundaries and handling FinallyContext.
fn (mut g Generator) generate_break(label ?[]u16) {
	if lbl := label {
		g.generate_labelled_jump(true, lbl)
	} else {
		g.generate_scoped_jump(true)
	}
}

// generate_continue generates a continue, walking boundaries and handling FinallyContext.
fn (mut g Generator) generate_continue(label ?[]u16) {
	if lbl := label {
		g.generate_labelled_jump(false, lbl)
	} else {
		g.generate_scoped_jump(false)
	}
}

// generate_scoped_jump walks boundaries for unlabelled break/continue.
fn (mut g Generator) generate_scoped_jump(is_break bool) {
	saved_ctx := g.current_finally_context
	mut env_offset := g.lexical_environment_register_stack.len

	mut i := g.boundaries.len
	for i > 0 {
		i--
		boundary := g.boundaries[i]
		match boundary {
			.break_boundary {
				if is_break {
					scope := if g.breakable_scopes.len > 0 {
						g.breakable_scopes[g.breakable_scopes.len - 1]
					} else {
						panic('no active breakable scope')
					}
					target := scope.bytecode_target
					if cur_comp := g.current_completion_register {
						if comp := scope.completion_register {
							if cur_comp != comp {
								g.emit_mov(comp, cur_comp)
							}
						}
					}
					g.emit(Instruction.jump(target))
					g.current_finally_context = saved_ctx
					return
				}
			}
			.continue_boundary {
				if !is_break {
					scope := if g.continuable_scopes.len > 0 {
						g.continuable_scopes[g.continuable_scopes.len - 1]
					} else {
						panic('no active continuable scope')
					}
					target := scope.bytecode_target
					if cur_comp := g.current_completion_register {
						if comp := scope.completion_register {
							if cur_comp != comp {
								g.emit_mov(comp, cur_comp)
							}
						}
					}
					g.emit(Instruction.jump(target))
					g.current_finally_context = saved_ctx
					return
				}
			}
			.leave_lexical_environment {
				env_offset--
				env := g.lexical_environment_register_stack[env_offset - 1]
				g.emit(Instruction.set_lexical_environment(env.operand))
			}
			.return_to_finally {
				if !g.has_outer_finally_before_target(is_break, i + 1) {
					scope := if is_break {
						if g.breakable_scopes.len > 0 {
							g.breakable_scopes[g.breakable_scopes.len - 1]
						} else {
							panic('no active breakable scope')
						}
					} else {
						if g.continuable_scopes.len > 0 {
							g.continuable_scopes[g.continuable_scopes.len - 1]
						} else {
							panic('no active continuable scope')
						}
					}
					target := scope.bytecode_target
					if cur_comp := g.current_completion_register {
						if comp := scope.completion_register {
							if cur_comp != comp {
								g.emit_mov(comp, cur_comp)
							}
						}
					}
					g.register_jump_in_finally_context(target)
					g.current_finally_context = saved_ctx
					return
				}
				g.emit_trampoline_through_finally()
			}
			.leave_finally {}
		}
	}
	g.current_finally_context = saved_ctx
}

// generate_labelled_jump walks boundaries for labelled break/continue.
fn (mut g Generator) generate_labelled_jump(is_break bool, label []u16) {
	saved_ctx := g.current_finally_context
	mut env_offset := g.lexical_environment_register_stack.len

	mut jumpable_scopes := []JumpableScopeInfo{}
	if is_break {
		for si := g.breakable_scopes.len - 1; si >= 0; si-- {
			s := g.breakable_scopes[si]
			jumpable_scopes << JumpableScopeInfo{
				target:     s.bytecode_target
				label_set:  s.language_label_set
				completion: s.completion_register
			}
		}
	} else {
		for si := g.continuable_scopes.len - 1; si >= 0; si-- {
			s := g.continuable_scopes[si]
			jumpable_scopes << JumpableScopeInfo{
				target:     s.bytecode_target
				label_set:  s.language_label_set
				completion: s.completion_register
			}
		}
	}

	mut current_boundary := g.boundaries.len

	for scope_info in jumpable_scopes {
		for current_boundary > 0 {
			current_boundary--
			boundary := g.boundaries[current_boundary]
			match boundary {
				.leave_lexical_environment {
					env_offset--
					env := g.lexical_environment_register_stack[env_offset - 1]
					g.emit(Instruction.set_lexical_environment(env.operand))
				}
				.return_to_finally {
					if !g.has_outer_finally_before_target(is_break, current_boundary + 1)
						&& label_set_contains(scope_info.label_set, label) {
						if cur_comp := g.current_completion_register {
							if comp := scope_info.completion {
								if cur_comp != comp {
									g.emit_mov(comp, cur_comp)
								}
							}
						}
						g.register_jump_in_finally_context(scope_info.target)
						g.current_finally_context = saved_ctx
						return
					}
					g.emit_trampoline_through_finally()
				}
				else {
					if (is_break && boundary == .break_boundary)
						|| (!is_break && boundary == .continue_boundary) {
						break
					}
				}
			}
		}

		if label_set_contains(scope_info.label_set, label) {
			if cur_comp := g.current_completion_register {
				if comp := scope_info.completion {
					if cur_comp != comp {
						g.emit_mov(comp, cur_comp)
					}
				}
			}
			g.emit(Instruction.jump(scope_info.target))
			g.current_finally_context = saved_ctx
			return
		}
	}
	g.current_finally_context = saved_ctx
}

// Helper struct for labelled jump scope iteration.
struct JumpableScopeInfo {
	target     Label
	label_set  []Utf16String
	completion ?ScopedOperand
}

// label_set_contains checks if any label in the set matches the given label.
fn label_set_contains(label_set []Utf16String, label []u16) bool {
	for l in label_set {
		if utf16_equals(l.data, label) {
			return true
		}
	}
	return false
}

// perform_needed_unwinds walks the boundary stack and emits SetLexicalEnvironment
// instructions for each LeaveLexicalEnvironment boundary.
fn (mut g Generator) perform_needed_unwinds() {
	mut env_stack_offset := g.lexical_environment_register_stack.len
	for i := g.boundaries.len - 1; i >= 0; i-- {
		match g.boundaries[i] {
			.leave_lexical_environment {
				env_stack_offset--
				parent_env := g.lexical_environment_register_stack[env_stack_offset - 1]
				g.emit(Instruction.set_lexical_environment(parent_env.operand))
			}
			.return_to_finally {
				return
			}
			else {}
		}
	}
}

// generate_return generates a return, routing through FinallyContext if needed.
fn (mut g Generator) generate_return(value ScopedOperand) {
	g.perform_needed_unwinds()
	if g.current_finally_context >= 0 {
		index := g.current_finally_context
		ctx := g.finally_contexts[index]
		g.emit_mov(ctx.completion_value, value)
		ret_const := g.add_constant_i32(finally_return)
		g.emit_mov(ctx.completion_type, ret_const)
		g.emit(Instruction.jump(ctx.finally_body))
	} else if g.is_in_generator_or_async_function() {
		g.emit(Instruction.yield_instr(none, value.operand))
	} else {
		g.emit(Instruction.return_instr(value.operand))
	}
}

// --- Local variable initialization tracking ---

fn (g &Generator) is_local_initialized(index u32) bool {
	idx := int(index)
	if idx < g.initialized_locals.len {
		return g.initialized_locals[idx]
	}
	return false
}

fn (g &Generator) is_local_lexically_declared(index u32) bool {
	idx := int(index)
	if idx < g.local_variables.len {
		return g.local_variables[idx].is_lexically_declared
	}
	return false
}

fn (mut g Generator) mark_local_initialized(index u32) {
	idx := int(index)
	for g.initialized_locals.len <= idx {
		g.initialized_locals << false
	}
	g.initialized_locals[idx] = true
}

fn (g &Generator) is_argument_initialized(index u32) bool {
	idx := int(index)
	if idx < g.initialized_arguments.len {
		return g.initialized_arguments[idx]
	}
	return false
}

fn (mut g Generator) mark_argument_initialized(index u32) {
	idx := int(index)
	for g.initialized_arguments.len <= idx {
		g.initialized_arguments << false
	}
	g.initialized_arguments[idx] = true
}

// --- Compile/assemble/link pipeline ---

// Replacement action during assembly phase 2.
enum InstAction {
	emit
	skip
	jump_to_return
	jump_to_end
	emit_jump_true
	emit_jump_false
}

// Per-instruction action with associated data for assembly.
struct InstActionData {
mut:
	action    InstAction
	value     Operand
	condition Operand
	target    Label
}

// AssembledBytecode is the result of assembling bytecode from basic blocks.
struct AssembledBytecode {
mut:
	bytecode                  []u8
	source_map                []SourceMapEntry
	exception_handlers        []ExceptionHandler
	basic_block_start_offsets []int
	number_of_registers       u32
}

// ExceptionHandler represents an exception handler range (with byte offsets, post-linking).
struct ExceptionHandler {
mut:
	start_offset   u32
	end_offset     u32
	handler_offset u32
}

// assemble compiles all basic blocks into a flat bytecode buffer.
fn (mut g Generator) assemble() AssembledBytecode {
	// If any block is unterminated, ensure the undefined constant exists.
	has_unterminated := g.basic_blocks.any(!it.terminated)
	mut undefined_constant_operand := Operand.invalid()
	if has_unterminated {
		undefined_constant_operand = g.add_constant_undefined().operand
	}

	number_of_registers := g.next_register
	number_of_locals := u32(g.local_variables.len)
	number_of_constants := u32(g.constants.len)

	// Phase 1: Operand rewriting
	for mut block in g.basic_blocks {
		for mut ism in block.instructions {
			ism.instruction.rewrite_operands(number_of_registers, number_of_locals, number_of_constants)
		}
	}

	// Phase 2: Compute block byte offsets with optimizations
	num_blocks := g.basic_blocks.len
	mut block_offsets := []int{cap: num_blocks}
	mut actions := [][]InstActionData{cap: num_blocks}
	mut offset := 0

	for block_index in 0 .. num_blocks {
		block_offsets << offset
		block := g.basic_blocks[block_index]
		mut block_actions := []InstActionData{cap: block.instructions.len}

		for ism in block.instructions {
			instr := ism.instruction
			match instr.tag {
				.jump {
					if t := instr.target {
						target_block := int(t.value)
						// Skip jumps to next block
						if target_block == block_index + 1 {
							block_actions << InstActionData{
								action: .skip
							}
							continue
						}
						// Inline return/end from single-instruction target blocks
						target_blk := g.basic_blocks[target_block]
						if target_blk.terminated && target_blk.instructions.len == 1 {
							ti := target_blk.instructions[0].instruction
							if ti.tag == .return_op {
								replacement := Instruction.return_instr(ti.value)
								block_actions << InstActionData{
									action: .jump_to_return
									value:  ti.value
								}
								offset += replacement.encoded_size()
								continue
							}
							if ti.tag == .end_op {
								replacement := Instruction.end_instr(ti.value)
								block_actions << InstActionData{
									action: .jump_to_end
									value:  ti.value
								}
								offset += replacement.encoded_size()
								continue
							}
						}
					}
					block_actions << InstActionData{
						action: .emit
					}
					offset += instr.encoded_size()
				}
				.jump_if {
					if tt := instr.true_target {
						if ft := instr.false_target {
							true_block := int(tt.value)
							false_block := int(ft.value)
							// Replace JumpIf where one target is next block
							if true_block == block_index + 1 {
								replacement := Instruction.jump_false(instr.condition,
									ft)
								block_actions << InstActionData{
									action:    .emit_jump_false
									condition: instr.condition
									target:    ft
								}
								offset += replacement.encoded_size()
								continue
							}
							if false_block == block_index + 1 {
								replacement := Instruction.jump_true(instr.condition,
									tt)
								block_actions << InstActionData{
									action:    .emit_jump_true
									condition: instr.condition
									target:    tt
								}
								offset += replacement.encoded_size()
								continue
							}
						}
					}
					block_actions << InstActionData{
						action: .emit
					}
					offset += instr.encoded_size()
				}
				else {
					block_actions << InstActionData{
						action: .emit
					}
					offset += instr.encoded_size()
				}
			}
		}

		// Unterminated blocks get an implicit End(undefined) appended
		if !block.terminated {
			dummy_end := Instruction.end_instr(Operand.constant(0))
			offset += dummy_end.encoded_size()
		}
		actions << block_actions
	}

	// Phase 3: Patch labels (block index -> byte offset)
	for mut block in g.basic_blocks {
		for mut ism in block.instructions {
			ism.instruction.patch_labels(block_offsets)
		}
	}

	// Phase 4: Encode to bytes with optimizations applied
	mut bytecode := []u8{cap: offset}
	mut source_map := []SourceMapEntry{}
	mut exception_handlers := []ExceptionHandler{}
	mut basic_block_start_offsets := []int{cap: num_blocks}

	for block_index, block in g.basic_blocks {
		basic_block_start_offsets << bytecode.len
		block_start := bytecode.len
		handler := block.handler
		block_actions := actions[block_index]

		for instruction_index, ism in block.instructions {
			action_data := block_actions[instruction_index]
			match action_data.action {
				.skip {
					if basic_block_start_offsets.len > 0
						&& basic_block_start_offsets.last() == bytecode.len {
						basic_block_start_offsets.pop()
					}
				}
				.emit {
					instruction_offset := bytecode.len
					source_map << SourceMapEntry{
						bytecode_offset: u32(instruction_offset)
						source_start:    ism.source_map.source_start
						source_end:      ism.source_map.source_end
					}
					ism.instruction.encode(g.strict, mut bytecode)
				}
				.jump_to_return {
					instruction_offset := bytecode.len
					source_map << SourceMapEntry{
						bytecode_offset: u32(instruction_offset)
						source_start:    ism.source_map.source_start
						source_end:      ism.source_map.source_end
					}
					replacement := Instruction.return_instr(action_data.value)
					replacement.encode(g.strict, mut bytecode)
				}
				.jump_to_end {
					instruction_offset := bytecode.len
					source_map << SourceMapEntry{
						bytecode_offset: u32(instruction_offset)
						source_start:    ism.source_map.source_start
						source_end:      ism.source_map.source_end
					}
					replacement := Instruction.end_instr(action_data.value)
					replacement.encode(g.strict, mut bytecode)
				}
				.emit_jump_false {
					mut target_label := action_data.target
					target_block_idx := int(target_label.value)
					if target_block_idx < block_offsets.len {
						target_label.value = u32(block_offsets[target_block_idx])
					}
					instruction_offset := bytecode.len
					source_map << SourceMapEntry{
						bytecode_offset: u32(instruction_offset)
						source_start:    ism.source_map.source_start
						source_end:      ism.source_map.source_end
					}
					replacement := Instruction.jump_false(action_data.condition, target_label)
					replacement.encode(g.strict, mut bytecode)
				}
				.emit_jump_true {
					mut target_label := action_data.target
					target_block_idx := int(target_label.value)
					if target_block_idx < block_offsets.len {
						target_label.value = u32(block_offsets[target_block_idx])
					}
					instruction_offset := bytecode.len
					source_map << SourceMapEntry{
						bytecode_offset: u32(instruction_offset)
						source_start:    ism.source_map.source_start
						source_end:      ism.source_map.source_end
					}
					replacement := Instruction.jump_true(action_data.condition, target_label)
					replacement.encode(g.strict, mut bytecode)
				}
			}
		}

		// Unterminated blocks get an implicit End(undefined)
		if !block.terminated {
			mut undef_rewritten := undefined_constant_operand
			undef_rewritten.offset_index_by(number_of_registers + number_of_locals)
			end_instruction := Instruction.end_instr(undef_rewritten)
			instruction_offset := bytecode.len
			source_map << SourceMapEntry{
				bytecode_offset: u32(instruction_offset)
				source_start:    0
				source_end:      0
			}
			end_instruction.encode(g.strict, mut bytecode)
		}

		// Close exception handler range
		if handler_label := handler {
			exception_handlers << ExceptionHandler{
				start_offset:   u32(block_start)
				end_offset:     u32(bytecode.len)
				handler_offset: u32(block_offsets[handler_label.basic_block_index()])
			}
		}
	}

	// Merge adjacent exception handlers with the same handler offset
	mut merged_handlers := []ExceptionHandler{}
	for handler in exception_handlers {
		if merged_handlers.len > 0 {
			mut last := merged_handlers[merged_handlers.len - 1]
			if last.end_offset == handler.start_offset
				&& last.handler_offset == handler.handler_offset {
				merged_handlers[merged_handlers.len - 1].end_offset = handler.end_offset
				continue
			}
		}
		merged_handlers << handler
	}
	merged_handlers.sort(a.start_offset < b.start_offset)

	return AssembledBytecode{
		bytecode:                  bytecode
		source_map:                source_map
		exception_handlers:        merged_handlers
		basic_block_start_offsets: basic_block_start_offsets
		number_of_registers:       number_of_registers
	}
}

// --- ConstantValue ---

// A typed constant value stored in the constant pool.
enum ConstantValueTag {
	number
	boolean
	null_val
	undefined_val
	empty_val
	string_val
	bigint_val
	raw_value
}

struct ConstantValue {
mut:
	tag           ConstantValueTag
	number_value  f64
	bool_value    bool
	string_value  Utf16String
	bigint_value  string
	raw_u64_value u64
}

// constant_to_boolean converts a constant value to a boolean, matching JS ToBoolean.
fn constant_to_boolean(value ConstantValue) ?bool {
	match value.tag {
		.boolean {
			return value.bool_value
		}
		.null_val, .undefined_val, .empty_val {
			return false
		}
		.number {
			return value.number_value != 0.0 && !math.is_nan(value.number_value)
		}
		.string_val {
			return !value.string_value.is_empty()
		}
		.bigint_val {
			return value.bigint_value != '0' && value.bigint_value != ''
		}
		.raw_value {
			return none
		}
	}
}

// is_array_index checks if a UTF-16 string is a valid array index (0..0xFFFFFFFE).
fn is_array_index(s []u16) bool {
	if s.len == 0 || s.len > 10 {
		return false
	}
	if s.len > 1 && s[0] == u16(`0`) {
		return false
	}
	mut value := u64(0)
	for c in s {
		if c < u16(`0`) || c > u16(`9`) {
			return false
		}
		value = value * 10 + u64(c - u16(`0`))
	}
	return value <= 0xFFFF_FFFE
}

// choose_dst uses preferred_dst if available, otherwise allocates a fresh register.
fn choose_dst(mut generator Generator, preferred_dst ?ScopedOperand) ScopedOperand {
	if dst := preferred_dst {
		return dst
	}
	return generator.allocate_register()
}
