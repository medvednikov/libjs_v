module main

// =============================================================================
// Statement codegen + remaining expression generators
// =============================================================================

fn generate_statement(statement &Statement, mut gen Generator) ?ScopedOperand {
	saved_source_start := gen.current_source_start
	saved_source_end := gen.current_source_end
	gen.current_source_start = statement.range.start.offset
	gen.current_source_end = statement.range.end_.offset

	result := generate_statement_inner(statement, mut gen)

	gen.current_source_start = saved_source_start
	gen.current_source_end = saved_source_end
	return result
}

fn generate_statement_inner(statement &Statement, mut gen Generator) ?ScopedOperand {
	match statement.kind.tag {
		.empty, .debugger_stmt {
			return none
		}
		.expression_stmt {
			if expr := statement.kind.expression {
				return generate_expression(expr, mut gen, none)
			}
			return none
		}
		.return_stmt {
			value := if arg := statement.kind.return_arg {
				generate_expression_or_undefined(arg, mut gen, none)
			} else {
				gen.add_constant_undefined()
			}
			gen.generate_return(value)
			return none
		}
		.throw_stmt {
			if expr := statement.kind.throw_expr {
				value := generate_expression(expr, mut gen, none) or { return none }
				gen.emit(Instruction{
					tag: .throw_op
					src: value.operand
				})
			}
			return none
		}
		.block, .function_body {
			if scope := statement.kind.scope {
				return generate_block_statement(mut gen, scope)
			}
			return none
		}
		.if_stmt {
			return generate_if_statement(mut gen, statement)
		}
		.while_stmt {
			return generate_while_statement(mut gen, statement)
		}
		.do_while {
			return generate_do_while_statement(mut gen, statement)
		}
		.for_stmt {
			return generate_for_statement(mut gen, statement)
		}
		.for_in_of {
			return generate_for_in_of_statement(mut gen, statement)
		}
		.switch_stmt {
			return generate_switch_statement(mut gen, statement)
		}
		.variable_declaration {
			return generate_variable_declaration(mut gen, statement)
		}
		.break_stmt {
			if tl := statement.kind.target_label {
				gen.generate_break(tl.as_slice())
			} else {
				gen.generate_break(none)
			}
			return none
		}
		.continue_stmt {
			if tl := statement.kind.target_label {
				gen.generate_continue(tl.as_slice())
			} else {
				gen.generate_continue(none)
			}
			return none
		}
		.labelled {
			return generate_labelled_statement(mut gen, statement)
		}
		.try_stmt {
			return generate_try_statement(mut gen, statement)
		}
		.with_stmt {
			if obj := statement.kind.object {
				obj_val := generate_expression(obj, mut gen, none) or { return none }
				gen.emit(Instruction{
					tag: .enter_object_environment
					src: obj_val.operand
				})
				gen.start_boundary(.leave_lexical_environment)
				new_env := gen.allocate_register()
				gen.emit(Instruction{
					tag: .get_lexical_environment
					dst: new_env.operand
				})
				gen.lexical_environment_register_stack << new_env
				if body := statement.kind.body {
					generate_statement(body, mut gen)
				}
				gen.end_variable_scope()
			}
			return none
		}
		.class_declaration {
			if cd := statement.kind.class_data {
				generate_class_expression(mut gen, cd, none)
			}
			return none
		}
		.function_declaration {
			// Function declarations are hoisted; handled by FDI.
			return none
		}
		.import_stmt, .export_stmt {
			// Module-level declarations handled elsewhere.
			return none
		}
		.program {
			if pd := statement.kind.program_data {
				for child in pd.scope.children {
					generate_statement(&child, mut gen)
					if gen.is_current_block_terminated() {
						break
					}
				}
			}
			return none
		}
		.using_declaration {
			return generate_variable_declaration(mut gen, statement)
		}
		.class_field_initializer {
			if expr := statement.kind.expression {
				return generate_expression(expr, mut gen, none)
			}
			return none
		}
		.error_stmt, .error_declaration {
			return none
		}
	}
}

// =============================================================================
// Statement generators
// =============================================================================

fn generate_if_statement(mut gen Generator, statement &Statement) ?ScopedOperand {
	test := statement.kind.test or { return none }
	condition := generate_expression(test, mut gen, none) or { return none }

	completion := gen.allocate_completion_register()
	then_block := gen.make_block()
	else_block := gen.make_block()
	end_block := gen.make_block()

	has_alternate := statement.kind.alternate != none
	false_target := if has_alternate { else_block } else { end_block }
	gen.emit_jump_if(condition, then_block, false_target)

	gen.switch_to_basic_block(then_block)
	if consequent := statement.kind.consequent {
		result := generate_with_completion(mut gen, consequent, completion)
		if comp := completion {
			if r := result {
				gen.emit_mov(comp, r)
			}
		}
	}
	if !gen.is_current_block_terminated() {
		gen.emit(Instruction.jump(end_block))
	}

	if has_alternate {
		gen.switch_to_basic_block(else_block)
		if alternate := statement.kind.alternate {
			result := generate_with_completion(mut gen, alternate, completion)
			if comp := completion {
				if r := result {
					gen.emit_mov(comp, r)
				}
			}
		}
		if !gen.is_current_block_terminated() {
			gen.emit(Instruction.jump(end_block))
		}
	}

	gen.switch_to_basic_block(end_block)
	return if comp := completion { ?ScopedOperand(comp) } else { none }
}

fn generate_while_statement(mut gen Generator, statement &Statement) ?ScopedOperand {
	test := statement.kind.test or { return none }

	completion := gen.allocate_completion_register()
	test_block := gen.make_block()
	body_block := gen.make_block()
	end_block := gen.make_block()

	gen.emit(Instruction.jump(test_block))
	gen.switch_to_basic_block(test_block)

	condition := generate_expression(test, mut gen, none) or { return none }
	gen.emit_jump_if(condition, body_block, end_block)

	gen.switch_to_basic_block(body_block)
	gen.begin_continuable_scope(test_block, gen.pending_labels.clone(), completion)
	gen.begin_breakable_scope(end_block, gen.pending_labels.clone(), completion)
	gen.pending_labels.clear()

	if body := statement.kind.body {
		result := generate_with_completion(mut gen, body, completion)
		if comp := completion {
			if r := result {
				gen.emit_mov(comp, r)
			}
		}
	}
	if !gen.is_current_block_terminated() {
		gen.emit(Instruction.jump(test_block))
	}

	gen.end_breakable_scope()
	gen.end_continuable_scope()
	gen.switch_to_basic_block(end_block)
	return if comp := completion { ?ScopedOperand(comp) } else { none }
}

fn generate_do_while_statement(mut gen Generator, statement &Statement) ?ScopedOperand {
	completion := gen.allocate_completion_register()
	body_block := gen.make_block()
	test_block := gen.make_block()
	end_block := gen.make_block()

	gen.emit(Instruction.jump(body_block))
	gen.switch_to_basic_block(body_block)
	gen.begin_continuable_scope(test_block, gen.pending_labels.clone(), completion)
	gen.begin_breakable_scope(end_block, gen.pending_labels.clone(), completion)
	gen.pending_labels.clear()

	if body := statement.kind.body {
		result := generate_with_completion(mut gen, body, completion)
		if comp := completion {
			if r := result {
				gen.emit_mov(comp, r)
			}
		}
	}

	if !gen.is_current_block_terminated() {
		gen.emit(Instruction.jump(test_block))
	}

	gen.end_breakable_scope()
	gen.end_continuable_scope()

	gen.switch_to_basic_block(test_block)
	if test := statement.kind.test {
		condition := generate_expression(test, mut gen, none) or { return none }
		gen.emit_jump_if(condition, body_block, end_block)
	}

	gen.switch_to_basic_block(end_block)
	return if comp := completion { ?ScopedOperand(comp) } else { none }
}

fn generate_for_statement(mut gen Generator, statement &Statement) ?ScopedOperand {
	// Handle initializer
	if init := statement.kind.for_init {
		match init {
			ForInitExpression {
				generate_expression(init.expression, mut gen, none)
			}
			ForInitDeclaration {
				generate_statement(init.statement, mut gen)
			}
		}
	}

	completion := gen.allocate_completion_register()
	test_block := gen.make_block()
	body_block := gen.make_block()
	update_block := gen.make_block()
	end_block := gen.make_block()

	gen.emit(Instruction.jump(test_block))
	gen.switch_to_basic_block(test_block)

	if test := statement.kind.test {
		condition := generate_expression(test, mut gen, none) or { return none }
		gen.emit_jump_if(condition, body_block, end_block)
	} else {
		gen.emit(Instruction.jump(body_block))
	}

	gen.switch_to_basic_block(body_block)
	gen.begin_continuable_scope(update_block, gen.pending_labels.clone(), completion)
	gen.begin_breakable_scope(end_block, gen.pending_labels.clone(), completion)
	gen.pending_labels.clear()

	if body := statement.kind.body {
		result := generate_with_completion(mut gen, body, completion)
		if comp := completion {
			if r := result {
				gen.emit_mov(comp, r)
			}
		}
	}
	if !gen.is_current_block_terminated() {
		gen.emit(Instruction.jump(update_block))
	}

	gen.end_breakable_scope()
	gen.end_continuable_scope()

	gen.switch_to_basic_block(update_block)
	if update := statement.kind.update {
		generate_expression(update, mut gen, none)
	}
	gen.emit(Instruction.jump(test_block))

	gen.switch_to_basic_block(end_block)
	return if comp := completion { ?ScopedOperand(comp) } else { none }
}

fn generate_for_in_of_statement(mut gen Generator, statement &Statement) ?ScopedOperand {
	// Stub: simplified for-in/of implementation
	rhs_expr := statement.kind.rhs or { return none }
	rhs_val := generate_expression(rhs_expr, mut gen, none) or { return none }

	hint := if statement.kind.for_in_of_kind == .for_await_of {
		u32(IteratorHint.async_iter)
	} else if statement.kind.for_in_of_kind == .for_in {
		u32(0)
	} else {
		u32(IteratorHint.sync_iter)
	}

	iterator := gen.allocate_register()
	if statement.kind.for_in_of_kind == .for_in {
		gen.emit(Instruction{
			tag: .get_object_property_iterator
			dst: iterator.operand
			src: rhs_val.operand
		})
	} else {
		gen.emit(Instruction{
			tag:      .get_iterator
			dst:      iterator.operand
			src:      rhs_val.operand
			hint_val: hint
		})
	}

	loop_block := gen.make_block()
	body_block := gen.make_block()
	end_block := gen.make_block()

	gen.emit(Instruction.jump(loop_block))
	gen.switch_to_basic_block(loop_block)

	next_value := gen.allocate_register()
	next_done := gen.allocate_register()
	gen.emit(Instruction{
		tag:        .iterator_next_unpack
		dst:        next_value.operand
		callee_dst: next_done.operand
		src:        iterator.operand
	})
	gen.emit_jump_if(next_done, end_block, body_block)

	gen.switch_to_basic_block(body_block)
	gen.begin_continuable_scope(loop_block, gen.pending_labels.clone(), none)
	gen.begin_breakable_scope(end_block, gen.pending_labels.clone(), none)
	gen.pending_labels.clear()

	// Assign to lhs
	if lhs := statement.kind.for_in_of_lhs {
		assign_to_for_in_of_lhs(mut gen, lhs, next_value)
	}

	if body := statement.kind.body {
		generate_statement(body, mut gen)
	}
	if !gen.is_current_block_terminated() {
		gen.emit(Instruction.jump(loop_block))
	}

	gen.end_breakable_scope()
	gen.end_continuable_scope()
	gen.switch_to_basic_block(end_block)
	return none
}

fn assign_to_for_in_of_lhs(mut gen Generator, lhs ForInOfLhs, value ScopedOperand) {
	match lhs {
		ForInOfLhsExpression {
			if lhs.expression.kind.tag == .identifier_expr {
				if ident := lhs.expression.kind.identifier {
					emit_set_variable(mut gen, ident, value)
					return
				}
			}
		}
		ForInOfLhsDeclaration {
			// Variable declaration in for-in/of header
			if lhs.statement.kind.tag == .variable_declaration {
				if lhs.statement.kind.declarations.len > 0 {
					decl := lhs.statement.kind.declarations[0]
					match decl.target {
						IdentifierVarTarget {
							ident := decl.target.identifier
							emit_set_variable(mut gen, ident, value)
						}
						BindingPatternVarTarget {
							generate_binding_pattern_bytecode(mut gen, &decl.target.pattern,
								.set_binding, value)
						}
					}
				}
			}
		}
		ForInOfLhsPattern {
			generate_binding_pattern_bytecode(mut gen, &lhs.pattern, .set_binding, value)
		}
	}
}

fn generate_switch_statement(mut gen Generator, statement &Statement) ?ScopedOperand {
	sd := statement.kind.switch_data or { return none }
	discriminant := generate_expression(&sd.discriminant, mut gen, none) or { return none }

	completion := gen.allocate_completion_register()
	end_block := gen.make_block()
	gen.begin_breakable_scope(end_block, gen.pending_labels.clone(), completion)
	gen.pending_labels.clear()

	// Create blocks for each case + default
	mut case_blocks := []Label{}
	mut default_block := end_block
	for i, case_ in sd.cases {
		block := gen.make_block()
		case_blocks << block
		if case_.test == none {
			default_block = block
		}
		_ = i
	}

	// Emit comparisons
	mut next_test_block := gen.make_block()
	for i, case_ in sd.cases {
		if test := case_.test {
			test_val := generate_expression(&test, mut gen, none) or { continue }
			gen.emit(Instruction{
				tag:          .jump_strictly_equals
				lhs:          discriminant.operand
				rhs:          test_val.operand
				true_target:  case_blocks[i]
				false_target: next_test_block
			})
			gen.switch_to_basic_block(next_test_block)
			next_test_block = gen.make_block()
		}
	}
	gen.emit(Instruction.jump(default_block))

	// Emit case bodies (fall-through)
	for i, case_ in sd.cases {
		gen.switch_to_basic_block(case_blocks[i])
		for child in case_.scope.children {
			result := generate_statement(&child, mut gen)
			if comp := completion {
				if r := result {
					gen.emit_mov(comp, r)
				}
			}
			if gen.is_current_block_terminated() {
				break
			}
		}
		if !gen.is_current_block_terminated() && i + 1 < sd.cases.len {
			gen.emit(Instruction.jump(case_blocks[i + 1]))
		}
	}
	if !gen.is_current_block_terminated() {
		gen.emit(Instruction.jump(end_block))
	}

	gen.end_breakable_scope()
	gen.switch_to_basic_block(end_block)
	return if comp := completion { ?ScopedOperand(comp) } else { none }
}

fn generate_variable_declaration(mut gen Generator, statement &Statement) ?ScopedOperand {
	for decl in statement.kind.declarations {
		if init := decl.init {
			match decl.target {
				IdentifierVarTarget {
					ident := decl.target.identifier
					if ident.is_local() {
						local_index := ident.local_index
						dst := if ident.local_type == 1 {
							gen.scoped_operand(Operand.argument(local_index))
						} else {
							gen.local(local_index)
						}
						gen.pending_lhs_name = gen.intern_identifier(ident.name.as_slice())
						value := generate_expression(&init, mut gen, dst) or { continue }
						gen.pending_lhs_name = none
						gen.emit_mov(dst, value)
						if ident.local_type == 2 {
							gen.mark_local_initialized(local_index)
						} else if ident.local_type == 1 {
							gen.mark_argument_initialized(local_index)
						}
					} else {
						gen.pending_lhs_name = gen.intern_identifier(ident.name.as_slice())
						value := generate_expression(&init, mut gen, none) or { continue }
						gen.pending_lhs_name = none
						emit_set_variable(mut gen, ident, value)
					}
				}
				BindingPatternVarTarget {
					value := generate_expression(&init, mut gen, none) or { continue }
					mode := if statement.kind.decl_kind == .let_kind
						|| statement.kind.decl_kind == .const_kind {
						BindingMode.initialize_lexical
					} else {
						BindingMode.set_binding
					}
					generate_binding_pattern_bytecode(mut gen, &decl.target.pattern, mode,
						value)
				}
			}
		}
	}
	return none
}

fn generate_labelled_statement(mut gen Generator, statement &Statement) ?ScopedOperand {
	label := statement.kind.label
	gen.pending_labels << label

	if item := statement.kind.labelled_item {
		if is_for_loop(item) || item.kind.tag == .while_stmt || item.kind.tag == .do_while
			|| item.kind.tag == .switch_stmt {
			return generate_statement(item, mut gen)
		}
		// Non-loop labelled statement gets a breakable scope.
		end_block := gen.make_block()
		completion := gen.allocate_completion_register()
		gen.begin_breakable_scope(end_block, gen.pending_labels.clone(), completion)
		gen.pending_labels.clear()

		result := generate_with_completion(mut gen, item, completion)
		if comp := completion {
			if r := result {
				gen.emit_mov(comp, r)
			}
		}
		if !gen.is_current_block_terminated() {
			gen.emit(Instruction.jump(end_block))
		}

		gen.end_breakable_scope()
		gen.switch_to_basic_block(end_block)
		return if comp := completion { ?ScopedOperand(comp) } else { none }
	}
	return none
}

fn generate_try_statement(mut gen Generator, statement &Statement) ?ScopedOperand {
	td := statement.kind.try_data or { return none }

	completion := gen.allocate_completion_register()
	handler_block := gen.make_block()
	end_block := gen.make_block()

	_ = td.finalizer != none // has_finally - TODO: implement finally support

	// Try block
	saved_handler := gen.current_unwind_handler
	gen.current_unwind_handler = handler_block
	generate_statement(&td.block, mut gen)
	gen.current_unwind_handler = saved_handler

	if !gen.is_current_block_terminated() {
		gen.emit(Instruction.jump(end_block))
	}

	// Catch block
	gen.switch_to_basic_block(handler_block)
	catch_reg := gen.allocate_register()
	gen.emit(Instruction{
		tag: .catch_op
		dst: catch_reg.operand
	})

	if handler := td.handler {
		if param := handler.parameter {
			match param {
				CatchBindingIdentifier {
					emit_set_variable(mut gen, param.identifier, catch_reg)
				}
				CatchBindingPattern {
					generate_binding_pattern_bytecode(mut gen, &param.pattern, .initialize_lexical,
						catch_reg)
				}
			}
		}
		generate_statement(&handler.body, mut gen)
	}

	if !gen.is_current_block_terminated() {
		gen.emit(Instruction.jump(end_block))
	}

	gen.switch_to_basic_block(end_block)
	return if comp := completion { ?ScopedOperand(comp) } else { none }
}

fn generate_block_statement(mut gen Generator, scope &ScopeData) ?ScopedOperand {
	needs_env := needs_block_declaration_instantiation(scope)
	if needs_env {
		emit_block_declaration_instantiation(mut gen, scope)
	}
	result := generate_scope_children(mut gen, scope)
	if needs_env {
		gen.end_variable_scope()
	}
	return result
}

fn generate_scope_children(mut gen Generator, scope &ScopeData) ?ScopedOperand {
	mut last := ?ScopedOperand(none)
	for child in scope.children {
		result := generate_statement(&child, mut gen)
		if result != none {
			last = result
		}
		if gen.is_current_block_terminated() {
			break
		}
	}
	return last
}

fn generate_with_completion(mut gen Generator, statement &Statement, completion ?ScopedOperand) ?ScopedOperand {
	saved := gen.current_completion_register
	if comp := completion {
		gen.current_completion_register = comp
	}
	result := generate_statement(statement, mut gen)
	gen.current_completion_register = saved
	return result
}

fn emit_block_declaration_instantiation(mut gen Generator, scope &ScopeData) {
	mut count := u32(0)
	for child in scope.children {
		match child.kind.tag {
			.variable_declaration {
				if child.kind.decl_kind == .let_kind || child.kind.decl_kind == .const_kind {
					for decl in child.kind.declarations {
						names := collect_target_names_from_target(decl.target)
						count += u32(names.len)
					}
				}
			}
			.class_declaration {
				if cd := child.kind.class_data {
					if name := cd.name {
						if !name.is_local() {
							count += 1
						}
					}
				}
			}
			.function_declaration {
				count += 1
			}
			else {}
		}
	}
	if count > 0 {
		gen.push_new_lexical_environment(count)
		gen.start_boundary(.leave_lexical_environment)
	}
}

fn collect_target_names_from_target(target VariableDeclaratorTarget) []Utf16String {
	mut names := []Utf16String{}
	match target {
		IdentifierVarTarget {
			if !target.identifier.is_local() {
				names << target.identifier.name.clone()
			}
		}
		BindingPatternVarTarget {
			collect_target_names(&target.pattern, mut names)
		}
	}
	return names
}

fn collect_target_names(pattern &BindingPattern, mut names []Utf16String) {
	for entry in pattern.entries {
		if entry.alias.kind == .identifier_kind {
			if ident := entry.alias.identifier {
				if !ident.is_local() {
					names << ident.name.clone()
				}
			}
		} else if entry.alias.kind == .binding_pattern_kind {
			if bp := entry.alias.binding_pattern {
				collect_target_names(&bp, mut names)
			}
		} else if entry.alias.kind == .none_kind {
			if entry.name.kind == .identifier_kind {
				if ident := entry.name.identifier {
					if !ident.is_local() {
						names << ident.name.clone()
					}
				}
			}
		}
	}
}

fn needs_block_declaration_instantiation(scope &ScopeData) bool {
	for child in scope.children {
		match child.kind.tag {
			.function_declaration {
				return true
			}
			.variable_declaration {
				if child.kind.decl_kind == .let_kind || child.kind.decl_kind == .const_kind {
					for decl in child.kind.declarations {
						names := collect_target_names_from_target(decl.target)
						if names.len > 0 {
							return true
						}
					}
				}
			}
			.class_declaration {
				if cd := child.kind.class_data {
					if name := cd.name {
						if !name.is_local() {
							return true
						}
					}
				}
			}
			else {}
		}
	}
	return false
}

fn is_for_loop(statement &Statement) bool {
	return statement.kind.tag == .for_stmt || statement.kind.tag == .for_in_of
}

// =============================================================================
// Call expression
// =============================================================================

fn generate_call_expression(mut gen Generator, data &CallExpressionData, preferred_dst ?ScopedOperand, is_new bool) ?ScopedOperand {
	has_spread := data.arguments.any(it.is_spread)

	if !is_new && !has_spread {
		if builtin_id_ := get_builtin(&data.callee) {
			if data.arguments.len == builtin_argument_count(builtin_id_) {
				dst := choose_dst(mut gen, preferred_dst)
				mut first_arg_op := Operand.invalid()
				if data.arguments.len > 0 {
					first := generate_expression(&data.arguments[0].value, mut gen, none) or {
						return none
					}
					first_arg_op = first.operand
					for i in 1 .. data.arguments.len {
						generate_expression(&data.arguments[i].value, mut gen, none) or {
							return none
						}
					}
				}
				gen.emit(Instruction{
					tag:        .call_builtin
					dst:        dst.operand
					first_arg:  first_arg_op
					argc:       u32(data.arguments.len)
					builtin_id: builtin_id_
				})
				return dst
			}
		}
	}

	if has_spread {
		return generate_call_with_spread(mut gen, data, preferred_dst, is_new)
	}

	// Determine callee and this value.
	mut callee_val := ScopedOperand{}
	mut this_val := gen.add_constant_undefined()
	mut is_direct_eval := false

	if data.callee.kind.tag == .identifier_expr {
		if ident := data.callee.kind.identifier {
			if utf16_equals(ident.name.as_slice(), utf16('eval')) {
				is_direct_eval = true
			}
		}
	}

	if data.callee.kind.tag == .member {
		if obj := data.callee.kind.object {
			if prop := data.callee.kind.property {
				callee_base := generate_expression(obj, mut gen, none) or { return none }
				this_val = gen.copy_if_needed_to_preserve_evaluation_order(callee_base)
				if !data.callee.kind.computed {
					if prop.kind.tag == .identifier_expr {
						if ident := prop.kind.identifier {
							prop_key := gen.intern_property_key(ident.name.as_slice())
							callee_val = gen.allocate_register()
							base_id := intern_base_identifier(mut gen, obj)
							gen.emit(Instruction{
								tag:                 .get_by_id
								dst:                 callee_val.operand
								base:                callee_base.operand
								property_key:        prop_key
								cache_index:         gen.next_property_lookup_cache_()
								has_base_identifier: base_id != none
								base_identifier_id:  if bid := base_id {
									bid
								} else {
									IdentifierTableIndex{}
								}
							})
						}
					}
				} else {
					prop_val := generate_expression(prop, mut gen, none) or { return none }
					callee_val = gen.allocate_register()
					gen.emit(Instruction{
						tag:  .get_by_value
						dst:  callee_val.operand
						base: callee_base.operand
						src:  prop_val.operand
					})
				}
			}
		}
	} else if data.callee.kind.tag == .super_expr {
		callee_val = generate_expression(&data.callee, mut gen, none) or { return none }
		this_val = gen.this_value()
	} else {
		callee_val = generate_expression(&data.callee, mut gen, none) or { return none }
	}

	// Generate arguments
	dst := choose_dst(mut gen, preferred_dst)
	mut first_arg_op := Operand.invalid()
	if data.arguments.len > 0 {
		first := generate_expression(&data.arguments[0].value, mut gen, none) or { return none }
		first_arg_op = first.operand
		for i in 1 .. data.arguments.len {
			generate_expression(&data.arguments[i].value, mut gen, none) or { return none }
		}
	}

	expr_str := expression_string_approximation(&data.callee)
	tag := if is_new {
		InstructionTag.call_construct
	} else if is_direct_eval {
		InstructionTag.call_direct_eval
	} else {
		InstructionTag.call_op
	}

	gen.emit(Instruction{
		tag:                   tag
		dst:                   dst.operand
		callee:                callee_val.operand
		this_op:               this_val.operand
		first_arg:             first_arg_op
		argc:                  u32(data.arguments.len)
		cache_index:           gen.next_property_lookup_cache_()
		has_expression_string: expr_str != none
		expression_string_id:  if es := expr_str {
			gen.intern_identifier(es.as_slice())
		} else {
			IdentifierTableIndex{}
		}
	})
	return dst
}

fn generate_call_with_spread(mut gen Generator, data &CallExpressionData, preferred_dst ?ScopedOperand, is_new bool) ?ScopedOperand {
	args := generate_arguments_array(mut gen, data.arguments)
	mut callee_val := ScopedOperand{}
	mut this_val := gen.add_constant_undefined()

	if data.callee.kind.tag == .member {
		if obj := data.callee.kind.object {
			callee_base := generate_expression(obj, mut gen, none) or { return none }
			this_val = gen.copy_if_needed_to_preserve_evaluation_order(callee_base)
			if prop := data.callee.kind.property {
				if !data.callee.kind.computed && prop.kind.tag == .identifier_expr {
					if ident := prop.kind.identifier {
						prop_key := gen.intern_property_key(ident.name.as_slice())
						callee_val = gen.allocate_register()
						gen.emit(Instruction{
							tag:          .get_by_id
							dst:          callee_val.operand
							base:         callee_base.operand
							property_key: prop_key
							cache_index:  gen.next_property_lookup_cache_()
						})
					}
				} else {
					prop_val := generate_expression(prop, mut gen, none) or { return none }
					callee_val = gen.allocate_register()
					gen.emit(Instruction{
						tag:  .get_by_value
						dst:  callee_val.operand
						base: callee_base.operand
						src:  prop_val.operand
					})
				}
			}
		}
	} else {
		callee_val = generate_expression(&data.callee, mut gen, none) or { return none }
	}

	dst := choose_dst(mut gen, preferred_dst)
	tag := if is_new {
		InstructionTag.call_construct_with_argument_array
	} else {
		InstructionTag.call_with_argument_array
	}
	gen.emit(Instruction{
		tag:     tag
		dst:     dst.operand
		callee:  callee_val.operand
		this_op: this_val.operand
		src:     args.operand
	})
	return dst
}

fn generate_arguments_array(mut gen Generator, arguments []CallArgument) ScopedOperand {
	result := gen.allocate_register()
	gen.emit(Instruction{
		tag: .new_array
		dst: result.operand
	})
	for arg in arguments {
		value := generate_expression_or_undefined(&arg.value, mut gen, none)
		if arg.is_spread {
			gen.emit(Instruction{
				tag:   .array_append
				dst:   result.operand
				value: value.operand
			})
		} else {
			gen.emit(Instruction{
				tag:   .array_append
				dst:   result.operand
				value: value.operand
			})
		}
	}
	return result
}

// =============================================================================
// Update expression
// =============================================================================

fn generate_update_expression(mut gen Generator, op UpdateOp, argument &Expression, prefixed bool, preferred_dst ?ScopedOperand) ?ScopedOperand {
	if argument.kind.tag == .identifier_expr {
		if ident := argument.kind.identifier {
			old_val := generate_identifier(ident, mut gen, none) or { return none }
			dst := choose_dst(mut gen, preferred_dst)
			if prefixed {
				tag := if op == .increment {
					InstructionTag.increment
				} else {
					InstructionTag.decrement
				}
				gen.emit(Instruction{
					tag: tag
					dst: dst.operand
					src: old_val.operand
				})
				emit_set_variable(mut gen, ident, dst)
				return dst
			} else {
				tag := if op == .increment {
					InstructionTag.postfix_increment
				} else {
					InstructionTag.postfix_decrement
				}
				gen.emit(Instruction{
					tag: tag
					dst: dst.operand
					src: old_val.operand
				})
				new_val := gen.allocate_register()
				inc_tag := if op == .increment {
					InstructionTag.increment
				} else {
					InstructionTag.decrement
				}
				gen.emit(Instruction{
					tag: inc_tag
					dst: new_val.operand
					src: old_val.operand
				})
				emit_set_variable(mut gen, ident, new_val)
				return dst
			}
		}
	}
	// Member expression update
	return none
}

// =============================================================================
// Assignment expression
// =============================================================================

fn generate_assignment_expression_from_kind(mut gen Generator, expression &Expression, preferred_dst ?ScopedOperand) ?ScopedOperand {
	op := expression.kind.assignment_op
	if assign_lhs := expression.kind.assignment_lhs {
		match assign_lhs {
			AssignmentLhsExpression {
				if rhs := expression.kind.rhs {
					return generate_assignment_to_expression(mut gen, op, assign_lhs.expression,
						rhs, preferred_dst)
				}
			}
			AssignmentLhsPattern {
				if rhs := expression.kind.rhs {
					value := generate_expression(rhs, mut gen, none) or { return none }
					generate_binding_pattern_bytecode(mut gen, &assign_lhs.pattern, .set_binding,
						value)
					return value
				}
			}
		}
	}
	return none
}

fn generate_assignment_to_expression(mut gen Generator, op AssignmentOp, lhs &Expression, rhs &Expression, preferred_dst ?ScopedOperand) ?ScopedOperand {
	if op == .assignment {
		// Simple assignment
		if lhs.kind.tag == .identifier_expr {
			if ident := lhs.kind.identifier {
				if ident.is_local() {
					local_index := ident.local_index
					dst := if ident.local_type == 1 {
						gen.scoped_operand(Operand.argument(local_index))
					} else {
						gen.local(local_index)
					}
					gen.pending_lhs_name = gen.intern_identifier(ident.name.as_slice())
					value := generate_expression(rhs, mut gen, dst) or { return none }
					gen.pending_lhs_name = none
					gen.emit_mov(dst, value)
					return dst
				}
				gen.pending_lhs_name = gen.intern_identifier(ident.name.as_slice())
				value := generate_expression(rhs, mut gen, preferred_dst) or { return none }
				gen.pending_lhs_name = none
				emit_set_variable(mut gen, ident, value)
				return value
			}
		}
		// Member assignment
		if lhs.kind.tag == .member {
			if obj := lhs.kind.object {
				if prop := lhs.kind.property {
					base := generate_expression(obj, mut gen, none) or { return none }
					base_copy := gen.copy_if_needed_to_preserve_evaluation_order(base)
					if !lhs.kind.computed && prop.kind.tag == .identifier_expr {
						if ident := prop.kind.identifier {
							value := generate_expression(rhs, mut gen, preferred_dst) or {
								return none
							}
							prop_key := gen.intern_property_key(ident.name.as_slice())
							gen.emit(Instruction{
								tag:          .put_normal_by_id
								base:         base_copy.operand
								property_key: prop_key
								src:          value.operand
								cache_index:  gen.next_property_lookup_cache_()
							})
							return value
						}
					}
					prop_val := generate_expression(prop, mut gen, none) or { return none }
					value := generate_expression(rhs, mut gen, preferred_dst) or { return none }
					gen.emit(Instruction{
						tag:  .put_normal_by_value
						base: base_copy.operand
						src:  value.operand
						lhs:  prop_val.operand
					})
					return value
				}
			}
		}
	}
	// Compound assignment - stub: just evaluate rhs
	value := generate_expression(rhs, mut gen, preferred_dst) or { return none }
	return value
}

// =============================================================================
// Variable/binding helpers
// =============================================================================

fn emit_set_variable(mut gen Generator, ident &Identifier, value ScopedOperand) {
	if ident.is_local() {
		local_index := ident.local_index
		if ident.local_type == 1 {
			// Argument
			dst := gen.scoped_operand(Operand.argument(local_index))
			gen.emit_mov(dst, value)
		} else {
			// Variable
			if gen.is_local_lexically_declared(local_index)
				&& !gen.is_local_initialized(local_index) {
				id := gen.intern_identifier(ident.name.as_slice())
				gen.emit(Instruction{
					tag:        .initialize_lexical_binding
					identifier: id
					src:        value.operand
					cache:      EnvironmentCoordinate.empty()
				})
				gen.mark_local_initialized(local_index)
			} else {
				dst := gen.local(local_index)
				gen.emit_mov(dst, value)
			}
		}
		return
	}
	if ident.is_global {
		id := gen.intern_identifier(ident.name.as_slice())
		gen.emit(Instruction{
			tag:         .set_global
			identifier:  id
			src:         value.operand
			cache_index: gen.next_global_variable_cache_()
		})
		return
	}
	id := gen.intern_identifier(ident.name.as_slice())
	if ident.declaration_kind == 2 || ident.declaration_kind == 3 {
		// let or const
		gen.emit(Instruction{
			tag:        .set_lexical_binding
			identifier: id
			src:        value.operand
			cache:      EnvironmentCoordinate.empty()
		})
	} else {
		gen.emit(Instruction{
			tag:        .set_variable_binding
			identifier: id
			src:        value.operand
			cache:      EnvironmentCoordinate.empty()
		})
	}
}

// =============================================================================
// Binding pattern destructuring
// =============================================================================

fn generate_binding_pattern_bytecode(mut gen Generator, pattern &BindingPattern, mode BindingMode, value ScopedOperand) {
	match pattern.kind {
		.array {
			generate_array_binding_pattern(mut gen, pattern, mode, value)
		}
		.object {
			generate_object_binding_pattern(mut gen, pattern, mode, value)
		}
	}
}

fn generate_array_binding_pattern(mut gen Generator, pattern &BindingPattern, mode BindingMode, value ScopedOperand) {
	iterator := gen.allocate_register()
	gen.emit(Instruction{
		tag:      .get_iterator
		dst:      iterator.operand
		src:      value.operand
		hint_val: u32(IteratorHint.sync_iter)
	})

	for entry in pattern.entries {
		if entry.is_rest {
			rest_val := gen.allocate_register()
			gen.emit(Instruction{
				tag: .iterator_to_array
				dst: rest_val.operand
				src: iterator.operand
			})
			assign_binding_entry_alias(mut gen, entry, mode, rest_val)
			break
		}

		next_val := gen.allocate_register()
		next_done := gen.allocate_register()
		gen.emit(Instruction{
			tag:        .iterator_next_unpack
			dst:        next_val.operand
			callee_dst: next_done.operand
			src:        iterator.operand
		})

		if entry.has_initializer {
			if init := entry.initializer {
				not_undefined_block := gen.make_block()
				after_block := gen.make_block()
				gen.emit(Instruction{
					tag:          .jump_undefined
					condition:    next_val.operand
					true_target:  not_undefined_block
					false_target: after_block
				})
				gen.switch_to_basic_block(not_undefined_block)
				default_val := generate_expression(&init, mut gen, next_val) or { continue }
				gen.emit_mov(next_val, default_val)
				gen.emit(Instruction.jump(after_block))
				gen.switch_to_basic_block(after_block)
			}
		}

		assign_binding_entry_alias(mut gen, entry, mode, next_val)
	}
}

fn generate_object_binding_pattern(mut gen Generator, pattern &BindingPattern, mode BindingMode, value ScopedOperand) {
	gen.emit(Instruction{
		tag: .throw_if_nullish
		src: value.operand
	})

	for entry in pattern.entries {
		if entry.is_rest {
			rest_val := gen.allocate_register()
			gen.emit(Instruction{
				tag: .copy_object_excluding_properties
				dst: rest_val.operand
				src: value.operand
			})
			assign_binding_entry_alias(mut gen, entry, mode, rest_val)
			break
		}

		prop_val := gen.allocate_register()
		if entry.name.kind == .identifier_kind {
			if ident := entry.name.identifier {
				prop_key := gen.intern_property_key(ident.name.as_slice())
				gen.emit(Instruction{
					tag:          .get_by_id
					dst:          prop_val.operand
					base:         value.operand
					property_key: prop_key
					cache_index:  gen.next_property_lookup_cache_()
				})
			}
		} else if entry.has_name_expression {
			if name_expr := entry.name.expression {
				key := generate_expression(&name_expr, mut gen, none) or { continue }
				gen.emit(Instruction{
					tag:  .get_by_value
					dst:  prop_val.operand
					base: value.operand
					src:  key.operand
				})
			}
		}

		if entry.has_initializer {
			if init := entry.initializer {
				not_undefined_block := gen.make_block()
				after_block := gen.make_block()
				gen.emit(Instruction{
					tag:          .jump_undefined
					condition:    prop_val.operand
					true_target:  not_undefined_block
					false_target: after_block
				})
				gen.switch_to_basic_block(not_undefined_block)
				default_val := generate_expression(&init, mut gen, prop_val) or { continue }
				gen.emit_mov(prop_val, default_val)
				gen.emit(Instruction.jump(after_block))
				gen.switch_to_basic_block(after_block)
			}
		}

		assign_binding_entry_alias(mut gen, entry, mode, prop_val)
	}
}

fn assign_binding_entry_alias(mut gen Generator, entry BindingEntry, mode BindingMode, value ScopedOperand) {
	if entry.alias.kind == .identifier_kind {
		if ident := entry.alias.identifier {
			emit_set_variable_with_mode(mut gen, ident, value, mode)
		}
	} else if entry.alias.kind == .binding_pattern_kind {
		if bp := entry.alias.binding_pattern {
			generate_binding_pattern_bytecode(mut gen, &bp, mode, value)
		}
	} else if entry.alias.kind == .none_kind {
		// No alias — use the name itself
		if entry.name.kind == .identifier_kind {
			if ident := entry.name.identifier {
				emit_set_variable_with_mode(mut gen, ident, value, mode)
			}
		}
	}
}

fn emit_set_variable_with_mode(mut gen Generator, ident &Identifier, value ScopedOperand, mode BindingMode) {
	if mode == .initialize_lexical {
		if ident.is_local() {
			local_index := ident.local_index
			dst := if ident.local_type == 1 {
				gen.scoped_operand(Operand.argument(local_index))
			} else {
				gen.local(local_index)
			}
			gen.emit_mov(dst, value)
			if ident.local_type == 2 {
				gen.mark_local_initialized(local_index)
			}
		} else {
			id := gen.intern_identifier(ident.name.as_slice())
			gen.emit(Instruction{
				tag:        .initialize_lexical_binding
				identifier: id
				src:        value.operand
				cache:      EnvironmentCoordinate.empty()
			})
		}
	} else {
		emit_set_variable(mut gen, ident, value)
	}
}

// =============================================================================
// Template literals
// =============================================================================

fn generate_template_literal(mut gen Generator, data &TemplateLiteralData, preferred_dst ?ScopedOperand) ?ScopedOperand {
	if data.expressions.len == 0 && data.raw_strings.len > 0 {
		return gen.add_constant_string(data.raw_strings[0].clone())
	}
	dst := choose_dst(mut gen, preferred_dst)
	first_str := gen.add_constant_string(data.raw_strings[0].clone())
	gen.emit_mov(dst, first_str)
	for i, expr in data.expressions {
		value := generate_expression(&expr, mut gen, none) or { continue }
		str_val := gen.allocate_register()
		gen.emit(Instruction{
			tag: .to_string_op
			dst: str_val.operand
			src: value.operand
		})
		gen.emit(Instruction{
			tag: .concat_string
			dst: dst.operand
			lhs: dst.operand
			rhs: str_val.operand
		})
		if i + 1 < data.raw_strings.len {
			next_str := gen.add_constant_string(data.raw_strings[i + 1].clone())
			gen.emit(Instruction{
				tag: .concat_string
				dst: dst.operand
				lhs: dst.operand
				rhs: next_str.operand
			})
		}
	}
	return dst
}

fn generate_tagged_template_literal_expr(mut gen Generator, expression &Expression, preferred_dst ?ScopedOperand) ?ScopedOperand {
	// Stub: simplified tagged template
	if tag_expr := expression.kind.tag_expr {
		if tl := expression.kind.template_literal_expr {
			tag_val := generate_expression(tag_expr, mut gen, none) or { return none }
			_ = tl
			dst := choose_dst(mut gen, preferred_dst)
			gen.emit_mov(dst, tag_val)
			return dst
		}
	}
	return none
}

// =============================================================================
// Object expression
// =============================================================================

fn generate_object_expression(mut gen Generator, properties []ObjectProperty, preferred_dst ?ScopedOperand) ?ScopedOperand {
	dst := choose_dst(mut gen, preferred_dst)
	gen.emit(Instruction{
		tag: .new_object
		dst: dst.operand
	})
	for prop in properties {
		if prop.property_type == .spread {
			value := generate_expression(&prop.key, mut gen, none) or { continue }
			gen.emit(Instruction{
				tag:  .put_by_spread
				base: dst.operand
				src:  value.operand
			})
			continue
		}
		if prop.property_type == .proto_setter {
			value := if val := prop.value {
				generate_expression(&val, mut gen, none) or { continue }
			} else {
				continue
			}
			if prop.key.kind.tag == .identifier_expr {
				if ident := prop.key.kind.identifier {
					prop_key := gen.intern_property_key(ident.name.as_slice())
					gen.emit(Instruction{
						tag:          .put_prototype_by_id
						base:         dst.operand
						property_key: prop_key
						src:          value.operand
					})
				}
			}
			continue
		}
		value := if val := prop.value {
			generate_expression(&val, mut gen, none) or { continue }
		} else {
			generate_expression(&prop.key, mut gen, none) or { continue }
		}
		if !prop.is_computed && prop.key.kind.tag == .identifier_expr {
			if ident := prop.key.kind.identifier {
				prop_key := gen.intern_property_key(ident.name.as_slice())
				put_tag := match prop.property_type {
					.key_value { InstructionTag.put_own_by_id }
					.getter { InstructionTag.put_getter_by_id }
					.setter { InstructionTag.put_setter_by_id }
					else { InstructionTag.put_own_by_id }
				}
				gen.emit(Instruction{
					tag:          put_tag
					base:         dst.operand
					property_key: prop_key
					src:          value.operand
				})
				continue
			}
		}
		key_val := generate_expression(&prop.key, mut gen, none) or { continue }
		put_tag := match prop.property_type {
			.key_value { InstructionTag.put_own_by_value }
			.getter { InstructionTag.put_getter_by_value }
			.setter { InstructionTag.put_setter_by_value }
			else { InstructionTag.put_own_by_value }
		}
		gen.emit(Instruction{
			tag:  put_tag
			base: dst.operand
			src:  value.operand
			lhs:  key_val.operand
		})
	}
	return dst
}

// =============================================================================
// Optional chain
// =============================================================================

fn generate_optional_chain_inner(mut gen Generator, base &Expression, references []OptionalChainReference, current_value ScopedOperand, current_base ScopedOperand) ?ScopedOperand {
	base_val := generate_expression(base, mut gen, none) or { return none }
	gen.emit_mov(current_value, base_val)
	// Stub: simplified optional chain
	return current_value
}

// =============================================================================
// Delete reference
// =============================================================================

fn emit_delete_reference(mut gen Generator, expression &Expression) ?ScopedOperand {
	if expression.kind.tag == .identifier_expr {
		if ident := expression.kind.identifier {
			dst := gen.allocate_register()
			id := gen.intern_identifier(ident.name.as_slice())
			gen.emit(Instruction{
				tag:        .delete_variable
				dst:        dst.operand
				identifier: id
			})
			return dst
		}
	}
	if expression.kind.tag == .member {
		if obj := expression.kind.object {
			if prop := expression.kind.property {
				base := generate_expression(obj, mut gen, none) or { return none }
				if !expression.kind.computed && prop.kind.tag == .identifier_expr {
					if ident := prop.kind.identifier {
						dst := gen.allocate_register()
						prop_key := gen.intern_property_key(ident.name.as_slice())
						gen.emit(Instruction{
							tag:          .delete_by_id
							dst:          dst.operand
							base:         base.operand
							property_key: prop_key
						})
						return dst
					}
				}
				prop_val := generate_expression(prop, mut gen, none) or { return none }
				dst := gen.allocate_register()
				gen.emit(Instruction{
					tag:  .delete_by_value
					dst:  dst.operand
					base: base.operand
					src:  prop_val.operand
				})
				return dst
			}
		}
	}
	// Deleting a non-reference always returns true
	generate_expression(expression, mut gen, none)
	return gen.add_constant_boolean(true)
}

// =============================================================================
// Class expression
// =============================================================================

fn generate_class_expression(mut gen Generator, class_data &ClassData, preferred_dst ?ScopedOperand) ?ScopedOperand {
	// Stub: simplified class expression
	dst := choose_dst(mut gen, preferred_dst)
	gen.emit(Instruction{
		tag: .new_object
		dst: dst.operand
	})
	return dst
}

// =============================================================================
// emit_new_function
// =============================================================================

fn emit_new_function(mut gen Generator, data JsFunctionData, home_object ?ScopedOperand) u32 {
	sfd := ffi_create_shared_function_data(gen.vm_ptr, gen.source_code_ptr, gen.source_len,
		&data, gen.strict)
	return gen.register_shared_function_data(sfd)
}

// =============================================================================
// emit_function_declaration_instantiation
// =============================================================================

fn emit_function_declaration_instantiation(mut gen Generator, body_scope &ScopeData, function_data &JsFunctionData, strict bool) {
	// Stub: simplified FDI
	fsd := body_scope.function_scope_data or { return }

	// Initialize hoisted function declarations
	for function_to_init in fsd.functions_to_initialize {
		child := body_scope.children[function_to_init.child_index]
		if child.kind.tag == .function_declaration {
			inner_function_data := gen.function_table.take(child.kind.function_id)
			sfd_index := emit_new_function(mut gen, inner_function_data, none)
			if name := child.kind.func_name {
				if name.is_local() {
					local_index := name.local_index
					local := gen.local(local_index)
					gen.emit(Instruction{
						tag:       .new_function
						dst:       local.operand
						sfd_index: sfd_index
					})
					gen.mark_local_initialized(local_index)
				} else {
					function_reg := gen.allocate_register()
					gen.emit(Instruction{
						tag:       .new_function
						dst:       function_reg.operand
						sfd_index: sfd_index
					})
					id := gen.intern_identifier(name.name.as_slice())
					gen.emit(Instruction{
						tag:        .set_variable_binding
						identifier: id
						src:        function_reg.operand
						cache:      EnvironmentCoordinate.empty()
					})
				}
			}
		}
	}
}

// =============================================================================
// count_non_local_lexical_bindings
// =============================================================================

fn count_non_local_lexical_bindings(scope &ScopeData) u32 {
	mut count := u32(0)
	for child in scope.children {
		match child.kind.tag {
			.variable_declaration {
				if child.kind.decl_kind == .let_kind || child.kind.decl_kind == .const_kind {
					for decl in child.kind.declarations {
						names := collect_target_names_from_target(decl.target)
						count += u32(names.len)
					}
				}
			}
			.class_declaration {
				if cd := child.kind.class_data {
					if name := cd.name {
						if !name.is_local() {
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
