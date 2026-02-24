module main

// Basic block types, translated from basic_block.rs.

// A source map entry mapping a bytecode offset to a source range.
struct SourceMapEntry {
mut:
	bytecode_offset u32
	source_start    u32
	source_end      u32
}

// A basic block in the bytecode generator.
// During codegen, instructions are appended as typed Instruction values.
// During flattening (compile/assemble), instructions are serialized
// into the final byte stream.
struct BasicBlock {
mut:
	index         u32
	instructions  []InstructionWithSourceMap
	handler       ?Label
	terminated    bool
	resolved_this bool
}

// Pairs an Instruction with its SourceMapEntry.
struct InstructionWithSourceMap {
mut:
	instruction Instruction
	source_map  SourceMapEntry
}

fn BasicBlock.new(index u32) BasicBlock {
	return BasicBlock{
		index: index
	}
}

fn (mut bb BasicBlock) append(instruction Instruction, source_map SourceMapEntry) {
	is_term := instruction.is_terminator()
	bb.instructions << InstructionWithSourceMap{
		instruction: instruction
		source_map:  source_map
	}
	if is_term {
		bb.terminated = true
	}
}

fn (bb &BasicBlock) is_empty() bool {
	return bb.instructions.len == 0
}
