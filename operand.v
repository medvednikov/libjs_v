module main

// Bytecode operand types, translated from operand.rs.

// A bytecode register index.
// Reserved registers:
//   0: accumulator
//   1: exception
//   2: this_value
//   3: return_value
//   4: saved_lexical_environment
//   5+: user registers
struct Register {
	value u32
}

fn Register.new(v u32) Register {
	return Register{
		value: v
	}
}

const register_accumulator = Register{
	value: 0
}
const register_exception = Register{
	value: 1
}
const register_this_value = Register{
	value: 2
}
const register_return_value = Register{
	value: 3
}
const register_saved_lexical_environment = Register{
	value: 4
}
const register_reserved_count = u32(5)

// A bytecode operand.
// Encoded as a single u32 with a 3-bit type tag in the top 3 bits
// and a 29-bit index in the lower 29 bits:
//   raw = (type << 29) | index
struct Operand {
mut:
	raw u32
}

const operand_type_shift = u32(29)
const operand_index_mask = u32(0x1FFF_FFFF)
const operand_invalid = u32(0xFFFF_FFFF)

fn Operand.register(reg Register) Operand {
	return Operand{
		raw: (u32(0) << operand_type_shift) | reg.value
	}
}

fn Operand.local(index u32) Operand {
	return Operand{
		raw: (u32(1) << operand_type_shift) | index
	}
}

fn Operand.constant(index u32) Operand {
	return Operand{
		raw: (u32(2) << operand_type_shift) | index
	}
}

fn Operand.argument(index u32) Operand {
	return Operand{
		raw: (u32(3) << operand_type_shift) | index
	}
}

fn Operand.invalid() Operand {
	return Operand{
		raw: operand_invalid
	}
}

fn (o Operand) is_invalid() bool {
	return o.raw == operand_invalid
}

fn (o Operand) is_register() bool {
	return !o.is_invalid() && o.operand_type() == .register
}

fn (o Operand) is_local() bool {
	return !o.is_invalid() && o.operand_type() == .local
}

fn (o Operand) is_constant() bool {
	return !o.is_invalid() && o.operand_type() == .constant
}

fn (o Operand) operand_type() OperandType {
	tag := (o.raw >> operand_type_shift) & 0x7
	match tag {
		0 { return .register }
		1 { return .local }
		2 { return .constant }
		3 { return .argument }
		else { panic('operand type bits can only be 0-3') }
	}
}

fn (o Operand) index() u32 {
	return o.raw & operand_index_mask
}

fn (mut o Operand) offset_index_by(offset u32) {
	o.raw &= operand_index_mask
	o.raw = o.raw + offset
}

fn (a Operand) == (b Operand) bool {
	return a.raw == b.raw
}

enum OperandType {
	register
	local
	constant
	argument
}

// A bytecode label.
// During compilation, holds a basic block index. After linking,
// holds the final byte offset in the flat bytecode stream.
struct Label {
mut:
	value u32
}

fn Label.new(v u32) Label {
	return Label{
		value: v
	}
}

fn (l Label) basic_block_index() int {
	return int(l.value)
}

// Index into the string table.
struct StringTableIndex {
	value u32
}

const string_table_index_invalid = u32(0xFFFF_FFFF)

// Index into the identifier table.
struct IdentifierTableIndex {
	value u32
}

const identifier_table_index_invalid = u32(0xFFFF_FFFF)

// Index into the property key table.
struct PropertyKeyTableIndex {
	value u32
}

// Index into the regex table.
struct RegexTableIndex {
	value u32
}

// Environment coordinate used as a mutable cache in some instructions.
struct EnvironmentCoordinate {
mut:
	hops  u32
	index u32
}

const environment_coordinate_invalid = u32(0xFFFF_FFFE)

fn EnvironmentCoordinate.empty() EnvironmentCoordinate {
	return EnvironmentCoordinate{
		hops:  environment_coordinate_invalid
		index: environment_coordinate_invalid
	}
}
