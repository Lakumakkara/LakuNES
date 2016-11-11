#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include "nes_cpu.h"
#include "nes_mem.h"
#include "nes_rom.h"
#include "nes_ppu.h"
#include "nes_apu.h"

#ifdef DEBUG

#define DEBUG_CPU

#endif

#ifndef CPU_OPCODES
#define CPU_OPCODES

#define	ADC 	  0x69 
#define	ADC_ZP 	  0x65 
#define	ADC_ZPX   0x75 
#define	ADC_A 	  0x6d 
#define	ADC_AX 	  0x7d 
#define	ADC_AY 	  0x79 
#define	ADC_IX 	  0x61 
#define	ADC_IY 	  0x71 
	
#define	AND 	  0x29 
#define	AND_ZP 	  0x25 
#define	AND_ZPX   0x35 
#define	AND_A 	  0x2d 
#define	AND_AX 	  0x3d 
#define	AND_AY 	  0x39 
#define	AND_IX 	  0x21 
#define	AND_IY 	  0x31 
	
#define	ASL 	  0x0a 
#define	ASL_ZP 	  0x06 
#define	ASL_ZPX   0x16 
#define	ASL_A 	  0x0e 
#define	ASL_AX 	  0x1e 
	
#define	BCC 	  0x90 
	
#define	BCS 	  0xb0 
	
#define	BEQ 	  0xf0 
	
#define	BIT_ZP 	  0x24 
#define	BIT_A 	  0x2c 
	
#define	BMI 	  0x30 
	
#define	BNE 	  0xd0 
	
#define	BPL 	  0x10 
	
#define	BRK 	  0x00 
	
#define	BVC 	  0x50 
	
#define	BVS 	  0x70 
	
#define	CLC 	  0x18 
	
#define	CLD 	  0xd8 
	
#define	CLI 	  0x58 
	
#define	CLV 	  0xb8 
	
#define	CMP 	  0xc9 
#define	CMP_ZP 	  0xc5 
#define	CMP_ZPX   0xd5 
#define	CMP_A 	  0xcd 
#define	CMP_AX 	  0xdd 
#define	CMP_AY 	  0xd9 
#define	CMP_IX 	  0xc1 
#define	CMP_IY 	  0xd1 
	
#define	CPX 	  0xe0 
#define	CPX_ZP 	  0xe4 
#define	CPX_A 	  0xec 
	
#define	CPY 	  0xc0 
#define	CPY_ZP 	  0xc4 
#define	CPY_A	  0xcc 
	
#define	DEC_ZP	  0xc6 
#define	DEC_ZPX	  0xd6 
#define	DEC_A	  0xce 
#define	DEC_AX	  0xde 
	
#define	DEX 	  0xca 
	
#define	DEY		  0x88 
	
#define	EOR		  0x49 
#define	EOR_ZP	  0x45 
#define	EOR_ZPX   0x55 
#define	EOR_A 	  0x4d 
#define	EOR_AX 	  0x5d 
#define	EOR_AY 	  0x59 
#define	EOR_IX 	  0x41 
#define	EOR_IY 	  0x51 
	
#define	INC_ZP	  0xe6 
#define	INC_ZPX	  0xf6 
#define	INC_A	  0xee 
#define	INC_AX	  0xfe 
	
#define	INX 	  0xe8 
	
#define	INY		  0xc8 
	
#define	JMP 	  0x4c 
#define	JMP_I	  0x6c 
	
#define	JSR		  0x20 
	
#define	LDA		  0xa9 
#define	LDA_ZP	  0xa5 
#define	LDA_ZPX   0xb5 
#define	LDA_A 	  0xad 
#define	LDA_AX 	  0xbd 
#define	LDA_AY 	  0xb9 
#define	LDA_IX 	  0xa1 
#define	LDA_IY 	  0xb1 
	
#define	LDX 	  0xa2 
#define	LDX_ZP	  0xa6 
#define	LDX_ZPY	  0xb6 
#define	LDX_A	  0xae 
#define	LDX_AY	  0xbe 
	
#define	LDY 	  0xa0 
#define	LDY_ZP	  0xa4 
#define	LDY_ZPX	  0xb4 
#define	LDY_A	  0xac 
#define	LDY_AX	  0xbc 
	
#define	LSR 	  0x4a 
#define	LSR_ZP	  0x46 
#define	LSR_ZPX	  0x56 
#define	LSR_A	  0x4e 
#define	LSR_AX	  0x5e 
	
#define	NOP 	  0xea 
	
#define	ORA		  0x09 
#define	ORA_ZP	  0x05 
#define	ORA_ZPX   0x15 
#define	ORA_A 	  0x0d 
#define	ORA_AX 	  0x1d 
#define	ORA_AY 	  0x19 
#define	ORA_IX 	  0x01 
#define	ORA_IY 	  0x11 
	
#define	PHA 	  0x48 
	
#define	PHP		  0x08 
	
#define	PLA		  0x68 
	
#define	PLP		  0x28 
	
#define	ROL 	  0x2a 
#define	ROL_ZP	  0x26 
#define	ROL_ZPX	  0x36 
#define	ROL_A	  0x2e 
#define	ROL_AX	  0x3e 
	
#define	ROR 	  0x6a 
#define	ROR_ZP	  0x66 
#define	ROR_ZPX	  0x76 
#define	ROR_A	  0x6e 
#define	ROR_AX	  0x7e 
	
#define	RTI		  0x40 
	
#define	RTS		  0x60 
	
#define	SBC		  0xe9 
#define	SBC_ZP	  0xe5 
#define	SBC_ZPX   0xf5 
#define	SBC_A 	  0xed 
#define	SBC_AX 	  0xfd 
#define	SBC_AY 	  0xf9 
#define	SBC_IX 	  0xe1 
#define	SBC_IY 	  0xf1 
	
#define	SEC		  0x38 

#define	SED		  0xf8 
	
#define	SEI		  0x78 
	
#define	STA_ZP	  0x85 
#define	STA_ZPX   0x95 
#define	STA_A 	  0x8d 
#define	STA_AX 	  0x9d 
#define	STA_AY 	  0x99 
#define	STA_IX 	  0x81 
#define	STA_IY 	  0x91 
	
#define	STX_ZP	  0x86 
#define	STX_ZPY	  0x96 
#define	STX_A	  0x8e 
	
#define	STY_ZP	  0x84 
#define	STY_ZPX	  0x94 
#define	STY_A	  0x8c 
	
#define	TAX		  0xaa 
	
#define	TAY		  0xa8 
	
#define	TSX		  0xba 
	
#define	TXA		  0x8a 
	
#define	TXS		  0x9a 
	
#define	TYA		  0x98 


#endif

#define CARRY(a, b) ((((a) + (b)) & 0xff) < ((a) + (b)) ? 1 : 0)
#define BORROW(a, b) ((a < b) ? 1 : 0)
#define OVERFLOW(a, b) ((a & 0x80) == (b & 0x80) ? (((a + b) & 0x80) != (a & 0x80) ? 1 : 0) : 0)
#define UNDERFLOW(a, b) ((a & 0x80) != (b & 0x80) ? (((a - b) & 0x80) == (b & 0x80) ? 1 : 0) : 0)

//BTOI: makes an uint16_t from two uint8_t's
#define BTOI(lo, hi) ((lo) + ((hi) * 0x100))
#define TWOS_COMPLEMENT(n) (((~n) + 1) & 0xFF)
#define IS_NEGATIVE(n) ((n) & 0x80)
#define BOOL(n) (n != 0 ? 1 : 0)
#define BIT_N(byte, n) (((byte) >> (n)) & 0x01)


void op_adc(NesCPU *cpu, uint8_t val) {
	uint8_t a = cpu->a;
	val += get_carry(cpu);
	cpu->a += val;
	if(cpu->a == 0) {
		set_zero(cpu, 1);
	} else {
		set_zero(cpu, 0);
	}
	if(CARRY(a, val)) {
		set_carry(cpu, 1);
	}
	if(OVERFLOW(a, val)) {
		set_overflow(cpu, 1);
	}
	if(cpu->a & 0x80) {
		set_negative(cpu, 1);
	} else {
		set_negative(cpu, 0);
	}
}

void op_and(NesCPU *cpu, uint8_t val) {
	cpu->a = cpu->a & val;
	if(cpu->a == 0) {
		set_zero(cpu, 1);
	} else {
		set_zero(cpu, 0);
	}
	if(cpu->a & 0x80) {
		set_negative(cpu, 1);
	} else {
		set_negative(cpu, 0);
	}
}

void op_asl_a(NesCPU *cpu) {
	set_carry(cpu, cpu->a >> 7);
	cpu->a = cpu->a << 1;
	if(cpu->a == 0) {
		set_zero(cpu, 1);
	} else {
		set_zero(cpu, 0);
	}
	if(cpu->a & 0x80) {
		set_negative(cpu, 1);
	} else {
		set_negative(cpu, 0);
	}
}

void op_asl_mem(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu, uint8_t val, uint16_t addr) {
	set_carry(cpu, val >> 7);
	write_memory(mem, ppu, apu, addr, val << 1);
	if(val << 1 == 0) {
		set_zero(cpu, 1);
	} else {
		set_zero(cpu, 0);
	}
	if((val << 1) & 0x80) {
		set_negative(cpu, 1);
	} else {
		set_negative(cpu, 0);
	}
}

void do_branch(NesCPU *cpu, uint8_t val) {
	#ifdef DEBUG_CPU_JUMP
	printf("pc %.4x", cpu->pc);
	#endif
	if(IS_NEGATIVE(val)) {
		val = TWOS_COMPLEMENT(val);
		if(BORROW((cpu->pc & 0xff), val)) {
			cpu->cycles++;
		}
		cpu->pc -= val;
		#ifdef DEBUG_CPU_JUMP
		printf(" -> %.4x\n", cpu->pc);
		#endif
	} else {
		if(CARRY((cpu->pc & 0xff), val)) {
			cpu->cycles++;
		}
		cpu->pc += val;	
		#ifdef DEBUG_CPU_JUMP
		printf(" -> %.4x\n", cpu->pc);
		#endif		
	}
	cpu->cycles++;
}

void op_bcc(NesCPU *cpu, uint8_t val) {
	cpu->cycles = 2;
	cpu->pc += 2;
	if(get_carry(cpu) == 0) {
		do_branch(cpu, val);
	}
}

void op_bcs(NesCPU *cpu, uint8_t val) {
	cpu->cycles = 2;
	cpu->pc += 2;
	if(get_carry(cpu) == 1) {
		do_branch(cpu, val);
	}
}

void op_beq(NesCPU *cpu, uint8_t val) {
	cpu->cycles = 2;
	cpu->pc += 2;
	if(get_zero(cpu) == 1) {
		do_branch(cpu, val);
	}
}

void op_bit(NesCPU *cpu, uint8_t val) {
	if(cpu->a & val == 0) {
		set_zero(cpu, 1);
	} else {
		set_zero(cpu, 0);
	}
	set_overflow(cpu, BIT_N(val, 6));
	set_negative(cpu, BIT_N(val, 7));	
}

void op_bmi(NesCPU *cpu, uint8_t val) {
	cpu->cycles = 2;
	cpu->pc += 2;
	if(get_negative(cpu) == 1) {
		do_branch(cpu, val);
	}
}

void op_bne(NesCPU *cpu, uint8_t val) {
	cpu->cycles = 2;
	cpu->pc += 2;
	if(get_zero(cpu) == 0) {
		do_branch(cpu, val);
	}
}

void op_bpl(NesCPU *cpu, uint8_t val) {
	cpu->cycles = 2;
	cpu->pc += 2;
	if(get_negative(cpu) == 0) {
		do_branch(cpu, val);
	}
}

void op_brk(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu, NesROM *rom) {
	#ifdef DEBUG_CPU_JUMP
	printf("pc %.4x", cpu->pc);
	#endif	
	cpu->pc += 2;
	write_memory(mem, ppu, apu, 0x0100 + cpu->sp, (cpu->pc & 0xff00) >> 8);
	cpu->sp = (cpu->sp - 1) & 0xff;
	write_memory(mem, ppu, apu, 0x0100 + cpu->sp, cpu->pc & 0x00ff);
	cpu->sp = (cpu->sp - 1) & 0xff;
	write_memory(mem, ppu, apu, 0x0100 + cpu->sp, cpu->ps | 0x30);
	cpu->sp = (cpu->sp - 1) & 0xff;
	set_interrupt(cpu, 1);
	cpu->pc = BTOI(read_memory(mem, ppu, apu, rom, 0xfffe), read_memory(mem, ppu, apu, rom, 0xffff));
	#ifdef DEBUG_CPU_JUMP
	printf(" -> %.4x\n", cpu->pc);
	#endif
	cpu->cycles = 7;	
}

void op_bvc(NesCPU *cpu, uint8_t val) {
	cpu->cycles = 2;
	cpu->pc += 2;
	if(get_overflow(cpu) == 0) {
		do_branch(cpu, val);
	}
}

void op_bvs(NesCPU *cpu, uint8_t val) {
	cpu->cycles = 2;
	cpu->pc += 2;
	if(get_overflow(cpu) == 1) {
		do_branch(cpu, val);
	}
}

void op_clc(NesCPU *cpu) {
	cpu->cycles = 2;
	cpu->pc += 1;
	set_carry(cpu, 0);
}

void op_cld(NesCPU *cpu) {
	cpu->cycles = 2;
	cpu->pc += 1;
	set_decimal(cpu, 0);
}

void op_cli(NesCPU *cpu) {
	cpu->cycles = 2;
	cpu->pc += 1;
	set_interrupt(cpu, 0);
}

void op_clv(NesCPU *cpu) {
	cpu->cycles = 2;
	cpu->pc += 1;
	set_overflow(cpu, 0);
}

void op_cmp(NesCPU *cpu, uint8_t val) {
	if(cpu->a >= val) {
		set_carry(cpu, 1);
	}
	if(cpu->a == val) {
		set_zero(cpu, 1);
	} else {
		set_zero(cpu, 0);
	}
	if(IS_NEGATIVE((cpu->a - val) & 0xff)) {
		set_negative(cpu, 1);
	} else {
		set_negative(cpu, 0);
	}	
} 

void op_cpx(NesCPU *cpu, uint8_t val) {
	if(cpu->x >= val) {
		set_carry(cpu, 1);
	}
	if(cpu->x == val) {
		set_zero(cpu, 1);
	} else {
		set_zero(cpu, 0);
	}
	if(IS_NEGATIVE((cpu->x - val) & 0xff)) {
		set_negative(cpu, 1);
	} else {
		set_negative(cpu, 0);
	}	
} 

void op_cpy(NesCPU *cpu, uint8_t val) {
	if(cpu->y >= val) {
		set_carry(cpu, 1);
	}
	if(cpu->y == val) {
		set_zero(cpu, 1);
	} else {
		set_zero(cpu, 0);
	}
	if(IS_NEGATIVE((cpu->y - val) & 0xff)) {
		set_negative(cpu, 1);
	} else {
		set_negative(cpu, 0);
	}	
} 

void op_dec(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu, uint8_t val, uint16_t addr) {
	val = (val - 1) % 0x100;
	write_memory(mem, ppu, apu, addr, val);
	if(val == 0) {
		set_zero(cpu, 1);
	} else {
		set_zero(cpu, 0);
	}
	if(IS_NEGATIVE(val)) {
		set_negative(cpu, 1);
	} else {
		set_negative(cpu, 0);
	}
}

void op_dex(NesCPU *cpu) {
	cpu->x -= 1;
	if(cpu->x == 0) {
		set_zero(cpu, 1);
	} else {
		set_zero(cpu, 0);
	}
	if(IS_NEGATIVE(cpu->x)) {
		set_negative(cpu, 1);
	} else {
		set_negative(cpu, 0);
	}
	cpu->cycles = 2;
	cpu->pc += 1;
}

void op_dey(NesCPU *cpu) {
	cpu->y -= 1;
	if(cpu->y == 0) {
		set_zero(cpu, 1);
	} else {
		set_zero(cpu, 0);
	}
	if(IS_NEGATIVE(cpu->y)) {
		set_negative(cpu, 1);
	} else {
		set_negative(cpu, 0);
	}
	cpu->cycles = 2;
	cpu->pc += 1;
}

void op_eor(NesCPU *cpu, uint8_t val) {
	cpu->a = cpu->a ^ val;
	if(cpu->a == 0) {
		set_zero(cpu, 1);
	} else {
		set_zero(cpu, 0);
	}
	if(IS_NEGATIVE(cpu->a)) {
		set_negative(cpu, 1);
	} else {
		set_negative(cpu, 0);
	}
}

void op_inc(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu, uint8_t val, uint16_t addr) {
	val = (val + 1) % 0x100;
	write_memory(mem, ppu, apu, addr, val);
	if(val == 0) {
		set_zero(cpu, 1);
	} else {
		set_zero(cpu, 0);
	}
	if(IS_NEGATIVE(val)) {
		set_negative(cpu, 1);
	} else {
		set_negative(cpu, 0);
	}
}

void op_inx(NesCPU *cpu) {
	cpu->x += 1;
	if(cpu->x == 0) {
		set_zero(cpu, 1);
	} else {
		set_zero(cpu, 0);
	}
	if(IS_NEGATIVE(cpu->x)) {
		set_negative(cpu, 1);
	} else {
		set_negative(cpu, 0);
	}
	cpu->cycles = 2;
	cpu->pc += 1;
}

void op_iny(NesCPU *cpu) {
	cpu->y += 1;
	if(cpu->y == 0) {
		set_zero(cpu, 1);
	} else {
		set_zero(cpu, 0);
	}
	if(IS_NEGATIVE(cpu->y)) {
		set_negative(cpu, 1);
	} else {
		set_negative(cpu, 0);
	}
	cpu->cycles = 2;
	cpu->pc += 1;
}

void op_jmp(NesCPU *cpu, uint16_t addr) {
	#ifdef DEBUG_CPU_JUMP
	printf("pc %.4x", cpu->pc);
	#endif
	cpu->pc = addr;
	#ifdef DEBUG_CPU_JUMP
	printf(" -> %.4x\n", cpu->pc);
	#endif
}

void op_jsr(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu, uint16_t addr) {
	#ifdef DEBUG_CPU_JUMP
	printf("pc %.4x", cpu->pc);
	#endif
	cpu->pc += 2;
	write_memory(mem, ppu, apu, 0x0100 + cpu->sp, (cpu->pc & 0xff00) >> 8);
	cpu->sp = (cpu->sp - 1) & 0xff;
	write_memory(mem, ppu, apu, 0x0100 + cpu->sp, cpu->pc & 0x00ff);
	cpu->sp = (cpu->sp - 1) & 0xff;
	
	cpu->pc = addr;
	#ifdef DEBUG_CPU_JUMP
	printf(" -> %.4x\n", cpu->pc);
	#endif
	cpu->cycles = 6;
}

void op_lda(NesCPU *cpu, uint8_t val) {
	cpu->a = val;
	if(cpu->a == 0) {
		set_zero(cpu, 1);
	} else {
		set_zero(cpu, 0);
	}
	if(IS_NEGATIVE(cpu->a)) {
		set_negative(cpu, 1);
	} else {
		set_negative(cpu, 0);
	}
}

void op_ldx(NesCPU *cpu, uint8_t val) {
	cpu->x = val;
	if(cpu->x == 0) {
		set_zero(cpu, 1);
	} else {
		set_zero(cpu, 0);
	}
	if(IS_NEGATIVE(cpu->x)) {
		set_negative(cpu, 1);
	} else {
		set_negative(cpu, 0);
	}
}

void op_ldy(NesCPU *cpu, uint8_t val) {
	cpu->y = val;
	if(cpu->y == 0) {
		set_zero(cpu, 1);
	} else {
		set_zero(cpu, 0);
	}
	if(IS_NEGATIVE(cpu->y)) {
		set_negative(cpu, 1);
	} else {
		set_negative(cpu, 0);
	}
}

void op_lsr_a(NesCPU *cpu) {
	set_carry(cpu, cpu->a & 0x01);
	cpu->a = cpu->a >> 1;
	if(cpu->a == 0) {
		set_zero(cpu, 1);
	} else {
		set_zero(cpu, 0);
	}
	if(cpu->a & 0x80) {
		set_negative(cpu, 1);
	} else {
		set_negative(cpu, 0);
	}
}

void op_lsr_mem(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu, uint8_t val, uint16_t addr) {
	set_carry(cpu, val & 0x01);
	val = val >> 1;
	write_memory(mem, ppu, apu, addr, val);
	if(val == 0) {
		set_zero(cpu, 1);
	} else {
		set_zero(cpu, 0);
	}
	if(IS_NEGATIVE(val)) {
		set_negative(cpu, 1);
	} else {
		set_negative(cpu, 0);
	}
}

void op_nop(NesCPU *cpu) {
	cpu->cycles = 2;
	cpu->pc += 1;
}

void op_ora(NesCPU *cpu, uint8_t val) {
	cpu->a = cpu->a | val;
	if(cpu->a == 0) {
		set_zero(cpu, 1);
	} else {
		set_zero(cpu, 0);
	}
	if(IS_NEGATIVE(cpu->a)) {
		set_negative(cpu, 1);
	} else {
		set_negative(cpu, 0);
	}
}

void op_pha(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu) {
	write_memory(mem, ppu, apu, 0x0100 + cpu->sp, cpu->a);
	cpu->sp = (cpu->sp - 1) & 0xff;
	cpu->cycles = 3;
	cpu->pc += 1;
}

void op_php(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu) {
	write_memory(mem, ppu, apu, 0x0100 + cpu->sp, cpu->ps);
	cpu->sp = (cpu->sp - 1) & 0xff;
	cpu->cycles = 3;
	cpu->pc += 1;
}

void op_pla(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu, NesROM *rom) {
	cpu->sp = (cpu->sp + 1) & 0xff;
	cpu->a = read_memory(mem, ppu, apu, rom, 0x0100 + cpu->sp);
	cpu->cycles = 4;
	cpu->pc += 1;
}

void op_plp(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu, NesROM *rom) {
	cpu->sp = (cpu->sp + 1) & 0xff;
	cpu->ps = read_memory(mem, ppu, apu, rom, 0x0100 + cpu->sp);
	cpu->cycles = 4;
	cpu->pc += 1;
}

void op_rol_a(NesCPU *cpu) {
	uint8_t a = cpu->a;
	cpu->a = (cpu->a << 1) + get_carry(cpu);
	set_carry(cpu, a >> 7);	
	if(cpu->a == 0) {
		set_zero(cpu, 1);
	} else {
		set_zero(cpu, 0);
	}
	if(IS_NEGATIVE(cpu->a)) {
		set_negative(cpu, 1);
	} else {
		set_negative(cpu, 0);
	}
}

void op_rol_mem(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu, uint8_t val, uint16_t addr) {
	uint8_t val_old = val;
	val = (val << 1) + get_carry(cpu);
	write_memory(mem, ppu, apu, addr, val);
	set_carry(cpu, val_old >> 7);
	if(val == 0) {
		set_zero(cpu, 1);
	} else {
		set_zero(cpu, 0);
	}
	if(IS_NEGATIVE(val)) {
		set_negative(cpu, 1);
	} else {
		set_negative(cpu, 0);
	}
}

void op_ror_a(NesCPU *cpu) {
	uint8_t a = cpu->a;
	cpu->a = (cpu->a >> 1) + (get_carry(cpu) << 7);
	set_carry(cpu, a & 0x01);	
	if(cpu->a == 0) {
		set_zero(cpu, 1);
	} else {
		set_zero(cpu, 0);
	}
	if(IS_NEGATIVE(cpu->a)) {
		set_negative(cpu, 1);
	} else {
		set_negative(cpu, 0);
	}
}

void op_ror_mem(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu, uint8_t val, uint16_t addr) {
	uint8_t val_old = val;
	val = (val >> 1) + (get_carry(cpu) << 7);
	write_memory(mem, ppu, apu, addr, val);
	set_carry(cpu, val_old & 0x01);
	if(val == 0) {
		set_zero(cpu, 1);
	} else {
		set_zero(cpu, 0);
	}
	if(IS_NEGATIVE(val)) {
		set_negative(cpu, 1);
	} else {
		set_negative(cpu, 0);
	}
}

void op_rti(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu, NesROM *rom) {
	cpu->sp = (cpu->sp + 1) & 0xff;
	cpu->ps = read_memory(mem, ppu, apu, rom, 0x0100 + cpu->sp);
	cpu->sp = (cpu->sp + 1) & 0xff;	
	uint8_t lo = read_memory(mem, ppu, apu, rom, 0x0100 + cpu->sp);
	cpu->sp = (cpu->sp + 1) & 0xff;	
	uint8_t hi = read_memory(mem, ppu, apu, rom, 0x0100 + cpu->sp);
	cpu->pc = BTOI(lo, hi);
	cpu->cycles = 6;
}

void op_rts(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu, NesROM *rom) {
	cpu->sp = (cpu->sp + 1) & 0xff;	
	uint8_t lo = read_memory(mem, ppu, apu, rom, 0x0100 + cpu->sp);
	cpu->sp = (cpu->sp + 1) & 0xff;	
	uint8_t hi = read_memory(mem, ppu, apu, rom, 0x0100 + cpu->sp);
	cpu->pc = BTOI(lo, hi);
	cpu->pc += 1;
	cpu->cycles = 6;
}

void op_sbc(NesCPU *cpu, uint8_t val) {
	uint8_t a = cpu->a;
	val = val - (1 - get_carry(cpu));
	cpu->a -= val;
	if(cpu->a == 0) {
		set_zero(cpu, 1);
	} else {
		set_zero(cpu, 0);
	}
	if(BORROW(a, val)) {
		set_carry(cpu, 0);
	}
	if(UNDERFLOW(a, val)) {
		set_overflow(cpu, 1);
	}
	if(IS_NEGATIVE(cpu->a)) {
		set_negative(cpu, 1);
	} else {
		set_negative(cpu, 0);
	}
}

void op_sec(NesCPU *cpu) {
	cpu->cycles = 2;
	cpu->pc += 1;
	set_carry(cpu, 1);
}

void op_sed(NesCPU *cpu) {
	cpu->cycles = 2;
	cpu->pc += 1;
	set_decimal(cpu, 1);
}

void op_sei(NesCPU *cpu) {
	cpu->cycles = 2;
	cpu->pc += 1;
	set_interrupt(cpu, 1);
}

void op_sta(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu, uint16_t addr) {
	write_memory(mem, ppu, apu, addr, cpu->a);
	//if OAM DMA
	if(addr == 0x4014) {
		cpu->cycles += 513;
	}
}

void op_stx(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu, uint16_t addr) {
	write_memory(mem, ppu, apu, addr, cpu->x);
	//if OAM DMA
	if(addr == 0x4014) {
		cpu->cycles += 513;
	}
}

void op_sty(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu, uint16_t addr) {
	write_memory(mem, ppu, apu, addr, cpu->y);
	//if OAM DMA
	if(addr == 0x4014) {
		cpu->cycles += 513;
	}
}

void op_tax(NesCPU *cpu) {
	cpu->x = cpu->a;
	cpu->cycles = 2;
	cpu->pc += 1;
}

void op_tay(NesCPU *cpu) {
	cpu->y = cpu->a;
	cpu->cycles = 2;
	cpu->pc += 1;
}

void op_tsx(NesCPU *cpu) {
	cpu->x = cpu->sp;
	cpu->cycles = 2;
	cpu->pc += 1;
}

void op_txa(NesCPU *cpu) {
	cpu->a = cpu->x;
	cpu->cycles = 2;
	cpu->pc += 1;
}

void op_txs(NesCPU *cpu) {
	cpu->sp = cpu->x;
	cpu->cycles = 2;
	cpu->pc += 1;
}

void op_tya(NesCPU *cpu) {
	cpu->a = cpu->y;
	cpu->cycles = 2;
	cpu->pc += 1;
}

//read memory at arg
uint8_t zero_page_x(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu, NesROM *rom, uint8_t arg) {
	return read_memory(mem, ppu, apu, rom, (arg + cpu->x) % 0x100);
}
uint8_t zero_page_y(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu, NesROM *rom, uint8_t arg) {
	return read_memory(mem, ppu, apu, rom, (arg + cpu->y) % 0x100);
}
uint8_t absolute_x(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu, NesROM *rom, uint16_t arg) {
	return read_memory(mem, ppu, apu, rom, arg + cpu->x);
}
uint8_t absolute_y(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu, NesROM *rom, uint16_t arg) {
	return read_memory(mem, ppu, apu, rom, arg + cpu->y);
}
uint8_t indirect_x(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu, NesROM *rom, uint8_t arg) {
	uint8_t lo_byte = read_memory(mem, ppu, apu, rom, (arg + cpu->x) % 0x100);
	uint8_t hi_byte = read_memory(mem, ppu, apu, rom, (arg + cpu->x + 1) % 0x100);
	
	return read_memory(mem, ppu, apu, rom, lo_byte + (hi_byte * 0x100));
}
uint8_t indirect_y(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu, NesROM *rom, uint8_t arg) {
	uint8_t lo_byte = read_memory(mem, ppu, apu, rom, arg);
	uint8_t hi_byte = read_memory(mem, ppu, apu, rom, (arg + 1) % 0x100);
	
	return read_memory(mem, ppu, apu, rom, lo_byte + (hi_byte * 0x100) + cpu->y);
}
uint8_t zero_page(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu, NesROM *rom, uint8_t arg) {
	return read_memory(mem, ppu, apu, rom, arg);
}
uint8_t absolute(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu, NesROM *rom, uint16_t arg) {
	return read_memory(mem, ppu, apu, rom, arg);
}
uint16_t indirect(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu, NesROM *rom, uint16_t arg) {
	/*
	//original 6502 bug reproduced ;)
	return read_memory(mem, ppu, apu, rom, arg) + (read_memory(mem, ppu, apu, rom, ((arg & 0xff00) + ((arg + 1) & 0x00ff)) * 0x100));
	*/
	return read_memory(mem, ppu, apu, rom, arg) + (read_memory(mem, ppu, apu, rom, arg + 1) * 0x100);
}

//get the addr used for writing to that address

uint8_t zero_page_x_addr(NesCPU *cpu, uint8_t arg) {
	return (arg + cpu->x) % 0x100;
}
uint8_t zero_page_y_addr(NesCPU *cpu, uint8_t arg) {
	return (arg + cpu->y) % 0x100;
}
uint16_t absolute_x_addr(NesCPU *cpu, uint16_t arg) {
	return arg + cpu->x;
}
uint16_t absolute_y_addr(NesCPU *cpu, uint16_t arg) {
	return arg + cpu->y;
}
uint16_t indirect_x_addr(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu, NesROM *rom, uint8_t arg) {
	uint8_t lo_byte = read_memory(mem, ppu, apu, rom, (arg + cpu->x) % 0x100);
	uint8_t hi_byte = read_memory(mem, ppu, apu, rom, (arg + cpu->x + 1) % 0x100);
	
	return lo_byte + (hi_byte * 0x100);
}
uint16_t indirect_y_addr(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu, NesROM *rom, uint8_t arg) {
	uint8_t lo_byte = read_memory(mem, ppu, apu, rom, arg);
	uint8_t hi_byte = read_memory(mem, ppu, apu, rom, (arg + 1) % 0x100);
	
	return lo_byte + (hi_byte * 0x100) + cpu->y;
}
uint8_t zero_page_addr(uint8_t arg) {
	return arg;
}
uint16_t absolute_addr(uint16_t arg) {
	return arg;
}


int run_cpu(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu, NesROM *rom){
	cpu->cycles = 0;
	uint8_t opcode = read_memory(mem, ppu, apu, rom, cpu->pc);
	uint8_t arg8_0 = read_memory(mem, ppu, apu, rom, cpu->pc + 1);
	uint8_t arg8_1 = read_memory(mem, ppu, apu, rom, cpu->pc + 2);
	uint16_t arg16 = BTOI(arg8_0, arg8_1);
	#ifdef DEBUG_CPU
	printf("op: %.2x %.2x %.2x, A %.2x X %.2x Y %.2x PC %.4x SP %.2x PS %.2x", 
			opcode, arg8_0, arg8_1, cpu->a, cpu->x, cpu->y, cpu->pc, cpu->sp, cpu->ps);
	printf(" -> C%d Z%d I%d D%d V%d N%d\n", get_carry(cpu), get_zero(cpu), get_interrupt(cpu), get_decimal(cpu), get_overflow(cpu), get_negative(cpu));
	#endif
	uint8_t arg0, arg1, arg2;
	uint16_t arg1_16;
	switch(opcode) {
		case ADC    :{
			op_adc(cpu, arg8_0);
			cpu->cycles = 2;
			cpu->pc += 2;
			break;
		}
		case ADC_ZP :{
			op_adc(cpu, zero_page(cpu, mem, ppu, apu, rom, arg8_0));
			cpu->cycles = 3;
			cpu->pc += 2;
			break;
		}
		case ADC_ZPX:{			
			op_adc(cpu, zero_page_x(cpu, mem, ppu, apu, rom, arg8_0));
			cpu->cycles = 4;
			cpu->pc += 2;
			break;
		}
		case ADC_A  :{
			op_adc(cpu, absolute(cpu, mem, ppu, apu, rom, arg16));
			cpu->cycles = 4;
			cpu->pc += 3;
			break;
		}
		case ADC_AX :{
			op_adc(cpu, absolute_x(cpu, mem, ppu, apu, rom, arg16));
			cpu->cycles = 4;
			if(CARRY(cpu->x, arg8_0)) {
				cpu->cycles++;
			}
			cpu->pc += 3;
			break;
		}
		case ADC_AY :{
			op_adc(cpu, absolute_y(cpu, mem, ppu, apu, rom, arg16));
			cpu->cycles = 4;
			if(CARRY(cpu->y, arg0)) {
				cpu->cycles++;
			}
			cpu->pc += 3;
			break;
		}
		case ADC_IX :{
			op_adc(cpu, indirect_x(cpu, mem, ppu, apu, rom, arg8_0));
			cpu->cycles = 6;
			cpu->pc += 2;
			break;
		}
		case ADC_IY :{
			op_adc(cpu, indirect_y(cpu, mem, ppu, apu, rom, arg8_0));
			cpu->cycles = 5;
			if(CARRY(cpu->y, read_memory(mem, ppu, apu, rom, arg8_0))) {
				cpu->cycles++;
			}
			cpu->pc += 2;
			break;
		}
		case AND    :{
			op_and(cpu, arg8_0);
			cpu->cycles = 2;
			cpu->pc += 2;
			break;
		}
		case AND_ZP :{
			op_and(cpu, zero_page(cpu, mem, ppu, apu, rom, arg8_0));
			cpu->cycles = 3;
			cpu->pc += 2;
			break;
		}
		case AND_ZPX:{
			op_and(cpu, zero_page_x(cpu, mem, ppu, apu, rom, arg8_0));
			cpu->cycles = 4;
			cpu->pc += 2;
			break;
		}
		case AND_A  :{
			op_and(cpu, absolute(cpu, mem, ppu, apu, rom, arg16));
			cpu->cycles = 4;
			cpu->pc += 3;
			break;
		}
		case AND_AX :{
			op_and(cpu, absolute_x(cpu, mem, ppu, apu, rom, arg16));
			cpu->cycles = 4;
			if(CARRY(cpu->x, arg8_0)) {
				cpu->cycles++;
			}
			cpu->pc += 3;
			break;
		}
		case AND_AY :{
			op_and(cpu, absolute_y(cpu, mem, ppu, apu, rom, arg16));
			cpu->cycles = 4;
			if(CARRY(cpu->y, arg8_0)) {
				cpu->cycles++;
			}
			cpu->pc += 3;
			break;
		}
		case AND_IX :{
			op_and(cpu, indirect_x(cpu, mem, ppu, apu, rom, arg8_0));
			cpu->cycles = 6;
			cpu->pc += 2;
			break;
		}
		case AND_IY :{
			op_and(cpu, indirect_y(cpu, mem, ppu, apu, rom, arg8_0));
			cpu->cycles = 5;
			if(CARRY(cpu->y, read_memory(mem, ppu, apu, rom, arg8_0))) {
				cpu->cycles++;
			}
			cpu->pc += 2;
			break;
		}
		case ASL    :{
			op_asl_a(cpu);
			cpu->cycles = 2;
			cpu->pc += 1;
			break;
		}
		case ASL_ZP :{
			op_asl_mem(cpu, mem, ppu, apu, zero_page(cpu, mem, ppu, apu, rom, arg8_0), zero_page_addr(arg8_0));
			cpu->cycles = 5;
			cpu->pc += 2;
			break;
		}
		case ASL_ZPX:{
			op_asl_mem(cpu, mem, ppu, apu, zero_page_x(cpu, mem, ppu, apu, rom, arg8_0), zero_page_x_addr(cpu, arg8_0));
			cpu->cycles = 6;
			cpu->pc += 2;
			break;
		}
		case ASL_A  :{
			op_asl_mem(cpu, mem, ppu, apu, absolute(cpu, mem, ppu, apu, rom, arg16), absolute_addr(arg16));
			cpu->cycles = 6;
			cpu->pc += 3;
			break;
		}
		case ASL_AX :{
			op_asl_mem(cpu, mem, ppu, apu, absolute_x(cpu, mem, ppu, apu, rom, arg16), absolute_x_addr(cpu, arg16));
			cpu->cycles = 7;
			cpu->pc += 3;
			break;
		}
		case BCC    :{
			op_bcc(cpu, arg8_0);
			break;
		}
		case BCS    :{
			op_bcs(cpu, arg8_0);
			break;
		}
		case BEQ    :{
			op_beq(cpu, arg8_0);
			break;
		}
		case BIT_ZP :{
			op_bit(cpu, zero_page(cpu, mem, ppu, apu, rom, arg8_0));
			cpu->cycles = 3;
			cpu->pc += 2;
			break;
		}
		case BIT_A  :{
			op_bit(cpu, absolute(cpu, mem, ppu, apu, rom, arg16));
			cpu->cycles = 4;
			cpu->pc += 3;
			break;
		}
		case BMI    :{
			op_bmi(cpu, arg8_0);
			break;
		}
		case BNE    :{
			op_bne(cpu, arg8_0);
			break;
		}
		case BPL    :{
			op_bpl(cpu, arg8_0);
			break;
		}
		case BRK    :{
			op_brk(cpu, mem, ppu, apu, rom);
			break;
		}
		case BVC    :{
			op_bvc(cpu, arg8_0);
			break;
		}
		case BVS    :{
			op_bvs(cpu, arg8_0);
			break;
		}
		case CLC    :{
			op_clc(cpu);
			break;
		}
		case CLD    :{
			op_cld(cpu);
			break;
		}
		case CLI    :{
			op_cli(cpu);
			break;
		}
		case CLV    :{
			op_clv(cpu);
			break;
		}
		case CMP    :{
			op_cmp(cpu, arg8_0);
			cpu->cycles = 2;
			cpu->pc += 2;
			break;
		}
		case CMP_ZP :{
			op_cmp(cpu, zero_page(cpu, mem, ppu, apu, rom, arg8_0));
			cpu->cycles = 3;
			cpu->pc += 2;
			break;
		}
		case CMP_ZPX:{
			op_cmp(cpu, zero_page_x(cpu, mem, ppu, apu, rom, arg8_0));
			cpu->cycles = 4;
			cpu->pc += 2;
			break;
		}
		case CMP_A  :{
			op_cmp(cpu, absolute(cpu, mem, ppu, apu, rom, arg16));
			cpu->cycles = 4;
			cpu->pc += 3;
			break;
		}
		case CMP_AX :{
			op_cmp(cpu, absolute_x(cpu, mem, ppu, apu, rom, arg16));
			cpu->cycles = 4;
			if(CARRY(cpu->x, arg8_0)) {
				cpu->cycles++;
			}
			cpu->pc += 3;
			break;
		}
		case CMP_AY :{
			op_cmp(cpu, absolute_y(cpu, mem, ppu, apu, rom, arg16));
			cpu->cycles = 4;
			if(CARRY(cpu->y, arg8_0)) {
				cpu->cycles++;
			}
			cpu->pc += 3;
			break;
		}
		case CMP_IX :{
			op_cmp(cpu, indirect_x(cpu, mem, ppu, apu, rom, arg8_0));
			cpu->cycles = 6;
			cpu->pc += 2;
			break;
		}
		case CMP_IY :{
			op_cmp(cpu, indirect_y(cpu, mem, ppu, apu, rom, arg8_0));
			cpu->cycles = 5;
			if(CARRY(cpu->y, read_memory(mem, ppu, apu, rom, arg8_0))) {
				cpu->cycles++;
			}
			cpu->pc += 2;
			break;
		}
		case CPX    :{
			op_cpx(cpu, arg8_0);
			cpu->cycles = 2;
			cpu->pc += 2;
			break;
		}
		case CPX_ZP :{
			op_cpx(cpu, zero_page(cpu, mem, ppu, apu, rom, arg8_0));
			cpu->cycles = 3;
			cpu->pc += 2;
			break;
		}
		case CPX_A  :{
			op_cpx(cpu, absolute(cpu, mem, ppu, apu, rom, arg16));
			cpu->cycles = 4;
			cpu->pc += 3;
			break;
		}
		case CPY    :{
			op_cpy(cpu, arg8_0);
			cpu->cycles = 2;
			cpu->pc += 2;
			break;
		}
		case CPY_ZP :{
			op_cpy(cpu, zero_page(cpu, mem, ppu, apu, rom, arg8_0));
			cpu->cycles = 3;
			cpu->pc += 2;
			break;
		}
		case CPY_A  :{
			op_cpy(cpu, absolute(cpu, mem, ppu, apu, rom, arg16));
			cpu->cycles = 4;
			cpu->pc += 3;
			break;
		}
		case DEC_ZP :{
			op_dec(cpu, mem, ppu, apu, zero_page(cpu, mem, ppu, apu, rom, arg8_0), zero_page_addr(arg8_0));
			cpu->cycles = 5;
			cpu->pc += 2;
			break;
		}
		case DEC_ZPX:{
			op_dec(cpu, mem, ppu, apu, zero_page_x(cpu, mem, ppu, apu, rom, arg8_0), zero_page_x_addr(cpu, arg8_0));
			cpu->cycles = 6;
			cpu->pc += 2;
			break;
		}
		case DEC_A  :{
			op_dec(cpu, mem, ppu, apu, absolute(cpu, mem, ppu, apu, rom, 16), absolute_addr(arg16));
			cpu->cycles = 6;
			cpu->pc += 3;
			break;
		}
		case DEC_AX :{
			op_dec(cpu, mem, ppu, apu, absolute_x(cpu, mem, ppu, apu, rom, 16), absolute_x_addr(cpu, arg16));
			cpu->cycles = 7;
			cpu->pc += 3;
			break;
		}
		case DEX    :{
			op_dex(cpu);
			break;
		}
		case DEY    :{
			op_dey(cpu);
			break;
		}
		case EOR    :{
			op_eor(cpu, arg8_0);
			cpu->cycles = 2;
			cpu->pc += 2;
			break;
		}
		case EOR_ZP :{
			op_eor(cpu, zero_page(cpu, mem, ppu, apu, rom, arg8_0));
			cpu->cycles = 3;
			cpu->pc += 2;
			break;
		}
		case EOR_ZPX:{
			op_eor(cpu, zero_page_x(cpu, mem, ppu, apu, rom, arg8_0));
			cpu->cycles = 4;
			cpu->pc += 2;
			break;
		}
		case EOR_A  :{
			op_eor(cpu, absolute(cpu, mem, ppu, apu, rom, arg16));
			cpu->cycles = 4;
			cpu->pc += 3;
			break;
		}
		case EOR_AX :{
			op_eor(cpu, absolute_x(cpu, mem, ppu, apu, rom, arg16));
			cpu->cycles = 4;
			if(CARRY(cpu->x, arg8_0)) {
				cpu->cycles++;
			}
			cpu->pc += 3;
			break;
		}
		case EOR_AY :{
			op_eor(cpu, absolute_y(cpu, mem, ppu, apu, rom, arg16));
			cpu->cycles = 4;
			if(CARRY(cpu->y, arg8_0)) {
				cpu->cycles++;
			}
			cpu->pc += 3;
			break;
		}
		case EOR_IX :{
			op_eor(cpu, indirect_x(cpu, mem, ppu, apu, rom, arg8_0));
			cpu->cycles = 6;
			cpu->pc += 2;
			break;
		}
		case EOR_IY :{
			op_eor(cpu, indirect_y(cpu, mem, ppu, apu, rom, arg8_0));
			cpu->cycles = 5;
			if(CARRY(cpu->y, read_memory(mem, ppu, apu, rom, arg8_0))) {
				cpu->cycles++;
			}
			cpu->pc += 2;
			break;
		}
		case INC_ZP :{
			op_inc(cpu, mem, ppu, apu, zero_page(cpu, mem, ppu, apu, rom, arg8_0), zero_page_addr(arg8_0));
			cpu->cycles = 5;
			cpu->pc += 2;
			break;
		}
		case INC_ZPX:{
			op_inc(cpu, mem, ppu, apu, zero_page_x(cpu, mem, ppu, apu, rom, arg8_0), zero_page_x_addr(cpu, arg8_0));
			cpu->cycles = 6;
			cpu->pc += 2;
			break;
		}
		case INC_A  :{
			op_inc(cpu, mem, ppu, apu, absolute(cpu, mem, ppu, apu, rom, 16), absolute_addr(arg16));
			cpu->cycles = 6;
			cpu->pc += 3;
			break;
		}
		case INC_AX :{
			op_inc(cpu, mem, ppu, apu, absolute_x(cpu, mem, ppu, apu, rom, 16), absolute_x_addr(cpu, arg16));
			cpu->cycles = 7;
			cpu->pc += 3;
			break;
		}
		case INX    :{
			op_inx(cpu);
			break;
		}
		case INY    :{
			op_iny(cpu);
			break;
		}
		case JMP    :{
			op_jmp(cpu, arg16);
			cpu->cycles = 3;
			break;
		}
		case JMP_I  :{
			op_jmp(cpu, indirect(cpu, mem, ppu, apu, rom, arg16));
			cpu->cycles = 6;
			break;
		}
		case JSR    :{
			op_jsr(cpu, mem, ppu, apu, arg16);
			break;
		}
		case LDA    :{
			op_lda(cpu, arg8_0);
			cpu->cycles = 2;
			cpu->pc += 2;
			break;
		}
		case LDA_ZP :{
			op_lda(cpu, zero_page(cpu, mem, ppu, apu, rom, arg8_0));
			cpu->cycles = 3;
			cpu->pc += 2;
			break;
		}
		case LDA_ZPX:{
			op_lda(cpu, zero_page_x(cpu, mem, ppu, apu, rom, arg8_0));
			cpu->cycles = 4;
			cpu->pc += 2;
			break;
		}
		case LDA_A  :{
			op_lda(cpu, absolute(cpu, mem, ppu, apu, rom, arg16));
			cpu->cycles = 4;
			cpu->pc += 3;
			break;
		}
		case LDA_AX :{
			op_lda(cpu, absolute_x(cpu, mem, ppu, apu, rom, arg16));
			cpu->cycles = 4;
			if(CARRY(cpu->x, arg8_0)) {
				cpu->cycles++;
			}
			cpu->pc += 3;
			break;
		}
		case LDA_AY :{
			op_lda(cpu, absolute_y(cpu, mem, ppu, apu, rom, arg16));
			cpu->cycles = 4;
			if(CARRY(cpu->y, arg8_0)) {
				cpu->cycles++;
			}
			cpu->pc += 3;
			break;
		}
		case LDA_IX :{
			op_lda(cpu, indirect_x(cpu, mem, ppu, apu, rom, arg8_0));
			cpu->cycles = 6;
			cpu->pc += 2;
			break;
		}
		case LDA_IY :{
			op_lda(cpu, indirect_y(cpu, mem, ppu, apu, rom, arg8_0));
			cpu->cycles = 5;
			if(CARRY(cpu->y, read_memory(mem, ppu, apu, rom, arg8_0))) {
				cpu->cycles++;
			}
			cpu->pc += 2;
			break;
		}
		case LDX    :{
			op_ldx(cpu, arg8_0);
			cpu->cycles = 2;
			cpu->pc += 2;
			break;
		}
		case LDX_ZP :{
			op_ldx(cpu, zero_page(cpu, mem, ppu, apu, rom, arg8_0));
			cpu->cycles = 3;
			cpu->pc += 2;
			break;
		}
		case LDX_ZPY:{
			op_ldx(cpu, zero_page_y(cpu, mem, ppu, apu, rom, arg8_0));
			cpu->cycles = 4;
			cpu->pc += 2;
			break;
		}
		case LDX_A  :{
			op_ldx(cpu, absolute(cpu, mem, ppu, apu, rom, arg16));
			cpu->cycles = 4;
			cpu->pc += 3;
			break;
		}
		case LDX_AY :{
			op_ldx(cpu, absolute_y(cpu, mem, ppu, apu, rom, arg16));
			cpu->cycles = 4;
			if(CARRY(cpu->y, arg8_0)) {
				cpu->cycles++;
			}
			cpu->pc += 3;
			break;
		}
		case LDY    :{
			op_ldy(cpu, arg8_0);
			cpu->cycles = 2;
			cpu->pc += 2;
			break;
		}
		case LDY_ZP :{
			op_ldy(cpu, zero_page(cpu, mem, ppu, apu, rom, arg8_0));
			cpu->cycles = 3;
			cpu->pc += 2;
			break;
		}
		case LDY_ZPX:{
			op_ldy(cpu, zero_page_x(cpu, mem, ppu, apu, rom, arg8_0));
			cpu->cycles = 4;
			cpu->pc += 2;
			break;
		}
		case LDY_A  :{
			op_ldy(cpu, absolute(cpu, mem, ppu, apu, rom, arg16));
			cpu->cycles = 4;
			cpu->pc += 3;
			break;
		}
		case LDY_AX :{
			op_ldy(cpu, absolute_x(cpu, mem, ppu, apu, rom, arg16));
			cpu->cycles = 4;
			if(CARRY(cpu->x, arg8_0)) {
				cpu->cycles++;
			}
			cpu->pc += 3;
			break;
		}
		case LSR    :{
			op_lsr_a(cpu);
			cpu->cycles = 2;
			cpu->pc += 1;
			break;
		}
		case LSR_ZP :{
			op_lsr_mem(cpu, mem, ppu, apu, zero_page(cpu, mem, ppu, apu, rom, arg8_0), zero_page_addr(arg8_0));
			cpu->cycles = 5;
			cpu->pc += 2;
			break;
		}
		case LSR_ZPX:{
			op_lsr_mem(cpu, mem, ppu, apu, zero_page_x(cpu, mem, ppu, apu, rom, arg8_0), zero_page_x_addr(cpu, arg8_0));
			cpu->cycles = 6;
			cpu->pc += 2;
			break;
		}
		case LSR_A  :{
			op_lsr_mem(cpu, mem, ppu, apu, absolute(cpu, mem, ppu, apu, rom, arg16), absolute_addr(arg16));
			cpu->cycles = 6;
			cpu->pc += 3;
			break;
		}
		case LSR_AX :{
			op_lsr_mem(cpu, mem, ppu, apu, absolute_x(cpu, mem, ppu, apu, rom, arg16), absolute_x_addr(cpu, arg16));
			cpu->cycles = 7;
			cpu->pc += 3;
			break;
		}
		case NOP    :{
			op_nop(cpu);
			break;
		}
		case ORA    :{
			op_ora(cpu, arg8_0);
			cpu->cycles = 2;
			cpu->pc += 2;
			break;
		}
		case ORA_ZP :{
			op_ora(cpu, zero_page(cpu, mem, ppu, apu, rom, arg8_0));
			cpu->cycles = 3;
			cpu->pc += 2;
			break;
		}
		case ORA_ZPX:{
			op_ora(cpu, zero_page_x(cpu, mem, ppu, apu, rom, arg8_0));
			cpu->cycles = 4;
			cpu->pc += 2;
			break;
		}
		case ORA_A  :{
			op_ora(cpu, absolute(cpu, mem, ppu, apu, rom, arg16));
			cpu->cycles = 4;
			cpu->pc += 3;
			break;
		}
		case ORA_AX :{
			op_ora(cpu, absolute_x(cpu, mem, ppu, apu, rom, arg16));
			cpu->cycles = 4;
			if(CARRY(cpu->x, arg8_0)) {
				cpu->cycles++;
			}
			cpu->pc += 3;
			break;
		}
		case ORA_AY :{
			op_ora(cpu, absolute_y(cpu, mem, ppu, apu, rom, arg16));
			cpu->cycles = 4;
			if(CARRY(cpu->y, arg8_0)) {
				cpu->cycles++;
			}
			cpu->pc += 3;
			break;
		}
		case ORA_IX :{
			op_ora(cpu, indirect_x(cpu, mem, ppu, apu, rom, arg8_0));
			cpu->cycles = 6;
			cpu->pc += 2;
			break;
		}
		case ORA_IY :{
			op_ora(cpu, indirect_y(cpu, mem, ppu, apu, rom, arg8_0));
			cpu->cycles = 5;
			if(CARRY(cpu->y, read_memory(mem, ppu, apu, rom, arg8_0))) {
				cpu->cycles++;
			}
			cpu->pc += 2;
			break;
		}
		case PHA    :{
			op_pha(cpu, mem, ppu, apu);
			break;
		}
		case PHP    :{
			op_php(cpu, mem, ppu, apu);
			break;
		}
		case PLA    :{
			op_pla(cpu, mem, ppu, apu, rom);
			break;
		}
		case PLP    :{
			op_plp(cpu, mem, ppu, apu, rom);
			break;
		}
		case ROL    :{
			op_rol_a(cpu);
			cpu->cycles = 2;
			cpu->pc += 1;
			break;
		}
		case ROL_ZP :{
			op_rol_mem(cpu, mem, ppu, apu, zero_page(cpu, mem, ppu, apu, rom, arg8_0), zero_page_addr(arg8_0));
			cpu->cycles = 5;
			cpu->pc += 2;
			break;
		}
		case ROL_ZPX:{
			op_rol_mem(cpu, mem, ppu, apu, zero_page_x(cpu, mem, ppu, apu, rom, arg8_0), zero_page_x_addr(cpu, arg8_0));
			cpu->cycles = 6;
			cpu->pc += 2;
			break;
		}
		case ROL_A  :{
			op_rol_mem(cpu, mem, ppu, apu, absolute(cpu, mem, ppu, apu, rom, arg16), absolute_addr(arg16));
			cpu->cycles = 6;
			cpu->pc += 3;
			break;
		}
		case ROL_AX :{
			op_rol_mem(cpu, mem, ppu, apu, absolute_x(cpu, mem, ppu, apu, rom, arg16), absolute_x_addr(cpu, arg16));
			cpu->cycles = 7;
			cpu->pc += 3;
			break;
		}
		case ROR    :{
			op_ror_a(cpu);
			cpu->cycles = 2;
			cpu->pc += 1;
			break;
		}
		case ROR_ZP :{
			op_ror_mem(cpu, mem, ppu, apu, zero_page(cpu, mem, ppu, apu, rom, arg8_0), zero_page_addr(arg8_0));
			cpu->cycles = 5;
			cpu->pc += 2;
			break;
		}
		case ROR_ZPX:{
			op_ror_mem(cpu, mem, ppu, apu, zero_page_x(cpu, mem, ppu, apu, rom, arg8_0), zero_page_x_addr(cpu, arg8_0));
			cpu->cycles = 6;
			cpu->pc += 2;
			break;
		}
		case ROR_A  :{
			op_ror_mem(cpu, mem, ppu, apu, absolute(cpu, mem, ppu, apu, rom, arg16), absolute_addr(arg16));
			cpu->cycles = 6;
			cpu->pc += 3;
			break;
		}
		case ROR_AX :{
			op_ror_mem(cpu, mem, ppu, apu, absolute_x(cpu, mem, ppu, apu, rom, arg16), absolute_x_addr(cpu, arg16));
			cpu->cycles = 7;
			cpu->pc += 3;
			break;
		}
		case RTI    :{
			op_rti(cpu, mem, ppu, apu, rom);
			break;
		}
		case RTS    :{
			op_rts(cpu, mem, ppu, apu, rom);
			break;
		}
		case SBC   :{
			op_adc(cpu, arg8_0);
			cpu->cycles = 2;
			cpu->pc += 2;
			break;
		}
		case SBC_ZP :{
			op_sbc(cpu, zero_page(cpu, mem, ppu, apu, rom, arg8_0));
			cpu->cycles = 3;
			cpu->pc += 2;
			break;
		}
		case SBC_ZPX:{			
			op_sbc(cpu, zero_page_x(cpu, mem, ppu, apu, rom, arg8_0));
			cpu->cycles = 4;
			cpu->pc += 2;
			break;
		}
		case SBC_A  :{
			op_sbc(cpu, absolute(cpu, mem, ppu, apu, rom, arg16));
			cpu->cycles = 4;
			cpu->pc += 3;
			break;
		}
		case SBC_AX :{
			op_sbc(cpu, absolute_x(cpu, mem, ppu, apu, rom, arg16));
			cpu->cycles = 4;
			if(CARRY(cpu->x, arg8_0)) {
				cpu->cycles++;
			}
			cpu->pc += 3;
			break;
		}
		case SBC_AY :{
			op_sbc(cpu, absolute_y(cpu, mem, ppu, apu, rom, arg16));
			cpu->cycles = 4;
			if(CARRY(cpu->y, arg0)) {
				cpu->cycles++;
			}
			cpu->pc += 3;
			break;
		}
		case SBC_IX :{
			op_sbc(cpu, indirect_x(cpu, mem, ppu, apu, rom, arg8_0));
			cpu->cycles = 6;
			cpu->pc += 2;
			break;
		}
		case SBC_IY :{
			op_sbc(cpu, indirect_y(cpu, mem, ppu, apu, rom, arg8_0));
			cpu->cycles = 5;
			if(CARRY(cpu->y, read_memory(mem, ppu, apu, rom, arg8_0))) {
				cpu->cycles++;
			}
			cpu->pc += 2;
			break;
		}
		case SEC    :{
			op_sec(cpu);
			break;
		}
		case SED    :{
			op_sed(cpu);
			break;
		}
		case SEI    :{
			op_sei(cpu);
			break;
		}
		case STA_ZP :{
			cpu->cycles = 3;
			op_sta(cpu, mem, ppu, apu, zero_page_addr(arg8_0));
			cpu->pc += 2;
			break;
		}
		case STA_ZPX:{
			cpu->cycles = 4;
			op_sta(cpu, mem, ppu, apu, zero_page_x_addr(cpu, arg8_0));
			cpu->pc += 2;
			break;
		}
		case STA_A  :{
			cpu->cycles = 4;
			op_sta(cpu, mem, ppu, apu, absolute_addr(arg16));
			cpu->pc += 3;
			break;
		}
		case STA_AX :{
			cpu->cycles = 5;
			op_sta(cpu, mem, ppu, apu, absolute_x_addr(cpu, arg16));
			cpu->pc += 3;
			break;
		}
		case STA_AY :{
			cpu->cycles = 5;
			op_sta(cpu, mem, ppu, apu, absolute_y_addr(cpu, arg16));
			cpu->pc += 3;
			break;
		}
		case STA_IX :{
			cpu->cycles = 6;
			op_sta(cpu, mem, ppu, apu, indirect_x_addr(cpu, mem, ppu, apu, rom, arg8_0));
			cpu->pc += 2;
			break;
		}
		case STA_IY :{
			cpu->cycles = 6;
			op_sta(cpu, mem, ppu, apu, indirect_y_addr(cpu, mem, ppu, apu, rom, arg8_0));
			cpu->pc += 2;
			break;
		}
		case STX_ZP :{
			cpu->cycles = 3;
			op_stx(cpu, mem, ppu, apu, zero_page_addr(arg8_0));
			cpu->pc += 2;
			break;
		}
		case STX_ZPY:{
			cpu->cycles = 4;
			op_stx(cpu, mem, ppu, apu, zero_page_y_addr(cpu, arg8_0));
			cpu->pc += 2;
			break;
		}
		case STX_A  :{
			cpu->cycles = 4;
			op_stx(cpu, mem, ppu, apu, absolute_addr(arg16));
			cpu->pc += 3;
			break;
		}
		case STY_ZP :{
			cpu->cycles = 3;
			op_sty(cpu, mem, ppu, apu, zero_page_addr(arg8_0));
			cpu->pc += 2;
			break;
		}
		case STY_ZPX:{
			cpu->cycles = 4;
			op_sty(cpu, mem, ppu, apu, zero_page_x_addr(cpu, arg8_0));
			cpu->pc += 2;
			break;
		}
		case STY_A  :{
			cpu->cycles = 4;
			op_sty(cpu, mem, ppu, apu, absolute_addr(arg16));
			cpu->pc += 3;
			break;
		}
		case TAX    :{
			op_tax(cpu);
			break;
		}
		case TAY    :{
			op_tay(cpu);
			break;
		}
		case TSX    :{
			op_tsx(cpu);
			break;
		}
		case TXA    :{
			op_txa(cpu);
			break;
		}
		case TXS    :{
			op_txs(cpu);
			break;
		}
		case TYA    :{
			op_tya(cpu);
			break;
		}
		
		default: {
			printf("nes_cpu: illegal opcode at %.4x, incrementing program counter\n", cpu->pc);
			cpu->pc += 1;
		}
		
	}
	
	
	return cpu->cycles;
}



NesCPU* initCPU() {
	NesCPU *cpu = calloc(1, sizeof(NesCPU));
	
	return cpu;
}

void freeCPU(NesCPU *cpu) {
	if(cpu != NULL) {
		free(cpu);
	}
}

int reset_cpu(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu, NesROM *rom) {
	cpu->pc = read_memory(mem, ppu, apu, rom, 0xfffc) + (read_memory(mem, ppu, apu, rom, 0xfffd) * 0x100);
	cpu->a = 0;
	cpu->x = 0;
	cpu->y = 0;
	cpu->sp = 0xfd;
	cpu->ps = 0x34;
	write_memory(mem, ppu, apu, 0x4017, 0x00);
	write_memory(mem, ppu, apu, 0x4015, 0x00);
	
	for(int addr = 0x4000; addr <= 0x400f; addr++) {
		write_memory(mem, ppu, apu, addr, 0x00);
	}
	
	return 0;
}

void do_nmi(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu, NesROM *rom) {
	cpu->pc += 2;
	write_memory(mem, ppu, apu, 0x0100 + cpu->sp, (cpu->pc & 0xff00) >> 8);
	cpu->sp = (cpu->sp - 1) & 0xff;
	write_memory(mem, ppu, apu, 0x0100 + cpu->sp, cpu->pc & 0x00ff);
	cpu->sp = (cpu->sp - 1) & 0xff;
	write_memory(mem, ppu, apu, 0x0100 + cpu->sp, cpu->ps | 0x30);
	cpu->sp = (cpu->sp - 1) & 0xff;
	set_interrupt(cpu, 1);
	cpu->pc = BTOI(read_memory(mem, ppu, apu, rom, 0xfffa), read_memory(mem, ppu, apu, rom, 0xfffb));
	cpu->cycles = 7;	
}

void set_carry(NesCPU *cpu, uint8_t value) {
	cpu->ps = (cpu->ps & 0xfe) | BOOL(value);
}

void set_zero(NesCPU *cpu, uint8_t value){
	cpu->ps = (cpu->ps & 0xfd) | (BOOL(value) << 1);
}

void set_interrupt(NesCPU *cpu, uint8_t value){
	cpu->ps = (cpu->ps & 0xfb) | (BOOL(value) << 2);
}

void set_decimal(NesCPU *cpu, uint8_t value){
	cpu->ps = (cpu->ps & 0xf7) | (BOOL(value) << 3);
}

void set_overflow(NesCPU *cpu, uint8_t value){
	cpu->ps = (cpu->ps & 0xbf) | (BOOL(value) << 6);
}

void set_negative(NesCPU *cpu, uint8_t value){
	cpu->ps = (cpu->ps & 0x7f) | (BOOL(value) << 7);
}


uint8_t get_carry(NesCPU *cpu){
	return (cpu->ps & 0x01);
}

uint8_t get_zero(NesCPU *cpu){
	return (cpu->ps & 0x02) >> 1;
}

uint8_t get_interrupt(NesCPU *cpu){
	return (cpu->ps & 0x04) >> 2;
}

uint8_t get_decimal(NesCPU *cpu){
	return (cpu->ps & 0x08) >> 3;
}

uint8_t get_overflow(NesCPU *cpu){
	return (cpu->ps & 0x40) >> 6;
}

uint8_t get_negative(NesCPU *cpu){
	return (cpu->ps & 0x80) >> 7;
}

