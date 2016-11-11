#ifndef NES_CPU_HEADER
#define NES_CPU_HEADER
#include <stdint.h>
#include "nes_mem.h"
#include "nes_rom.h"

enum _ops{
	ADC 	= 0x69,
	ADC_ZP 	= 0x65,
	ADC_ZPX = 0x75,
	ADC_A 	= 0x6d,
	ADC_AX 	= 0x7d,
	ADC_AY 	= 0x79,
	ADC_IX 	= 0x61,
	ADC_IY 	= 0x71,
	
	AND 	= 0x29,
	AND_ZP 	= 0x25,
	AND_ZPX = 0x35,
	AND_A 	= 0x2d,
	AND_AX 	= 0x3d,
	AND_AY 	= 0x39,
	AND_IX 	= 0x21,
	AND_IY 	= 0x31,
	
	ASL 	= 0x0a,
	ASL_ZP 	= 0x06,
	ASL_ZPX = 0x16,
	ASL_A 	= 0x0e,
	ASL_AX 	= 0x1e,
	
	BCC 	= 0x90,
	
	BCS 	= 0xb0,
	
	BEQ 	= 0xf0,
	
	BIT_ZP 	= 0x24,
	BIT_A 	= 0x2c,
	
	BMI 	= 0x30,
	
	BNE 	= 0xd0,
	
	BPL 	= 0x10,
	
	BRK 	= 0x00,
	
	BVC 	= 0x50,
	
	BVS 	= 0x70,
	
	CLC 	= 0x18,
	
	CLD 	= 0xd8,
	
	CLI 	= 0x58,
	
	CLV 	= 0xb8,
	
	CMP 	= 0xc9,
	CMP_ZP 	= 0xc5,
	CMP_ZPX = 0xd5,
	CMP_A 	= 0xcd,
	CMP_AX 	= 0xdd,
	CMP_AY 	= 0xd9,
	CMP_IX 	= 0xc1,
	CMP_IY 	= 0xd1,
	
	CPX 	= 0xe0,
	CPX_ZP 	= 0xe4,
	CPX_A 	= 0xec,
	
	CPY 	= 0xc0,
	CPY_ZP 	= 0xc4,
	CPY_A	= 0xcc,
	
	DEC_ZP	= 0xc6,
	DEC_ZPX	= 0xd6,
	DEC_A	= 0xce,
	DEC_AX	= 0xde,
	
	DEX 	= 0xca,
	
	DEY		= 0x88,
	
	EOR		= 0x49,
	EOR_ZP	= 0x45,
	EOR_ZPX = 0x55,
	EOR_A 	= 0x4d,
	EOR_AX 	= 0x5d,
	EOR_AY 	= 0x59,
	EOR_IX 	= 0x41,
	EOR_IY 	= 0x51,
	
	INC_ZP	= 0xe6,
	INC_ZPX	= 0xf6,
	INC_A	= 0xee,
	INC_AX	= 0xfe,
	
	INX 	= 0xe8,
	
	INY		= 0xc8,
	
	JMP 	= 0x4c,
	JMP_I	= 0x6c,
	
	JSR		= 0x20,
	
	LDA		= 0xa9,
	LDA_ZP	= 0xa5,
	LDA_ZPX = 0xb5,
	LDA_A 	= 0xad,
	LDA_AX 	= 0xbd,
	LDA_AY 	= 0xb9,
	LDA_IX 	= 0xa1,
	LDA_IY 	= 0xb1,
	
	LDX 	= 0xa2,
	LDX_ZP	= 0xa6,
	LDX_ZPY	= 0xb6,
	LDX_A	= 0xae,
	LDX_AY	= 0xbe,
	
	LDY 	= 0xa0,
	LDY_ZP	= 0xa4,
	LDY_ZPX	= 0xb4,
	LDY_A	= 0xac,
	LDY_AX	= 0xbc,
	
	LSR 	= 0x4a,
	LSR_ZP	= 0x46,
	LSR_ZPX	= 0x56,
	LSR_A	= 0x4e,
	LSR_AX	= 0x5e,
	
	NOP 	= 0xea,
	
	ORA		= 0x09,
	ORA_ZP	= 0x05,
	ORA_ZPX = 0x15,
	ORA_A 	= 0x0d,
	ORA_AX 	= 0x1d,
	ORA_AY 	= 0x19,
	ORA_IX 	= 0x01,
	ORA_IY 	= 0x11,
	
	PHA 	= 0x48,
	
	PHP		= 0x08,
	
	PLA		= 0x68,
	
	PLP		= 0x28,
	
	ROL 	= 0x2a,
	ROL_ZP	= 0x26,
	ROL_ZPX	= 0x36,
	ROL_A	= 0x2e,
	ROL_AX	= 0x3e,
	
	ROR 	= 0x6a,
	ROR_ZP	= 0x66,
	ROR_ZPX	= 0x76,
	ROR_A	= 0x6e,
	ROR_AX	= 0x7e,
	
	RTI		= 0x40,
	
	RTS		= 0x60,
	
	SBC		= 0xe9,
	SBC_ZP	= 0xe5,
	SBC_ZPX = 0xf5,
	SBC_A 	= 0xed,
	SBC_AX 	= 0xfd,
	SBC_AY 	= 0xf9,
	SBC_IX 	= 0xe1,
	SBC_IY 	= 0xf1,
	
	SEC		= 0x38,

	SED		= 0xf8,
	
	SEI		= 0x78,
	
	STA_ZP	= 0x85,
	STA_ZPX = 0x95,
	STA_A 	= 0x8d,
	STA_AX 	= 0x9d,
	STA_AY 	= 0x99,
	STA_IX 	= 0x81,
	STA_IY 	= 0x91,
	
	STX_ZP	= 0x86,
	STX_ZPY	= 0x96,
	STX_A	= 0x8e,
	
	STY_ZP	= 0x84,
	STY_ZPX	= 0x94,
	STY_A	= 0x8c,
	
	TAX		= 0xaa,
	
	TAY		= 0xa8,
	
	TSX		= 0xba,
	
	TXA		= 0x8a,
	
	TXS		= 0x9a,
	
	TYA		= 0x98
	
};

typedef enum _ops Opcode;

typedef struct {
	uint16_t pc; 	//program counter
	uint8_t sp;		//stack pointer
	uint8_t ps;		//processor status
	uint8_t a;		//accumulator
	uint8_t x;		//index register x
	uint8_t y;		//index register y
	uint64_t total_cycles;
	uint16_t cycles;
} NesCPU;

void op_adc(NesCPU *cpu, uint8_t val);
void op_and(NesCPU *cpu, uint8_t val);
void op_asl_a(NesCPU *cpu);
void op_asl_mem(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu, uint8_t val, uint16_t addr);
void do_branch(NesCPU *cpu, uint8_t val);
void op_bcc(NesCPU *cpu, uint8_t val);
void op_bcs(NesCPU *cpu, uint8_t val);
void op_beq(NesCPU *cpu, uint8_t val);
void op_bit(NesCPU *cpu, uint8_t val);
void op_bmi(NesCPU *cpu, uint8_t val);
void op_bne(NesCPU *cpu, uint8_t val);
void op_bpl(NesCPU *cpu, uint8_t val);
void op_brk(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu, NesROM *rom);
void op_bvc(NesCPU *cpu, uint8_t val);
void op_bvs(NesCPU *cpu, uint8_t val);
void op_clc(NesCPU *cpu);
void op_cld(NesCPU *cpu);
void op_cli(NesCPU *cpu);
void op_clv(NesCPU *cpu);
void op_cmp(NesCPU *cpu, uint8_t val);
void op_cpx(NesCPU *cpu, uint8_t val);
void op_cpy(NesCPU *cpu, uint8_t val);
void op_dec(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu, uint8_t val, uint16_t addr);
void op_dex(NesCPU *cpu);
void op_dey(NesCPU *cpu);
void op_eor(NesCPU *cpu, uint8_t val);
void op_inc(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu, uint8_t val, uint16_t addr);
void op_inx(NesCPU *cpu);
void op_iny(NesCPU *cpu);
void op_jmp(NesCPU *cpu, uint16_t addr);
void op_jsr(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu, uint16_t addr);
void op_lda(NesCPU *cpu, uint8_t val);
void op_ldx(NesCPU *cpu, uint8_t val);
void op_ldy(NesCPU *cpu, uint8_t val);
void op_lsr_a(NesCPU *cpu);
void op_lsr_mem(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu, uint8_t val, uint16_t addr);
void op_nop(NesCPU *cpu);
void op_ora(NesCPU *cpu, uint8_t val);
void op_pha(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu);
void op_php(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu);
void op_pla(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu, NesROM *rom);
void op_plp(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu, NesROM *rom);
void op_rol_a(NesCPU *cpu);
void op_rol_mem(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu, uint8_t val, uint16_t addr);
void op_ror_a(NesCPU *cpu);
void op_ror_mem(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu, uint8_t val, uint16_t addr);
void op_rti(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu, NesROM *rom);
void op_rts(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu, NesROM *rom);
void op_sbc(NesCPU *cpu, uint8_t val);
void op_sec(NesCPU *cpu);
void op_sed(NesCPU *cpu);
void op_sei(NesCPU *cpu);
void op_sta(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu, uint16_t addr);
void op_stx(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu, uint16_t addr);
void op_sty(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu, uint16_t addr);
void op_tax(NesCPU *cpu);
void op_tay(NesCPU *cpu);
void op_tsx(NesCPU *cpu);
void op_txa(NesCPU *cpu);
void op_txs(NesCPU *cpu);
void op_tya(NesCPU *cpu);




uint8_t zero_page_x(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu, NesROM *rom, uint8_t arg);
uint8_t zero_page_y(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu, NesROM *rom, uint8_t arg);
uint8_t absolute_x(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu, NesROM *rom, uint16_t arg);
uint8_t absolute_y(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu, NesROM *rom, uint16_t arg);
uint8_t indirect_x(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu, NesROM *rom, uint8_t arg);
uint8_t indirect_y(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu, NesROM *rom, uint8_t arg);
uint8_t zero_page(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu, NesROM *rom, uint8_t arg);
uint8_t absolute(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu, NesROM *rom, uint16_t arg);
uint16_t indirect(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu, NesROM *rom, uint16_t arg);

uint8_t zero_page_x_addr(NesCPU *cpu, uint8_t arg);
uint8_t zero_page_y_addr(NesCPU *cpu, uint8_t arg);
uint16_t absolute_x_addr(NesCPU *cpu, uint16_t arg);
uint16_t absolute_y_addr(NesCPU *cpu, uint16_t arg);
uint16_t indirect_x_addr(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu, NesROM *rom, uint8_t arg);
uint16_t indirect_y_addr(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu, NesROM *rom, uint8_t arg);
uint8_t zero_page_addr(uint8_t arg);
uint16_t absolute_addr(uint16_t arg);

int run_cpu(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu, NesROM *rom);
NesCPU* initCPU();
void freeCPU(NesCPU *cpu);
int reset_cpu(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu, NesROM *rom);

void do_nmi(NesCPU *cpu, NesMEM *mem, NesPPU *ppu, NesAPU *apu, NesROM *rom);

void set_carry(NesCPU *cpu, uint8_t value);
void set_zero(NesCPU *cpu, uint8_t value);
void set_interrupt(NesCPU *cpu, uint8_t value);
void set_decimal(NesCPU *cpu, uint8_t value);
void set_overflow(NesCPU *cpu, uint8_t value);
void set_negative(NesCPU *cpu, uint8_t value);

uint8_t get_carry(NesCPU *cpu);
uint8_t get_zero(NesCPU *cpu);
uint8_t get_interrupt(NesCPU *cpu);
uint8_t get_decimal(NesCPU *cpu);
uint8_t get_overflow(NesCPU *cpu);
uint8_t get_negative(NesCPU *cpu);



#endif