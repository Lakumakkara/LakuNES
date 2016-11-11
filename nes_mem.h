#ifndef NES_MEM_HEADER
#define NES_MEM_HEADER

#include <stdio.h>
#include <stdint.h>

#include "nes_rom.h"
#include "nes_ppu.h"
#include "nes_apu.h"

typedef struct {
	uint8_t ram[0x800];
	uint8_t sram[0x2000];
	uint8_t rom_lo_bank;
	uint8_t rom_hi_bank;	
	uint8_t mapper;
} NesMEM;

NesMEM* initMEM(NesROM *rom);
void freeMEM(NesMEM *mem);
uint8_t read_memory(NesMEM *mem, NesPPU *ppu, NesAPU *apu, NesROM *rom, uint16_t addr);
int write_memory(NesMEM *mem, NesPPU *ppu, NesAPU *apu, uint16_t addr, uint8_t data);

void write_OAMDMA(NesMEM *mem, NesPPU *ppu, NesAPU *apu, NesROM *rom, uint8_t val);

#endif