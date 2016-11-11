#ifndef LAKUNES_HEADER
#define LAKUNES_HEADER

#include "nes_cpu.h"
#include "nes_rom.h"
#include "nes_mem.h"

typedef struct {
	NesCPU *cpu;
	NesROM *rom;
	NesMEM *mem;
	NesPPU *ppu;
	NesAPU *apu;
} Nes;

Nes* initNes(char *filename);
void freeNes(Nes *nes);

int runNes(Nes *nes);

#endif