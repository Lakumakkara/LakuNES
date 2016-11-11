#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "LakuNES.h"
#include "nes_rom.h"
#include "nes_mem.h"
#include "nes_cpu.h"

#ifdef DEBUG

#define DEBUG_CPU
#define DEBUG_PPU

#endif


Nes* initNes(char *filename) {
	Nes *nes = calloc(1, sizeof(Nes));
	nes->rom = readROM(filename);
	if(nes->rom == NULL) {
		printf("nes: error while opening rom \"%s\"\n", filename);
	}
	printInfo(nes->rom);
	nes->mem = initMEM(nes->rom);
	nes->cpu = initCPU();
	nes->ppu = initPPU();
	reset_cpu(nes->cpu, nes->mem, nes->ppu, nes->apu, nes->rom);
	
	return nes;
}

void freeNes(Nes *nes) {
	if(nes != NULL) {
		freeROM(nes->rom);
		freeMEM(nes->mem);
		freeCPU(nes->cpu);
		freePPU(nes->ppu);
		free(nes);
	}
}

int runNes(Nes *nes) {
	printf("Starting emulation\n");
	int cpu_cycles;
	int nmi_trigger;
	while(1) {
		cpu_cycles = run_cpu(nes->cpu, nes->mem, nes->ppu, nes->apu, nes->rom);
		nes->cpu->total_cycles += cpu_cycles;
		nmi_trigger = run_ppu(nes->ppu, nes->rom, cpu_cycles);
		if(nmi_trigger == -2) {
			printf("NMI!\n");
			do_nmi(nes->cpu, nes->mem, nes->ppu, nes->apu, nes->rom);
		}
	}
}

int main(int argc, char *argv[]) {
	//printf("LakuNES 1\n");
	
	if(argc > 1) {
		Nes *nes;
		nes = initNes(argv[1]);
		runNes(nes);
	}
	return 0;
}