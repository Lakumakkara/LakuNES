#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#include "nes_mem.h"
#include "nes_rom.h"
#include "nes_ppu.h"
#include "nes_apu.h"

#define IN_RANGE(n, start, end) (((n) >= (start)) && ((n) <= (end)))

NesMEM* initMEM(NesROM *rom) {
	NesMEM *mem = calloc(1, sizeof(NesMEM));
	mem->mapper = mapper(rom);
	switch(mem->mapper) {
		case 0: {
			if(prg_rom_banks(rom) == 1) {
				mem->rom_lo_bank = 0;
				mem->rom_hi_bank = 0;				
			} else {
				mem->rom_lo_bank = 0;
				mem->rom_hi_bank = 1;
			}
			break;
		}
	}
	
	return mem;
}

void freeMEM(NesMEM *mem) {
	if(mem != NULL) {
		free(mem);
	}	
}

uint8_t read_memory(NesMEM *mem, NesPPU *ppu, NesAPU *apu, NesROM *rom, uint16_t addr) {
	#ifdef DEBUG_MEM
	printf("nes_mem: reading from %.4x,", addr);
	#endif
	uint8_t data = 0;
	if(IN_RANGE(addr, 0x0000, 0x1fff)) {
		//RAM
		data = mem->ram[addr % 0x800];
	} else if(IN_RANGE(addr, 0x2000, 0x3fff)) {
		//PPU registers
		addr = ((addr - 0x2000) % 8) + 0x2000;
		//data = 0xff;
		switch(addr) {
			case 0x2002: {
				//PPUSTATUS
				data = read_PPUSTATUS(ppu);
				break;
			}
			case 0x2004: {
				//OAMDATA
				data = read_OAMDATA(ppu);
				break;
			}
			case 0x2007: {
				//PPUDATA
				data = read_PPUDATA(ppu);
				break;
			}
		}
		
		//TODO: PPU stuff
	} else if(IN_RANGE(addr, 0x4000, 0x4017)) {
		//APU registers and controller registers
		
		//TODO: APU stuff
	} else if(IN_RANGE(addr, 0x4020, 0x5fff)) {
		//Expansion ROM
		
		//TODO: Expansion ROM(?)
	} else if(IN_RANGE(addr, 0x6000, 0x7fff)) {
		//SRAM
		data = mem->sram[addr - 0x6000];
	} else if(IN_RANGE(addr, 0x8000, 0xbfff)) {
		//PRG-ROM lower bank
		data = rom->prg_rom[(0x4000 * mem->rom_lo_bank) + (addr - 0x8000)];
	} else if(IN_RANGE(addr, 0xc000, 0xffff)) {
		//PRG-ROM higher bank
		data = rom->prg_rom[(0x4000 * mem->rom_hi_bank) + (addr - 0xc000)];
	}
	#ifdef DEBUG_MEM
	printf(" data %.2x\n", data);
	#endif
	
	return data;
}

int write_memory(NesMEM *mem, NesPPU *ppu, NesAPU *apu, uint16_t addr, uint8_t data) {
	#ifdef DEBUG_MEM
	printf("nes_mem: writing %.2x to %.4x\n", data, addr);
	#endif
	if(IN_RANGE(addr, 0x0000, 0x1fff)) {
		//RAM
		mem->ram[addr % 0x800] = data;
	} else if(IN_RANGE(addr, 0x2000, 0x3fff)) {
		//PPU registers
		addr = ((addr - 0x2000) % 8) + 0x2000;
		#ifdef DEBUG_MEM
		printf("ppu addr: %.4x\n", addr);
		#endif
		switch(addr) {
			case 0x2000: {
				//PPUCTRL
				write_PPUCTRL(ppu, data);
				break;
			}
			case 0x2001: {
				//PPUMASK
				write_PPUMASK(ppu, data);
				break;
			}
			case 0x2003: {
				//OAMADDR
				write_OAMADDR(ppu, data);
				break;
			}
			case 0x2004: {
				//OAMDATA
				write_OAMDATA(ppu, data);
				break;
			}
			case 0x2005: {
				//SCROLL
				write_PPUSCROLL(ppu, data);
				break;
			}
			case 0x2006: {
				//PPUADDR
				write_PPUADDR(ppu, data);
				break;
			}
			case 0x2007: {
				//PPUDATA
				write_PPUDATA(ppu, data);
				break;
			}
			
		}
		
		//TODO: PPU stuff
	} else if(IN_RANGE(addr, 0x4000, 0x4017)) {
		//APU registers and controller registers
		switch(addr) {
			case 0x4014: {
				write_OAMDMA(mem, ppu, apu, NULL, data);
				break;
			}
		}
		//TODO: APU stuff
	} else if(IN_RANGE(addr, 0x4020, 0x5fff)) {
		//Expansion ROM
		
		//TODO: Expansion ROM(?)
	} else if(IN_RANGE(addr, 0x6000, 0x7fff)) {
		//SRAM
		mem->sram[addr - 0x6000] = data;
	}
	
	return 0;
}

void write_OAMDMA(NesMEM *mem, NesPPU *ppu, NesAPU *apu, NesROM *rom, uint8_t val) {
	uint16_t addr = val * 0x100;
	for(uint16_t n = 0; n < 0x100; n++) {
		ppu->oam_ram[n] = read_memory(mem, ppu, apu, rom, addr + n);
	}
}