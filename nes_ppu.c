#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#include "nes_ppu.h"

#ifdef DEBUG

#define DEBUG_PPU

#endif

#define BIT_N(byte, n) (((byte) >> (n)) & 0x01)
#define SET_BIT_N(byte, n, val) (((byte) & (0xff ^ (0x01 << (n)))) | (((val) == 0 ? 0 : 1) << (n)))

void write_PPUCTRL(NesPPU *ppu, uint8_t val) {
	//set base nametable address
	ppu->ppu_ctrl.nt_base_addr = 0x2000;
	if(BIT_N(val, 0)) {
		ppu->ppu_ctrl.nt_base_addr += 0x400;
	}
	if(BIT_N(val, 1)) {
		ppu->ppu_ctrl.nt_base_addr += 0x800;
	}
	
	//set VRAM address increment per CPU read/write of PPUDATA
	if(BIT_N(val, 2)) {
		ppu->ppu_ctrl.vram_addr_inc = 0x10;
	} else {
		ppu->ppu_ctrl.vram_addr_inc = 0x01;
	}
	
	//set sprite pattern table address for 8x8 sprites
	if(BIT_N(val, 3)) {
		ppu->ppu_ctrl.sprite_pt_addr = 0x1000;
	} else {
		ppu->ppu_ctrl.sprite_pt_addr = 0x0000;
	}
	
	//set bg pattern table address for 8x8 sprites
	if(BIT_N(val, 4)) {
		ppu->ppu_ctrl.bg_pt_addr = 0x1000;
	} else {
		ppu->ppu_ctrl.bg_pt_addr = 0x0000;
	}
	
	//set sprite size
	if(BIT_N(val, 5)) {
		ppu->ppu_ctrl.sprite_size = 16;
	} else {
		ppu->ppu_ctrl.sprite_size = 8;
	}
	
	//set PPU master/slave
	ppu->ppu_ctrl.ppu_master = BIT_N(val, 6);
	
	//turn NMI on or off
	ppu->ppu_ctrl.nmi_on = BIT_N(val, 7);
	printf("nmi bit: %d\n", BIT_N(val, 7));
}


void write_PPUMASK(NesPPU *ppu, uint8_t val) {
	PPUMASK *mask = &(ppu->ppu_mask);
	mask->greyscale 		= BIT_N(val, 0);
	mask->show_sprites_l8 	= BIT_N(val, 1);
	mask->show_bg_l8 		= BIT_N(val, 2);
	mask->show_sprites 		= BIT_N(val, 3);
	mask->show_bg 			= BIT_N(val, 4);
	mask->boost_red			= BIT_N(val, 5);
	mask->boost_green		= BIT_N(val, 6);
	mask->boost_blue 		= BIT_N(val, 7);
}


void write_OAMADDR(NesPPU *ppu, uint8_t val) {
	ppu->oam_addr = val;
}


void write_OAMDATA(NesPPU *ppu, uint8_t val) {
	if(ppu->current_scanline < 240) {
		//ignore during rendering (?)
	} else {
		ppu->oam_ram[ppu->oam_addr] = val;
		ppu->oam_addr += 1;		
	}
}


void write_PPUSCROLL(NesPPU *ppu, uint8_t val) {
	if(ppu->ppu_latch_writes == 0) {
		ppu->ppu_scroll.x = val;
		ppu->ppu_latch_writes = 1;
	} else if(ppu->ppu_latch_writes == 1) {
		ppu->ppu_scroll.y = val;
		ppu->ppu_latch_writes = 2;
	}
}


void write_PPUADDR(NesPPU *ppu, uint8_t val) {
	if(ppu->ppu_latch_writes == 0) {
		ppu->ppu_addr = val * 0x100;
		ppu->ppu_latch_writes = 1;
	} else if(ppu->ppu_latch_writes == 1) {
		ppu->ppu_addr += val;
		ppu->ppu_latch_writes = 2;
		
		ppu->ppu_addr = ppu->ppu_addr % 0x4000;
	}
}


void write_PPUDATA(NesPPU *ppu, uint8_t val) {
	uint16_t old_addr = ppu->ppu_addr;
	//nametable mirror addresses
	if(ppu->ppu_addr > 0x2fff && ppu->ppu_addr < 0x3f00) {
		ppu->ppu_addr -= 0x1000;
	}
	
	//palette mirror addresses
	if(ppu->ppu_addr > 0x3f1f) {
		ppu->ppu_addr = ((ppu->ppu_addr - 0x3f20) % 0x20) + 0x3f00;
	}
	
	if(ppu->ppu_addr > 0x1fff && ppu->ppu_addr < 0x3000) {
		//nametables
		if(ppu->ppu_addr < 0x2400) {
			//nametable 0 0x2000
			
			ppu->ppu_ram[ppu->ppu_addr] = val;
		} else if(ppu->ppu_addr < 0x2800) {
			//nametable 1 0x2400
			switch(ppu->mirroring) {
				case H_MIRRORING: {
					ppu->ppu_ram[ppu->ppu_addr - 0x0400] = val;
					break;
				}
				case V_MIRRORING: {
					ppu->ppu_ram[ppu->ppu_addr] = val;
					break;
				}
				case FOUR_SCREEN: {
					ppu->ppu_ram[ppu->ppu_addr] = val;
					break;
				}
			}
			
		} else if(ppu->ppu_addr < 0x2c00) {
			//nametable 2 0x2800
			switch(ppu->mirroring) {
				case H_MIRRORING: {
					ppu->ppu_ram[ppu->ppu_addr] = val;
					break;
				}
				case V_MIRRORING: {
					ppu->ppu_ram[ppu->ppu_addr - 0x0800] = val;
					break;
				}
				case FOUR_SCREEN: {
					ppu->ppu_ram[ppu->ppu_addr] = val;
					break;
				}
			}
			
		} else {
			//nametable 3 0x2c00
			switch(ppu->mirroring) {
				case H_MIRRORING: {
					ppu->ppu_ram[ppu->ppu_addr - 0x0400] = val;
					break;
				}
				case V_MIRRORING: {
					ppu->ppu_ram[ppu->ppu_addr - 0x0800] = val;
					break;
				}
				case FOUR_SCREEN: {
					ppu->ppu_ram[ppu->ppu_addr] = val;
					break;
				}
			}			
		}
		
	} else if(ppu->ppu_addr >= 0x3f00) {
		//palettes
		if(ppu->ppu_addr < 0x3f10) {
			//bg palettes
			ppu->ppu_ram[ppu->ppu_addr] = val;
		} else {
			if((ppu->ppu_addr & 0xff) % 0x04 == 0) {
				//mirrors of 00, 04, 08, 0c of bg
				ppu->ppu_ram[ppu->ppu_addr - 0x10] = val;
			} else {
				//sprite palettes
				ppu->ppu_ram[ppu->ppu_addr] = val;
			}
		}
	}
	
	
	ppu->ppu_addr = old_addr + ppu->ppu_ctrl.vram_addr_inc;
}

uint8_t read_PPUSTATUS(NesPPU *ppu) {
	PPUSTATUS *status = &(ppu->ppu_status);
	uint8_t ret = 0x00;
	//set the returned bits
	ret = SET_BIT_N(ret, 5, status->sprite_overflow);
	ret = SET_BIT_N(ret, 6, status->sprite0_hit);
	ret = SET_BIT_N(ret, 7, status->vblank_started);
	
	//clear the vblank bits
	status->vblank_started = 0;
	
	//reset latch used by PPUSCROLL and PPUADDR
	ppu->ppu_latch_writes = 0;
	
	return ret;
}


uint8_t read_OAMDATA(NesPPU *ppu) {
	return ppu->oam_ram[ppu->oam_addr];
}


uint8_t read_PPUDATA(NesPPU *ppu) {
	uint8_t ret;
	if(ppu->ppu_addr < 0x3f00) {
		ret = ppu->ppu_data_buffer;
		ppu->ppu_data_buffer = ppu->ppu_ram[ppu->ppu_addr];
		ppu->ppu_addr += ppu->ppu_ctrl.vram_addr_inc;
		return ret;
	} else {
		ret = ppu->palette_ram[ppu->ppu_addr & 0x1f];
		ppu->ppu_addr += ppu->ppu_ctrl.vram_addr_inc;
		return ret;
	}
}

void set_mirroring(NesPPU *ppu, NesROM *rom) {
	uint8_t *h = rom->header;
	if(BIT_N(h[6], 3)) {
		ppu->mirroring = FOUR_SCREEN;
	} else {
		if(BIT_N(h[6], 0)) {
			ppu->mirroring = H_MIRRORING;
		} else {
			ppu->mirroring = V_MIRRORING;
		}
	}
}

NesPPU* initPPU() {
	NesPPU *ppu = calloc(1, sizeof(NesPPU));
	
	return ppu;
}

void freePPU(NesPPU *ppu) {
	if(ppu != NULL) {
		free(ppu);
	}
}

int run_ppu(NesPPU *ppu, NesROM *rom, uint16_t cycles) {
	int ret = 0;
	ppu->ppu_cycles += (3 * cycles);
	while(ppu->ppu_cycles >= 341) {
		ppu->current_scanline += 1;
		ppu->ppu_cycles -= 341;
	}
	
	if(ppu->current_scanline == 241) {
		#ifdef DEBUG_PPU
		printf("vblank start\n");
		#endif
		ppu->ppu_status.vblank_started = 1;
		if(ppu->ppu_ctrl.nmi_on) {			
			ret = -2;
		}
	}
	
	
	if(ppu->current_scanline == 263) {
		ppu->current_scanline = 0;
	}
	
	#ifdef DEBUG_PPU
	printf("PPU cycle %.3d, scanline %.3d\n", ppu->ppu_cycles, ppu->current_scanline);
	#endif
	return ret;
}

