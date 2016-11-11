#ifndef NES_PPU_HEADER
#define NES_PPU_HEADER

#include <stdint.h>

#include "nes_rom.h"

typedef enum {
	H_MIRRORING,
	V_MIRRORING,
	FOUR_SCREEN
} Mirroring;

typedef struct {
	uint8_t sprite_overflow;
	uint8_t sprite0_hit;
	uint8_t vblank_started;
} PPUSTATUS;

typedef struct {
	uint16_t nt_base_addr;
	uint8_t vram_addr_inc;
	uint16_t sprite_pt_addr;
	uint16_t bg_pt_addr;
	uint8_t sprite_size;
	uint8_t ppu_master;
	uint8_t nmi_on;	
} PPUCTRL;

typedef struct {
	uint8_t greyscale;
	uint8_t show_sprites_l8;
	uint8_t show_bg_l8;
	uint8_t show_sprites;
	uint8_t show_bg;
	uint8_t boost_red;
	uint8_t boost_green;
	uint8_t boost_blue;	
} PPUMASK;

typedef struct {
	uint8_t x;
	uint8_t y;
} PPUSCROLL;

typedef struct {
	uint8_t ppu_ram[0x4000];
	uint8_t oam_ram[0x100];
	uint8_t palette_ram[0x20];
	PPUSTATUS ppu_status;
	PPUCTRL ppu_ctrl;
	PPUMASK ppu_mask;
	PPUSCROLL ppu_scroll;
	Mirroring mirroring;
	
	uint8_t oam_addr;
	uint16_t ppu_addr;	
	uint8_t ppu_latch_writes;
	uint8_t ppu_data_buffer;
	
	uint8_t current_scanline;
	uint8_t current_pixel;
	uint16_t ppu_cycles;
} NesPPU;

void write_PPUCTRL(NesPPU *ppu, uint8_t val);
void write_PPUMASK(NesPPU *ppu, uint8_t val);
void write_OAMADDR(NesPPU *ppu, uint8_t val);
void write_OAMDATA(NesPPU *ppu, uint8_t val);
void write_PPUSCROLL(NesPPU *ppu, uint8_t val);
void write_PPUADDR(NesPPU *ppu, uint8_t val);
void write_PPUDATA(NesPPU *ppu, uint8_t val);


uint8_t read_PPUSTATUS(NesPPU *ppu);
uint8_t read_OAMDATA(NesPPU *ppu);
uint8_t read_PPUDATA(NesPPU *ppu);

void set_mirroring(NesPPU *ppu, NesROM *rom);

NesPPU* initPPU();
void freePPU(NesPPU *ppu);

int run_ppu(NesPPU *ppu, NesROM *rom, uint16_t cycles);

#endif