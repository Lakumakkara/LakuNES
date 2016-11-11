#ifndef NES_ROM_HEADER
#define NES_ROM_HEADER

#include <stdint.h>

typedef struct {
	uint8_t header[16];
	uint8_t *prg_rom;
	uint8_t *chr_rom;
	uint8_t *inst_rom;
	uint8_t *prom;
	uint8_t *trainer;
} NesROM;

NesROM* readROM(char *filename);
void freeROM(NesROM *rom);
int isNesROM(NesROM *rom);
int prg_rom_size(NesROM *rom);
int chr_rom_size(NesROM *rom);
uint8_t mapper(NesROM *rom);
uint8_t prg_rom_banks(NesROM *rom);
uint8_t chr_rom_banks(NesROM *rom);
void printInfo(NesROM *rom);
int hasTrainer(NesROM *rom);

#endif