#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include "nes_rom.h"

NesROM* readROM(char *filename) {
	FILE *file;
	file = fopen(filename, "rb");
	if(file == NULL) {
		printf("nes_rom: Error while opening file \"%s\"\n", filename);
		return NULL;
	}
	NesROM *rom = calloc(1, sizeof(NesROM));
	
	rom->prg_rom = NULL;
	rom->chr_rom = NULL;
	rom->inst_rom = NULL;
	rom->prom = NULL;
	rom->trainer = NULL;
	
	//read rom header
	if(fread(rom->header, sizeof(uint8_t), 16, file) < 16) {
		printf("nes_rom: Couldn't read the rom header\n");
		fclose(file);
		freeROM(rom);
		return NULL;
	}
	
	//check if the file is a NES rom
	if(isNesROM(rom) == 0) {
		printf("nes_rom: File is not a NES rom\n");
		fclose(file);
		freeROM(rom);
		return NULL;
	}
	
	//check if the rom has a 512-byte trainer (whatever that is)
	if(hasTrainer(rom)) {
		//calloc and fread it to memory
		rom->trainer = calloc(0x200, sizeof(uint8_t));
		if(fread(rom->trainer, sizeof(uint8_t), 0x200, file) < 0x200) {
			printf("nes_rom: Error while reading trainer\n");
			fclose(file);
			freeROM(rom);
		}
	}
	
	//allocate memory for the roms and read them from file
	rom->prg_rom = calloc(prg_rom_size(rom), sizeof(uint8_t));
	if(fread(rom->prg_rom, sizeof(uint8_t), prg_rom_size(rom), file) < prg_rom_size(rom)) {
		printf("nes_rom: Error while reading PRG ROM\n");
		fclose(file);
		freeROM(rom);
		return NULL;
	}
	
	if(chr_rom_size(rom) != 0) {
		rom->chr_rom = calloc(chr_rom_size(rom), sizeof(uint8_t));
		if(fread(rom->chr_rom, sizeof(uint8_t), chr_rom_size(rom), file) < chr_rom_size(rom)) {
			printf("nes_rom: Error while reading CHR ROM\n");
			fclose(file);
			freeROM(rom);
			return NULL;
		}
	}
	
	//TODO: PlayChoice ROM reading
	
	printf("nes_rom: File \"%s\" read\n", filename);
	fclose(file);
	return rom;
}

void freeROM(NesROM *rom) {
	if(rom != NULL) {
		if(rom->prg_rom != NULL) {
			free(rom->prg_rom);
		}
		if(rom->chr_rom != NULL) {
			free(rom->chr_rom);
		}
		if(rom->inst_rom != NULL) {
			free(rom->inst_rom);
		}
		if(rom->prom != NULL) {
			free(rom->prom);
		}
		if(rom->trainer != NULL) {
			free(rom->trainer);
		}
	}
	
	free(rom);
}

int isNesROM(NesROM *rom) {
	uint8_t *h = rom->header;
	if(h[0] == 0x4e && h[1] == 0x45 && h[2] == 0x53 && h[3] == 0x1a) {
		return 1;
	}
	
	return 0;
}

int prg_rom_size(NesROM *rom) {
	return rom->header[4] * 16384;
}

int chr_rom_size(NesROM *rom) {
	return rom->header[5] * 8192;
}

uint8_t mapper(NesROM *rom) {
	return (rom->header[6] >> 4) | (rom->header[7] & 0xf0);
}

uint8_t prg_rom_banks(NesROM *rom) {
	return rom->header[4];
}
uint8_t chr_rom_banks(NesROM *rom) {
	return rom->header[5];
}

void printInfo(NesROM *rom) {
	if(rom != NULL) {
		printf("Header: ");
		for(int n = 0; n < 16; n++) {
			printf("%.2x ", rom->header[n]);
		}
		printf("\n");
		printf("PRG ROM banks: %d\n", prg_rom_banks(rom));
		printf("PRG ROM size: %d bytes\n", prg_rom_size(rom));
		printf("CHR ROM banks: %d\n", chr_rom_banks(rom));
		printf("CHR ROM size: %d bytes\n", chr_rom_size(rom));
		printf("Mapper: %d\n", mapper(rom));
		
	}
}

int hasTrainer(NesROM *rom) {
	uint8_t *h = rom->header;
	if(h[6] & 0x04) {
		return 1;
	}
	
	return 0;
}