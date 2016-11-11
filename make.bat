@echo off
if -%1-==-- echo No arguments
set a=%1
set args=%1
shift
:argsloopstart
if -%1-==-- goto argsloopend
set args=%args% %1
shift
goto argsloopstart
:argsloopend

gcc -std=c99 %args% -o LakuNES.exe LakuNES.c nes_cpu.c nes_rom.c nes_mem.c nes_apu.c nes_ppu.c -g