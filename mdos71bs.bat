@echo off
rem make FAT32 DOS 7.1 boot sectors for MBR and GPT
jwasm -bin -nologo -Fl=dos71bsm.lst -Fo=dos71bsm.bin             dos71bs.asm
jwasm -bin -nologo -Fl=dos71bsg.lst -Fo=dos71bsg.bin -DMAKEGPTBS dos71bs.asm
