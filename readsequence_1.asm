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
xor ah,ah                                       ; Function 00h: Get Keystroke
int 16h
popad

execute_origin_mbr:
; this will execute the origin mbr 
jmp 0000h:7c00h

; prints the text given in si
Print_Text:

mov bx,0007h                                    ; Page Number = 0, Attribute = 07h
mov ah,0Eh                                      ; Function 0Eh: Teletype Output

cs lodsb                                        ; load the first character

Next_Char:
int 10h
cs lodsb                                        ; al = next character
or al,al                                        ; last letter?
jnz Next_Char                                   ; if not print next letter
ret

Stage_0_Message                 db      '^____^  press any key to continue:', 0
Interrupt_13h_Message          db      'Interrupt_13h call:', 0

Forward_Interrupt_13h_Original:
; jump to the original interrupt 13h handler (segment:offset will be patched dynamically)
jmp word 0000h:0000h

; now our background "service" starts, we get control only by int 13
; the code is now located at the end of memory (most likely 9F400h)
;direct declare string for print will crash because this code wille be relocate.
Interrupt_13h_Hook:
    pushad
    pushf
    mov si,Interrupt_13h_Message
    call Print_Text
    popf
    popad
    jmp Forward_Interrupt_13h_Original
Total_End_of_Binary:
times 4*1024-($-$$) db 0