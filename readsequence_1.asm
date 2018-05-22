[bits 16]
CPU 386 
org 7e00h

; save all regs before execute the origin code
pushad
mov si,Stage_0_Message
call Print_Text
xor ah,ah                                       ; Function 00h: Get Keystroke
int 16h
popad

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

Stage_0_Message         db      '^____^ press any key to continue:', 0