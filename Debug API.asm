
; debug functions for Real Mode


HexToStr_dword:

; eax = number
; edi = offset

; "000F1234"

; calculate figure 8
xor edx,edx
mov ebx,010000000h
div ebx

xchg eax,edx
call Store_Number

; calculate figure 7
xor edx,edx
mov ebx,01000000h
div ebx

xchg eax,edx
call Store_Number

; calculate figure 6
xor edx,edx
mov ebx,0100000h
div ebx

xchg eax,edx
call Store_Number

; calculate figure 5
xor edx,edx
mov ebx,010000h
div ebx

xchg eax,edx
call Store_Number

; calculate figure 4
xor edx,edx
mov ebx,01000h
div ebx

xchg eax,edx
call Store_Number

; calculate figure 3
xor edx,edx
mov ebx,0100h
div ebx

xchg eax,edx
call Store_Number

; calculate figure 2 and 1 (and 0)
xor edx,edx
mov ebx,010h
div ebx

xchg eax,edx
call Store_Number

xchg eax,edx
call Store_Number

; terminate the string
;mov [edi],byte 0

ret



Store_Number:

; dl = Number (0-15) to store to edi
cmp dl,0Ah
jnc Store_Number_h

add dl,30h
mov [cs:di],dl
inc di
ret

Store_Number_h:
add dl,(41h-10)
mov [cs:di],dl
inc di
ret



Print_Text:

; prints the text given in si

mov bx,0007h                                    ; Page Number = 0, Attribute = 07h
;xor bx,bx
mov ah,0Eh                                      ; Function 0Eh: Teletype Output

cs lodsb                                        ; load the first character

Next_Char:
int 10h
cs lodsb                                        ; al = next character
or al,al                                        ; last letter?
jnz Next_Char                                   ; if not print next letter

ret
