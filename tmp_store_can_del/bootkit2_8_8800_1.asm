; ******************************************************

;       Name: Textmode Text User Interface
;       Author: Peter Kleissner
;       Version: 0.1
;       Date: 17.02.2008 17:11:31
;       last Update: 17.02.2008 17:11:33

;       Forensic Lockdown Software
;       (C) 2008 Vienna Computer Products

;       This module provides functions for the text mode.

; ******************************************************

[bits 16]                                       ; create a 16 Bit Code
CPU 386                                         ; Assemble instructions up to the 386 instruction set

%include "Module Functions.asm"
%include "Kernel Storage.asm"

org Textmode_TUI


; export functions

jmp word Clear_Textmode_Screen
jmp word Scroll_Text_down
jmp word Scroll_Text_up
jmp word Set_Textmode_Mode
jmp word Display_Text_Attribute_Position
jmp word Screen_Textmode_Window
jmp word Screen_Character_Attribute_Position
jmp word Screen_Character_Multiple_Attribute_Position
jmp word Screen_Animation_1
jmp word Screen_Animation_2
jmp word Debug_Message
jmp word Debug_Message_Append
jmp word Hide_Cursor
jmp word Enable_Extended_Colors






Clear_Textmode_Screen:

; clears the whole screen

pushad
push es

mov ecx,[Textmode_Resolution_X]
imul ecx,[Textmode_Resolution_Y]

; set es:di to the Textmode Buffer location
mov edi,Textmode_Buffer
ror edi,4
mov es,di
shr edi,28

xor eax,eax

rep stosw

; reset positions
mov [Screen_PosX],dword 0
mov [Screen_PosY],dword 3

pop es
popad

ret






Scroll_Text_down:

; Text goes up, User scrolls down

pushad

xor eax,eax
mov edi,Textmode_Buffer
mov esi,[Textmode_Resolution_X]
shl esi,1
add esi,Textmode_Buffer

mov ecx,[Textmode_Resolution_Y]
dec ecx
imul ecx,[Textmode_Resolution_X]
rep movsw
mov ecx,[Textmode_Resolution_Y]
rep stosw

popad

ret






Scroll_Text_up:

; Text goes down, User scrolls up

pushad

xor eax,eax
mov esi,Textmode_Buffer
mov edi,[Textmode_Resolution_X]
shl edi,1
add edi,Textmode_Buffer

mov ecx,[Textmode_Resolution_Y]
dec ecx
imul ecx,[Textmode_Resolution_X]
rep movsw

mov edi,Textmode_Buffer
mov ecx,[Textmode_Resolution_Y]
rep stosw

popad

ret






Set_Textmode_Mode:

; sets the Text mode

; Input:
;        Text mode Number................eax
;        Resolution X [requested]........ebx
;        Resolution Y [requested]........ecx

; Output:
;        if the requested resolution is not equal to the resolution resloved by the card, the function aborts (eax != 0)
;        if successful, no output is given and eax = 0

; Standard VGA Textmodes:

;   00h     40x25       01h     40x25
;   02h     80x25       03h     80x50
;   07h     80x25       08h     132x25

pushad


; INT 10 - VIDEO - SET VIDEO MODE
; 	  AH = 00h
; 	  AL = desired video mode (see #00010)
; Return: AL = video mode flag (Phoenix, AMI BIOS)
; 	    20h mode > 7
; 	    30h modes 0-5 and 7
; 	    3Fh mode 6
; Desc:	specify the display mode for the currently active display adapter


; check resolutions of Text Mode [for FUTURE!]

; remark set mode
mov [Textmode_Mode],dword eax

; default Textmode is 3
;   this Textmode seems to be equal on all emulators and graphic cards (others may vary)

xor ebx,ebx
xor ecx,ecx
xor edx,edx

int 10h


popad

xor eax,eax

ret






Display_Text_Attribute_Position:

; API Display_Text_Attribute_Position, Text, Attribute, PositionX, PositionY

enter 0, 0
pushad
mpush ds, es


; insert Positions
mov eax,[Param3]
mov [cs:Screen_PosX],eax
mov ebx,[Param4]
mov [cs:Screen_PosY],ebx

; set the Source Text pointer
mov esi,[Param1]
ror esi,4
mov ds,si
shr esi,32-4

; insert Attribute if not already set
cmp [Param2],dword 0
jne Next_Char
mov [Param2],dword 07h


Next_Char:
; next character, check positions
mov eax,[cs:Textmode_Resolution_X]
cmp [cs:Screen_PosX],dword eax
jl Position_x_ok

mov [cs:Screen_PosX],dword 00h
inc dword [cs:Screen_PosY]

Position_x_ok:
mov eax,[cs:Textmode_Resolution_Y]
cmp [cs:Screen_PosY],dword eax
jl Position_y_ok

call API_Scroll_Text_up
mov eax,[cs:Textmode_Resolution_Y]
dec eax
mov [cs:Screen_PosY],dword eax

Position_y_ok:
; load and write the character
lodsb

or al,al
jz Display_Text_Attribute_Position_Exit

cmp al,9
jne Char_no_9

add [cs:Screen_PosX],dword 4
jmp Next_Char

Char_no_9:
cmp al,10
jne Char_no_10

inc dword [cs:Screen_PosY]
jmp Next_Char

Char_no_10:
cmp al,13
jne Char_no_13

; (at this time, use the parameter Position X for windows-display text)
;mov [cs:Screen_PosX],dword 00h
mov eax,[Param3]
mov [cs:Screen_PosX],dword eax
jmp Next_Char

Char_no_13:
push ax

; calculate the y-position
mov eax,[cs:Screen_PosY]
mov edx,[cs:Textmode_Resolution_X]
shl edx,1
mul edx

; calculate the x-position
mov ebx,[cs:Screen_PosX]
shl ebx,1
add eax,ebx

; set the pointer to the correct letter in the Textmode Buffer
mov edi,eax
add edi,Textmode_Buffer
ror edi,4
mov es,di
shr edi,28

; write the letter
pop ax
mov ah,byte [Param2]
stosw

; next character
inc dword [cs:Screen_PosX]
jmp Next_Char


Display_Text_Attribute_Position_Exit:

mpop ds, es
popad
leave

ret 4*4






Screen_Textmode_Window:

; API Screen_Textmode_Window, PositionX, PositionY, Width, Height, Attribute

; warning: There are no position/width/height validation checks performed!
; This function sets the Attribute of the set windows (positions, size).

; enter the function
enter 0, 0
pushad
mpush ds, es


; set the Text pointer (= Position X + Position Y * Resolution X + Text Mode Buffer)
mov edi,[Param1]
shl edi,1

mov eax,[Param2]
mov edx,[cs:Textmode_Resolution_X]
shl edx,1
mul edx
add eax,edx

add edi,eax
add edi,Textmode_Buffer

ror edi,4
mov es,di
shr edi,32-4


; outer loop: Height Counter
mov ecx,[Param4]

Screen_Textmode_Window_Height:
push ecx

; inner loop: draw one line
mov ecx,[Param3]
mov al,[Param5]

Screen_Textmode_Window_xor:
inc di
;and [es:di],byte 0Fh
;or [es:di],al
;inc di
stosb
loop Screen_Textmode_Window_xor

; update the Text Pointer (+= Resolution X - Width)
mov edx,[cs:Textmode_Resolution_X]
sub edx,[Param3]
shl edx,1
add di,dx


pop ecx

loop Screen_Textmode_Window_Height


; exit the function
mpop ds, es
popad
leave

ret






Screen_Character_Attribute_Position:

; API API_Screen_Character_Attribute_Position, Character, Attribute, PositionX, PositionY

; no position check is done!

enter 0, 0
pushad
push es


; calculate the y-position
mov eax,[Param4]
mov edx,[cs:Textmode_Resolution_X]
shl edx,1
mul edx

; calculate the x-position
mov ebx,[Param3]
shl ebx,1
add eax,ebx

; set the pointer to the correct letter in the Textmode Buffer
mov edi,eax
add edi,Textmode_Buffer
ror edi,4
mov es,di
shr edi,28

; write the character
mov al,byte [Param1]
mov ah,byte [Param2]
stosw


pop es
popad
leave

ret






Screen_Character_Multiple_Attribute_Position:

; API Screen_Character_Multiple_Attribute_Position, Character, Attribute, Count, PositionX, PositionY

; no position check is done!

enter 0, 0
pushad
push es


; calculate the y-position
mov eax,[Param5]
mov edx,[cs:Textmode_Resolution_X]
shl edx,1
mul edx

; calculate the x-position
mov ebx,[Param4]
shl ebx,1
add eax,ebx

; set the pointer to the correct letter in the Textmode Buffer
mov edi,eax
add edi,Textmode_Buffer
ror edi,4
mov es,di
shr edi,28

; write the character
mov al,byte [Param1]
mov ah,byte [Param2]
mov ecx,[Param3]

rep stosw


pop es
popad
leave

ret






Screen_Animation_1:

; API Screen_Animation_1

; enter the function
enter 0, 0
pushad
push es


; Count = Resolution X * Resolution Y
mov ecx,[Textmode_Resolution_X]
imul ecx,[Textmode_Resolution_Y]

; set es:di to the Textmode Buffer location
mov edi,Textmode_Buffer
ror edi,4
mov es,di
shr edi,28


Screen_Animation_1_loop:
inc di
mov al,byte [es:di]
mov ah,al

; subtracts from every character attribute 11h (makes it darker)

and ax,0F00Fh
or al,al
jz Screen_Animation_1_loop_background

sub [es:di],byte 01h

Screen_Animation_1_loop_background:

or ah,ah
jz Screen_Animation_1_looped

sub [es:di],byte 10h

Screen_Animation_1_looped:
inc di
loop Screen_Animation_1_loop


; leave it
pop es
popad
leave

ret






Screen_Animation_2:

; API Screen_Animation_2

; enter the function
enter 0, 0
pushad
push es


; Count = Resolution X * Resolution Y
mov ecx,[Textmode_Resolution_X]
imul ecx,[Textmode_Resolution_Y]

; set es:di to the Textmode Buffer location
mov edi,Textmode_Buffer
ror edi,4
mov es,di
shr edi,28


Screen_Animation_2_loop:
inc di
mov al,byte [es:di]
mov ah,al

; adds to every character attribute 11h (makes it more white)

and ax,0F00Fh
cmp al,0Fh
je Screen_Animation_2_loop_background

add [es:di],byte 01h

Screen_Animation_2_loop_background:

cmp ah,0F0h
je Screen_Animation_2_looped

add [es:di],byte 10h

Screen_Animation_2_looped:
inc di
loop Screen_Animation_2_loop


; leave it
pop es
popad
leave

ret






Debug_Message:

; API Debug_Message, Message

; outputs fast a debug message given in eax

; invoke text output function
push dword [Screen_PosY]        ; Y-Position
push dword 6                    ; X-Position
push dword 0Fh                  ; Attribute
push dword eax
call API_Display_Text_Attribute_Position

; next output -> next line
inc dword [Screen_PosY]

ret






Debug_Message_Append:

; API Debug_Message_Append, Message

; completely appends a debug message to the previous one

; (append to the last line)
dec dword [Screen_PosY]

; invoke text output function
push dword [Screen_PosY]        ; Y-Position
push dword [Screen_PosX]        ; X-Position
push dword 0Fh                  ; Attribute
push dword eax
call API_Display_Text_Attribute_Position

; next output -> next line
inc dword [Screen_PosY]

ret






Hide_Cursor:

; API Hide_Cursor

; hide the Cursor
mov ah,01h
mov ch,00100000b
mov cl,0

int 10h

ret






Enable_Extended_Colors:

; enable background colors, disable auto-blink
mov ax,1003h    ; INT 10 - VIDEO - TOGGLE INTENSITY/BLINKING BIT
mov bl,0        ; 00h background intensity enabled
mov bh,0000h    ; according to documentation, to avoid problems

int 10h

ret












; Standard VESA Textmodes:

;   108h     80x60
;   109h     132x25
;   10Ah     132x43
;   10Bh     132x50
;   10Ch     132x60

; 40x25, 80x25, 80x43, 80x50

; Standard VGA Textmodes:

;   00h      40x25
;   01h      40x25
;   02h      80x25
;   03h      80x50
;   07h      80x25
;   08h      132x25





Screen_PosX     dd      00h
Screen_PosY     dd      3       ;00h    (set to 3 for debug message output)

Textmode_Resolution_X   dd      80
Textmode_Resolution_Y   dd      25

Textmode_Mode           dd      2

Textmode_Frame_Buffer   dd      Textmode_Buffer


