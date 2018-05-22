
; CDFS Functions
;   Load_File_CDFS (load/open)
;   Read_File_CDFS




Load_File_CDFS:

; loads a file from the current volume (CDFS file system)

; Source
;       Drive Context..........................[Drive_Context]
;       File Name..............................[Param1]
;       Buffer.................................[Param2]

; parse through the Path Table
mov ecx,[Drive_Context + 0Fh]                                                   ; Path Table Size
mov [Path_Table_Size],ecx
mov ebx,[Drive_Context + 0Bh]                                                   ; Location of Occurrence of Type L Path Table
mov [Path_Table_Sector],ebx

; verify the size and location of the path table
or ecx,ecx                                                                      ; verify the path table size (not zero)
jz Load_File_CDFS_Error_Invalid_Path_Table
or ebx,ebx                                                                      ; verify the path table location (not zero)
jz Load_File_CDFS_Error_Invalid_Path_Table

; initialize name pointer and current index
movzx edx,word [Param1]                                                         ; File Name           (C:\->File Path\File Name)
dec dx                                                                          ; move to first slash (C:->\File Path\File Name)
mov [edx],byte '/'                                                              ; normalize the slash for later faster identification
mov [Name_Buffer_Position],dx                                                   ; -> name buffer pointer
mov [Current_Index],dword 1                                                     ; always starts with 1



Parse_Path_Table:

; read one sector of the path table
Read  [Path_Table_Sector], 1, Load_File_CDFS_Exit
inc dword [Path_Table_Sector]                                                   ; read one sector of the path table
mov si,Sector_Buffer                                                            ; si = pointer to path table
mov [Position],word Sector_Buffer                                               ; set the initial position


Parse_Path_Table_Entry:

; [Position] points to the absolute position of the current path table entry

; check the length of the next entry
movzx eax,byte [si]                                                             ; Length of Directory Identifier (LEN_DI) of the entry
add ax,8                                                                        ;  + size of entry
sub [Path_Table_Size],eax                                                       ; subtract the handled entry
jb Load_File_CDFS_Error_Invalid_Path_Table                                      ; entry size > path table size?

; reading over sector boundaries?
add ax,si                                                                       ; ax = current position in path table + size of entry + size of identifier
sub ax,Sector_Buffer                                                            ; get only the offset
cmp ax,[Drive_Context + 04h]                                                    ; offset after entry > sector size?
jbe Parse_Path_Table_Entry_Interpret                                            ; if not just interpret

; otherwise read an additional sector
%define CDFS_Sector_2                                                           ; (tell the read macro to read at the next sector offset in the buffer)
Read  [Path_Table_Sector], 1, Load_File_CDFS_Exit
%undef CDFS_Sector_2
inc dword [Path_Table_Sector]                                                   ; read one sector of the path table
mov si,[Position]                                                               ; restore the position

Parse_Path_Table_Entry_Interpret:                                               ; now check if its the searched entry

; root directory?
mov di,[Name_Buffer_Position]                                                   ; pointer to file name
cmp [di],byte '/'
jne CDFS_Not_Root_Directory

; current entry = root directory? (should be always the first of the path table)
cmp [si + 0],byte 1                                                             ; Length of Directory Identifier (LEN_DI) = 1?
jne Parse_Path_Table_Entry_Next
cmp [si + 8],byte 0                                                             ; Directory Identifier = 0?
jne Parse_Path_Table_Entry_Next

inc word [Name_Buffer_Position]                                                 ; skip the slash

; check if a subdirectory is requested in the path
call Check_CDFS_Subdirectory_Request
jnc Load_File_CDFS_Found_Directory

; set the top index to the current found one
mov eax,[Current_Index]
mov [Top_Index],eax
jmp Parse_Path_Table_Entry_Next

CDFS_Not_Root_Directory:                                                        ; so check for any directory

; parent directory number matching? if not the searched directory is not within the same directory
mov ax,word [Top_Index]
cmp word [si + 6],ax                                                            ; compare with Parent Directory Number
jne Parse_Path_Table_Entry_Next

; it's in the same directory! compare the names
call Compare_Path_Table_Entry_Name
jc Parse_Path_Table_Entry_Next                                                  ; if not matching, next entry

; any other directory requested?
call Check_CDFS_Subdirectory_Request
jnc Load_File_CDFS_Found_Directory

; otherwise find the next sub directory
;jmp Parse_Path_Table_Entry_Next


Parse_Path_Table_Entry_Next:                                                    ; go to the next entry

cmp [Path_Table_Size],dword 0                                                   ; remaining size of path table = 0?
je Load_File_CDFS_Error_Not_Found                                               ; (no more directories available)

; seek to the next entry
movzx ax,byte [si]                                                              ; Length of Directory Identifier (LEN_DI)
add ax,8                                                                        ; + size of entry
inc ax                                                                          ; round up to 2
and ax,-2                                                                       ; (mask out bit 1 if set)
add [Position],ax                                                               ; update the position variable

; validate the position
mov bx,si                                                                       ; absolute position - base buffer = offset
sub bx,Sector_Buffer

; position is at the end of the sector (then there must be a new sector read)?
cmp bx,[Drive_Context + 04h]                                                    ; offset = bytes per sector?
je CDFS_Entry_Position_Read_Sector

; position in the sector sector?
cmp bx,[Drive_Context + 04h]                                                    ; offset > sector size?
jb CDFS_Entry_Position_OK

; relocate one sector (upper to lower in the sector buffer)
mov cx,[Drive_Context + 04h]                                                    ; copy exactly 1 sector
mov si,Sector_Buffer                                                            ; source = sector buffer + sector size
add si,[Drive_Context + 04h]
mov di,Sector_Buffer                                                            ; destination = sector buffer
shr cx,1                                                                        ; do a faster word operation
rep movsw
jmp CDFS_Entry_Position_OK

CDFS_Entry_Position_Read_Sector:

; read another sector of the path table
Read  [Path_Table_Sector], 1, Load_File_CDFS_Exit
inc dword [Path_Table_Sector]                                                   ; read one sector of the path table
mov [Position],si                                                               ; at the start of the sector buffer

CDFS_Entry_Position_OK:

inc dword [Current_Index]
mov si,[Position]
jmp Parse_Path_Table_Entry




Load_File_CDFS_Found_Directory:                                                 ; now only the file has to be found

; start with parsing the directory
mov eax,dword [si + 2]                                                          ; get the extent number (starting directory sector number)
mov dword [Current_Sector],eax
movzx eax,word [Drive_Context + 04h]
mov dword [Directory_Record_Size],eax                                           ; initially 1 sector to handle


Parse_Directory:

; read one sector of the directory
Read  [Current_Sector], 1, Load_File_CDFS_Exit
inc dword [Current_Sector]                                                      ; read one sector of the directory
mov si,Sector_Buffer                                                            ; si = pointer to path table
mov [Position],word Sector_Buffer                                               ; set the initial position


Parse_Directory_Record:

; validate the entry (directory size is normally rounded up to the sector size)
cmp byte [si],byte 0
je Load_File_CDFS_Error_Not_Found

; check the length of the next entry
movzx eax,byte [si]                                                             ; Length of Directory Record (LEN-DR)
sub [Directory_Record_Size],eax                                                 ; subtract the handled entry
jb Load_File_CDFS_Error_Invalid_Directory_Record                                ; directory record size > directory size?

; reading over sector boundaries?
add ax,si                                                                       ; ax = current position in path table + size of entry + size of identifier
sub ax,Sector_Buffer                                                            ; get only the offset
cmp ax,[Drive_Context + 04h]                                                    ; offset after entry > sector size?
jbe Parse_Directory_Record_Name                                                 ; if not just interpret

; otherwise read an additional sector
%define CDFS_Sector_2                                                           ; (tell the read macro to read at the next sector offset in the buffer)
Read  [Current_Sector], 1, Load_File_CDFS_Exit
%undef CDFS_Sector_2
inc dword [Current_Sector]                                                      ; read one sector of the directory
mov si,[Position]                                                               ; restore the position

Parse_Directory_Record_Name:                                                    ; now check the name

; dot-entry?
cmp [si + 32],byte 1                                                            ; Length of File Identifier (LEN_FI) = 1?
jne Parse_Directory_Record_Verify_Name
cmp [si + 33],byte 0                                                            ; Directory Identifier = 0?
jne Parse_Directory_Record_Verify_Name

; if yes take the size-of-directory value (Data Length)
mov eax,[si + 10]                                                               ; Data Length
movzx ebx,byte [si + 0]                                                         ; Length of Directory Record (LEN-DR)
sub eax,ebx                                                                     ; (subtract the already parsed dot-entry)
mov dword [Directory_Record_Size],eax
jmp Parse_Directory_Record_Next


Parse_Directory_Record_Verify_Name:                                             ; check if the file is found

call Compare_Directory_Record_Name
jc Parse_Directory_Record_Next                                                  ; if not matching, next entry

; only open the file?
cmp [Handle_Context + 16],dword 'open'
je Open_File_CDFS_Found

; read the file!

; set the sector number
mov eax,dword [si + 2]                                                          ; Location of Extent
mov [Disk_Address_Packet_LBA_Low],eax                                           ; (Sector Number)

; set the count
movzx eax,word [Drive_Context + 04h]                                            ; bytes per sector
dec eax                                                                         ; -1 (will be rounded up to bytes per sector)
add eax,dword [si + 10]                                                         ; Data Length
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
jc Load_File_CDFS_Exit

; return successful
clc
xor eax,eax
jmp Load_File_CDFS_Exit


Parse_Directory_Record_Next:                                                    ; next entry!
cmp [Directory_Record_Size],dword 0                                             ; remaining size of the directory = 0?
je Load_File_CDFS_Error_Not_Found                                               ; (no more files available)

; seek to the next entry
movzx ax,byte [si]                                                              ; Length of Directory Record (LEN-DR)
inc ax                                                                          ; round up to 2
and ax,-2                                                                       ; (mask out bit 1 if set)
add [Position],ax                                                               ; update the position variable

; validate the position
mov bx,si                                                                       ; absolute position - base buffer = offset
sub bx,Sector_Buffer

; position is at the end of the sector (then there must be a new sector read)?
cmp bx,[Drive_Context + 04h]                                                    ; offset = bytes per sector?
je CDFS_Directory_Record_Position_Read_Sector

; position in the sector sector?
cmp bx,[Drive_Context + 04h]                                                    ; offset > sector size?
jb CDFS_Directory_Record_Position_OK

; relocate one sector (upper to lower in the sector buffer)
mov cx,[Drive_Context + 04h]                                                    ; copy exactly 1 sector
mov si,Sector_Buffer                                                            ; source = sector buffer + sector size
add si,[Drive_Context + 04h]
mov di,Sector_Buffer                                                            ; destination = sector buffer
shr cx,1                                                                        ; do a faster word operation
rep movsw
jmp CDFS_Directory_Record_Position_OK

CDFS_Directory_Record_Position_Read_Sector:

; read another sector of the directory record
Read  [Current_Sector], 1, Load_File_CDFS_Exit
inc dword [Current_Sector]                                                      ; read one sector of the directory
mov [Position],si                                                               ; at the start of the sector buffer

CDFS_Directory_Record_Position_OK:

inc dword [Current_Index]
mov si,[Position]
jmp Parse_Directory_Record




Load_File_CDFS_Exit:                                                            ; exit with CF = 0 successful, CF = 1 error

ret

Load_File_CDFS_Error_Invalid_Path_Table:                                        ; (invalid CDFS file system)
mov eax,CDFS_Invalid_Path_Table
stc

ret

Load_File_CDFS_Error_Not_Found:                                                 ; file not found
mov eax,Invalid_Name
stc

ret

Load_File_CDFS_Error_Invalid_Directory_Record:                                  ; (invalid CDFS file system)
mov eax,CDFS_Invalid_Directory_Record
stc

ret






Compare_Path_Table_Entry_Name:

; checks the path table entry with the current path
;   [Position]                pointer to path table entry
;   [Name_Buffer_Position]    path (will be updated if matching)
;   [Top_Index]               Path table index of upper directory (will be updated if matching)

; return
;   CF = 0                    matching, path and index updated
;   CF = 1                    not matching

push si

movzx cx,byte [si]                                                              ; Length of Directory Identifier (LEN_DI)
add si,8                                                                        ; seek to the file name
mov di,[Name_Buffer_Position]                                                   ; compare against the path


Name_Path_Table_Check_loop:

; lowercase both characters, because the NTFS filesystem is case insensitive lol
char_lowercase [si]
char_lowercase [di]

; compare the characters
cmpsb
jne Names_Path_Table_Unequal

; they are equal, check next character (note ALL characters must match exactly)
loop Name_Path_Table_Check_loop

; cx = zero, all matching, next character must be a slash
cmp byte [di],byte '\'
je Names_Path_Table_Equal
cmp byte [di],byte '/'
jne Names_Path_Table_Unequal


Names_Path_Table_Equal:

; update the file name pointer
inc di                                                                          ; skip the slash
mov [Name_Buffer_Position],di                                                   ; update the file name pointer

; update the top directory index
mov ax,[Current_Index]
mov [Top_Index],ax

clc                                                                             ; report names are matching
pop si

ret


Names_Path_Table_Unequal:
stc                                                                             ; report names not matching
pop si

ret




Check_CDFS_Subdirectory_Request:

; checks if there is another directory requested in the specified file path (performs a slow check)
;   [Name_Buffer_Position]    path to check

movzx eax,word [Name_Buffer_Position]

Check_CDFS_Subdirectory_Request_loop:
cmp [eax],byte 0
je Check_CDFS_Subdirectory_Request_No
cmp [eax],byte '\'
je Check_CDFS_Subdirectory_Request_Yes
cmp [eax],byte '/'
je Check_CDFS_Subdirectory_Request_Yes
inc ax
jmp Check_CDFS_Subdirectory_Request_loop

Check_CDFS_Subdirectory_Request_Yes:
; there is at least one sub-directory requested
stc

ret

Check_CDFS_Subdirectory_Request_No:
; no sub-directory requested, only file name available
clc

ret




Compare_Directory_Record_Name:

; checks the directory record with the file name
;   [Position]                pointer to path table entry
;   [Name_Buffer_Position]    file name to compare

; return
;   CF = 0                    matching! file found!
;   CF = 1                    not matching

push si

movzx cx,byte [si + 32]                                                         ; Length of File Identifier (LEN_FI)
add si,33                                                                       ; seek to the file name
mov di,[Name_Buffer_Position]                                                   ; compare against the path


Name_Directory_Record_Check_Loop:

; lowercase both characters, because the NTFS filesystem is case insensitive lol
char_lowercase [si]
char_lowercase [di]

; compare the characters
cmpsb
jne Names_Directory_Record_Unequal

; they are equal, check next character (note ALL characters must match exactly)
loop Name_Directory_Record_Check_Loop

; ecx = 0, all characters matched, if file name is also at the end its found
cmp byte [di],byte 0                                                            ; zero termination of file name
jne Names_Path_Table_Unequal

File_Name_Match:

clc                                                                             ; report names are matching
pop si

ret


Names_Directory_Record_Unequal:

; special naming on CDFS: File.ext;1  (revision)
cmp byte [di-1],byte 0                                                          ; zero termination of destination file name
jne Names_Path_Table_Unequal
cmp byte [si-1],byte ';'                                                        ; revision termination of source file name
jne Names_Path_Table_Unequal

jmp File_Name_Match




Open_File_CDFS_Found:                                                           ; when only opening the file

; -> store important variables in Handle_Context

; set the sector number
mov eax,dword [si + 2]                                                          ; Location of Extent
mov [Handle_Context + 4],eax                                                    ; (Sector Number)

; set the size
mov ebx,dword [si + 10]                                                         ; Data Length
mov [Handle_Context + 8],ebx                                                    ; (Sector Count)

; return successful
clc
xor eax,eax

ret




Read_File_CDFS:

; set the sector number
mov eax,dword [Handle_Context + 4]
mov [Disk_Address_Packet_LBA_Low],eax                                           ; (Sector Number)

; set the count
movzx eax,word [Drive_Context + 04h]                                            ; bytes per sector
dec eax                                                                         ; -1 (will be rounded up to bytes per sector)
add eax,dword [Param3]                                                          ; Data Length
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
jc Read_File_CDFS_Exit

; return successful
clc
xor eax,eax

Read_File_CDFS_Exit:

ret






