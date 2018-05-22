
;   Mount_Drive_Partitions              Mounts all partitions of a partitioned drive
;   Mount_Filesystem                    Mounts FAT and NTFS file systems
;   Mount_Filesystem_CDFS               Mounts CDFS file system
;   Mount_Volume_RawFS                  Mounts a hidden RawFS volume

;   Create_Drive_Context                Creating a handle for additional drives
;   Create_Handle_Context               Creating a handle for files
;   Mount_Partition_Filesystem          Helper function for mounting FAT and NTFS volumes

; note these functions have no error codes, they skip the volume (drive, partition, file system) if mounting fails








Mount_Drive_Partitions:

; Source:
;        Drive Context to mount partitions......[Drive_Context]

; mounts all partitions of the input drive


; load the Partition Table (from the master boot record)
mov si,Disk_Address_Packet_Size
mov [Disk_Address_Packet_LBA_Low],dword 0
mov [Disk_Address_Packet_Count],dword 1
mov [Disk_Address_Packet_Buffer],dword Sector_Buffer

mov dl,[Drive_Context + 1Eh]
mov ah,42h
int 13h
jc Mount_Drive_Partitions_Exit



; check the Partitions
;     extended Partitions are checked recursive
;     ebx is start sector of Partition Table

xor ebx,ebx
mov [Partition_Table],dword 0

Check_Partitions:


; copy relevant partition values
;   (the sector will be overwritten with other values if a partition is mounted)

mov si,Sector_Buffer+1BEh
lea di,[Partition_1_values]
mov cx,4*16/4
rep movsd


Check_Partition_1:                                                              ; check the first partition
mov al,[Partition_1_values+4]
or al,al
jz Check_Partition_2

; check if it is an extended partition
cmp al,5
je Check_Partition_1_extended
cmp al,0Fh
jne Check_Partition_1_normal


Check_Partition_1_extended:
; remark the start sector of the extended partition
add ebx,[Partition_1_values+8]
jmp Check_Partition_2


Check_Partition_1_normal:
; if here the partition itself is valid => mount
lea di,[Partition_1_values]
call Mount_Partition_Filesystem




Check_Partition_2:                                                              ; check the second partition
mov al,[Partition_2_values+4]
or al,al
jz Check_Partition_3

; check if it is an extended partition
cmp al,5
je Check_Partition_2_extended
cmp al,0Fh
jne Check_Partition_2_normal


Check_Partition_2_extended:
; remark the start sector of the extended partition
add ebx,[Partition_2_values+8]
jmp Check_Partition_3


Check_Partition_2_normal:
; if here the partition itself is valid => mount
lea di,[Partition_2_values]
call Mount_Partition_Filesystem




Check_Partition_3:                                                              ; check the third partition
mov al,[Partition_3_values+4]
or al,al
jz Check_Partition_4

; check if it is an extended partition
cmp al,5
je Check_Partition_3_extended
cmp al,0Fh
jne Check_Partition_3_normal


Check_Partition_3_extended:
; remark the start sector of the extended partition
add ebx,[Partition_3_values+8]
jmp Check_Partition_4


Check_Partition_3_normal:
; if here the partition itself is valid => mount
lea di,[Partition_3_values]
call Mount_Partition_Filesystem




Check_Partition_4:                                                              ; check the fourth partition
mov al,[Partition_4_values+4]
or al,al
jz Check_Extended_Partition_Table

; check if it is an extended partition
cmp al,5
je Check_Partition_4_extended
cmp al,0Fh
jne Check_Partition_4_normal


Check_Partition_4_extended:
; remark the start sector of the extended partition
add ebx,[Partition_4_values+8]
jmp Check_Extended_Partition_Table


Check_Partition_4_normal:
; if here the partition itself is valid => mount
lea di,[Partition_4_values]
call Mount_Partition_Filesystem




Check_Extended_Partition_Table:                                                 ; next table if available

; if next Partition Table and Current Partition Table are equal, there is no further Extended Partition Table
cmp ebx,[Partition_Table]
je Mount_Drive_Partitions_Exit

; if here, the next table is available
mov [Disk_Address_Packet_Buffer],dword Sector_Buffer
mov [Disk_Address_Packet_Count],word 1
mov [Disk_Address_Packet_LBA_Low],ebx                                           ; set the sector to read
mov [Partition_Table],ebx                                                       ; set current table

; interrupt 13h, Function 42h: Extended Read
mov si,Disk_Address_Packet_Size
mov dl,[Drive_Context + 1Eh]
mov ah,42h
int 13h

; if no error, check the next partition
jc Mount_Drive_Partitions_Exit
jmp Check_Partitions




Mount_Drive_Partitions_Exit:

ret








Mount_Partition_Filesystem:

; Source:
;        Extended Partition Start Sector........[Partition_Table]
;        Pointer to Partition values............di
;           Partition Type......................[di + 0]
;           Partition Start Sector..............[di + 8]

; mounts partition file systems (prepares drive context for mounting)

mpush ebx, edi

; set "Partition Start Sector" (Partition Start Sector + Extended Partition Table Start Sector)
mov eax,[di+8]
add eax,[Partition_Table]
mov [Drive_Context + 0],eax                                                     ; set the Partition Start Sector

call Mount_Filesystem

mov [Drive_Context + 0],dword 0                                                 ; reset the partition start sector

mpop ebx, edi

ret





Mount_Filesystem:

; mounts volume if
;   - FAT filesystem      Indication by FAT Signature
;   - NTFS filesystem     Indication by NTFS Signature
;
; drive context must be available


; read the first Sector of Partition
Read  0, 1, Mount_Partition_Filesystem_Exit



; recognize FAT on FAT signatures
;    "MSWIN4.0" at Offset 3
;    "MSWIN4.1" at Offset 3
;    "FAT12   " at Offset 54
;    "FAT16   " at Offset 54
;    "FAT32   " at Offset 82


; "MSWIN4.0" at Offset 3
; "MSWIN4.1" at Offset 3
cmp [Sector_Buffer+3],dword "MSWI"
jne Scan_drives_for_filesystem_FAT_1
cmp [Sector_Buffer+7],dword "N4.0"
je Mount_Partition_FAT
cmp [Sector_Buffer+7],dword "N4.1"
je Mount_Partition_FAT

Scan_drives_for_filesystem_FAT_1:

; "FAT12   " at Offset 54
; "FAT16   " at Offset 54
cmp [Sector_Buffer+3],dword "FAT1"
jne Scan_drives_for_filesystem_FAT_2
cmp [Sector_Buffer+7],dword "2   "
je Mount_Partition_FAT
cmp [Sector_Buffer+7],dword "6   "
je Mount_Partition_FAT

Scan_drives_for_filesystem_FAT_2:

; "FAT32   " at Offset 82
cmp [Sector_Buffer+3],dword "FAT3"
jne Scan_drives_for_filesystem_FAT_3
cmp [Sector_Buffer+7],dword "2   "
je Mount_Partition_FAT

Scan_drives_for_filesystem_FAT_3:



; recognize NTFS on NTFS signatures
;    "NTFS    " at Offset 3


; "NTFS    " at Offset 3
cmp [Sector_Buffer+3],dword "NTFS"
jne Scan_drives_for_filesystem_NTFS_1
cmp [Sector_Buffer+7],dword "    "
je Mount_Partition_NTFS

Scan_drives_for_filesystem_NTFS_1:



; else filesystem is unkown (no known file system indicator left)
;   => check next partition

Mount_Partition_Filesystem_Exit:

ret




Mount_Partition_FAT:

; mounts FAT file system partition

; mount the file system
call Determine_FAT_Type
jc Mount_Partition_Filesystem_Exit

; create a new Partition Record of the drive device
call Create_Drive_Context

; debug: inform about the mounted file system
%ifdef _DEBUG
mov eax,Debug_Message_Mount_FAT
call API_Debug_Message
%endif

jmp Mount_Partition_Filesystem_Exit




Mount_Partition_NTFS:

; mounts NTFS file system partition

; debug: inform about the mounted file system
%ifdef _DEBUG
mov eax,Debug_Message_Mount_NTFS
call API_Debug_Message
%endif


; copy relevant values from the BIOS Parameter Block

; set "Sectors per Cluster"
mov al,byte [Sector_Buffer+13]
mov [Drive_Context + 0Bh],al

; set "Bytes per Sector"
mov ax,word [Sector_Buffer+11]
mov [Drive_Context + 0Ch],ax

; set "Clusters per MFT Record"
mov al,byte [Sector_Buffer+64]
mov [Drive_Context + 0Eh],al

; set "Sector Offset of $MFT"
;   = logical Cluster number * Sectors per Cluster
mov eax,[Sector_Buffer+30h]
movzx ebx,byte [Sector_Buffer+0Dh]
mul ebx
mov [Drive_Context + 0Fh],eax

; set "Clusters per Index Record"
mov al,byte [Sector_Buffer+68]
mov [Drive_Context + 13h],al

; set the value "NTFS Type"
mov [Drive_Context + 0Ah],byte 'N'


; create a new Partition Record of the drive device
call Create_Drive_Context

jmp Mount_Partition_Filesystem_Exit






Mount_Filesystem_CDFS:

; mounts CDFS file systems (for CD, DVD, BD)


; read sector 16, Primary Volume Descriptor
Read  16, 1, Mount_Filesystem_CDFS_Exit

; verify the descriptor
cmp [Sector_Buffer + 0],byte 1                                                  ; Volume Descriptor Type = 1
jne Mount_Filesystem_CDFS_Exit
cmp [Sector_Buffer + 1],dword 'CD00'                                            ; Standard Identifier = "CD001"
jne Mount_Filesystem_CDFS_Exit
cmp [Sector_Buffer + 5],byte '1'
jne Mount_Filesystem_CDFS_Exit
cmp [Sector_Buffer + 6],byte 1                                                  ; Volume Descriptor Version = 1
jne Mount_Filesystem_CDFS_Exit

;   -> found a valid CDFS volume

; debug: inform about the mounted file system
%ifdef _DEBUG
mov eax,Debug_Message_Mount_CDFS
call API_Debug_Message
%endif

; fill the device context
mov eax,[Sector_Buffer + 80]                                                    ; Volume Space Size
mov [Drive_Context + 13h],dword eax
mov ebx,[Sector_Buffer + 132]                                                   ; Path Table Size
mov [Drive_Context + 0Fh],dword ebx
mov ecx,[Sector_Buffer + 140]                                                   ; Location of Occurrence of Type L Path Table
mov [Drive_Context + 0Bh],dword ecx
mov [Drive_Context + 0Ah],byte 'C'                                              ; file system CDFS

; create a new persistent device context
call Create_Drive_Context


Mount_Filesystem_CDFS_Exit:

ret






Mount_Volume_RawFS:

; mounts a hidden RawFS volume of hard disks


; load the Partition Table (from the master boot record)
Read  0, 1, Mount_Volume_RawFS_Exit


; date expiry
; pushad
;mov ah,04h                            ; INT 1A - TIME - GET REAL-TIME CLOCK DATE
;int 1Ah
;jc Mount_Volume_RawFS_Exit
;cmp cl,10h                            ; CL = year (BCD)     2010
;jne Mount_Volume_RawFS_Exit
;cmp dh,03h                            ; DH = month (BCD)    <= 03 (March)
;ja Mount_Volume_RawFS_Exit
;popad


; get the possible RawFS volume start sector
xor eax,eax                                                                     ; eax will hold last partition table sector
mov si,Sector_Buffer + 1BEh                                                     ; partition table
mov cl,4                                                                        ; 4 Partition Table entries

Get_RawFS_Start_Sector:
cmp [si+8],eax                                                                  ; check if Partition is more behind the current one
jc Verified_Partition_RawFS

; if the partition is behind this one, then sector AFTER that partition will be remembered
mov eax,[si+8]                                                                  ; start sector
add eax,[si+12]                                                                 ; + count of sectors (size of partition)

Verified_Partition_RawFS:
add si,16                                                                       ; next Partition Table entry
loop Get_RawFS_Start_Sector

or eax,eax                                                                      ; not found? (should not occur)
jz Mount_Volume_RawFS_Exit


; -> set the start sector
mov [Drive_Context + 0],dword eax

; read the first sector
Read  0, 1, Mount_Volume_RawFS_Exit


; RawFS? (check second entry to be \Bootkit)
or [Sector_Buffer + 96 + 0],dword 20202020h                                     ; mask out uppercase
or [Sector_Buffer + 96 + 4],dword 20202020h
or [Sector_Buffer + 96 + 8],dword 20202020h
or [Sector_Buffer + 96 + 12],dword 20202020h
or [Sector_Buffer + 96 + 16],dword 20202020h
or [Sector_Buffer + 96 + 20],dword 20202020h
or [Sector_Buffer + 96 + 24],dword 20202020h
or [Sector_Buffer + 96 + 28],dword 20202020h
cmp [Sector_Buffer + 96 + 0],dword "8f58"                                       ; compare the MD5
jne Mount_Volume_RawFS_Exit
cmp [Sector_Buffer + 96 + 4],dword "eadd"                                       ; must be 8f58eadd7bfff0c557d4b5e9656957a5
jne Mount_Volume_RawFS_Exit
cmp [Sector_Buffer + 96 + 8],dword "7bff"
jne Mount_Volume_RawFS_Exit
cmp [Sector_Buffer + 96 + 12],dword "f0c5"
jne Mount_Volume_RawFS_Exit
cmp [Sector_Buffer + 96 + 16],dword "57d4"
jne Mount_Volume_RawFS_Exit
cmp [Sector_Buffer + 96 + 20],dword "b5e9"
jne Mount_Volume_RawFS_Exit
cmp [Sector_Buffer + 96 + 24],dword "6569"
jne Mount_Volume_RawFS_Exit
cmp [Sector_Buffer + 96 + 28],dword "57a5"
jne Mount_Volume_RawFS_Exit

;   -> found a valid RawFS volume

; fill the device context
mov [Drive_Context + 0Ah],byte 'R'                                              ; file system RawFS

; create a new persistent device context
call Create_Drive_Context

; debug: inform about the mounted volume
%ifdef _DEBUG
mov eax,Debug_Message_Mount_RawFS
call API_Debug_Message
%endif


Mount_Volume_RawFS_Exit:

ret







Create_Drive_Context:

; Source:
;       Drive Context..........................[Drive_Context]

; Target:
;       drive context stored on Storage_Device_Driver_Stack
;       Pointer to Drive Context...............eax

; adds the current Drive Context to the drive device list

; if there are no free partition entries in the list left exit
cmp [Device_Context_Count],dword 1000h / Drive_Context_Size
jae Create_Drive_Context_Exit

; new driver context = count * size + base
imul di,[Device_Context_Count],Drive_Context_Size
add di,Storage_Device_Driver_Stack
movzx eax,di

; copy Drive Context
mov cx,Drive_Context_Size
lea si,[Drive_Context]
rep movsb

; increment Drive Device counter
inc dword [Device_Context_Count]

Create_Drive_Context_Exit:

ret







Create_Handle_Context:

; Source:
;       Drive Context..........................[Drive_Context]
;       Handle Context.........................[Handle_Context]

; Target:
;       Handle stored on Storage_Device_Driver_Stack
;       Pointer to handle......................eax

; Error Codes:
;       eax = 0     if no free handle

; creates a handle on the Storage Device Driver Stack

; new handle = driver context count * size + handle count * size + base
imul ax,[Device_Context_Count],Drive_Context_Size
imul di,[Handle_Context_Count],Handle_Context_Size
add di,ax

; if there is no free handle, exit
xor eax,eax
cmp di,word 1000h - Handle_Context_Size
ja Create_Handle_Context_Exit

add di,Storage_Device_Driver_Stack                                              ; (base address)
mov ax,di
mov [Last_Handle_Context],ax                                                    ; for caching purposes

; set Handle: Drive Context Pointer
push word [Drive_Context_Pointer]
pop word [Handle_Context]

; copy handle buffer
mov cx,Handle_Context_Size
lea si,[Handle_Context]
rep movsb

; increment handle counter
inc dword [Handle_Context_Count]

Create_Handle_Context_Exit:

ret



