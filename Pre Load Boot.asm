
; sub functions

;   - Load_MBR_Backup_File_RawFS
;   - Load_MBR_Backup_File_Previous
;   - Load_Embedded_Bootloader_Image
;   - ExecuteFirstHardDiskBootloader


Pre_Load_Exit_Successful:                                                       ; exit successful

; (dl already set)
mov byte [PreLoadDrive],dl
clc
ret


Pre_Load_Exit_Fail:
stc

ret




ExecuteFirstHardDiskBootloader:

; tries to execute the bootloader (if valid) or the bootloader of the active partition of the first hard disk
mov [Disk_Address_Packet_LBA_Low],dword 0                                       ; sector 0 (bootloader, contains Partition Table)
mov dl,80h
call Read_And_Relocate_Bootloader
jc Pre_Load_Exit_Fail                                                           ; if error exit

; validate the read bootloader image
cmp [7C00h + 510],word 0AA55h                                                   ; check for the boot signature (55h, AAh)
jne ExecuteFirstHardDiskBootloader_NotValid
cmp [7C00h + 0],dword 0                                                         ; and if the bootloader contains "something"
je ExecuteFirstHardDiskBootloader_NotValid
cmp [Boot_Drive],byte 80h                                                       ; if already started from that device do not load it (prevent possible endless loop)
je ExecuteFirstHardDiskBootloader_NotValid

; (dl is already set, = 80h, first hard disk)
jmp Pre_Load_Exit_Successful                                                    ; (execute the bootloader image at 7C00h)

ExecuteFirstHardDiskBootloader_NotValid:                                        ; bootloader itself is invalid (or Stoned itself) - check partition bootloaders
mov cx,4                                                                        ; check 4 partition table entries
mov si,7C00h + 1BEh                                                             ; partition table in memory
Find_active_Partition_loop:
cmp [si],byte 80h                                                               ; 80h = active, believe it or not but this has its origins in drive number
je Active_Partition_found
add si,16                                                                       ; + size of partition table entry
loop Find_active_Partition_loop                                                 ; if none found then exit

jmp Pre_Load_Exit_Fail

Active_Partition_found:

; read the partition bootloader
mov eax,[si + 8]                                                                ; use the partition start sector
mov dl,80h                                                                      ; at this time first hard disk, 80h (independent from boot drive)
mov [Disk_Address_Packet_LBA_Low],dword eax                                     ; first sector of partition is the bootloader
call Read_And_Relocate_Bootloader
jc Pre_Load_Exit_Fail                                                           ; if error exit

; validate the read bootloader image
cmp [7C00h + 510],word 0AA55h                                                   ; check for the boot signature (55h, AAh)
jne Pre_Load_Exit_Fail
cmp [7C00h + 0],dword 0                                                         ; and if the bootloader contains "something"
je Pre_Load_Exit_Fail

; (dl is already set, = 80h, first hard disk)
jmp Pre_Load_Exit_Successful                                                    ; (execute the bootloader image at 7C00h)




Load_Embedded_Bootloader_Image:

; check if the original bootloader image is present and valid
cmp [Original_Bootloader_Image],dword 0
je Pre_Load_Exit_Fail
cmp [Original_Bootloader_Image + 510],word 0AA55h
jne Pre_Load_Exit_Fail

; relocate original Bootloader to 7C00h
mov esi,Original_Bootloader_Image
mov edi,7C00h
mov ecx,512 / 4
rep movsd

; (from the boot drive)
mov dl,[Boot_Drive]
jmp Pre_Load_Exit_Successful




Load_MBR_Backup_File_RawFS:

; load the MBR backup from RawFS file

push dword 7C00h
push dword File_MBR_Backup_RawFS
call API_Load_File
jc Load_MBR_Backup_File_RawFS_Error

; (copy the BIOS disk number out of the disk system module, should be 80h+ in most cases)
Load_MBR_Backup_File_BIOS:
mov edx,[Symbol_Last_Device_Context]
movzx edx,word [edx]
mov edx,[edx + 1Eh]
jmp Pre_Load_Exit_Successful

Load_MBR_Backup_File_RawFS_Error:

ret



Load_MBR_Backup_File_Previous:

; load the original MBR
push dword MBR_Backup_Buffer        ; Buffer
push dword File_MBR_Backup          ; File
call API_Load_File
jc Load_MBR_Backup_File_RawFS_Error

; copy Bootloader
mov ax,MBR_Backup_Buffer / 16
mov ds,ax
xor si,si
mov es,si
mov di,7C00h
mov ecx,512 / 4
rep movsd

xor ebx,ebx
mov ds,bx
mov es,bx

jmp Load_MBR_Backup_File_BIOS




Read_And_Relocate_Bootloader:

; reads the bootloader from any drive and relocates it to 7C00h

; hard disks    512 bytes
; CD/DVD/BD     2048 bytes

; Disk_Address_Packet_LBA_Low must be set to the sector containing the bootloader
; dl must be set to the drive reading from

; first read it to the temporary buffer
mov [Disk_Address_Packet_Count],word 1                                          ; only the bootloader (1 sector)
;mov [Disk_Address_Packet_LBA_Low],dword 0                                       ; sector 0 (bootloader, contains Partition Table)
mov [Disk_Address_Packet_Buffer],dword Sector_Buffer                            ; target buffer = bootloader address in real mode

mov si,Disk_Address_Packet_Size
mov ax,4200h                                                                    ; Function "Extended Read" (dl is already set)
int 13h
jc Pre_Load_Exit_Fail                                                           ; if error exit

; only copy the relevant 512 bytes
mov si,Sector_Buffer
mov di,7C00h
mov ecx,512 / 4
rep movsd

ret


