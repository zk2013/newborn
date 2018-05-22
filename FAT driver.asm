
; Boot FAT Functions
;   Determine_FAT_Type
;   Open_File_FAT
;   Read_File_FAT


; FAT help functions
;   Get_Short_Name
;   Determine_FAT_Type
;   Get_Count_of_Clusters
;   Get_DataSec
;   First_Sector_of_Cluster
;   Get_First_Data_Sector
;   Get_Root_Dir_Sectors
;   Get_FirstRootDirSecNum
;   Get_FATSz
;   Get_TotSec
;   Get_FATSecNum_Offset
;   Read_FAT_entry
;   Write_FAT_entry
;   Get_Next_Cluster
;   Validate_Cluster_Number

; all functions require a valid Drive Context and Handle Context











; FAT attributes
%define	ATTR_READ_ONLY		00000001b
%define	ATTR_HIDDEN		00000010b
%define	ATTR_SYSTEM		00000100b
%define	ATTR_VOLUME_ID		00001000b
%define	ATTR_DIRECTORY		00010000b
%define	ATTR_ARCHIVE		00100000b
%define	ATTR_LONG_NAME		ATTR_READ_ONLY+ATTR_HIDDEN+ATTR_SYSTEM+ATTR_VOLUME_ID
%define	ATTR_LONG_NAME_MASK	ATTR_READ_ONLY+ATTR_HIDDEN+ATTR_SYSTEM+ATTR_VOLUME_ID+ATTR_DIRECTORY+ATTR_ARCHIVE




; a special macro for lowercasing a character (needed for case insensitive comparison)

%macro	char_lowercase 1.nolist

; %1 = ASCII character

; if char < "A" then exit
cmp %1,byte 41h
jl %%exit

; if char > "Z" then exit
cmp %1,byte 5Ah
jg %%exit

; lowercase the character (+ 20h)
add %1,byte 20h

%%exit:

%endmacro








Determine_FAT_Type:

; Source
;       Volume to determine FAT type...........[Drive_Context]

; return:
;   all FAT values in Drive_Context are set correct (inclusive FAT determination)
;   FAT Error [unmountable]


; copy relevant values of the BIOS Parameter Block

; set "Sectors per Cluster"
mov bl,byte [Sector_Buffer+13]
mov [Drive_Context+0Bh],bl

; set "dwords per Sector"
mov ax,word [Sector_Buffer+11]
shr ax,2
mov [Drive_Context+0Ch],ax

; set "File Allocation Table" (= "Reserved Sector Count")
mov ax,word [Sector_Buffer+14]					; FAT Offset = Reserved Sector Count
mov [Drive_Context+1Ch],ax


; set the value "First_Data_Sector"
call Get_First_Data_Sector

; set the value "FAT Type"
call Get_Count_of_Clusters

; set the values "first root directory sector" and "last root directory sector"
call Get_FirstRootDirSecNum
jc Determine_FAT_Type_Exit


; exit successful
clc

Determine_FAT_Type_Exit:

ret






Get_First_Data_Sector:

; static function (called twice), needs BIOS Parameter Block
; writes "First_Data_Sector" into Drive_Buffer

; (BPB_NumFATs * FATSz)
call Get_FATSz
movzx ebx,byte [Sector_Buffer+16]
mul ebx

; BPB_ResvdSecCnt + (BPB_NumFATs * FATSz)
movzx ecx,word [Sector_Buffer+14]
add ecx,eax

; BPB_ResvdSecCnt + (BPB_NumFATs * FATSz) + RootDirSectors
call Get_Root_Dir_Sectors
add eax,ecx

mov [Drive_Context+12h],eax					; set "first data sector"

ret






Get_FATSz:

; static function (only called once), needs BIOS Parameter Block
; returns the FATSz


; C++

; If(BPB_FATSz16 != 0)
;   FATSz = BPB_FATSz16;
; Else
;   FATSz = BPB_FATSz32;


; Assembler

movzx eax,word [Sector_Buffer+22]				; eax = BPB_FATSz16

or ax,ax							; eax = 0 ?
jnz Get_FATSz_Exit						; if not zero, it's FATSz16

; else it's FATSz32
mov eax,[Sector_Buffer+36]					; eax = BPB_FATSz32

Get_FATSz_Exit:

ret






Get_Count_of_Clusters:

; static function (only called once), needs BIOS Parameter Block
; writes "FAT Type" into Drive_Buffer

; CountofClusters = DataSec / BPB_SecPerClus
call Get_DataSec

xor edx,edx
movzx ebx,byte [Drive_Context+0Bh]
div ebx


; C++

; If(CountofClusters < 4085)
;   /* Volume is FAT12 */
; else
;   if(CountofClusters < 65525)
;     /* Volume is FAT16 */
;   else
;     /* Volume is FAT32 */


; Assembler

mov [Drive_Context+0Ah],byte 12				        ; suppose FAT Type is FAT12
cmp eax,4085
jb Get_Count_of_Clusters_Exit					; if CountofClusters < 4085 exit

mov [Drive_Context+0Ah],byte 16				        ; suppose FAT Type is FAT16
cmp eax,65525
jb Get_Count_of_Clusters_Exit					; if CountofClusters < 65525 exit

; else FAT Type is FAT32
mov [Drive_Context+0Ah],byte 32

Get_Count_of_Clusters_Exit:

ret






Get_DataSec:

; static function (only called once), needs BIOS Parameter Block and returns static value
; return = DataSec

; DataSec = TotSec - FirstDataSector
call Get_TotSec
sub eax,[Drive_Context+12h]

ret






Get_TotSec:

; static function (only called once), needs BIOS Parameter Block and returns static value
; return = TotSec

; C++

; If(BPB_TotSec16 != 0)
;   TotSec = BPB_TotSec16;
; Else
;   TotSec = BPB_TotSec32;


; Assembler

movzx eax,word [Sector_Buffer+19]				; eax = BPB_TotSec16

or ax,ax							; eax = 0 ?
jnz Get_TotSec_Exit						; if not zero, it's TotSec16

; else it's TotSec32
mov eax,[Sector_Buffer+32]					; eax = BPB_TotSec32

Get_TotSec_Exit:

ret






Get_Root_Dir_Sectors:

; static function (only called once), needs BIOS Parameter Block
; return = Root Directory Sectors

; (BPB_RootEntCnt * 32)
movzx eax,word [Sector_Buffer+17]
shl eax,5

; eax = zero ? (only on FAT32 drives)
or eax,eax
jz Get_Root_Dir_Sectors_Exit

; (BPB_RootEntCnt * 32) + (BPB_BytsPerSec - 1)
movzx edx,word [Sector_Buffer+11]
dec edx
add eax,edx

; ((BPB_RootEntCnt * 32) + (BPB_BytsPerSec - 1)) / BPB_BytsPerSec
xor edx,edx
movzx ebx,word [Sector_Buffer+11]
div ebx

Get_Root_Dir_Sectors_Exit:

ret






Get_FirstRootDirSecNum:

; static function (only called once), needs BIOS Parameter Block
; writes "first root directory sector" and "last root directory sector" into Drive_Buffer


; share function into FAT12/16 and FAT32
cmp [Drive_Context+0Ah],byte 32
je Get_FirstRootDirSecNum_FAT32


; FirstRootDirSecNum = BPB_ResvdSecCnt + (BPB_NumFATs * BPB_FATSz);
call Get_FATSz
movzx ebx,byte [Sector_Buffer+16]
mul ebx								; BPB_FATSz * BPB_NumFATs

movzx ebx,word [Sector_Buffer+14]
add eax,ebx							; BPB_ResvdSecCnt + (BPB_NumFATs * BPB_FATSz16)

mov [Drive_Context+16h],eax					; set "first root directory sector"

call Get_Root_Dir_Sectors					; calculate last root directory sector (add values)
add ax,[Drive_Context+16h]
mov [Drive_Context+1Ah],ax					; set "last root directory sector"

clc

ret 


Get_FirstRootDirSecNum_FAT32:

; first sector is normal first sector of cluster

; validate the Root Directory's Cluster Number
mov eax,[Sector_Buffer+44]
call Validate_Cluster_Number
jc Get_FirstRootDirSecNum_FAT32_Exit

; get the First Sector of the first Root Directory Cluster
call First_Sector_of_Cluster

mov [Drive_Context+16h],eax					; set "first root directory sector"
mov [Drive_Context+1Ah],word 0				        ; set "last root directory sector" (on FAT32 not available)

clc

Get_FirstRootDirSecNum_FAT32_Exit:

ret









; non static functions, which are called more than once and returns non static values



First_Sector_of_Cluster:

; normal FAT32 ?
cmp [Drive_Context+0Ah],byte 32
je First_Sector_of_Cluster_normal

; normal cluster type ?
cmp [cluster_type],byte 18h
jne First_Sector_of_Cluster_normal

; else the cluster is a sector in the root directory area
; (do nothing)

ret


; eax = cluster n
; return = first sector of cluster n

; n - 2
First_Sector_of_Cluster_normal:
dec eax
dec eax

; (n - 2) * BPB_SecPerClus
movzx ebx,byte [Drive_Context+0Bh]
mul ebx

; ((n - 2) * BPB_SecPerClus) + FirstDataSector
add eax,[Drive_Context+12h]

ret







Get_FATSecNum_Offset:

; eax = cluster number n

; return = sector to read (element of the File Allocation Table)
; reutrn(edx) = Offset in the EFFECTIVE sector


; if FAT12 is set, and offset streches over boundarys, the carry flag is set
; in all other cases (non FAT12 or in boundarys) the carry flag is cleared



; If(FATType == FAT16)
;   FATOffset = N * 2;
; Else
;   if (FATType == FAT32)
;     FATOffset = N * 4;
;
; ThisFATSecNum = BPB_ResvdSecCnt + (FATOffset / BPB_BytsPerSec);
; ThisFATEntOffset = REM(FATOffset / BPB_BytsPerSec);


; special FAT12?
cmp [Drive_Context+0Ah],byte 12
je Get_FATSecNum_Offset_FAT12

; calculate the FATOffset (N * 2 or 4)
shl eax,1							; suppose FAT Type is 16
cmp [Drive_Context+0Ah],byte 16
je Calculate_ThisFAT						; if FAT 16, exit

shl eax,1							; else mul one more with 2 (in effect eax * 4)

Calculate_ThisFAT:
; FATOffset / BPB_BytsPerSec
xor edx,edx
sub ebx,ebx
imul bx, word [Drive_Context+0Ch], 4    			; bx = bytes per sector
div ebx								; return(edx) = Offset in sector

; BPB_ResvdSecCnt + (FATOffset / BPB_BytsPerSec)
movzx ebx,word [Drive_Context+1Ch]
add eax,ebx							; return = sector

clc
ret



Get_FATSecNum_Offset_FAT12:

; FATOffset = N + (N / 2);
; /* Multiply by 1.5 without using floating point, the divide by 2 rounds DOWN */
; ThisFATSecNum = BPB_ResvdSecCnt + (FATOffset / BPB_BytsPerSec);
; ThisFATEntOffset = REM(FATOffset / BPB_BytsPerSec);
;
; We now have to check for the sector boundary case:
;
; If(ThisFATEntOffset == (BPB_BytsPerSec – 1)) {
; /* This cluster access spans a sector boundary in the FAT */
; /* There are a number of strategies to handling this. The */
; /* easiest is to always load FAT sectors into memory */
; /* in pairs if the volume is FAT12 (if you want to load */
; /* FAT sector N, you also load FAT sector N+1 immediately */
; /* following it in memory unless sector N is the last FAT */
; /* sector). It is assumed that this is the strategy used here */
; /* which makes this if test for a sector boundary span */
; /* unnecessary. */
; }


; the same code as by "Calculate_ThisFAT"
; FATOffset / BPB_BytsPerSec
xor edx,edx
sub ebx,ebx
imul bx, word [Drive_Context+0Ch], 4			        ; bx = bytes per sector
div ebx								; return(edx) = Offset in sector

; BPB_ResvdSecCnt + (FATOffset / BPB_BytsPerSec)
movzx ebx,word [Drive_Context+1Ch]
add eax,ebx							; return = sector

; If(ThisFATEntOffset == (BPB_BytsPerSec – 1)) then boundary strech
dec ebx
cmp edx,ebx
je Get_FATSecNum_Offset_FAT12_boundary

clc
ret

; if here, the entry is over boundarys, so set cf to indicate it
Get_FATSecNum_Offset_FAT12_boundary:
stc
ret









; functions, which are not used by determination



Read_FAT_entry:

; eax = Cluster

mov [Temp],eax						; store the Cluster temporary (for FAT12)

; special FAT12?
cmp [Drive_Context+0Ah],byte 12
je Read_FAT12_entry


call Get_FATSecNum_Offset				; get the Sector and the Offset of the entry

; read the FAT
push dx

Read eax, 1, Read_FAT_Entry_Exit

pop di
add di,Sector_Buffer					; edi = Offset in the Buffer


; now share the function into FAT16 and FAT32
cmp [Drive_Context+0Ah],byte 32
je Read_FAT32_entry

movzx eax,word [di]					; eax = searched entry
jmp Read_FAT_Entry_succ


Read_FAT32_entry:
mov eax,[di]						; eax = searched entry
;jmp Read_FAT_Entry_succ


Read_FAT_Entry_succ:
clc

Read_FAT_Entry_Exit:
ret



Read_FAT12_entry:					; now: same code as by Read_FAT16/32_entry

call Get_FATSecNum_Offset				; get the Sector and the Offset of the entry
jc Read_FAT12_entry_boundary				; if the entry streches about sector boundarys, handle special

; read the FAT
push dx
Read eax, 1, Read_FAT_Entry_Exit

pop di
shl di,1
add di,Sector_Buffer					; edi = Offset in the Buffer
movzx eax,word [di]					; eax = searched (word) entry

; odd?
test [Temp],dword 01b
je Read_FAT12_entry_odd

; if here, we want the low 12 bits of the word, so clear the other (and exit)
or eax,0F000h
xor eax,0F000h
jmp Read_FAT_Entry_succ

Read_FAT12_entry_odd:
; if here, we want only the high 12 bits of the word, so shift right about 4 bits (and exit)
shr eax,4
jmp Read_FAT_Entry_succ


Read_FAT12_entry_boundary:				; if here, the entry streches over sector boundarys

mov [Temp],eax						; save the sector

; read the FAT
push dx
Read eax, 1, Read_FAT_Entry_Exit

pop di
shl di,1
add di,Sector_Buffer					; edi = Offset in the Buffer

; restore the needed sector (+ 1 !)
mov eax,[Temp]
inc eax

; read the LEAST 4 bits of the ENTRY, which are - in fact - the HIGH NIBBLE bits of the read byte
mov bl,[di]						; just use bl instead of al; bl wouldn't change by the ToasterOS function definition

; read the next Sector of the FAT
Read eax, 1, Read_FAT_Entry_Exit

; now merge the 4 bits in bl and the byte at Sector_Buffer + 0
movzx eax,byte [Sector_Buffer+0]
shl eax,4						; shl the byte (by 4 bits)

shr bl,4						; the high nibble of the byte is the low nibble we need

or al,bl						; add the entry (end exit)
clc

ret









Get_Next_Cluster:

; eax = Cluster
;   even on FAT12/16 root directorys it's interpreted as cluster size

; this function gets the next CLUSTER of any directory (specified by the given cluster), included the Root Directory
; cf is set if EOF is reached, then return = entry


; share into FAT12/16 and FAT32
cmp [Drive_Context+0Ah],byte 32
je Get_Next_Cluster_FAT32


; check if cluster is a sector of the FAT12/16 root directory
cmp [cluster_type],byte 18h
jne Get_Next_Cluster_no_root_directory


; if here, the cluster is a sector of the root directory
; if (Sectors per Cluster + Current Cluster) > Last_Root_Dir_Sector then EOC is reached
push bx
movzx bx,byte [Drive_Context+0Bh]
add ax,bx							; add Sectors per Cluster
pop bx

cmp [Drive_Context+1Ah],ax					; eax > last sector of the root directory?
jae Get_Next_Cluster_RD_inc

; if here, the last sector of the root directory is reached, so there is no more cluster
mov eax,0FFFFFFFFh						; EOC is reached
stc
ret

Get_Next_Cluster_RD_inc:					; normal exit (already incremented)
; eax is already up to date (because of the add Sectors per Cluster)
clc
ret



Get_Next_Cluster_no_root_directory:				; share into FAT12 and FAT16
cmp [Drive_Context+0Ah],byte 16
je Get_Next_Cluster_FAT16


Get_Next_Cluster_FAT12:
call Read_FAT_entry

; EOF reached?
cmp eax,0FF8h
jb Get_Next_Cluster_succ					; if (eax < 0FF8h) then it's not the end

; else set cf (and exit)
stc
ret


Get_Next_Cluster_FAT16:
call Read_FAT_entry

; EOF reached?
cmp eax,0FFF8h
jb Get_Next_Cluster_succ					; if (eax < 0FFF8h) then it's not the end

; else set cf (and exit)
stc
ret


Get_Next_Cluster_FAT32:
call Read_FAT_entry

; only 28 bits used
and eax,0FFFFFFFh

; EOF reached?
cmp eax,0FFFFFF8h
jb Get_Next_Cluster_succ					; if (eax < 0FFFFFF8h) then it's not the end

; else set cf (and exit)
stc
ret


Get_Next_Cluster_succ:

clc
ret








Validate_Cluster_Number:

; Input:
;        Cluster Number..................eax

; Output:
;        Valid Cluster...................cf


; if cluster < 2 then invalid
cmp eax,2
jb Validate_Cluster_Number_failed


; share into FAT12, FAT16 and FAT32 to compare last cluster masks
cmp [Drive_Context+0Ah],byte 12
je Validate_Cluster_Number_FAT12
cmp [Drive_Context+0Ah],byte 16
je Validate_Cluster_Number_FAT16
cmp [Drive_Context+0Ah],byte 32
je Validate_Cluster_Number_FAT32

jmp Validate_Cluster_Number_failed


Validate_Cluster_Number_FAT12:

; if cluster < 0FF8h then it's valid
cmp eax,0FF8h
jnb Validate_Cluster_Number_failed

clc
ret


Validate_Cluster_Number_FAT16:

; if cluster < 0FFF8h then it's valid
cmp eax,0FFF8h
jnb Validate_Cluster_Number_failed

clc
ret


Validate_Cluster_Number_FAT32:

; only 28 bits are used
and eax,0FFFFFFFh

; if cluster < 0FFFFFF8h then it's valid
cmp eax,0FFFFFF8h
jnb Validate_Cluster_Number_failed

clc
ret


Validate_Cluster_Number_failed:

mov eax,File_EOF
stc

ret






















Open_File_FAT:

; Source
;       Drive Context..........................[Drive_Context]
;       File Name..............................[Param1]

; Target
;       Handle Context to open File............[Handle_Context]


; Able Sectors = 4 KB / 512 B = 8

; calculate able sectors per cluster
;   sectors per cluster / able sectors
movzx eax,byte [Drive_Context+0Bh]
shr eax,3
mov ebx,8


; able sectors <= sectors per cluster ?
cmp bl,[Drive_Context+0Bh]
jbe Open_File_FAT_store

; else (able sectors = sector per cluster) and (able sectors per cluster = 1)
mov eax,1
movzx ebx,byte [Drive_Context+0Bh]


Open_File_FAT_store:
mov [Able_sectors],ebx
mov [Able_sectors_counter],eax
mov [Able_sectors_counter_static],eax
mov di,[Param1]


; load the first able sectors of the root directory
mov eax,[Drive_Context+16h]
mov [Current_Sector],eax
mov [Current_Cluster],eax
Read  eax, [Able_sectors], Open_File_Error

; set the counter (of entries to check)
mov [FAT_entry_count],bl
shl byte [FAT_entry_count],4

; set the pointers to the current entry
mov si,Sector_Buffer
mov [Last_entry],dword Sector_Buffer

; use sectors as cluster type (only FAT12/16 for root directory)
mov [cluster_type],byte 18h

jmp Next_FAT_Entry



Open_File_Error:

; fatal error

ret








Next_FAT_Entry:							; scan the next entry

; al = DIR_Name[0]
mov al,[si]

; entry free and no other entries ?
or eax,eax
jz Long_file_not_found						; so indicate invalid name if there is no free entry

; entry free ?
cmp eax,0E5h
je Long_entry_next						; load next entry if the current entry is free


; long entry ?
cmp [si+11],byte ATTR_LONG_NAME
jne Long_entry_next						; if not, load next entry

; first (e. last) sub component ?
test eax,40h
jz Long_entry_next






Check_FAT_entry_long_name:					; compare (new) long names
; esi = start of entry
; edi = user filename (current position)


mov [FAT_checksum],byte 00h
mov [Name_Buffer_Position],dword Name_Buffer + 400              ; reverse pointer (top down)
mov [Name_Buffer + 400],dword 0                                 ; zero termination for file name





Long_entry:							; check next long filename entry
dec byte [FAT_order]						; next entry

; check if it's the last (e. first) long entry
lodsb
test al,40h
jz Long_entry_order

; set the order
sub al,40h
mov [FAT_order],al
jmp Long_entry_order_ok


; compare the order of the long entry
Long_entry_order:
cmp [FAT_order],al
jne Long_entry_next_save

Long_entry_order_ok:


; reserve 13 characters on the name buffer
mov di,[Name_Buffer_Position]
sub di,13*2
mov [Name_Buffer_Position],di


; copy name1 (5 characters)
mov cx,5
rep movsw


; skip the attributes and the type
add si,2


; load, compare and store the checksum
lodsb
cmp [FAT_checksum],byte 00h
jne Long_checksum

mov [FAT_checksum],al
jmp Long_checksum_ok

Long_checksum:
; compare the checksums (if not equal the entry is an orphan)
cmp [FAT_checksum],al
jne Long_entry_next_save

Long_checksum_ok:


; copy name2 (6 characters)
mov cx,3
rep movsd


; skip the FstClusLO
add si,2


; copy name3 (2 characters)
movsd


; last long entry ?
cmp [FAT_order],byte 1
je Long_name_check


; else load next entry
call Load_next_entry

; check the validity of the entry (should be a long filename one)
cmp [si+11],byte ATTR_LONG_NAME
jne Long_entry_current_save

jmp Long_entry






Long_name_check:

; the filename is stored in the Name Buffer, so compare the names
mov si,[Name_Buffer_Position]
mov di,[Param1]

; is the (user) filename UTF8 or UTF16?
; - ?



Long_name_check_UTF8:

; lowercase both characters, because the FAT filesystem is case insensitive
char_lowercase [si]
char_lowercase [di]

; compare the characters
cmpsb
jne Long_Name_check_unequal

; check the high 8 bits of the UTF16
lodsb
or al,al
jnz Long_entry_next_save

; file termination ?
cmp [di-1],byte 00h
je Long_file_found

jmp Long_name_check_UTF8


Long_Name_check_unequal:
; directory termination ?
cmp [si-1],word 00h
jne Long_entry_next_save

cmp [di-1],byte "\"
je Long_directory_found

cmp [di-1],byte "/"
je Long_directory_found

jmp Long_entry_next_save



Long_name_check_UTF16:

; not supported yet

jmp Long_entry_next_save






Long_file_found:

; use the normal cluster type
mov [cluster_type],byte 00h

; update the filename position
mov [Param1],di

; get the short entry (which is in connection with the long entry)
;   return:  [si]  entry
call Long_find_short_entry
jc Open_File_Error

; edi should point to the Handle
lea di,[Handle_Context]

; set the start & current cluster
mov ax,[si+20]
shl eax,16
mov ax,[si+26]

mov [di+04h],eax                                                ; Handle: Start Cluster
mov [di+08h],eax                                                ; Handle: Current Cluster = Start Cluster

; set the current position
mov [di+0Ch],dword 00h                                          ; Handle: Current Byte Position = 0

; set the attributes
mov al,[si+11]
mov [di+10h],al                                                 ; Handle: Attribute Flags

; copy System File Information
lea di,[Handle_Context + 17]
mov cx,32/4
rep movsd


; exit successful
clc

ret








Long_directory_found:

; use the normal cluster type
mov [cluster_type],byte 00h

; update the filename position
mov [Param1],di

; get the short entry (which is in connection with the long entry)
;   return:  [si]  entry
call Long_find_short_entry
jc Open_File_Error


; check if the short entry is a directory one
test [si+11],byte ATTR_DIRECTORY
jz Long_directory_found_invalid


; validate the Directory's Cluster Number
mov ax,[si+20]
shl eax,16
mov ax,[si+26]
mov [Current_Cluster],eax
call Validate_Cluster_Number
jc Open_File_Error

; get the First Sector of the first Directory Cluster
call First_Sector_of_Cluster

mov [Current_Sector],eax
mov ebx,eax


; load the directory
Read eax, [Able_sectors], Open_File_Error


; next cluster
mov eax,[Able_sectors_counter_static]
mov [Able_sectors_counter],eax

; set the counter (of entries to check)
mov bl,[Able_sectors]
mov [FAT_entry_count],bl
shl byte [FAT_entry_count],4

; set the pointers to the current entry
mov si,Sector_Buffer
mov [Last_entry],si

jmp Next_FAT_Entry


Long_directory_found_invalid:
; tryed to access a file as a directory, so raise an error
mov eax,Invalid_directory_access
stc

jmp Open_File_Error







Long_file_not_found:

; - ?
mov eax,Invalid_Name
stc

ret







Long_entry_next_save:

; next entry
call Load_next_entry

jmp Next_FAT_Entry




Long_entry_next:

; next entry
call Load_next_entry

jmp Next_FAT_Entry




Long_entry_current_save:

; use current entry
mov si,[Last_entry]

jmp Next_FAT_Entry




Long_entry_current:

; use current entry
mov si,[Last_entry]

jmp Next_FAT_Entry






Load_next_entry:						; load the next entry

; one entry is digested, so decrement and check if there is another entry in the memory
dec byte [FAT_entry_count]
jnz Load_next_entry_memory


; next able sectors or next cluster ?
dec dword [Able_sectors_counter]
jz Load_next_cluster						; if zero load next cluster



Load_next_able_sectors:						; load next "able sectors"
mov ebx,[Able_sectors]
add [Current_Sector],ebx					; update the current sector
Read  [Current_Sector], ebx, Load_next_entry_Error

; set the counter (of entries to check)
mov [FAT_entry_count],bl
shl byte [FAT_entry_count],4

; set the pointers to the current entry
mov si,Sector_Buffer
mov [Last_entry],dword Sector_Buffer

ret



Load_next_cluster:						; load next cluster
mov eax,[Able_sectors_counter_static]
mov [Able_sectors_counter],eax

; seek to the next cluster
mov eax,[Current_Cluster]
call Get_Next_Cluster
jc Load_next_entry_Error
mov [Current_Cluster],eax

; get the first sector of the next cluster
call First_Sector_of_Cluster
mov [Current_Sector],eax

; read the next "able sectors"
Read eax, [Able_sectors], Load_next_entry_Error

; set the counter (of entries to check)
mov al,[Able_sectors]
mov [FAT_entry_count],al
shl byte [FAT_entry_count],4

; set the pointers to the current entry
mov si,Sector_Buffer
mov [Last_entry],dword Sector_Buffer

ret



Load_next_entry_memory:

; set the pointers to the current entry
add [Last_entry],word 32
mov si,[Last_entry]

ret


Load_next_entry_Error:

add sp,2
stc

jmp Open_File_Error







Long_find_short_entry:

; next entry (should be a short one)
call Load_next_entry

; al = DIR_Name[0]
mov al,[si]

; entry free and no other entries ?
or eax,eax
jz Long_orphan

; entry free ?
cmp eax,0E5h
je Long_orphan


; long entry ?
cmp [si+11],byte ATTR_LONG_NAME
je Long_orphan

; VOLUME_ID ?
cmp al,ATTR_VOLUME_ID
je Long_orphan


; if here, the entry is (which the entry should be) a short name

; make the checksum
push si

lodsb
mov bl,al

mov ecx,10

Long_create_checksum:
; rotate with all bits
xor bh,bh
shl bx,7
or bl,bh

; add next letter
lodsb
add bl,al
loop Long_create_checksum

pop si


; compare the checksums
cmp bl,[FAT_checksum]
jne Long_orphan

ret


Long_orphan:							; if here, the long entry is an orphan
mov eax,FAT_Orphan
stc

ret




























Read_File_FAT:

; Source
;       Drive Context..........................[Drive_Context]
;       Handle Context.........................[Handle_Context]
;       Read Buffer............................[Param2]

; set the current position
mov eax,[Handle_Context + 12]
mov [Position],eax

; validate the File's Cluster Number
mov eax,[Handle_Context + 8]
mov [Current_Cluster],eax
call Validate_Cluster_Number
jc Read_File_FAT_Exit

; get the first Sector of the first File Cluster
call First_Sector_of_Cluster
mov [Current_Sector],eax


; Able Sectors = 4 KB / 512 B = 8

; calculate able sectors per cluster
;   sectors per cluster / able sectors
movzx eax,byte [Drive_Context+0Bh]
shr eax,3
mov ebx,8


; able sectors <= sectors per cluster ?
cmp bl,[Drive_Context+0Bh]
jbe Read_File_FAT_store

; else (able sectors = sector per cluster) and (able sectors per cluster = 1)
mov eax,1
movzx ebx,byte [Drive_Context+0Bh]


Read_File_FAT_store:
mov [Able_sectors],ebx
mov [Able_sectors_counter],eax
mov [Able_sectors_counter_static],eax
mov edi,[Param2]

; work with a correct position
call RW_File_set_position

; use the normal cluster type
mov [cluster_type],byte 00h

; differ between read and write mode
cmp [Handle_Context + 17+12],dword 'writ'
je Write_File_FAT_loop



Read_File_FAT_loop:

; call the unit module, it origins are Open_File
call RW_File_load_next_unit

; set Segment and Offset
ror edi,4
mov es,di
xor di,di
rol edi,4

; copy the bytes
rep movsb

ror edi,4
mov ax,es
add di,ax
rol edi,4
xor ax,ax
mov es,ax

; repeat if there are bytes to copy
cmp [Param3],dword 0
jne Read_File_FAT_loop

jmp Read_File_FAT_finished



Write_File_FAT_loop:

; for writing we have to read the sectors, modify and write them; thus following variables have to be remembered:
;   [Current_Cluster]       Cluster to operate Read/Write operation
;   [Current_Sector]        Sector to operate Read/Write operation
;   [Position]              Position inside Sector to operate Read/Write operation
;   edi                     user write buffer position
push dword [Current_Cluster]
pop dword [Partition_1_values + 0]                              ; we use it as temp variable
push dword [Current_Sector]
pop dword [Partition_1_values + 4]
push dword [Position]
pop dword [Partition_1_values + 8]

; first we have to read the sectors
call RW_File_load_next_unit

; bad enough but we have to remember the new Cluster, Sector, Position also
push dword [Current_Cluster]
pop dword [Partition_2_values + 0]                              ; we use it as temp variable
push dword [Current_Sector]
pop dword [Partition_2_values + 4]
push dword [Position]
pop dword [Partition_2_values + 8]

; exchange source and target (new source = user buffer, new target = read buffer)
xchg esi,edi

mov ax,ds                                                       ; target (read buffer) is only flat in segment 0, so edi will always be within the segment
mov es,ax

ror esi,4                                                       ; source (user buffer) can be anywhere in memory, so use segments
mov ds,si
shr esi,32-4

; now overwrite content in Cluster/Sector
rep movsb

; lets calculate back user write buffer and store it where it should be
ror esi,4
mov ax,ds
add si,ax
rol esi,4
mov edi,esi                                                     ; we store it in edi

xor ax,ax                                                       ; flat memory model for our driver (necessary when accesing data variables)
mov ds,ax

; restore the important variables for writing
push dword [Partition_1_values + 0]
pop dword [Current_Cluster]
push dword [Partition_1_values + 4]
pop dword [Current_Sector]
push dword [Partition_1_values + 8]
pop dword [Position]

; now write the sectors!
call Write_FAT_Cluster_Sectors

; restore variables for readment
push dword [Partition_2_values + 0]
pop dword [Current_Cluster]
push dword [Partition_2_values + 4]
pop dword [Current_Sector]
push dword [Partition_2_values + 8]
pop dword [Position]

; repeat if there are bytes to write
cmp [Param3],dword 0
jne Write_File_FAT_loop



Read_File_FAT_finished:

; update the handle after reading
lea di,[Handle_Context + 8]

mov eax,[Current_Cluster]
stosd

mov eax,[Position]              ; position inside cluster
stosd



; exit successful
clc

Read_File_FAT_Exit:

ret









RW_File_set_position:

; this function splits the Position into the "CASB" values "ables sector" and "bytes"



; calculate the amount of bytes to copy

; sectors per cluster <(=) able sectors ?
cmp [Able_sectors_counter_static],byte 1
je RW_File_bytes_small

; full 4096 bytes to be able to be copied
mov eax,4096
jmp RW_File_bytes_ok

RW_File_bytes_small:
; (sectors per cluster * 512) bytes to be able to be copied
mov eax,512
movzx ebx,byte [Drive_Context+0Bh]
mul ebx

RW_File_bytes_ok:
mov [Bytes_copy],eax



; is the position inside of bounds ?
cmp [Position],eax
jb RW_File_Position_ok

; if not calculate the position inside of the cluster
mov ebx,eax
mov eax,[Position]
xor edx,edx
div ebx
mov [Position],edx

; set the correct able sectors counter
sub [Able_sectors_counter],eax

; seek to the searched able sector
mul dword [Able_sectors]
add [Current_Sector],eax

; if Able Sectors Counter is zero, set to 1
cmp [Able_sectors_counter],dword 0
jne RW_File_Position_ok
mov [Able_sectors_counter],dword 1


RW_File_Position_ok:

ret







RW_File_load_next_unit:

; return(ecx) = byte count to copy
; return(esi) = source of copying the ecx bytes
; return[Param3] = rest of bytes to operate after copying
; return[Current_Cluster] = correct cluster to read (next)
; return[Current_Sector] = correct sector to read (next)
; return[Position] = correct position to read (next)

mov [Temp2],edi

; next able sectors or next cluster ?
cmp [Able_sectors_counter],dword 1
je RW_File_load_next_cluster					; if zero load next cluster

; decrement the counter cause one "able sectors" will be read
dec dword [Able_sectors_counter]



RW_File_load_next_able_sectors:					; load next "able sectors"
; read next able sectors
Read  [Current_Sector], [Able_sectors], RW_File_load_next_unit_Error

; update the current sector
mov ebx,[Able_sectors]
add [Current_Sector],ebx

jmp RW_File_calc_bytes



RW_File_load_next_cluster:					; load next cluster
; reset the able sectors counter
mov eax,[Able_sectors_counter_static]
mov [Able_sectors_counter],eax

; validate the Current Cluster Number
mov eax,[Current_Cluster]
call Validate_Cluster_Number
jc RW_File_load_next_unit_Error

; read after seeking (because the API Buffer)
push dword [Current_Sector]

; skip seeking if not required (if we do only operate in current cluster)
mov eax,[Bytes_copy]
sub eax,[Position]
cmp [Param3],eax
jb RW_File_load_next_cluster_1

; seek to the next cluster
mov eax,[Current_Cluster]
call Get_Next_Cluster
mov [Current_Cluster],eax

RW_File_load_next_cluster_1:

; get the first sector of the next cluster
call First_Sector_of_Cluster
mov [Current_Sector],eax

; read the next "able sectors"
pop eax
Read  eax, [Able_sectors], RW_File_load_next_unit_Error

;jmp RW_File_calc_bytes



RW_File_calc_bytes:

; we have now to calculate the to-copy bytes and the byte position

; set esi (with taking the position into consideration)
;   [ecx]  Counter = Able Bytes to Copy - Position
;   [esi]  Source = API Buffer + Position
mov ecx,[Bytes_copy]
sub ecx,[Position]
mov si,Sector_Buffer
add si,[Position]

; if User Read Bytes < Able Bytes take the less User Read Bytes
mov eax,[Param3]
cmp eax,ecx
jnb RW_File_copy_Able_Bytes

; read User Read Bytes as Byte Count
mov ecx,[Param3]
add [Position],ecx
mov [Param3],dword 0

mov edi,[Temp2]
clc

ret


RW_File_copy_Able_Bytes:

; copy full Able Bytes => Position will be zero
sub [Param3],ecx
mov [Position],dword 0

mov edi,[Temp2]
clc

ret



RW_File_load_next_unit_Error:

; delete stack frame of function call and directly exit read file function
add sp,2
stc

jmp Read_File_FAT_Exit







Write_FAT_Cluster_Sectors:

; we need to write Sector Buffer to disk (we do just reverse operation of read)

; write the read and modified sectors
WriteSectors  [Current_Sector], [Able_sectors], RW_File_load_next_unit_Error, Sector_Buffer

; workaround for WriteSectors macro which modifies ds for NTFS driver
xor ax,ax
mov ds,ax

ret




















Seek_File_FAT:

; Source
;       Drive Context..........................[Drive_Context]
;       Handle Context.........................[Handle_Context]
;       Type...................................[Param2]
;       Position...............................[Param3]

; seek from file begin or current position?
cmp [Param2],dword 0
jne Seek_File_FAT_Add

; Current Cluster = Start Cluster
mov eax,[Handle_Context + 4]
mov [Handle_Context + 8],eax

; Current Position = 0
mov [Handle_Context + 12],dword 0

Seek_File_FAT_Add:

; we need now to add [Param3] bytes to the current position

; check if its done to update only position (is the case if we only operate in the current cluster)
movzx ebx,byte [Drive_Context+0Bh]                              ; Sectors per Cluster
shl ebx,9                                                       ; = Bytes per Cluster
mov eax,[Handle_Context + 12]
add eax,[Param3]                                                ; Current Position + Update Position
cmp eax,ebx
jae Seek_File_FAT_Clusters

; just update position!
mov [Handle_Context + 12],eax
clc

ret

Seek_File_FAT_Clusters:

; we need to do next clusters

; seek to the next cluster
mov eax,[Handle_Context + 8]
push ebx
call Get_Next_Cluster
pop ebx
jc Seek_File_FAT_EOF

; now cheat around a little bit
mov [Handle_Context + 8],eax
mov [Handle_Context + 12],dword 0
sub [Param3],ebx
jmp Seek_File_FAT_Add


Seek_File_FAT_EOF:
mov eax,File_EOF

ret
