
; NTFS Functions
;   Load_System_File_NTFS
;   Read_File_NTFS


; NTFS help functions
;   Read_File_Record
;   Read_Data_Runs_of_MFT
;   Read_Clusters_by_Data_Runs
;   Load_Data_Runs
;   Get_Bytes_per_MFT_Record
;   Get_Bytes_per_Index_Record
;   Validate_File_Record
;   Validate_Index_Record
;   Find_FILE_Record_Attribute
;   Next_File_Record_Attribute
;   Skip_Standard_Attribute_Header
;   Read_Index_Records
;   Compare_Index_Entry_Name_Win32
;   Load_File_Data
;   Update_Sequence_fixup





; NTFS specific definitions
%define	dapAddress_Ext_Sector_Buffer	(Ext_Sector_Buffer / 16) * 10000h           ; value for disk address packet (segment:offset stored as dword)
















Load_System_File_NTFS:

; API Load_NTFS_File, File, Buffer

; Source
;       Drive Context..........................[Drive_Context]
;       File Name..............................[Param1]
;       Buffer.................................[Param2]

mpush  ds, es, fs


; Data Segments used in this Real Mode driver:
;    ds, es:    2000h   Sector Buffer
;    fs:        3000h   Data Runs Segment
;    ss:        0000h   Data Segment, File Name

mov ax,2000h
mov ds,ax
mov es,ax
mov ax,3000h
mov fs,ax
;mov ax,0B800h
;mov gs,ax
;mov [gs:160*2+1],word 0730h            ; for testing purposes only



; read Root Directory File Record (FILE Record 5)
mov eax,5
mov [File_Record],dword eax
mov [File_Record_48high],dword 0
call Read_File_Record



Load_File_NTFS_directory:

; validate the File Record
call Validate_File_Record
jc Load_File_NTFS_Exit

; find Attribute $INDEX_ROOT
mov eax,90h
call Find_FILE_Record_Attribute
jc Load_File_NTFS_Exit

; store attribute base (later used)
mov [Attribute_Base],si

; skip Standard Attribute Header
call Skip_Standard_Attribute_Header

; check Attribute Type (only $FILENAME)
cmp [si],dword 30h
jne Load_File_NTFS_Attribute_Error

; point to Index Header (skip Index Root)
add si,10h

; files can be both be in small and large index at the same time! "boot" file and winloader.exe are always in resident directory data
;; Index Allocation needed? (test Index Header Flags)
;test [si+0Ch],byte 00000001b
;jnz NTFS_Index_Entry_Large_Index

; store flags for later usage
mov al,[si+0xc]
mov [Index_Entry_Flags],al



NTFS_Index_Entry_Small_Index:

; Indexes fits in Index Root

; skip Index Header (move to first Index Entry)
;add si,dword [si]
add si,word [si]                                                ; Index Header: Offset to first Index Entry


NTFS_Index_Interpret_Small_Index_Entry:

; interpret next short Index Entry of the list
;   a short header of 10 bytes and a full $FILE_NAME attribute

mov [Last_entry],si

; last entry? (test Index Entry Flags)
test [si+0Ch],byte 00000010b
jnz NTFS_Index_Interpret_Small_Index_Entry_last

; check Index Entry
call Compare_Index_Entry_Name_Win32
jc NTFS_Index_Interpret_Small_Index_Entry_next

or eax,eax
jz NTFS_Index_Entry_File_found
cmp eax,1
je NTFS_Index_Entry_Directory_found

NTFS_Index_Interpret_Small_Index_Entry_next:

; (next entry)
mov si,[Last_entry]
add si,[si+8]                                                   ; Index Entry: Length of the index entry

jmp NTFS_Index_Interpret_Small_Index_Entry


NTFS_Index_Interpret_Small_Index_Entry_last:

; last index entry, determine special

; validate last index entry [can be a valid Index Entry, doesn't have to be]
mov eax,[si+10h]
cmp [File_Record],eax
jne NTFS_Small_Index_Entry_Exit
mov ax,[si+14h]
cmp [File_Record_48high],ax
jne NTFS_Small_Index_Entry_Exit

; valid Index Entry, compare name
call Compare_Index_Entry_Name_Win32
jc NTFS_Small_Index_Entry_Exit
mov [cluster_type],byte al                                      ; backup result of comparison

; if here file matches, get the Index Entry Header of DOS Index Entry

; now check if it is the DOS Index Entry
mov eax,[si+10h]
cmp [File_Record],eax
jne NTFS_Small_Index_Entry_Exit
mov ax,[si+14h]
cmp [File_Record_48high],ax
jne NTFS_Small_Index_Entry_Exit
cmp [si+10h+41h],byte 00000010b
jne NTFS_Small_Index_Entry_Exit

; we should really come not here, until the last Index Entry should be this (DOS) one
;    but we do not know if Windows handles an Entry as Index Entry + DOS Index Entry
movzx eax,byte [cluster_type]

or eax,eax
jz NTFS_Index_Entry_File_found
cmp eax,1
je NTFS_Index_Entry_Directory_found


NTFS_Small_Index_Entry_Exit:

; restore state and check for non-resident directory data
mov si,[Attribute_Base]
mov [Last_entry],si

test byte [Index_Entry_Flags],00000001b
jnz NTFS_Index_Entry_Large_Index

jmp Load_File_NTFS_not_found


;  $FILE_NAME stream:
;  
;     00h  8    File reference to the parent directory.
;     08h  8    C Time - File Creation
;     10h  8    A Time - File Altered
;     18h  8    M Time - MFT Changed
;     20h  8    R Time - File Read
;     28h  8    Allocated size of the file
;     30h  8    Real size of the file
;     38h  4    Flags
;     3Ch  4    Used by EAs and Reparse
;     40h  1    Filename length in characters (L)
;     41h  1    Filename namespace
;     42h  2L   File name in Unicode (not null terminated)





NTFS_Index_Entry_Large_Index:

; Index Allocation needed

; next Attribute
call Next_File_Record_Attribute

; find Attribute $INDEX_ALLOCATION
mov eax,0A0h
call Find_FILE_Record_Attribute
jc Load_File_NTFS_Exit

; read the Index Records
call Read_Index_Records



NTFS_Index_Interpret_Index_Record:

; interpret next Index Record
;    set of Index Entries

; validate the Index Record
call Validate_Index_Record
jc Load_File_NTFS_Exit


NTFS_Index_Interpret_Index_Entry:

; interpret next Index Entry
;   a short header of 52h bytes and a filename (this differs from the Index Entries)

; last entry?
test [si+0Ch],word 2
jnz NTFS_Index_Records_last

; check Index Entry
call Compare_Index_Entry_Name_Win32
jc NTFS_Index_Interpret_Index_Entry_1

or eax,eax
jz NTFS_Index_Entry_File_found
cmp eax,1
je NTFS_Index_Entry_Directory_found

NTFS_Index_Interpret_Index_Entry_1:

; (next entry)
add si,[si+8]

jmp NTFS_Index_Interpret_Index_Entry


NTFS_Index_Records_last:

; last index entry, determine special

; validate last index entry [can be a valid Index Entry, doesn't have to be]
mov eax,[si+10h]
cmp [File_Record],eax
jne NTFS_Index_Records_next_1
mov ax,[si+14h]
cmp [File_Record_48high],ax
jne NTFS_Index_Records_next_1

; valid Index Entry, compare name
call Compare_Index_Entry_Name_Win32
jc NTFS_Index_Records_next_1
mov [cluster_type],byte al

; if here file matches, get the Index Entry Header of DOS Index Entry

; next Index Record if available
dec ecx
jz Load_File_NTFS_not_found

push ecx
call Get_Bytes_per_Index_Record
pop ecx

mov si,[Last_entry]
add si,ax

; validate the Index Record
call Validate_Index_Record
jc Load_File_NTFS_Exit

; now check if it is the DOS Index Entry
mov eax,[si+10h]
cmp [File_Record],eax
jne NTFS_Index_Records_next_1
mov ax,[si+14h]
cmp [File_Record_48high],ax
jne NTFS_Index_Records_next_1
cmp [si+10h+41h],byte 2
jne NTFS_Index_Records_next_1

; if here the DOS Entry is found
movzx eax,byte [cluster_type]

or eax,eax
jz NTFS_Index_Entry_File_found
cmp eax,1
je NTFS_Index_Entry_Directory_found

NTFS_Index_Records_next_1:


; next Index Record if available
dec ecx
jz Load_File_NTFS_not_found

push ecx
call Get_Bytes_per_Index_Record
pop ecx

mov si,[Last_entry]
add si,ax

jmp NTFS_Index_Interpret_Index_Record






NTFS_Index_Entry_File_found:

; file found, Index Entry name matches
;    [esi]  Index Entry

; operating in Open File mode?
cmp [Handle_Context + 17],dword 'open'                                          ; only open or load file at once?
je Open_File_NTFS_found

; load respective MFT File Record
mov eax,[si]
mov [File_Record],dword eax
movzx eax,word [si+4]
mov [File_Record_48high],dword eax

call Read_File_Record
jc Load_File_NTFS_Exit

call Load_File_Data
jc Load_File_NTFS_Exit

; return value = buffer
mov eax,[Param2]
clc

jmp Load_File_NTFS_Exit






NTFS_Index_Entry_Directory_found:

; file found, Index Entry name matches
;    [esi]  Index Entry

; if directory is a normal file, raise error [because the file can never know the structure of the disk]
;   same as FAT file-as-directory usage check
test [si+10h+38h],dword 10000000h
jz NTFS_Index_Entry_Directory_invalid_usage

; load respective MFT File Record
mov eax,[si]
mov [File_Record],dword eax
movzx eax,word [si+4]
mov [File_Record_48high],dword eax
call Read_File_Record
jc Load_File_NTFS_Exit

jmp Load_File_NTFS_directory






Open_File_NTFS_found:

; Open File mode

; set default values for handle
mov [Handle_Context + 04h],dword 0
mov [Handle_Context + 08h],dword 0
mov [Handle_Context + 0Ch],dword 0
mov [Handle_Context + 10h],byte 0

; store MFT File Record number
mov eax,[si]
mov [Handle_Context + 17+4],dword eax
movzx eax,word [si+4]
mov [Handle_Context + 17+8],dword eax

; store the file size
mov eax,[si + 38h]
mov [Handle_Context + 29],eax
mov eax,[si + 38h + 4]
mov [Handle_Context + 29 +4],eax

; mark file as already open
mov [Handle_Context + 17],dword '$pen'

clc
jmp Load_File_NTFS_Exit

















Load_File_NTFS_not_found:

; file not found
mov eax,Invalid_Name
stc

jmp Load_File_NTFS_Exit



Load_File_NTFS_Attribute_Error:

; Attribute Error (invalid Index Root enumeration)
mov eax,NTFS_Attribute_Error
stc

jmp Load_File_NTFS_Exit



NTFS_Index_Entry_Directory_invalid_usage:

; when trying to access a file as a directory
mov eax,Invalid_directory_access
stc

;jmp Load_File_NTFS_Exit




Load_File_NTFS_Exit:

mpop  ds, es, fs

ret










; NTFS help functions
;   Read_File_Record
;   Read_Data_Runs_of_MFT
;   Read_Clusters_by_Data_Runs
;   Load_Data_Runs
;   Get_Bytes_per_MFT_Record
;   Get_Bytes_per_Index_Record
;   Validate_File_Record
;   Validate_Index_Record
;   Find_FILE_Record_Attribute
;   Next_File_Record_Attribute
;   Skip_Standard_Attribute_Header
;   Read_Index_Records
;   Compare_Index_Entry_Name_Win32
;   Load_File_Data
;   Update_Sequence_fixup









Read_File_Record:

; checked: NTFS compliant

; Input:
;        Record Number to read...........[File_Record] (48 bit)
;        NTFS File System Information....[Drive]

; Output:
;        File Record.....................[esi]

; Errors:
;        NTFS Clusters Overflow..........NTFS_Overflow



; 1. first calculate the Cluster the searched File Record is in
; 
;     Byte Offset.......(Bytes per Record * Record Number)
;     Cluster Offset....Byte Offset / Bytes per Cluster


; [edx:eax] Byte Offset (relative from $MFT) = Bytes per Record * Record Number
call Get_Bytes_per_MFT_Record
mov ecx,eax

mov eax,[File_Record]
mul ecx
movzx ebx,word [File_Record+4]
imul ebx,ecx
add edx,ebx


; do we have to read the data runs?
;     0 - 11:    System/Meta Records
;     12 - 15:   Marked as in use but empty
;     16 - 23:   Marked as unused
;     above:     free usable
cmp [File_Record],dword 24
jae Read_File_Record_data_runs_needed
cmp [File_Record+4],word 0
jne Read_File_Record_data_runs_needed



; we can read directly the File Record by adding simply the $MFT start position

; Sector Offset (relative from $MFT) = Byte Offset / Bytes per Sector
;    rest [edx] = Byte Offset inside relative Sector
movzx ebx,word [Drive_Context + 0Ch]
div ebx
mov [Add_Position],dx

; Absolute Sector Offset += start sector of $MFT
add eax,[Drive_Context + 0Fh]

; Sectors per Record (to read) = Bytes per Record / Bytes per Sector (round up)
;    [ecx]  Bytes per Record
;           Bytes per Sector = 512 Bytes assumed
;    add to Bytes per Record, the Offset inside the relative Sector, because of spanning-over
add ecx,edx
add ecx,511
shr ecx,9

; [eax]  Sector to read
; [ecx]  Sector Count to read
; [edx]  Byte Offset in Sector of Record

; read or write mode?
cmp [System_Var + 20],dword '$rit'
jz Write_File_Record

; read the Record (into the Sector Buffer)
Read  eax, ecx, Read_File_Record_Exit, dapAddress_Ext_Sector_Buffer

; Output, esi = File Record (edx is offset in the sector)
mov si,[Add_Position]

clc
jmp Read_File_Record_Exit


Write_File_Record:

; write the record (from the sector buffer)
WriteSectors  eax, ecx, Read_File_Record_Exit, dapAddress_Ext_Sector_Buffer

clc
jmp Read_File_Record_Exit



Read_File_Record_data_runs_needed:

; we need to make use of data runs to determine the location of the respective clusters


; Cluster Offset (relative from $MFT) = Byte Offset / Bytes per Cluster
;   [eax]  Cluster Offset
;   [ebx]  Bytes per Cluster
;   [edx]  Byte Offset in Cluster

; Bytes per Cluster = Sectors per Cluster * Bytes per Sector
push edx
movzx ebx,byte [Drive_Context + 0Bh]
movzx edx,word [Drive_Context + 0Ch]
imul ebx,edx
pop edx

div ebx
mov [Add_Position],edx

; Clusters per Record (to read) = Bytes per Record / Bytes per Sector / Sectors per Cluster (round up)
;   [ecx]  Bytes per Record [input]
;   [ecx]  Clusters per Record [output]
push eax
add ecx,edx
mov eax,ecx
xor edx,edx
movzx ebx,word [Drive_Context + 0Ch]
div ebx
movzx ebx,byte [Drive_Context + 0Bh]
xor edx,edx
add eax,ebx
dec eax
div ebx
mov ecx,eax
pop eax

; stream Data Runs of $MFT
call Read_Data_Runs_of_MFT
jc Read_File_Record_Exit

; read the Record's Clusters
;   [eax]  Cluster Offset
;   [ecx]  Clusters per Record
mov [Position],dword dapAddress_Ext_Sector_Buffer
call Read_Clusters_by_Data_Runs
jc Read_File_Record_Exit

; si is Offset in the Buffer
mov si,[Add_Position]
clc
;jmp Read_File_Record_Exit



Read_File_Record_Exit:

ret









Read_Data_Runs_of_MFT:

; checked: NTFS compliant

; Input:
;        NTFS File System Information....[Drive]

; Output:
;        Encoded Data Runs of $MFT.......[Driver_Buffer]


; one Note:

; ATTRIBUTE_RECORD_HEADER at offset 158 {
;     Type code, name:    B0 ($Bitmap)
;     Record length:      70
;     Form code:          1
;     Name length:        0
;     Name offset:        40
;     Flags:              0
;     Instance:           7
;     
;     Nonresident form {
;         Lowest vcn:             0
;         Highest vcn:            8
;         Mapping pairs offset:   40
;         Compression unit:       0
;         Allocated Length:       9000
;         File size:              8920
;         Valid data length:      8920
;         Extend list {
;             (vcn, lcn, run length): (0, BFFFF, 1)
;             (vcn, lcn, run length): (1, 8D4282, 1)
;             (vcn, lcn, run length): (2, 132A44, 1)
;             (vcn, lcn, run length): (3, 36D945, 1)
;             (vcn, lcn, run length): (4, 36974D, 1)
;             (vcn, lcn, run length): (5, 3DFA9F, 1)
;             (vcn, lcn, run length): (6, 8F37AE, 1)
;             (vcn, lcn, run length): (7, 901FB, 1)
;             (vcn, lcn, run length): (8, 906512, 1)
;         }
;     }
; }

mpush  eax, ebx, ecx, edx, esi


; read $MFT, File Record 0 [directly]

; [eax]  Start Sector
; [ecx]  Sectors per Record (at this time, no round-up needed, $MFT is the first file)
call Get_Bytes_per_MFT_Record
mov ecx,eax
add ecx,511
shr ecx,9

mov eax,[Drive_Context + 0Fh]

; read the Record (into the Sector Buffer)
Read  eax, ecx, Read_Data_Runs_of_MFT_Error, dapAddress_Ext_Sector_Buffer


; $MFT File Record (type 0)
xor si,si
call Validate_File_Record
jc Read_Data_Runs_of_MFT_Error

; Attribute $DATA
mov eax,80h
call Find_FILE_Record_Attribute
jc Read_Data_Runs_of_MFT_Error

; Standard Attribute Header (this will do nothing, because data MUST be non-resident)
call Skip_Standard_Attribute_Header

; decode Data Runs
call Load_Data_Runs

mpop  eax, ebx, ecx, edx, esi
clc

ret


Read_Data_Runs_of_MFT_Error:

mpop  ebx, ecx, edx, esi
add sp,4
stc

ret










Read_Clusters_by_Data_Runs:

; checked: NTFS compliant

; Input:
;        Start Cluster (VCN)...............eax
;        Count of Clusters to read.........ecx
;        NTFS File System Information......[Drive]
;        Decoded Data Runs.................[Driver_Buffer]
;        Load to...........................[Position]

; Output:
;        Data Clusters.....................[Position]

; Errors:
;        Cluster not in Data Run list......NTFS_Data_Runs_out_of_list


; (read Cluster by Cluster)
;    encode Data Run by Data Run

xor si,si
mov [Current_Cluster],eax
mov [Cluster_Count],ecx
xor edx,edx


Read_Clusters_by_Data_Runs_loop:

;  [Current_Cluster]   load VCN (updated after reading an cluster, delivered by user)
;  [edx]               Current VCN (updated after loop operation, by adding length)
;  [Current_LCN]       Current LCN (always load from offset which is encoded as LCN already)
;  [Cluster_Count]     Count of Clusters to read left
;  [ebx]               VCNs to read possible

; if Cluster Count is zero finished
cmp [Cluster_Count],dword 0
je Read_Clusters_by_Data_Runs_finished

; load length
fs lodsd
mov ebx,eax

; if length = zero end of list
or ebx,ebx
jz Read_Clusters_by_Data_Runs_NIL

; load offset
fs lodsd
mov [Current_LCN],eax

; if load VCN < Current VCN not in list
cmp [Current_Cluster],edx
jb Read_Clusters_by_Data_Runs_NIL

; if load VCN < Current VCN + length found
mov ecx,edx
add ecx,ebx
cmp [Current_Cluster],ecx
ja Read_Clusters_by_Data_Runs_next

; read the clusters
mpush  ebx, edx, esi
call Read_Clusters_by_Data_Runs_found
mpop  ebx, edx, esi

; update load VCN
add [Current_Cluster],ebx


; next Data Run

Read_Clusters_by_Data_Runs_next:

; update Current VCN
add edx,ebx

jmp Read_Clusters_by_Data_Runs_loop



; read the clusters

Read_Clusters_by_Data_Runs_found:

; LCN = Current_LCN + (load VCN - Current VCN)
mov eax,[Current_Cluster]
sub eax,edx
add eax,[Current_LCN]

; Cluster Count = length or [Cluster_Count], the less one
mov ecx,ebx
cmp [Cluster_Count],ebx
jae Read_Clusters_by_Data_Runs_low_1

mov ecx,[Cluster_Count]
Read_Clusters_by_Data_Runs_low_1:
sub [Cluster_Count],ecx

; if Offset = zero, sparse data
cmp [Current_LCN],dword 0
je Read_Clusters_by_Data_Runs_sparse

; read the Clusters
call Read_NTFS_Clusters
jc Read_Clusters_by_Data_Runs_Error

ret


Read_Clusters_by_Data_Runs_sparse:

; data is zero (alpha stage compression - sparse data)

; write sparse data is currently not supported
mov eax,NTFS_Write_Sparse_Data
cmp [Drive_Context + 20],dword '$rit'
jz Read_Clusters_by_Data_Runs_Error

; Sector Count =  Cluster Count * Sectors per Cluster
movzx ebx,byte [Drive_Context + 0Bh]
imul ecx,ebx

; set Segment and Offset
mov eax,[Position]
ror eax,4
mov es,ax
xor ax,ax
rol eax,4
mov di,ax

shl ecx,9
push ecx

; transfer double words
xor eax,eax
shr ecx,2

; [ToasterOS General Public Alignment: dword]
rep stosd

; update Segment and Offset
pop ecx
movzx eax,word [Position+2]
movzx ebx,word [Position+0]
shl eax,4
add eax,ebx
add eax,ecx

ror eax,4
mov [Position+2],ax
xor ax,ax
rol eax,4
mov [Position+0],ax

; set Extra Segment
mov ax,2000h
mov es,ax

ret


Read_Clusters_by_Data_Runs_NIL:

; NTFS Cluster not in Data Run list

mov eax,NTFS_Data_Runs_out_of_list
stc

ret


Read_Clusters_by_Data_Runs_finished:

xor eax,eax
clc

ret


Read_Clusters_by_Data_Runs_Error:

; destroy the used variables & return address
add sp,12 + 2
stc

ret










Read_NTFS_Clusters:

; Input:
;        Cluster Number..................eax
;        Cluster Count...................ecx
;        Destination.....................[Position]


; Sector Number = LCN * Sectors per Cluster
movzx ebx,byte [Drive_Context + 0Bh]
mul ebx
mov edx,eax

; Sector Count =  Cluster Count * Sectors per Cluster
movzx ebx,byte [Drive_Context + 0Bh]
imul ecx,ebx


Read_NTFS_Clusters_loop:

; read max. 256 sectors at once
cmp ecx,256
jbe Read_NTFS_Clusters_last_round

; write mode?
cmp [Drive_Context + 20],dword '$rit'
je Write_NTFS_Clusters_loop

; read 256 sectors (treated as 0 or 256)
push edx
Read  edx, 256, Read_NTFS_Clusters_Error, [Position]
pop edx

; update Position pointer
add edx,256
add [Position+2],word 2000h

sub ecx,256
jmp Read_NTFS_Clusters_loop


Write_NTFS_Clusters_loop:

push edx
WriteSectors  edx, 256, Read_NTFS_Clusters_Error, [Position]
pop edx

; next block
add edx,256
add [Position + 2],word 256*512 / 16
sub ecx,256
jmp Read_NTFS_Clusters_loop


Read_NTFS_Clusters_last_round:

; write mode?
cmp [Drive_Context + 20],dword '$rit'
jz Write_NTFS_Clusters_last_round

push edx
Read  edx, ecx, Read_NTFS_Clusters_Error, [Position]
pop edx

; update Segment and Offset
movzx eax,word [Position+2]
movzx ebx,word [Position+0]
shl eax,4
add eax,ebx
shl ecx,9
add eax,ecx

ror eax,4
mov [Position+2],ax
xor ax,ax
rol eax,4
mov [Position+0],ax

; exit succesfully
clc

ret


Read_NTFS_Clusters_Error:

pop edx

ret


Write_NTFS_Clusters_last_round:

push edx
WriteSectors  edx, ecx, Read_NTFS_Clusters_Error, [Position]
pop edx

; update Segment and Offset
movzx eax,word [Position+2]
movzx ebx,word [Position+0]
shl eax,4
add eax,ebx
shl ecx,9
add eax,ecx

ror eax,4
mov [Position+2],ax
xor ax,ax
rol eax,4
mov [Position+0],ax

; exit succesfully
clc

ret










Load_Data_Runs:

; checked: NTFS compliant

; Input:
;        Standard Attribute Header.......[esi]
;           (non resident)
;        NTFS File System Information....[Drive]

; Output:
;        Encoded Datan Runs..............[Driver_Buffer]

; Errors:
;        NTFS Clusters Overflow..........NTFS_Overflow


; loads the Data Runs into the Driver Buffer

; Encode Data Runs as dword:dword = Offset:Length
;    yyyyxxxx
;        xxxx......Size of the Length field in Bytes
;    yyyy    ......Size of the Offset field in Bytes


; [esi]  non resident Standard Attribute Header
push esi
add si,word [si+0x20]

xor edi,edi
mov [fs:di],dword 0
xor edx,edx

Load_Data_Runs_load:

lodsb

or al,al
je Load_Data_Runs_finished


; encode length first
mov bl,al
mov bh,al
and bx,1111000000001111b
cmp bl,4
ja Load_Data_Runs_Overflow
cmp bh,40h
ja Load_Data_Runs_Overflow

xor eax,eax
cmp bl,1
je Load_Data_Runs_length_1
cmp bl,2
je Load_Data_Runs_length_2
cmp bl,3
je Load_Data_Runs_length_3

Load_Data_Runs_length_4:
lodsd
mov [fs:di],eax
add di,4
jmp Load_Data_Runs_load_offset

Load_Data_Runs_length_1:
lodsb
mov [fs:di],eax
add di,4
jmp Load_Data_Runs_load_offset

Load_Data_Runs_length_2:
lodsw
mov [fs:di],eax
add di,4
jmp Load_Data_Runs_load_offset

Load_Data_Runs_length_3:
lodsd
dec si
and eax,00FFFFFFh
mov [fs:di],eax
add di,4
jmp Load_Data_Runs_load_offset



; encode offset second
Load_Data_Runs_load_offset:
shr bh,4

xor eax,eax
or bh,bh
jz Load_Data_Runs_offset_0
cmp bh,1
je Load_Data_Runs_offset_1
cmp bh,2
je Load_Data_Runs_offset_2
cmp bh,3
je Load_Data_Runs_offset_3

Load_Data_Runs_offset_4:
lodsd
or edx,edx
jz Load_Data_Runs_offset_store_add
test eax,80000000h
jz Load_Data_Runs_offset_store_add
neg eax
jmp Load_Data_Runs_offset_store_sub

Load_Data_Runs_offset_0:
; means sparse (Offset zero)
mov [fs:di],eax
add di,4
jmp Load_Data_Runs_load

Load_Data_Runs_offset_1:
lodsb
or edx,edx
jz Load_Data_Runs_offset_store_add
test al,80h
jz Load_Data_Runs_offset_store_add
neg al
jmp Load_Data_Runs_offset_store_sub

Load_Data_Runs_offset_2:
lodsw
or edx,edx
jz Load_Data_Runs_offset_store_add
test ax,8000h
jz Load_Data_Runs_offset_store_add
neg ax
jmp Load_Data_Runs_offset_store_sub

Load_Data_Runs_offset_3:
lodsd
dec si
and eax,00FFFFFFh
or edx,edx
jz Load_Data_Runs_offset_store_add
test eax,800000h
jz Load_Data_Runs_offset_store_add
or eax,0FF000000h
neg eax
jmp Load_Data_Runs_offset_store_sub


Load_Data_Runs_offset_store_add:
; eax = relative cluster +
add edx,eax
mov eax,edx
mov [fs:di],eax
add di,4
jmp Load_Data_Runs_load

Load_Data_Runs_offset_store_sub:
; eax = relative cluster -
cmp eax,1
je Load_Data_Runs_offset_store_sub_sparse
sub edx,eax
mov eax,edx
mov [fs:di],eax
add di,4
jmp Load_Data_Runs_load

Load_Data_Runs_offset_store_sub_sparse:
; NT4 sparse data runs use -1 as offset
mov [fs:di],dword 0
add di,4
jmp Load_Data_Runs_load



Load_Data_Runs_finished:

xor eax,eax
mov [fs:di],eax

pop esi
clc

ret


Load_Data_Runs_Overflow:

pop esi

mov eax,NTFS_Overflow
stc

ret










Get_Bytes_per_MFT_Record:

; checked: NTFS compliant

; return = Bytes per MFT Record (normally 4 KB)


; check Clusters per MFT Record
movzx ecx,byte [Drive_Context + 0Eh]
test cl,80h
jnz Get_Bytes_per_MFT_Record_negative



Get_Bytes_per_MFT_Record_positive:

; Bytes per Record = Clusters per Record * Sectors per Cluster * Bytes per Sector
movzx eax,byte [Drive_Context + 0Bh]
mul ecx
movzx ebx,word [Drive_Context + 0Ch]
mul ebx

ret



Get_Bytes_per_MFT_Record_negative:

; This can be negative, which means that the size of the MFT record is smaller than a cluster.
; In this case the size of the MFT record in bytes is equal to 2^(-1 * Clusters per MFT record).

neg cl

mov eax,00000001b
shl eax,cl

ret









Get_Bytes_per_Index_Record:

; checked: NTFS compliant

; return = Bytes per Index Record


; check Clusters per Index Record
movzx ecx,byte [Drive_Context + 13h]
test cl,80h
jnz Get_Bytes_per_Index_Record_negative



Get_Bytes_per_Index_Record_positive:

; Bytes per Record = Clusters per Record * Sectors per Cluster * Bytes per Sector
movzx eax,byte [Drive_Context + 0Bh]
mul ecx
movzx ebx,word [Drive_Context + 0Ch]
mul ebx

ret



Get_Bytes_per_Index_Record_negative:

; This can be negative, which means that the size of the Index record is smaller than a cluster.
; In this case the size of the Index record in bytes is equal to 2^(-1 * Clusters per Index record).

neg cl

mov eax,00000001b
shl eax,cl

ret










Validate_File_Record:

; checked: NTFS compliant

; Input:
;        File Record.....................[esi]

; Output:
;        first Attribute of File Record..[esi]

; Errors:
;        Invalid File Record.............NTFS_Invalid_File_Record


; Layout of a file record
; 
;   00h  4    Magic number 'FILE'
;   04h  2    Offset to the update sequence
;   06h  2    Size in words of Update Sequence Number & Array (S)
;   08h  8    $LogFile Sequence Number
;   10h  2    Sequence number
;   12h  2    Hard link count
;   14h  2    Offset to the first Attribute
;   16h  2    Flags
;   18h  4    Real size of the FILE record
;   1Ch  4    Allocated size of the FILE record
;   20h  8    File reference to the base FILE record
;   28h  2    Next Attribute Id
;   2Ah  2    XP Align to 4 byte boundary
;   2Ch  4    XP Number of this MFT Record


; valid?
cmp [si],dword 'FILE'
jne Validate_File_Record_failed

; Update Sequence
call Update_Sequence_fixup

; skip this FILE Record, and move pointer to the first Attribute
add si,word [si+14h]

clc

ret



Validate_File_Record_failed:

; Error: Invalid NTFS FILE Record

mov eax,NTFS_Invalid_File_Record
stc

ret










Validate_Index_Record:

; checked: NTFS compliant

; Input:
;        Index Record.....................[esi]

; Output:
;        first Attribute of File Record...[esi]

; Errors:
;        Invalid Index Record.............NTFS_Invalid_Index_Record


; Layout of a Index Record
; 
;   00h  4    Magic number 'INDX'
;   04h  2    Offset to the update sequence
;   06h  2    Size in words of Update Sequence Number & Array (S)
;   08h  8    $LogFile Sequence Number
;   10h  8    VCN of this INDX buffer in the Index Allocation
;   18h  4    Offset to the Index Entries
;   1Ch  4    Size of Index Entries
;   20h  4    Allocated size of the Index Entries
;   24h  1    1 if not leaf node
;   25h  3    Padding
;   28h  2    Update sequence
;   2Ah  S*   Update sequence array


; valid?
cmp [si],dword 'INDX'
jne Validate_Index_Record_failed

; Update Sequence
call Update_Sequence_fixup

; store pointer to this INDEX Record, for later use
mov [Last_entry],word si

; move to first entry
add si,[si+18h]
add si,18h

clc

ret



Validate_Index_Record_failed:

; Invalid Index Record

mov eax,NTFS_Invalid_Index_Record
stc

ret










Update_Sequence_fixup:

; checked: NTFS compliant

; writes the Update Sequence Array back into the last bytes of every sector
; given in esi an File/Index Record

mpush ecx, esi, edi


; get Bytes per File/Index Record
cmp [si],dword 'FILE'
je Update_Sequence_fixup_1
cmp [si],dword 'INDX'
je Update_Sequence_fixup_2
jmp Update_Sequence_fixup_ret

Update_Sequence_fixup_1:
call Get_Bytes_per_MFT_Record
jmp Update_Sequence_fixup_3

Update_Sequence_fixup_2:
call Get_Bytes_per_Index_Record

Update_Sequence_fixup_3:

; Sectors per Record
shr eax,9
mov ecx,eax


; move to Update Sequence
mov di,si
add di,510
add si,word [si+4]
add si,2

; loop: Sectors per File Record
Update_Sequence_loop:

movsw
add di,510

loop Update_Sequence_loop


Update_Sequence_fixup_ret:
mpop ecx, esi, edi

ret










Find_FILE_Record_Attribute:

; checked: NTFS compliant

; Input:
;        Attribute Type.....................eax
;        unnamed Attribute only.............ebx = 'unna'
;        named Attribute only...............ebx = 'name'
;        first Attribute of File Record.....[esi]

; Output:
;        found Attribute....................[esi]

; Errors:
;        Attribute not found................NTFS_Attribute_not_found



; Layout of a Standard Attribute Header
; 
;  Resident:
;  
;    00h  4    Attribute Type
;    04h  4    Length (including this header)
;    08h  1    Non-resident flag (0)
;    09h  1    Name length
;    0Ah  2    Offset to the Name (0, 18h)
;    0Ch  2    Flags
;    0Eh  2    Attribute Id
;    10h  4    Length of the Attribute
;    14h  2    Offset to the Attribute
;    16h  1    Indexed flag
;    17h  1    Padding
;    18h  N    The Attribute's Name
;    ...  L    The Attribute
; 
;  Non-Resident:
;  
;    00h  4    Attribute Type
;    04h  4    Length (including this header)
;    08h  1    Non-resident flag (1)
;    09h  1    Name length
;    0Ah  2    Offset to the Name (40h)
;    0Ch  2    Flags (Compressed, Encrypted, Sparse)
;    0Eh  2    Attribute Id
;    10h  8    Starting VCN
;    18h  8    Last VCN
;    20h  2    Offset to the Data Runs
;    22h  2    Compression Unit Size
;    24h  4    Padding
;    28h  8    Allocated size of the attribute
;    30h  8    Real size of the attribute
;    38h  8    Initialized data size of the stream
;    40h  N    The Attribute's Name
;    ...  ..   Data Runs


; End of FILE Record?
cmp [si],dword 0FFFFFFFFh
je Find_FILE_Record_Attribute_not_found

; Searched Attribute?
cmp [si],byte al
je Find_FILE_Record_Attribute_found

; else next Attribute
add si,[si+4]

jmp Find_FILE_Record_Attribute


Find_FILE_Record_Attribute_found:

; unnamed Attribute only?
cmp ebx,'unna'
jne Find_FILE_Record_Attribute_found_1

cmp [si+9],byte 0
jne Find_FILE_Record_Attribute

Find_FILE_Record_Attribute_found_1:

; named Attribute only?
cmp ebx,'name'
jne Find_FILE_Record_Attribute_found_2

cmp [si+9],byte 0
je Find_FILE_Record_Attribute

Find_FILE_Record_Attribute_found_2:

; store Backup pointer (for later use of moving to next Attribute)
mov [Last_entry],si

clc

ret




Find_FILE_Record_Attribute_not_found:

; Error: Attribute in NTFS File Record not found

mov eax,NTFS_Attribute_not_found
stc

ret










Next_File_Record_Attribute:

; checked: NTFS compliant

; simply moves to next Attribute

mov si,[Last_entry]
add si,[si+4]

ret










Skip_Standard_Attribute_Header:

; checked: NTFS compliant

; Input:
;        Standard Attribute Header..............[esi]

; Output:
;        Attribute..............................[esi]
;        Standard Attribute Header [backup].....OFFSET [Last_entry]


; Resident only
cmp [si+8],byte 0
jne Skip_Standard_Attribute_Header_Return

; Name length * 2
movzx ax,byte [si+9]
shl ax,1

; +18h Header
add ax,18h

; move to Attribute
add si,ax


Skip_Standard_Attribute_Header_Return:

ret










Read_Index_Records:

; checked: NTFS compliant

; read data runs, and read the Index Record
;    size:  defined in $Boot.Clusters per Index Record and always seem to be 4 KB


; Input:
;        $INDEX_ALLOCATION...................[esi]
;        NTFS File System Information........[Drive]

; Output:
;        Index Records.......................[esi]
;        Count of Index Records..............ecx

; Errors:
;        NTFS Clusters Overflow..............NTFS_Overflow
;        NTFS Cluster not in Data Run list...NTFS_Data_Runs_out_of_list
;        NTFS Compression unsupported........NTFS_Compression_unsupported
;        NTFS Encryption unsupported.........NTFS_Encryption_unsupported
;        ATA/ATAPI General Public Read Errors



; if size is zero, there's no data
cmp [si+30h],dword 0
je Read_Index_Record_nodata

; first, check if Compressed or Encrypted [unsupported]
test [si+0Ch],word 0001h
jnz Read_Index_Record_Compression_unsupported
test [si+0Ch],word 4000h
jnz Read_Index_Record_Encryption_unsupported

; store important information from the Standard Attribute Header
push dword [si+10h]
push dword [si+30h]


; read the Data Runs (list of Clusters to load)
call Load_Data_Runs
jc Read_Index_Record_Error

; Count of Clusters = Valid data length / Bytes per Cluster (round up)
pop eax
movzx ebx,byte [Drive_Context + 0Bh]
shl ebx,9
xor edx,edx
add eax,ebx
dec eax
div ebx
mov ecx,eax

mul ebx
mov [Bytes_copy],eax


; load the Cluster using the Data Runs
;   [eax]  Cluster Number
;   [ecx]  Count of Clusters
pop eax
mov [Position],dword dapAddress_Ext_Sector_Buffer
call Read_Clusters_by_Data_Runs
jc Read_Index_Record_Exit


; Count of Index Records = Data Clusters in Bytes / Bytes per Index Record [round down]
call Get_Bytes_per_Index_Record
mov ebx,eax
xor edx,edx
mov eax,[Bytes_copy]
div ebx

mov ecx,eax
xor si,si
clc


Read_Index_Record_Exit:

ret

Read_Index_Record_Error:

; delete temporary values
add sp,8

ret




Read_Index_Record_Compression_unsupported:

; can not read further: Compressed data found [and shouldn't occur in Index Record]

mov eax,NTFS_Compression_unsupported
stc

ret



Read_Index_Record_Encryption_unsupported:

; can not read further: Encrypted data found [and shouldn't occur in Index Record]

mov eax,NTFS_Encryption_unsupported
stc

ret



Read_Index_Record_nodata:

; can not read further: no data in Index Record

mov eax,NTFS_Index_Record_nodata
stc

ret










Compare_Index_Entry_Name_Win32:

; checked: NTFS compliant

; checks if the Index Entry is the searched one


; Input:
;        Index Entry.........................[si]
;        NTFS File System Information........[Drive]

; Output:
;        CF = 0, EAX = 0.....................File found
;        CF = 0, EAX = 1.....................Directory found
;        CF = 1..............................Name doesn't match


; Layout of an Index Entry
; 
;   00h  8    MFT Reference of the file
;   08h  2    Size of this Index Entry (included the header and the stream)
;   0Ah  2    Length of the stream [not reliable]
;   0Ch  1-2  Index Flags
;               01h   Index entry points to a sub-node
;               02h   Last index entry in the node
;   0Eh  2    Padding
;   10h  M    Stream ($FILE_NAME attribute)
;   L-8  8    VCN of the sub-node in the index allocation attribute
; 
; Layout of the Index Entry Stream
; 
;   $FILE_NAME


mpush ecx, esi

; move to stream (= $FILE_NAME)
add si,10h

; DOS name space not allowed (only POSIX, Win32, and Win32 & DOS)
cmp [si+41h],byte 2                                             ; $FILE_NAME: Namespace
je Compare_Index_Entry_Name_failed

; set file name length and move to file name
movzx ecx,byte [si+40h]
add si,42h
mov di,[Param1]


Compare_Index_Entry_Name_UTF:

; lowercase both characters, because the NTFS filesystem is case insensitive
char_lowercase [si]
char_lowercase [ss:di]

; compare the characters
;cmpsb (over segments)
mov al,[si]
mov ah,[ss:di]
inc si
inc di
cmp al,ah
jne Compare_Index_Entry_Name_failed

; check the high 8 bits of the UTF16
lodsb
or al,al
jnz Compare_Index_Entry_Name_failed

dec ecx
or ecx,ecx
jnz Compare_Index_Entry_Name_UTF

; file termination:  Counter = 0, Destination [edi] char = [0, ':']
cmp [ss:di],byte 0
je Compare_Index_Entry_Name_File_found
cmp [ss:di],byte ":"
je Compare_Index_Entry_Name_File_found

; directory termination:  Counter = 0, Destination [edi] char = ['\', '/']
cmp [ss:di],byte "\"
je Compare_Index_Entry_Name_Directory_found
cmp [ss:di],byte "/"
je Compare_Index_Entry_Name_Directory_found


Compare_Index_Entry_Name_failed:

; file name doesn't match
mpop ecx, esi

stc
ret


Compare_Index_Entry_Name_File_found:

; update file name pointer
;    [file name]    0   means normal file data
;    [file name]  ':'   means stream of file is accessed
mov [Param1],di

; file found (names matches)
xor eax,eax
mpop ecx, esi

clc
ret


Compare_Index_Entry_Name_Directory_found:

; update file name pointer
;    to point to next part file/directory name
inc di
mov [Param1],di

; directory found (names matches)
mov eax,1
mpop ecx, esi

clc
ret




















Load_File_Data:

; checked: NTFS compliant

; Input:
;        File Entry..........................[esi]
;        NTFS File System Information........[Drive]
;        Buffer..............................[Param2]

; Output:
;        File Data...........................[Param2]

; Errors:
;        NTFS Clusters Overflow..............NTFS_Overflow
;        NTFS Cluster not in Data Run list...NTFS_Data_Runs_out_of_list
;        NTFS Compression unsupported........NTFS_Compression_unsupported
;        NTFS Encryption unsupported.........NTFS_Encryption_unsupported
;        ATA/ATAPI General Public Read Errors


; validate the File Record
call Validate_File_Record
jc Load_File_Data_Return

; find Attribute $DATA, unnamed
mov eax,80h
mov ebx,'unna'
call Find_FILE_Record_Attribute
jc Load_File_Data_Return


; Compressed or Encrypted? [unsupported]
test [si+0Ch],word 0001h
jnz Load_File_Data_Compression_unsupported
test [si+0Ch],word 4000h
jnz Load_File_Data_Encryption_unsupported

; resident or non-resident?
cmp [si+8],byte 1
je NTFS_Index_Entry_File_found_non_resident


; zero data?
cmp [si+10h],dword 0
je Load_File_NTFS_finished

; set Segment and Offset to target buffer
mov eax,[Param2]
ror eax,4
mov es,ax
xor ax,ax
rol eax,4
mov di,ax

; copy data directly (resident attribute)
mov ecx,[si+10h]
call Skip_Standard_Attribute_Header
rep movsb

; set Extra Segment
mov ax,2000h
mov es,ax


Load_File_NTFS_finished:

xor eax,eax
clc

ret


NTFS_Index_Entry_File_found_non_resident:

; non-resident, read data by means of data runs

; zero data?
cmp [si+30h],dword 0
je Load_File_NTFS_finished

; store important information from the Standard Attribute Header
;    +  Starting VCN
;    -  Real size of the attribute
mov eax,dword [si+10h]
mov [Current_Cluster],eax
mov ebx,dword [si+30h]
mov [Bytes_copy],ebx

; read the Data Runs (list of Clusters to load)
call Load_Data_Runs
jc Load_File_Data_Return

; Count of Clusters = Valid data length / Bytes per Cluster (round up)
mov eax,[Bytes_copy]
movzx ebx,byte [Drive_Context + 0Bh]
shl ebx,9
xor edx,edx
add eax,ebx
dec eax
div ebx
mov [Bytes_copy],eax

; set Segment and Offset
mov eax,[Param2]
ror eax,4
mov [Position+2],ax
xor ax,ax
rol eax,4
mov [Position+0],ax

; load the Cluster using the Data Runs
;   [eax]  Cluster Number
;   [ecx]  Count of Clusters
mov eax,[Current_Cluster]
mov ecx,[Bytes_copy]
call Read_Clusters_by_Data_Runs

; set Extra Segment
mov ax,2000h
mov es,ax


Load_File_Data_Return:

ret




Load_File_Data_Compression_unsupported:

; compressed data found

mov eax,NTFS_Compression_unsupported
stc

ret



Load_File_Data_Encryption_unsupported:

; encrypted data found

mov eax,NTFS_Encryption_unsupported
stc

ret




















Read_File_NTFS:

; set up NTFS driver environment
mpush  ds, es, fs

mov ax,2000h                                                    ; ds, es:    2000h   Sector Buffer
mov ds,ax
mov es,ax
mov ax,3000h                                                    ; fs:        3000h   Data Runs Segment
mov fs,ax

; valid?
cmp [Handle_Context + 17],dword '$pen'
jne Read_File_NTFS_Exit

; load File Record
mov eax,[Handle_Context + 17+4]
mov [File_Record],dword eax
movzx eax,word [Handle_Context + 17+8]
mov [File_Record_48high],dword eax

call Read_File_Record
jc Read_File_NTFS_Exit

;call Load_File_Data
;jc Read_File_NTFS_Exit



Read_File_NTFS_Data:

; Input:
;        File Entry..........................[esi]
;        NTFS File System Information........[Drive]
;        Buffer..............................[Param2]
;        Bytes to read.......................[Param3]

; Output:
;        File Data...........................[Param2]

; Errors:
;        NTFS Clusters Overflow..............NTFS_Overflow
;        NTFS Cluster not in Data Run list...NTFS_Data_Runs_out_of_list
;        NTFS Compression unsupported........NTFS_Compression_unsupported
;        NTFS Encryption unsupported.........NTFS_Encryption_unsupported
;        ATA/ATAPI General Public Read Errors


; validate the File Record
call Validate_File_Record
jc Read_File_NTFS_Exit

; find Attribute $DATA, unnamed
mov eax,80h
mov ebx,'unna'
call Find_FILE_Record_Attribute
jc Read_File_NTFS_Exit


; unsupported compression or encryption?
test [si+0Ch],word 0001h
jnz Read_File_NTFS_compressed
test [si+0Ch],word 4000h
jnz Read_File_NTFS_encrypted

; resident or non-resident?
cmp [si+8],byte 1
je Read_File_NTFS_Data_non_resident



Read_File_NTFS_Data_resident:

; zero data?
mov eax,[si+10h]
or eax,eax
jz Read_File_NTFS_Exit_NoError

; invalid position? (current position < file size)
cmp [Handle_Context + 0Ch],eax
jae Read_File_NTFS_Invalid_Position

; set Segment and Offset to target buffer (es:di) for copying process
mov eax,[Param2]                                                ; Buffer (linear address)
ror eax,4
mov es,ax                                                       ; -> Segment (= Buffer Address > 4)
xor ax,ax
rol eax,4
mov di,ax                                                       ; -> Offset (= Buffer Address & 0FFh)

; calculate size to copy (read size or file size - start position, the less one)
mov ecx,[Param3]
mov eax,[si+10h]
sub eax,[Handle_Context + 0Ch]
cmp ecx,eax
jbe Read_File_NTFS_Data_resident_SizeOK
mov ecx,eax
Read_File_NTFS_Data_resident_SizeOK:

; set source
call Skip_Standard_Attribute_Header
add si,[Handle_Context + 0Ch]

; update handle
add [Handle_Context + 0Ch],ecx

; write requested?
cmp [System_Var + 20],dword 'writ'
jz Write_File_NTFS_Data_resident

; copy data directly
rep movsb


Read_File_NTFS_Exit_NoError:
clc

Read_File_NTFS_Exit:

mpop  ds, es, fs

ret


Write_File_NTFS_Data_resident:

; set correct segments
mov ax,es
mov bx,ds
mov ds,ax
mov es,bx

; write the data into the buffer
xchg esi,edi
rep movsb

; set the last read File Record to write
mov eax,[Handle_Context + 21]
mov [File_Record],eax
movzx eax,word [Handle_Context + 25]
mov [File_Record_48high],eax

; write the modified File Record
mov [System_Var + 20],dword '$rit'
call Read_File_Record

jmp Read_File_NTFS_Exit



Read_File_NTFS_Data_non_resident:

; non-resident, read data by means of data runs

; zero data?
mov eax,[si+30h]
or eax,eax
jz Read_File_NTFS_Exit_NoError

; invalid position? (current position < file size)
cmp [Handle_Context + 0Ch],eax
jae Read_File_NTFS_Invalid_Position

; calculate the total size to read (read size or $DATA size - start position, the less one)
mov eax,[si+30h]
sub eax,[Handle_Context + 0Ch]
cmp [Param3],eax
jbe Read_File_NTFS_Data_non_resident_SizeOK
mov [Param3],eax
Read_File_NTFS_Data_non_resident_SizeOK:

; store start Cluster VCN (should be zero)
mov eax,dword [si+10h]
mov [Handle_Context + 04h],eax

; read the Data Runs (list of Clusters to load)
call Load_Data_Runs
jc Read_File_NTFS_Exit


; read next cluster

Read_File_NTFS_Data_non_resident_loop:

; [Param3]    Bytes left to read

; cluster number to read = byte position / bytes per cluster
mov eax,[Handle_Context + 0Ch]
movzx ebx,byte [Drive_Context + 0Bh]
shl ebx,9                                                       ; sectors per cluster * bytes per sector
xor edx,edx
div ebx
add eax,[Handle_Context + 04h]                               ; add start cluster
mov [Current_Cluster],eax

; offset in cluster to read from = byte position % bytes per cluster
mov [Add_Position],edx

; size to copy in cluster = cluster size - offset in cluster or bytes left to read, the less one
sub ebx,edx
cmp ebx,[Param3]
jbe Read_File_NTFS_Data_non_resident_CopySizeOK
mov ebx,[Param3]
Read_File_NTFS_Data_non_resident_CopySizeOK:
mov [Bytes_copy],ebx

; now read one cluster
;   [eax]  Cluster Number
;   [ecx]  Count of Clusters
mov eax,[Current_Cluster]
mov ecx,1
mov [Position],dword dapAddress_Ext_Sector_Buffer               ; we use an outside buffer for our cluster

push dword [Current_Cluster]
call Read_Clusters_by_Data_Runs
pop dword [Current_Cluster]
jc Read_File_NTFS_Exit

; write mode?
cmp [System_Var + 20],dword 'writ'
je Write_File_NTFS_Data_non_resident_loop

; set Segment and Offset to target buffer (es:di) for copying process
mov eax,[Param2]                                                ; Buffer (linear address)
ror eax,4
mov es,ax                                                       ; -> Segment (= Buffer Address > 4)
xor ax,ax
rol eax,4
mov di,ax                                                       ; -> Offset (= Buffer Address & 0FFh)

; update handle
mov ecx,[Bytes_copy]
add [Param2],ecx
sub [Param3],ecx
add [Handle_Context + 0Ch],ecx

rep movsb

cmp [Param3],dword 0
jne Read_File_NTFS_Data_non_resident_loop

jmp Read_File_NTFS_Exit_NoError



Write_File_NTFS_Data_non_resident_loop:

; store segment registers (will be modified)
push ds
push es

mov esi,[Param2]
ror esi,4
mov ds,si
shr esi,32 - 4

mov edi,0x20000
ror edi,0x4
mov es,di
shr edi,32 - 4

mov ecx,[Bytes_copy]
add [Param2],ecx
sub [Param3],ecx
add [Handle_Context + 12],ecx

mov edi,[Add_Position]
rep movsb

pop es
pop ds

mov eax,[Current_Cluster]
mov ecx,1
mov [Position],dword 0x20000000

mov [System_Var + 20],dword '$rit'
call Read_Clusters_by_Data_Runs
mov [System_Var + 20],dword 'writ'
jc Read_File_NTFS_Exit

cmp [Param3],dword 0
jz Read_File_NTFS_Exit_NoError
jmp Read_File_NTFS_Data_non_resident_loop



Read_File_NTFS_compressed:

; compressed data found
mov eax,NTFS_Compression_unsupported
stc

jmp Read_File_NTFS_Exit


Read_File_NTFS_encrypted:

; encrypted data found
mov eax,NTFS_Encryption_unsupported
stc

jmp Read_File_NTFS_Exit


Read_File_NTFS_Invalid_Position:

; encrypted data found
mov eax,Invalid_Parameter
stc

jmp Read_File_NTFS_Exit

