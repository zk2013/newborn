
; Functions:
;   - Get_Dll_Function_Address
;   - Relocate_Executable
;   - Resolve_IAT
;   - CalcPESum


Get_Dll_Function_Address:

; Input:
;   Param 1     Function Name hash
;   Param 2     Base Address of Module to search for Export
; Output:
;   eax = Function Address (NULL if not found)
;   [esp-38] = ntoskrnl export address in address table
; preserves register contents

pushad
mov ebp,[esp+0x28]                                                              ; base address of module (param 2)
mov eax,[ebp+0x3c]                                                              ; PE Header
mov edx,[ebp+eax+0x78]                                                          ; access to Export Table
add edx,ebp                                                                     ; absolute pointer to Export Table
mov ecx,[edx+0x18]                                                              ; ecx = Number of Name Pointers (count of exports)
mov ebx,[edx+0x20]                                                              ; ebx = Name Pointer RVA
add ebx,ebp                                                                     ; absolute pointer to Export Name Pointers

Find_Dll_Export_loop:
jecxz Dll_Function_not_found                                                    ; if no export left exit
dec ecx                                                                         ; next one
mov esi,[ebx+ecx*4]                                                             ; get the function name of the next function
add esi,ebp                                                                     ; absolute address
xor edi,edi                                                                     ; edi stores our calculated hash
cld

; check the Dll function name (generate hash)
Get_Dll_Name_hash:
xor eax,eax
lodsb                                                                           ; inside a dll export, like "wctomb" (and others)
cmp al,ah                                                                       ; zero terminated string
jz Get_Dll_Name_hash_generated
ror edi,13                                                                      ; VERY ODD WAY for finding specific dll entry
add edi,eax                                                                     ; something like a hash
jmp short Get_Dll_Name_hash

Get_Dll_Name_hash_generated:
cmp edi,[esp+0x24]                                                              ; now compare calculated hash and input hash
jnz Find_Dll_Export_loop                                                        ; if not found => check next export

; set up addresses
mov ebx,[edx+0x24]                                                              ; Export Table.Ordinal Table RVA
add ebx,ebp                                                                     ;   (absolute pointer)
movzx ecx,word [ebx+ecx*2]                                                      ; -> Ordinal Number (needed for Address Table)
mov ebx,[edx+0x1c]                                                              ; Export Table.Export Address Table RVA
add ebx,ebp                                                                     ;   (absolute pointer)
mov eax,[ebx+ecx*4]                                                             ; -> Function Address
add eax,ebp                                                                     ;   (absolute pointer)
lea ebx,[ebx+ecx*4]                                                             ; Export Address
mov [esp-4],ebx                                                                 ; (stored on stack, can be later accessed by [esp-38])
jmp short Dll_Function_Address_set

Dll_Function_not_found:
xor eax,eax                                                                     ; error, return zero
Dll_Function_Address_set:
mov [esp+28],eax                                                                ; patch the value to be in eax
popad

ret 8



Relocate_Executable:

; esi = PE Header
; edi = Module Address

pushad

; ImageBase not the wanted one?
cmp [esi + 24 + 28],edi                                                         ; Image Base
je Relocate_Executable_Exit
mov ebx,edi                                                                     ; Relocation = Module Base Address - Preferred Image Base
sub ebx,[esi + 24 + 28]                                                         ; (must be added to all relocations)

; Relocation Table available?
cmp [esi + 24 + 92],dword 5                                                     ; NumberOfRvaAndSizes < Relocation Table Data Directory?
jb Relocate_Executable_Exit

; verify Base Relocation Table Data Directory
cmp [esi + 24 + 136],dword 0                                                    ; VirtualAddress
je Relocate_Executable_Exit
cmp [esi + 24 + 136 +4],dword 0                                                 ; Size
je Relocate_Executable_Exit

; go through all blocks
mov ecx,[esi + 24 + 136 +4]                                                     ; Size of Base Relocation Table
mov esi,[esi + 24 + 136]                                                        ; VirtualAddress of Base Relocation Table
add esi,edi

Next_Relocation_Block:

push ecx
push esi
push edi

; handle 1 block
add edi,[esi]                                                                   ; Page RVA of Base Relocation Block
mov ecx,[esi + 4]                                                               ; Block Size of Base Relocation Block
sub ecx,8
add esi,8

Next_Relocation_Entry:

; load 1 entry (low 12 bits = offset, high 4 bits = type)
lodsw

; apply the offset
push edi
push eax
and eax,00000FFFh
add edi,eax                                                                     ; + page offset
pop eax

; check the type
shr eax,12
cmp al,01h                                                                      ; IMAGE_REL_BASED_HIGH?
je Relocation_high
cmp al,02h                                                                      ; IMAGE_REL_BASED_LOW?
je Relocation_low
cmp al,03h                                                                      ; IMAGE_REL_BASED_HIGHLOW?
je Relocation_highlow
cmp al,04h                                                                      ; IMAGE_REL_BASED_HIGHADJ?
je Relocation_highadjust
jmp Skip_Relocation_Entry                                                       ; otherwise just skip

Relocation_high:
mov eax,ebx
shr eax,16
add [edi],ax
jmp Skip_Relocation_Entry

Relocation_low:
add [edi],bx
jmp Skip_Relocation_Entry

Relocation_highlow:
add [edi],ebx
jmp Skip_Relocation_Entry

Relocation_highadjust:
mov eax,ebx
shr eax,16
add [edi],ax
; ?
jmp Skip_Relocation_Entry

Skip_Relocation_Entry:
; next relocation entry
pop edi
sub ecx,2
jnbe Next_Relocation_Entry

; handled 1 block, go to next if available
pop edi
pop esi
pop ecx

mov eax,[esi + 4]                                                         ; next relocation block (+ Block Size)
add esi,eax
sub ecx,eax
jnbe Next_Relocation_Block


Relocate_Executable_Exit:
popad

ret



Resolve_IAT:

; esi = PE Header
; edi = Module Address

pushad

; ebp = ntoskrnl address (for resolving functions)
mov eax,[ebp - 32]                                                              ; data pointer
add eax,Ntoskrnl_BaseAddress - Data_Reference                                   ; accessing ntoskrnl base address
mov ebp,[eax]

; Import Address Table available?
cmp [esi + 24 + 92],dword 1                                                     ; NumberOfRvaAndSizes < IAT?
jb Resolve_IAT_Exit

; verify Import Table Data Directory
cmp [esi + 24 + 104],dword 0                                                    ; VirtualAddress
je Resolve_IAT_Exit
cmp [esi + 24 + 104 +4],dword 0                                                 ; Size
je Resolve_IAT_Exit

; process all Import Directory Entries
mov ecx,[esi + 24 + 104 +4]                                                     ; Size of Import Table
mov esi,[esi + 24 + 104]                                                        ; VirtualAddress of Import Table
add esi,edi                                                                     ;  (absolute address)


Next_Import_Directory:

; Name RVA valid?
cmp [esi + 12],dword 0
je Resolve_IAT_Exit                                                             ; if invalid => zero import directory = end
cmp [esi + 16],dword 0                                                          ; Import Address Table RVA (Thunk Table) valid?
je Resolve_IAT_Exit

; now check the module name - only ntoskrnl.exe supported!
mov ebx,[esi + 12]                                                              ; Name RVA
add ebx,edi                                                                     ;  (absolute address)
or [ebx],dword 20202020h                                                        ; mask out uppercase
cmp [ebx],dword "ntos"
jne Handled_Import_Directory
or [ebx+4],dword 20202020h                                                      ; mask out uppercase
cmp [ebx+4],dword "krnl"
jne Handled_Import_Directory
or [ebx+8],dword 20202020h                                                      ; mask out uppercase
cmp [ebx+8],dword ".exe"
jne Handled_Import_Directory
cmp [ebx+12],byte 0
jne Handled_Import_Directory

; symbols from ntoskrnl.exe are imported, we can use now ntoskrnl image base address!

; for now use Import Lookup Table RVA (Characteristics) to resolve them
mov ebx,[esi + 0]                                                               ; Name RVA
add ebx,edi                                                                     ;  (absolute address)

; and store the resolved import addresses to Import Address Table RVA (Thunk Table)
mov edx,[esi + 16]                                                              ; Name RVA
add edx,edi                                                                     ;  (absolute address)

Next_Import:
cmp [ebx],dword 0                                                               ; valid? last entry is zero
je Handled_Import_Directory

; Ordinal/Name (test bit 31)?
test [ebx],dword 80000000h                                                      ; if set => ordinal (unsupported), otherwise name
jnz Handled_Import

; => name import from ntoskrnl, read Hint/Name Table entry
mov eax,[ebx]                                                                   ; -> Hint/Name Table
add eax,edi                                                                     ;  (absolute address)
add eax,2                                                                       ; skip the hint (unsupported)

; generate the hash (cheating)
pushad
mov esi,eax
xor edi,edi                                                                     ; edi stores the calculated hash
cld

Generate_Import_Name_Hash:
xor eax,eax
lodsb                                                                           ; inside a dll export, like "wctomb" (and others)
cmp al,ah                                                                       ; zero terminated string
jz Import_Name_Hash_Finished
ror edi,13                                                                      ; according to Get_Dll_Function_Address
add edi,eax                                                                     ; something like a hash
jmp short Generate_Import_Name_Hash

Import_Name_Hash_Finished:

; now resolve the hash to function pointer
push ebp
push edi
call Get_Dll_Function_Address
mov [edx],eax                                                                   ; store the resolved import into the Import Address Table
popad

Handled_Import:
add ebx,4
add edx,4
jmp Next_Import

Handled_Import_Directory:
; (esi = old import directory)
add esi,20
jmp Next_Import_Directory


Resolve_IAT_Exit:
popad

ret



%if 0 = 1
;; Recalculate PE file check sum. Uses the procedure, made by tHE EGOiSTE!
;calc_pe_checksum	proc
;	call	CalcPESum, buffer, f_size
;	jc	@@exit				; If error - leave checksum untouched
;
;	mov	esi,pe_header_ptr		; ESI points to PE header
;	mov	dword ptr [esi+0058h],eax	; Save checksum
;@@exit:
;	ret
;calc_pe_checksum	endp



; written by "tHE EGOiSTE", edited by "Ego" and PK
; ftp://ftp.elf.stuba.sk/pub/pc/pack/unpes24.zip

;*----------------------------------------------*
; int CalcPESum( *Filebuffer, unsigned int Size); 
;
; Calculates Checksum for PE format files
; (remove PE check and OldSum-filter code to
;  calculate Checksum for other Images)
; Returns: PE Checksum/Carry clear on success
;          NULL       /Carry set   on error
; by tE!//TMG
;*-----------------------------------------------*
CalcPESum:
;CalcPESum proc uses ebx esi edi, lpImage:DWORD, ImageSize:DWORD

mov ecx,[esp + 8]                                                               ; Image File Size
shr ecx,1                                                                       ; check for alignment
jc CalcPESum_Error
add ecx,ecx                                                                     ; and overflow
jz CalcPESum_Error
shr ecx,2                                                                       ; / 4 (operate on dwords)
sbb edx,edx
neg edx
;xor ebx,ebx

; verify the PE image
mov esi,[esp + 4]                                                               ; pointer to image
cmp [esi],word 'MZ'                                                             ; valid PE image?
jnz CalcPESum_Error

mov eax,[esi + 3Ch]                                                             ; pointer to PE header
add eax,esi                                                                     ;  (absolute address)
cmp [eax],dword 00004550h                                                       ; PE signature?
jnz CalcPESum_Error

mov edi,dword [eax+58h]                                                         ; original Checksum
xor ebx,ebx                                                                     ; ebx will hold new checksum

Calculate_CheckSum:
lodsd
adc ebx, eax                                                                    ; add the dword with carry to the checksum
loop Calculate_CheckSum

adc ebx,0                                                                       ; final carry flag adjustment
test edx, edx
jz Final_Round
xor eax,eax                                                                     ; read last word
lodsw
add ebx, eax                                                                    ; and add it to the checksum
adc ebx, 0

Final_Round:                                                                    ; create correct checksum value
mov eax, ebx
shr ebx, 16
and eax, 0FFFFh
add eax, ebx
mov ebx, eax
shr ebx, 16
add eax, ebx
and eax, 0FFFFh

mov ebx, edi
shr ebx, 16                                 ; bx=high word original sum
mov ecx, edi

cmp ax, cx
sbb edx, edx
neg edx
add edx, ecx
sub eax, edx
cmp ax, bx
sbb edx, edx
neg edx
mov cx, bx
add edx, ecx
sub eax, edx
and eax, 0FFFFh
add eax,[esp + 8]                                                               ; + image size

mov ebx,[esp + 4]     ; pointer to image
add ebx,[ebx + 3Ch]   ; + pointer to PE header
mov [ebx+58h],eax     ; overwrite the checksum!

clc                                                                             ; return successfully

ret 8

CalcPESum_Error:                                                                ; an error occured
xor eax, eax                                                                    ; 0 = error
stc                                                                             ; return error

ret 8
%endif

