; ******************************************************

;       Name: Kernel Storage
;       Author: Peter Kleissner
;       Version: 0.1
;       Date: 16.02.2008 20:40:07
;       last Update: 17.02.2008 13:42:45

;       Forensic Lockdown Software
;       (C) 2008 Vienna Computer Products

;       This include file contains information about
;       the Kernel Memory Storage.

;       The whole Kernel is relocated to segment 0000h.

; ******************************************************

;%define Forensic_Lockdown_Software
;%define Hibernation_File_Attack
%define Stoned_Bootkit


; define the Module's start addresses
;   Bootloader                  512 Bytes       deprecated
;   Bootloader                  1024 Bytes      to be renamed to Loader
;   System Loader               2048 Bytes
;   Textmode TUI                1024 Bytes
;   Disk System                 11264 Bytes
;   Boot Application            7.5 KB preserved
;   Crypto Module               1536 Bytes
;   Boot Module                 2048 Bytes
;   Pwn Windows                 4096 Bytes
;   -- some free space --       1024 Bytes
;   Original Bootloader Image   512 Bytes       preserved
;   Configuration Area          512 Bytes       to be removed
; Module Base: 8000h
%define Bootloader                              7C00h
%define System_Loader                           8000h
%define Textmode_TUI                            8800h
%define Disk_System                             8C00h
%define Boot_Application                        0B800h
%define Crypto_Module                           0D600h
%define Boot_Module                             0DC00h
%define Pwn_Windows                             0E400h
%define Original_Bootloader_Image               0F800h
%define Configuration_Area                      0FA00h      ; and TrueCrypt Volume Header Information

; these modules are shipped as (natively):
;   - Master Boot Record
;     Hard disks, USB drives
;     Bootloader initializes and sets memory environment
;
;   - Boot Record Memory Image
;     On file system for all other drives, being directly a memory image that is copied 1:1 to 7C00h and with 64 KB of size


; define Runtime Variables

; Stack Pointer
%define Stack_Pointer                           7C00h

; various Boot variables **TODO: move this variables into Bootloader
%define Boot_stuff                              0FC00h
%define Boot_Drive                              Boot_stuff+0
%define Boot_Drive_Sector                       Boot_stuff+4
%define Boot_Stack_Pointer                      Boot_stuff+8


; data for the Disk System (within data segment 0000h)
%define Storage_Device_Driver_Stack             5000h
%define Sector_Buffer                           6000h                           ; max. 4096 bytes (1000h), for Disk System and Boot Module
%define Name_Buffer                             7E60h


; Global Tables [unused, ToasterOS only]
;%define GDT_p                                   10000h
;%define IDT_p                                   10800h
;%define IVT_p                                   11000h

; NTFS Sector Buffer
%define Ext_Sector_Buffer                       20000h

; NTFS Data Runs Buffer
%define Data_Runs_Buffer                        30000h

; MBR Backup Restore Area [Forensic Lockdown Software only]
%define MBR_Backup_Buffer                       40000h


; read buffer for hibernation file, 64 KB [Hibernation File Attack only]
%define Xpress_Image_Buffer                     50000h
%define Extended_Buffer                         70000h

; decryption buffer for Xpress Images [Hibernation File Attack only]
%define Hibernation_File_Attack_Data            60000h


; Textmode Buffer
%define Textmode_Buffer                         0B8000h

; VESA graphics
%define VESA_BIOS_A0000h                        0B0000h
%define VESA_BIOS_B0000h                        0B0000h
%define VESA_BIOS_B8000h                        0B8000h
%define VESA_BIOS_image                         0C0000h

; Memory Areas [unused, ToasterOS only]
%define Interrupt_Vector_Table                  0h
%define Real_Mode_Data_Area                     7000h






; Memory Management in Real Mode: [out-of-date]
; 
; 7C00h -    3072 Bytes        Stack
; 7C00h      512 Bytes         Bootloader
; 7E00h      512 Bytes         Boot Data
; 
; 8000h      32 KB             Preserved for Kernel Modules
; 8000h         KB             System Loader
; 8000h         KB             Disk System
; 8000h         KB             Textmode TUI
; 8000h         KB             Locking Module
; 8000h         KB             Crypto Module
; 8000h         KB             Boot Module
; 8000h         KB             User Interface
; 8000h         KB             API RM
; FE00h      1 sector          Configuration Area
; 
; 
; 10000h     2048 Bytes        GDT [physical]
; 10800h     2048 Bytes        IDT [physical]
; 11000h     1024 Bytes        IVT (copy of)
; 

; Notes:
;   For the preserved are for Kernel Modules, this should be synchronized with the location of the modules in the image.
;   The space for the Modules in the image preserved is 32 KB, and so it's here.





