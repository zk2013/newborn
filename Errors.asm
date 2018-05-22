; ********************************************

;   Name: Errors
;   Autor: Toaster Burger
;   Version: 1.00
;   Date: 26.11.2005
;   last Update: 26.11.2005
;   see document: ToasterOS.pdf

; ********************************************

%ifndef Errors
%define Errors


; public errors
%define Unknown_Error             00h
%define Invalid_Parameter         01h
%define Invalid_Handle            02h
%define Invalid_Drive             03h
%define Invalid_Name              04h
%define Invalid_Filesystem        05h
%define Invalid_Sector            06h
%define Invalid_Format            07h
%define Invalid_FileFormat        07h
%define Invalid_Object            08h
%define Invalid_Call              09h
%define Invalid_ATAPI_device      0Ah
%define Invalid_Window_Position   0Fh
%define Invalid_Type              10h
%define Invalid_Function          13h

%define Command_Aborted           0Bh
%define BIOS_Interrupt_failed     0Ch
%define Boot_Partition_lost       0Dh
%define Boot_drive_not_found      0Eh   ; static error code
%define Extended_Partition_not_found  11h
%define driver_not_available      12h
%define APM_not_available         14h



; Memory Manager/public errors
%define No_free_Memory            30h
%define No_free_Process           31h
%define No_free_Handle            32h
%define No_free_Sector            33h
%define No_free_Task              34h
%define No_free_Resources         34h
%define No_free_Descriptors       35h
%define No_free_drives            36h
%define No_drives_mounted         37h


; Drive Manager errors

; hard disks errors
%define Drive_Errors              40h
%define Bad_Sector                40h   ; error of a bad sector
%define Data_Error                41h   ; uncorrectable data error
%define ID_mark_lost              42h   ; ID address mark not found
%define Command_Abort             43h   ; Command aborted
%define Track0_lost               44h   ; Track 0 not found
%define Data_label_lost           45h   ; Data label not found
%define Medium_changed            46h   ; medium changed
%define Medium_changing           47h   ; medium changing

; floppy erros
%define Floppy_Errors             48h
%define Drive_not_ready           48h
%define End_of_Cylinder           49h
%define Time_out                  4Ah
%define No_Data                   4Bh
%define Write_protect             4Ch   ; drive is write protected
%define DADM                      4Dh   ; deleted address mark found
%define CRC_Error                 4Eh
%define Wrong_Cylinder            4Fh
%define Seek_Error                50h
%define Bad_Cylinder              51h
%define NDAM                      52h   ; no data address mark found
%define disk_protect              53h   ; disk is write protected
%define Head_above_Track0         54h
%define Drive_not_ready           55h

; file erros
%define File_Errors               57h
%define File_EOF                  57h
%define File_EOF_end              58h
%define Unsupported_Filesystem    59h   ; unsupported but known filesystem
%define FAT_Orphan                5Ah
%define Invalid_directory_access  5Bh   ; when trying to access a file as a directory
%define Directory_EOF             5Bh
%define NTFS_function_aborted     5Ch
%define NTFS_Attribute_not_found  5Dh
%define NTFS_Attribute_Error      5Dh
%define NTFS_Overflow             5Eh
%define NTFS_Invalid_File_Record  5Fh
%define NTFS_Compression_unsupported  61h
%define NTFS_Encryption_unsupported   62h
%define NTFS_Data_Runs_out_of_list    63h
%define NTFS_Index_Record_nodata  64h
%define NTFS_Invalid_Index_Record 65h
%define NTFS_Write_Sparse_Data    66h
%define CDFS_Invalid_Path_Table   67h
%define CDFS_Invalid_Directory_Record 67h


; graphic errors
%define VESA_na                   70h
%define VESA_Mode_Error           71h
%define VBE_not_available         72h
%define VESA_Mode_unset           73h
%define VESA_Mode_fault           76h
%define VESA_Mode_not_found       77h
%define VESA_Mode_no_high_resolution_mode_available 77h

%define Unsupported_BitsPerPixel  73h

%define dib_infoheader            80h   ; error in bitmap file: invalid bitmapinfoheader
%define dib_RLE_repeat_range      81h   ; pixel repeat count is out of bounds (offset after repeat would be bigger than max. Pixels)

%define Cursor_na                 82h   ; requested cursor in file not available
%define System_Font_na            83h


; VBE Errors
%define VBE_Get_Mode_Information_failed     72h
%define VBE_Set_Mode_failed                 73h
%define VBE_Return_Mode_Information_failed  74h


; Other Errors
%define Keyboard_abort            60h



; for debugging

%macro invoke_Breakpoint  1

jmp %%invoke_Breakpoint_1

%%invoke_Breakpoint_M db  %1, 10, 13, 0

%%invoke_Breakpoint_1:
API Extended_Print_Text, %%invoke_Breakpoint_M

%endmacro


%endif







; Boot Errors
%define Invalid_Boot_File_Format  90h







