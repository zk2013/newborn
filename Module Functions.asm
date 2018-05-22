; ******************************************************

;       Name: Module Functions
;       Author: Peter Kleissner
;       Version: 0.1
;       Date: 16.02.2008 20:40:07
;       last Update: 16.02.2008 20:40:10

;       Forensic Lockdown Software
;       (C) 2008 Vienna Computer Products

;       This include file contains information about
;       the location of the modules functions.

; ******************************************************


; Textmode Module Functions
%define API_Clear_Textmode_Screen                               Textmode_TUI + 3*0
%define API_Scroll_Text_down                                    Textmode_TUI + 3*1
%define API_Scroll_Text_up                                      Textmode_TUI + 3*2
%define API_Set_Textmode_Mode                                   Textmode_TUI + 3*3
%define API_Display_Text_Attribute_Position                     Textmode_TUI + 3*4
%define API_Screen_Textmode_Window                              Textmode_TUI + 3*5
%define API_Screen_Character_Attribute_Position                 Textmode_TUI + 3*6
%define API_Screen_Character_Multiple_Attribute_Position        Textmode_TUI + 3*7
%define API_Screen_Animation_1                                  Textmode_TUI + 3*8
%define API_Screen_Animation_2                                  Textmode_TUI + 3*9
%define API_Debug_Message                                       Textmode_TUI + 3*10
%define API_Debug_Message_Append                                Textmode_TUI + 3*11
%define API_Hide_Cursor                                         Textmode_TUI + 3*12
%define API_Enable_Extended_Colors                              Textmode_TUI + 3*13


; Disk System Functions
%define API_Mount_Drives                                        Disk_System + 3*0
%define API_Load_File                                           Disk_System + 3*1
%define API_Open_File                                           Disk_System + 3*2
%define API_Read_File                                           Disk_System + 3*3
%define API_Seek_File                                           Disk_System + 3*4
%define API_Write_File                                          Disk_System + 3*5
%define API_Get_Drive_Type                                      Disk_System + 3*6
%define API_Get_File_Size                                       Disk_System + 3*7
%define API_Get_File_Info                                       Disk_System + 3*8
%define Symbol_Last_Device_Context                              Disk_System + 27
%define Symbol_Last_Handle_Context                              Disk_System + 31


; Crypto Module Functions
%define API_Xpress_Compress                                     Crypto_Module + 3*0
%define API_Xpress_Decompress                                   Crypto_Module + 3*1


; Boot Module Functions
%define API_Load_Original_Bootloader                            Boot_Module + 3*0
%define API_Execute_7C00h_Bootloader_Image                      Boot_Module + 3*1


; Windows Pwn Exports
%define Symbol_Driver_Loading_Table                             Pwn_Windows + 2




; macros for System Functions


; mpush pushes multiple values onto stack

%macro mpush 1-*.nolist

 %rep %0

  push %1

 %rotate 1
 %endrep

%endmacro


; mpop pops multiple values from stack

%macro mpop 1-*.nolist

 %rep %0
 %rotate -1

  pop %1

 %endrep

%endmacro


; define the Parameters pushed on the stack
%define Param1          ebp+4           ; 4 = caller's IP, functions BP (instruction "enter stackframe, 0")
%define Param2          Param1 + 4
%define Param3          Param2 + 4
%define Param4          Param3 + 4
%define Param5          Param4 + 4
%define Param6          Param5 + 4
%define Param7          Param6 + 4
%define Param8          Param7 + 4


; Note:
; Only near calls are allowed to Real Mode API module
; Use a special calling module or Entry Routine for calling the API from different segments.
