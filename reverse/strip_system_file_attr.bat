@echo off&setlocal enableextensions enabledelayedexpansion 

title 修改系统文件属性和隐藏文件属性 

if "%~1"=="" (echo 你没有拖入 任何磁盘或文件或文件夹 

echo 请把 要处理的磁盘或文件或文件夹 拖到 批处理文件图标 

echo 不要 双击启动 批处理&pause&goto :eof) 

call:choose "设置隐藏文件属性,请选择:" 不改变 减隐藏文件属性 加隐藏文件属性 

(set h=)&(if !c! equ 2 set h=-)&(if !c! equ 3 set h=+) 

call:choose "设置系统文件属性,请选择:" 不改变 减系统文件属性 加系统文件属性 

(set s=)&(if !c! equ 2 set s=-)&(if !c! equ 3 set s=+) 

call:choose "设置应用范围,请选择:" 所选文件和文件夹 该容器内的文件和文件夹 

for %%i in (%*) do (if !c! equ 1 call:attrib "%%~i" 

for /f "delims=" %%j in ('dir /a /b "%%~i"') do call:attrib "%%~i\%%~j") 

pause&goto :eof 

:choose 

(set c=)&(set n=)&for %%i in (%*) do echo !n! %%~i&set /a n+=1 

(set /p c=)&(set /a c=c)&if !c! geq 0 if !c! leq !n! echo.&goto :eof 

goto choose 

:attrib 

setlocal&(set a=)&(set a=%~a1)&if "!a!"=="" goto :eof 

if not defined h if "!a:~3,1!"=="h" (set h=+) else (set h=-) 

if not defined s if "!a:~4,1!"=="s" (set s=+) else (set s=-) 

echo 正在处理: %~1&attrib !h!h !s!s "%~1"&goto :eof