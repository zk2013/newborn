@echo off&setlocal enableextensions enabledelayedexpansion 

title �޸�ϵͳ�ļ����Ժ������ļ����� 

if "%~1"=="" (echo ��û������ �κδ��̻��ļ����ļ��� 

echo ��� Ҫ����Ĵ��̻��ļ����ļ��� �ϵ� �������ļ�ͼ�� 

echo ��Ҫ ˫������ ������&pause&goto :eof) 

call:choose "���������ļ�����,��ѡ��:" ���ı� �������ļ����� �������ļ����� 

(set h=)&(if !c! equ 2 set h=-)&(if !c! equ 3 set h=+) 

call:choose "����ϵͳ�ļ�����,��ѡ��:" ���ı� ��ϵͳ�ļ����� ��ϵͳ�ļ����� 

(set s=)&(if !c! equ 2 set s=-)&(if !c! equ 3 set s=+) 

call:choose "����Ӧ�÷�Χ,��ѡ��:" ��ѡ�ļ����ļ��� �������ڵ��ļ����ļ��� 

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

echo ���ڴ���: %~1&attrib !h!h !s!s "%~1"&goto :eof