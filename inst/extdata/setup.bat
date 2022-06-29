@echo off
REM  --> Check for permissions
>nul 2>&1 "%SYSTEMROOT%\system32\cacls.exe" "%SYSTEMROOT%\system32\config\system"

REM 
if '%errorlevel%' NEQ '0' (
    echo Requesting administrative privileges...
    goto UACPrompt
) else ( goto gotAdmin )

:UACPrompt
    echo Set UAC = CreateObject^("Shell.Application"^) > "%temp%\getadmin.vbs"
    set params = %*:"="
    echo UAC.ShellExecute "cmd.exe", "/c %~s0 %params%", "", "runas", 1 >> "%temp%\getadmin.vbs"

    "%temp%\getadmin.vbs"
    del "%temp%\getadmin.vbs"
    exit /B

:gotAdmin
    pushd "%CD%"
    CD /D "%~dp0"
::--------------------------------------

set sdb_path=%1
set sqlite_file=%2
set sdb_backup_gen=%3
set sdb_environ_file=%4

:: 1) Set environment variable

findstr /i "SDB_PATH" %sdb_environ_file%
if %ERRORLEVEL% == 1 (ECHO "SDB_PATH=%sdb_path%">>%sdb_environ_file%)


for %%a in (%sdb_path%) do for %%b in ("%%~dpa\.") do set "sdb_parent_path=%%~nxb"

if NOT exist %sdb_parent_path%\ (
	md %sdb_parent_path%
)

:: 2) create files and folders

for %%f in (backups upload_files move_files) do (
	if NOT exist %sdb_parent_path%\ (
		md "%sdb_parent_path%\\%f%"
	)
)

:: 3) copy database file

if NOT exist %sdb_path% (
	copy %sqlite_file% %sdb_path%
)

:: 4) set file permissions

icacls %sdb_parent_path% /grant Everyone:M

