@echo off
REM Wrapper til GnuCOBOL på Windows
REM Sætter include- og lib-stier automatisk

set "COBINC=C:\Program Files\GnuCOBOL\include"
set "COBLIB=C:\Program Files\GnuCOBOL\lib"

REM Tilføj bin-mappen til PATH midlertidigt
set "PATH=C:\Program Files\GnuCOBOL\bin;%PATH%"

cobc -I"%COBINC%" -L"%COBLIB%" %* 
pause 