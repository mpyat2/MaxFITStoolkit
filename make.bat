@ECHO OFF
rem SET COMPILER="C:\PERSONAL\DC\DCC32" -UUnits\ -Ebin-out\
SET COMPILER="c:\lazarus\fpc\3.0.2\bin\i386-win32\fpc.exe" -MDELPHI -Xg -FuUnits\ -FEbin-out\
SET A7z="c:\Program Files\7-zip\7z.exe"

REM pfh.exe: Print FITS header: simple, nothing checked, single-file
REM %COMPILER% FitsHeader\pfh.pas
REM IF ERRORLEVEL 1 GOTO :ERROR
REM ECHO .

REM fihed.exe: Print/edit FITS header, multifile mode
%COMPILER% FitsHeader2\fihed.pas
IF ERRORLEVEL 1 GOTO :ERROR
ECHO .

REM idobs.exe: calculate mean DATE-OBS for set of files to be stacked 
%COMPILER% IrisDateObs\idobs.pas
IF ERRORLEVEL 1 GOTO :ERROR
ECHO .

REM iren.exe: rename files according to IRIS naming convention
%COMPILER% IrisRename\iren.pas
IF ERRORLEVEL 1 GOTO :ERROR
ECHO .

REM ipdat.exe: Convert IRIS photometry output to csv-file
%COMPILER% IPDAT\ipdat.pas
IF ERRORLEVEL 1 GOTO :ERROR
ECHO .

REM ...
%COMPILER% Hello\HelloIRISFITS.pas
IF ERRORLEVEL 1 GOTO :ERROR
ECHO .

del FITSutils_src.7z
%A7z% a FITSutils_src.7z FitsHeader2\*.pas IrisDateObs\*.pas IrisRename\*.pas IPDAT\*.pas IPDAT\testdata\*.dat Hello\*.pas Units\*.pas Setup\*.iss Setup\output\dirinfo bin-out\dirinfo clean.bat clean2.bat make.bat

GOTO :EOF

:ERROR
ECHO ***** ERROR! CANNOT COMPILE.

pause