@ECHO OFF
rem SET COMPILER="C:\PERSONAL\DC\DCC32" -UUnits\ -UFreeImage\ -Ebin-out\
SET COMPILER="c:\lazarus\fpc\3.0.2\bin\i386-win32\fpc.exe" -MDELPHI -Xg -FuUnits\ -FuFreeImage\ -FEbin-out\
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
%COMPILER% IPDAT\ipdat.lpr
IF ERRORLEVEL 1 GOTO :ERROR
ECHO .

REM apdat.exe: Convert AIJ photometry output to csv-file
%COMPILER% APDAT\apdat.lpr
IF ERRORLEVEL 1 GOTO :ERROR
ECHO .

REM fflip.exe: Flips 2-dimensional FITS
%COMPILER% FitsFlip\fflip.pas
IF ERRORLEVEL 1 GOTO :ERROR
ECHO .

REM fitscfa.exe: Splits CFA to color channels
%COMPILER% FitsCfa\FitsCfa.lpr
IF ERRORLEVEL 1 GOTO :ERROR
copy FitsCfa\FitsCfa.ini bin-out\
ECHO .

REM iconvraw.exe: RAW->FITS converter
%COMPILER% iconvraw\iconvraw.lpr
IF ERRORLEVEL 1 GOTO :ERROR
copy iconvraw\FreeImage.DLL bin-out\
ECHO .

REM ...
%COMPILER% Hello\HelloIRISFITS.pas
IF ERRORLEVEL 1 GOTO :ERROR
ECHO .

del FITSutils_src.zip
%A7z% a FITSutils_src.zip FitsHeader2\*.pas FitsFlip\*.pas IrisDateObs\*.pas IrisRename\*.pas IPDAT\ipdat.* IPDAT\testdata\*.dat APDAT\apdat.* APDAT\testdata\*.dat FitsCfa\* iconvraw\iconvraw.* iconvraw\FreeImage.dll Hello\*.pas Units\*.pas FreeImage\* Setup\*.iss Setup\output\dirinfo bin-out\dirinfo bin-out\fitsutils.ini clean.bat clean2.bat make.bat

GOTO :EOF

:ERROR
ECHO ***** ERROR! CANNOT COMPILE.

pause