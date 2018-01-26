@ECHO OFF
rem SET COMPILER="C:\PERSONAL\DC\DCC32" -UUnits\ -UFreeImage\ -Ebin-out\
SET COMPILER="c:\lazarus\fpc\3.0.2\bin\i386-win32\fpc.exe" -MDELPHI -Xg -FuUnits\ -FuFreeImage\ -FEbin-out\
SET A7z="c:\Program Files\7-zip\7z.exe"

REM fihed.exe: Print/edit FITS header, multifile mode
%COMPILER% FitsHeader2\fihed.lpr
IF ERRORLEVEL 1 GOTO :ERROR
ECHO .

REM idobs.exe: calculate mean DATE-OBS for set of files to be stacked 
%COMPILER% IrisDateObs\idobs.lpr
IF ERRORLEVEL 1 GOTO :ERROR
ECHO .

REM iren.exe: rename files according to IRIS naming convention
%COMPILER% IrisRename\iren.lpr
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
%COMPILER% FitsFlip\fflip.lpr
IF ERRORLEVEL 1 GOTO :ERROR
ECHO .

REM fitscfa.exe: Splits CFA to color channels
%COMPILER% FitsCfa\FitsCfa.lpr
IF ERRORLEVEL 1 GOTO :ERROR
copy FitsCfa\FitsCfa.ini bin-out\
ECHO .

REM fitsrgb.exe: Splits RGB to color channels
%COMPILER% FitsRGB\FitsRGB.lpr
IF ERRORLEVEL 1 GOTO :ERROR
ECHO .

REM iconvraw.exe: RAW->FITS converter
%COMPILER% iconvraw\iconvraw.lpr
IF ERRORLEVEL 1 GOTO :ERROR
copy iconvraw\libraw.dll bin-out\
copy iconvraw\librawmxwrapper.dll bin-out\
ECHO .

REM ...
%COMPILER% CalcMed\CalcMed.lpr
IF ERRORLEVEL 1 GOTO :ERROR
ECHO .

REM ...
%COMPILER% CalcSub\CalcSub.lpr
IF ERRORLEVEL 1 GOTO :ERROR
ECHO .

REM ...
%COMPILER% FindHot\FindHot.lpr
IF ERRORLEVEL 1 GOTO :ERROR
ECHO .

REM ...
%COMPILER% FITSstat\FITSstat.lpr
IF ERRORLEVEL 1 GOTO :ERROR
ECHO .

REM ...
%COMPILER% Hello\HelloIRISFITS.pas
IF ERRORLEVEL 1 GOTO :ERROR
ECHO .

IF "%1"=="A" GOTO :ARCHIVE
GOTO :END
:ARCHIVE
del FITSutils_src.zip
%A7z% a FITSutils_src.zip FitsHeader2\fihed.* FitsFlip\fflip.* IrisDateObs\idobs.* IrisRename\iren.* IPDAT\ipdat.* IPDAT\testdata\*.dat APDAT\apdat.* APDAT\testdata\*.dat FitsCfa\fitscfa.* FitsRGB\fitsrgb.* iconvraw\iconvraw.* iconvraw\FreeImage.dll Hello\*.pas Units\*.pas FreeImage\* Setup\*.iss Setup\output\dirinfo bin-out\dirinfo bin-out\fitsutils.ini Samples\* bin-out\fitsutils.txt clean.bat clean2.bat make.bat

GOTO :END

:ERROR
ECHO ***** ERROR! CANNOT COMPILE.

:END
pause
