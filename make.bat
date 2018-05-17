@ECHO OFF
REM
REM Use non-unicode Delphi or FPC [www.freepascal.org] in $DELPHI mode.
REM 
REM Should be compiled with NON-UNICODE 32bit Delphi Compiler!
REM SET COMPILER="C:\PERSONAL\DC\DCC32" -UUnits\ -Ebin-out\
REM
SET COMPILER="c:\personal\lazarus\fpc\3.0.2\bin\i386-win32\fpc.exe" -MDELPHI -Xg -FuUnits\ -FEbin-out\
REM
REM To produce source-code ZIP, run this script with "A" command-line option: make.bat A
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
copy iconvraw\librawmxwrapper_s.dll     bin-out\
rem copy iconvraw\librawmxwrapper_s_crt.dll bin-out\
ECHO .

REM MakeStack
%COMPILER% makestack\makestack.lpr
IF ERRORLEVEL 1 GOTO :ERROR
ECHO .

REM ...
REM %COMPILER% CalcMed\CalcMed.lpr
REM IF ERRORLEVEL 1 GOTO :ERROR
REM ECHO .

REM REM ...
REM %COMPILER% CalcSub\CalcSub.lpr
REM IF ERRORLEVEL 1 GOTO :ERROR
REM ECHO .

REM REM ...
REM %COMPILER% FindHot\FindHot.lpr
REM IF ERRORLEVEL 1 GOTO :ERROR
REM ECHO .

REM ...
%COMPILER% CFA2RGB\cfa2rgb.lpr
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
echo FitsHeader2\fihed.*            >  ziplist.txt
echo FitsFlip\fflip.*               >> ziplist.txt
echo IrisDateObs\idobs.*            >> ziplist.txt
echo IrisRename\iren.*              >> ziplist.txt
echo IPDAT\ipdat.*                  >> ziplist.txt
echo IPDAT\testdata\*.dat           >> ziplist.txt
echo APDAT\apdat.*                  >> ziplist.txt
echo APDAT\testdata\*.dat           >> ziplist.txt
echo FitsCfa\fitscfa.*              >> ziplist.txt
echo FitsRGB\fitsrgb.*              >> ziplist.txt
echo CFA2RGB\cfa2rgb.*              >> ziplist.txt
echo FitsStat\fitsstat.*            >> ziplist.txt
echo iconvraw\iconvraw.*            >> ziplist.txt
echo iconvraw\LibRawMxWrapper_s.dll >> ziplist.txt
echo iconvraw\LibRawMxWrapper_s_crt.dll >> ziplist.txt
echo makestack\makestack.*          >> ziplist.txt
echo Hello\*.pas                    >> ziplist.txt
echo Units\*.pas                    >> ziplist.txt
echo Scripts\*                      >> ziplist.txt
echo Samples\*                      >> ziplist.txt
echo Setup\*.iss                    >> ziplist.txt
echo Setup\output\dirinfo           >> ziplist.txt
echo bin-out\dirinfo                >> ziplist.txt
echo bin-out\fitsutils.ini          >> ziplist.txt
echo bin-out\fitsutils.txt          >> ziplist.txt
echo clean.bat                      >> ziplist.txt
echo clean2.bat                     >> ziplist.txt
echo make.bat                       >> ziplist.txt

del FITSutils_src.zip
%A7z% a FITSutils_src.zip @ziplist.txt

GOTO :END

:ERROR
ECHO ***** ERROR! CANNOT COMPILE.

:END
pause
