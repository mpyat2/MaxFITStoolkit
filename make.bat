@ECHO OFF
REM
REM Use FPC [www.freepascal.org] in $DELPHI mode.
REM 
SET COMPILE_PARAMS=-MDELPHI -Xg -FuUnitsCommon\;UnitsFits\ -FiUnitsFits\ -FEbin-out\
SET COMPILER="c:\personal\lazarus\fpc\3.0.4\bin\i386-win32\fpc.exe"     %COMPILE_PARAMS% -FUbin-out\unit32
SET COMPIL64="C:\Personal\lazarus64\fpc\3.0.4\bin\x86_64-win64\fpc.exe" %COMPILE_PARAMS% -FUbin-out\unit64
REM
REM To make source-code ZIP, run this script with "AA" command-line option: make.bat AA
REM WARNING: run clean.bat or clean2.bat first
SET A7z="c:\Program Files\7-zip\7z.exe"
IF "%1"=="AA" GOTO :ARCHIVE
IF "%1"=="64" GOTO :X64
GOTO :COMPILE

:X64
SET COMPILER=%COMPIL64%

:COMPILE

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
IF "%COMPIL64%"=="%COMPILER%" GOTO :LIBRAW64
copy iconvraw\LibRawMxWrapper_s_crt_0_19_0-beta1.dll bin-out\
copy iconvraw\librawmxwrapper_s_crt.dll              bin-out\
:LIBRAW64
IF "%COMPIL64%"=="%COMPILER%" copy iconvraw\librawmxwrapper_s_crt_64.dll bin-out\
IF ERRORLEVEL 1 GOTO :ERROR
ECHO .

REM MakeStack
%COMPILER% makestack\makestack.lpr
IF ERRORLEVEL 1 GOTO :ERROR
IF "%COMPIL64%"=="%COMPILER%" GOTO :SKIP3264
REN bin-out\makestack.exe makestack32.exe
IF ERRORLEVEL 1 GOTO :ERROR
%COMPIL64% makestack\makestack.lpr
IF ERRORLEVEL 1 GOTO :ERROR
REN bin-out\makestack.exe makestack64.exe
IF ERRORLEVEL 1 GOTO :ERROR
:SKIP3264
ECHO .

REM CFA2RGB
%COMPILER% CFA2RGB\cfa2rgb.lpr
IF ERRORLEVEL 1 GOTO :ERROR
ECHO .

REM FitsStat
%COMPILER% FITSstat\FITSstat.lpr
IF ERRORLEVEL 1 GOTO :ERROR
ECHO .

REM ...
%COMPILER% Hello\HelloIRISFITS.pas
IF ERRORLEVEL 1 GOTO :ERROR
ECHO .

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
echo iconvraw\LibRawMxWrapper*.dll  >> ziplist.txt
echo makestack\makestack.*          >> ziplist.txt
echo Hello\*.pas                    >> ziplist.txt
echo UnitsCommon\*.pas              >> ziplist.txt
echo UnitsFits\*.pas                >> ziplist.txt
echo UnitsFits\*.inc                >> ziplist.txt
echo Scripts\*                      >> ziplist.txt
echo Samples\*                      >> ziplist.txt
echo Setup\*.iss                    >> ziplist.txt
echo Setup\output\dirinfo           >> ziplist.txt
echo bin-out\dirinfo                >> ziplist.txt
echo bin-out\fitsutils.ini          >> ziplist.txt
echo bin-out\fitsutils.txt          >> ziplist.txt
echo bin-out\unit32\dirinfo         >> ziplist.txt
echo bin-out\unit32\dirinfo         >> ziplist.txt
echo clean.bat                      >> ziplist.txt
echo clean2.bat                     >> ziplist.txt
echo make.bat                       >> ziplist.txt
echo LICENSE                        >> ziplist.txt

del FITSutils_src.zip
%A7z% a FITSutils_src.zip @ziplist.txt
REM LibrawWrapper sources
%A7z% a LibRawWrapper_src.zip -r LibRawWrapper\* LibRawWrapper64\*

GOTO :END

:ERROR
ECHO ***** ERROR! CANNOT COMPILE.

:END
pause
