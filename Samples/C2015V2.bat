@ECHO OFF
REM ==========================================================================
REM BEGIN OF CONFIGURATION SECTION
REM ==========================================================================

REM This script converts .CR2 raw files to FITS files according IRIS naming convention.
REM FITS files are placed into IRIS working folder (specified by OUT variable)

REM Setting folders: OUT: output folder, LIGHT: science images folder, BIAS: bias frames folder, DARK: dark frames folder, FLAT: flat frames folder
SET BASE=.\2017-06-09(10)_C2015V2
SET OUT=C:\PERSONAL\SKY2
SET LIGHT=light
SET FLAT=flat
SET OFFSET=bias
SET DARK=dark
SET RAWEXT=.CR2

REM Time shift (from local to UTC) in seconds to be applied to DATE-OBS
SET TS=-10800
REM ADDITIONAL INFO
SET OBJECT=C/2015 V2
SET TELESCOP=Sky-Watcher 15075 OTAW

REM Commands
REM Converter: remove /E switch to disable TIME-OBS correction by exposure
SET CONVERTER_SWITCHES=/F /E
SET CONVERTER=iconvraw
REM FITS command-line header editor
SET FITSEDITOR=fihed

REM ==========================================================================
REM END OF CONFIGURATION SECTION
REM ==========================================================================

CLS
PAUSE
CLS

ECHO Cleaning output %OUT% ...
DEL "%OUT%\offset*.fit" "%OUT%\dark*.fit" "%OUT%\flat*.fit" "%OUT%\light*.fit"
IF ERRORLEVEL 1 GOTO :ERROR
ECHO .
ECHO .
ECHO .
ECHO Converting %OFFSET% ...
"%CONVERTER%" %CONVERTER_SWITCHES% "%BASE%\%OFFSET%\*%EAWEXT%" /O="%OUT%" /G=offset /TS"%TS%"
IF ERRORLEVEL 1 GOTO :ERROR
ECHO .
ECHO .
ECHO .
ECHO Converting %DARK% ...
"%CONVERTER%" %CONVERTER_SWITCHES% "%BASE%\%DARK%\*%EAWEXT%" /O="%OUT%" /G=dark /TS"%TS%"           
IF ERRORLEVEL 1 GOTO :ERROR
ECHO .
ECHO .
ECHO .
ECHO Converting %FLAT% ...
"%CONVERTER%" %CONVERTER_SWITCHES% "%BASE%\%FLAT%\*%EAWEXT%" /O="%OUT%" /G=flat /TS"%TS%"           
IF ERRORLEVEL 1 GOTO :ERROR
ECHO .
ECHO .
ECHO .
ECHO Converting %LIGHT% ...
"%CONVERTER%" %CONVERTER_SWITCHES% "%BASE%\%LIGHT%\*%EAWEXT%" /O="%OUT%" /G=light /TS"%TS%"         
IF ERRORLEVEL 1 GOTO :ERROR
ECHO .
ECHO .
ECHO .
ECHO Postprocessing ...
"%FITSEDITOR%" //SET /TELESCOP="%TELESCOP%" "%OUT%\flat*.fit"
IF ERRORLEVEL 1 GOTO :ERROR
"%FITSEDITOR%" //SET /OBJECT="%OBJECT%" /TELESCOP="%TELESCOP%" "%OUT%\light*.fit"
IF ERRORLEVEL 1 GOTO :ERROR

ECHO .
ECHO .
ECHO ==============================================================================
ECHO = CONVERTED!
ECHO ==============================================================================
PAUSE
GOTO :EOF

:ERROR
ECHO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ECHO ***** ERROR!
ECHO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

PAUSE
