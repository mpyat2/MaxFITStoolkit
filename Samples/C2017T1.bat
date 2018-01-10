@ECHO OFF
REM ==========================================================================
REM BEGIN OF CONFIGURATION SECTION
REM ==========================================================================

REM This script converts .CR2 raw files to FITS files according IRIS naming convention.
REM FITS files are placed into IRIS working folder (specified by OUT variable)

REM Setting folders: OUT: output folder, LIGHT: science images folder, BIAS: bias frames folder, DARK: dark frames folder, FLAT: flat frames folder
SET BASE=.\
SET OUT=C:\PERSONAL\SKY2
SET LIGHT=comet1
SET FLAT=flat
SET OFFSET=bias
SET DARK=dark
SET RAWEXT=.CR2

REM Time shift (from local to UTC) in seconds to be applied to DATE-OBS
REM **** WARNING! CHECK TIME SHIFT BEFORE RUN!
SET TS=-7200
REM Additional info to be written into FITS header
SET OBJECT=C/2017 T1
SET TELESCOP=Sky-Watcher 15075 OTAW

REM Commands
SET CONVERTER_SWITCHES=/F
SET CONVERTER=iconvraw

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
"%CONVERTER%" %CONVERTER_SWITCHES% "%BASE%\%OFFSET%\*%RAWEXT%" /O="%OUT%" /G=offset /TS"%TS%"
IF ERRORLEVEL 1 GOTO :ERROR
ECHO .
ECHO .
ECHO .
ECHO Converting %DARK% ...
"%CONVERTER%" %CONVERTER_SWITCHES% "%BASE%\%DARK%\*%RAWEXT%" /O="%OUT%" /G=dark /TS"%TS%"           
IF ERRORLEVEL 1 GOTO :ERROR
ECHO .
ECHO .
ECHO .
ECHO Converting %FLAT% ...
"%CONVERTER%" %CONVERTER_SWITCHES% "%BASE%\%FLAT%\*%RAWEXT%" /O="%OUT%" /G=flat /TS"%TS%" /$TELESCOP="%TELESCOP%"
IF ERRORLEVEL 1 GOTO :ERROR
ECHO .
ECHO .
ECHO .
ECHO Converting %LIGHT% ...
"%CONVERTER%" %CONVERTER_SWITCHES% "%BASE%\%LIGHT%\*%RAWEXT%" /O="%OUT%" /G=light /TS"%TS%" /$TELESCOP="%TELESCOP%"  /$OBJECT="%OBJECT%"
IF ERRORLEVEL 1 GOTO :ERROR
ECHO .
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
