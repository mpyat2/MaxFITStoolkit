@ECHO OFF
REM Setting folders: OUT: output folder, LIGHT: science images, BIAS: bias frames, DARK: dark frames, FLAT: flat frames
SET BASE=E:\SKY\___Photometry\source\20171208
SET OUT=C:\PERSONAL\SKY2
SET LIGHT=light_DODRA
SET FLAT=flat2
SET OFFSET=bias
SET DARK=dark
SET RAWEXT=.CR2

REM Time shift (from local to UTC) in seconds to be applied to DATE-OBS
SET TS=-7200
REM ADDITIONAL INFO
SET OBJECT=DO Dra
SET TELESCOP=Sky-Watcher 15075 OTAW

REM Commands. 
REM Converter: remove /E switch to disable TIME-OBS correction by exposure
SET CONVERTER_SWITCHES=/F /E
SET CONVERTER=iconvraw
REM FITS command-line header editor
SET FITSEDITOR=fihed

REM ==========================================================================
REM END OF CONFIGURATION SECTION
REM ==========================================================================

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
