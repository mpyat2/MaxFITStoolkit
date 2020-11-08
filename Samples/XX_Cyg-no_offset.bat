@ECHO OFF

REM Conveerts FITS to IRIS format (signed integer)

REM Configuration -- begin

SET OUT=C:\PERSONAL\SKY2
SET BASEDIR=.
SET LIGHT_FILES=%BASEDIR%\light1\*.fits %BASEDIR%\light2\*.fits %BASEDIR%\light3\*.fits
SET FLAT_FILES=%BASEDIR%\flat\*.fits
SET DARK_FILES=%BASEDIR%\dark\*.fits
SET FLAT_DARK_FILES=%BASEDIR%\flat_dark\*.fits

SET OBJECT=XX Cyg
SET TELESCOP=Sky-Watcher 15075 OTAW
REM DATE-OBS fix!
SET IDFIX_PARAMS=/TS0

REM Configuration -- end

CLS
PAUSE
CLS

ECHO .
ECHO Cleaning output %OUT% ...
DEL "%OUT%\dark*.fit" "%OUT%\flat*.fit" "%OUT%\fdark*.fit" "%OUT%\light*.fit"
REM IF ERRORLEVEL 1 GOTO :ERROR
rem PAUSE
ECHO .
ECHO .
ECHO .
ECHO Converting %DARK% ...
iren %DARK_FILES% /O="%OUT%" /G=dark /X=.fit
rem idfix "%OUT%\dark*" %IDFIX_PARAMS%
IF ERRORLEVEL 1 GOTO :ERROR
rem PAUSE
ECHO .
ECHO .
ECHO .
ECHO Converting %FLAT_DARK% ...
iren %FLAT_DARK_FILES% /O="%OUT%" /G=fdark /X=.fit
rem idfix "%OUT%\dark*" %IDFIX_PARAMS%
IF ERRORLEVEL 1 GOTO :ERROR
rem PAUSE
ECHO .
ECHO .
ECHO .
ECHO Converting %FLAT% ...
iren %FLAT_FILES% /O="%OUT%" /G=flat /X=.fit
FIHED //SET /TELESCOP="%TELESCOP%" "%OUT%\flat*"
rem idfix "%OUT%\flat*" %IDFIX_PARAMS%
IF ERRORLEVEL 1 GOTO :ERROR
rem PAUSE
ECHO .
ECHO .
ECHO .
ECHO Converting %LIGHT% ...
iren %LIGHT_FILES% /O="%OUT%" /G=light /X=.fit
IF ERRORLEVEL 1 GOTO :ERROR
FIHED //SET /OBJECT="%OBJECT%" /TELESCOP="%TELESCOP%" "%OUT%\light*"
idfix "%OUT%\light*" %IDFIX_PARAMS%
IF ERRORLEVEL 1 GOTO :ERROR
rem PAUSE
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
