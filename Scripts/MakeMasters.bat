@ECHO OFF
IF [%1]==[] GOTO :NOPARAM

REM master-offset
MakeStack /f /a /m=m /o=. /g=_offset                           /format=i16 offset*.fit
IF ERRORLEVEL 1 GOTO :ERROR

REM master-dark
MakeStack /f /a /m=m /o=. /g=_dark   /sub=_offset              /format=i16 dark*.fit
IF ERRORLEVEL 1 GOTO :ERROR

REM master-flat
REM fix /norm= value in case of overflow
MakeStack /f /a /m=m /o=. /g=_flat   /sub=_offset /norm=10000  /format=i16 flat*.fit
IF ERRORLEVEL 1 GOTO :ERROR

REM hot-pixel-map
find_hot _dark.fit /n=_cosme /l=%1
IF ERRORLEVEL 1 GOTO :ERROR

GOTO :EOF

:NOPARAM
ECHO ===================== IRIS-style calibration frames ====================
ECHO MakeMasters creates masters
ECHO   _offset.fit
ECHO   _dark.fit
ECHO   _flat.fit
ECHO from sets of individual 
ECHO   offset*.fit
ECHO   dark*.fit
ECHO   flat*.fit
ECHO files.
ECHO It also makes IRIS-compatible hot-pixel-map _cosme.lst.
ECHO The only parameter you should specify is a "hot-pixel threshold".
ECHO This parameter will be used while generating hot-pixel-map _cosme.lst.
ECHO You can always regenerate hot-pixel-map (using arbitrary threshold)
ECHO with "FindHot" command (assuming _dark.fit exists).
ECHO Usage:
ECHO MakeMasters {threshold}
GOTO :EOF
:ERROR
ECHO ***** ERROR!!!
