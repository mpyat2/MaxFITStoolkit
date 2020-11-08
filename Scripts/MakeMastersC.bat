@ECHO OFF
IF [%1]==[] GOTO :NOPARAM

REM master-dark
MakeStack /f /a /m=m /o=. /g=_dark_b                           /format=f32 dark*.fit
IF ERRORLEVEL 1 GOTO :ERROR

REM master-flat_dark (offset not removed!)
MakeStack /f /a /m=m /o=. /g=_fdark                            /format=f32 fdark*.fit
IF ERRORLEVEL 1 GOTO :ERROR

REM master-flat (removing flat_dark!)
REM fix /norm= value in case of overflow
MakeStack /f /a /m=m /o=. /g=_flat_fd /sub=_fdark /norm=10000  /format=f32 flat*.fit
IF ERRORLEVEL 1 GOTO :ERROR

REM hot-pixel-map
find_hot _dark_b.fit /n=_cosme /l=%1
IF ERRORLEVEL 1 GOTO :ERROR

GOTO :EOF

:NOPARAM
ECHO ===================== CMOS-style calibration frames ====================
ECHO MakeMastersC creates masters
ECHO   _dark_b.fit (dark + bias)
ECHO   _fdark.fit (flat dark: used to prepare _flat_fd.fit)
ECHO   _flat_fd.fit (flat - flat dark)
ECHO from sets of individual 
ECHO   dark*.fit
ECHO   fdark*.fit
ECHO   flat*.fit
ECHO files.
ECHO This script does NOT make master offset(bias)!
ECHO It also makes IRIS-compatible hot-pixel-map _cosme.lst.
ECHO The only parameter you should specify is a "hot-pixel threshold".
ECHO This parameter will be used while generating hot-pixel-map _cosme.lst.
ECHO You can always regenerate hot-pixel-map (using arbitrary threshold)
ECHO with "FindHot_b" command (assuming _dark_b.fit exists).
ECHO Warning! Since output frames have BITPIX=-32,
ECHO ECHO they maybe not fully compatible with IRIS!
ECHO Usage:
ECHO MakeMasters {threshold}
GOTO :EOF
:ERROR
ECHO ***** ERROR!!!
