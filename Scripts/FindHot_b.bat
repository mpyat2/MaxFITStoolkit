@ECHO OFF
REM Hot pixels (IRIS-compatible)
REM _dark.fit must exist!
IF [%1]==[] GOTO :NOPARAM
find_hot _dark_b.fit /n=_cosme /l=%1
GOTO :EOF
:NOPARAM
ECHO FindHot_b creates "hot-pixel-map" _cosme.lst (IRIS format) 
ECHO using master _dark_b.fit
ECHO Before running this script you should create _dark_b.fit using MakeDark_b.bat
ECHO Usage:
ECHO FindHot_b {threshold}
