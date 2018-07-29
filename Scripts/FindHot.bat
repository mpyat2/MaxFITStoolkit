@ECHO OFF
REM Hot pixels (IRIS-compatible)
REM _dark.fit must exist!
IF [%1]==[] GOTO :NOPARAM
find_hot _dark.fit /n=_cosme /l=%1
GOTO :EOF
:NOPARAM
ECHO FindHot creates "hot-pixel-map" _cosme.lst (IRIS format) 
ECHO using master _dark.fit
ECHO Before running this script you should create _dark.fit using MakeDark.bat
ECHO Usage:
ECHO FindHot {threshold}
