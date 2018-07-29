@ECHO OFF
REM Stacking dark files using median mode into one master dark "_dark.fit"
REM _offset.fit must exist!
IF [%1]==[] GOTO :NOPARAM
MakeStack /f /a /m=m /o=. /g=_dark /sub=_offset /format=i16 %*
GOTO :EOF
:NOPARAM
ECHO MakeDark creates master _dark.fit from a set of individual dark files using 
ECHO median stacking.
ECHO Before running this script you should create _offset.fit using MakeOffset.bat
ECHO Master offset image (_offset.fit) will be subtacted from each of individual 
ECHO flat files before stacking.
ECHO Usage:
ECHO MakeDark {dark_files}
