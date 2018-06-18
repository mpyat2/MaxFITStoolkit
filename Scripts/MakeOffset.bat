@ECHO OFF
REM Stacking offset files using median mode into one master offset "_offset.fit"
IF [%1]==[] GOTO :NOPARAM
MakeStack /F /A /m=m /o=. /g=_offset %*
GOTO :EOF
:NOPARAM
ECHO MakeOffset creates master _offset.fit from a set of individual offset files.
ECHO Usage:
ECHO MakeOffset {offset_files_filemask}
