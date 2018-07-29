@ECHO OFF
REM Stacking offset files using median mode into one master offset "_offset.fit"
IF [%1]==[] GOTO :NOPARAM
MakeStack /f /a /m=m /o=. /g=_offset /format=i16 %*
GOTO :EOF
:NOPARAM
ECHO MakeOffset creates master _offset.fit from a set of individual offset files
ECHO using median stacking.
ECHO Usage:
ECHO MakeOffset {offset_files}
