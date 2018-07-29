@ECHO OFF
REM Stacking flat files using median mode into one master flat "_flat.fit"
REM with normalization of median values of individual files.
REM Normalization value is defined by /norm= parameter,
REM you can adjust it if needed.
REM _offset.fit must exist!
IF [%1]==[] GOTO :NOPARAM
MakeStack /f /a /m=m /o=. /g=_flat /norm=10000 /sub=_offset /format=i16 %*
GOTO :EOF
:NOPARAM
ECHO MakeFlat creates master _flat.fit from a set of individual flat files using 
ECHO median stacking with multiplicative normalization of medians.
ECHO Before running this script you should create _offset.fit using MakeOffset.bat
ECHO Master offset image (_offset.fit) will be subtacted from each of individual 
ECHO flat files before normalization and stacking.
ECHO Usage:
ECHO MakeFlat {flat_files}
