@ECHO OFF
REM Calibrate
IF [%1]==[] GOTO :NOPARAM
iconvfits /f /offset=_offset.fit /dark=_dark.fit /flat=_flat.fit /cosme=_cosme.lst /format=f32 /g=calib /x=.fit %*
GOTO :EOF
:NOPARAM
ECHO ==================  Standard (IRIS-style) calibration  =================
ECHO The script makes calibration of the specified files.
ECHO Master FITS _offset.fit, _dark.fit, _flat.fit
ECHO and hot-pixel-map _cosme.lst must exist.
ECHO Output: files calib[number].fit with BITPIX=-32
ECHO Warning! Since output frames have BITPIX=-32,
ECHO ECHO they maybe not fully compatible with IRIS!
ECHO Usage:
ECHO Calibrate {light_fits}
