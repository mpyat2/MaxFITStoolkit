@ECHO OFF
REM Calibrate
IF [%1]==[] GOTO :NOPARAM
iconvfits /f /dark=_dark_b.fit /flat=_flat_fd.fit /cosme=_cosme.lst /format=f32 /g=calib /x=.fit %*
GOTO :EOF
:NOPARAM
ECHO ======================== CMOS-style calibration ========================
ECHO The script makes calibration of specified files.
ECHO Master FITS _dark_b.fit, _flat_fd.fit
ECHO and hot-pixel-map _cosme.lst must exist.
ECHO Master-offset is not used!
ECHO _dark_b.fit must contain offset sygnal.
ECHO _dark_fd.fit must be flat-dark subtracted.
ECHO Output: files calib[number].fit with BITPIX=-32
ECHO Warning! Since output frames have BITPIX=-32,
ECHO ECHO they maybe not fully compatible with IRIS!
ECHO Usage:
ECHO Calibrate {light_fits}
