@ECHO OFF
IF [%1]==[] GOTO :NOPARAM
SET SCRIPTNAME=preprocess.pgm
ECHO load _dark                                               > %SCRIPTNAME%
ECHO add _offset                                             >> %SCRIPTNAME%
ECHO save __dark_with_offset                                 >> %SCRIPTNAME%
ECHO load _flat                                              >> %SCRIPTNAME%
ECHO grey_flat                                               >> %SCRIPTNAME%
ECHO save __flat_neutral                                     >> %SCRIPTNAME%
ECHO pr light __dark_with_offset __flat_neutral ###c###_ %1  >> %SCRIPTNAME%
ECHO cosme_cfa2 ###c###_ c_light _cosme %1                   >> %SCRIPTNAME%
GOTO :EOF
:NOPARAM
ECHO MakePRscript creates IRIS "preprocess.pgm" script for a specified number
ECHO of light images.
ECHO To use the resulting script from within IRIS you should prepare
ECHO master-frames by "MakeMasters" command.
ECHO A sequence of your light images should have generic name "light"
ECHO with numbering from 1 to N_of_images, i.e.:
ECHO   light1.fit
ECHO   light2.fit
ECHO   [...]
ECHO Resulting sequence of calibrated images will be c_light*.fit
ECHO Usage:
ECHO MakePRscript {N_of_images}
