@echo off
for %%i in ("*.pol")  do del %%i
for %%i in ("@*.fit") do del %%i
for %%i in ("#*.fit") do del %%i
for %%i in (*.lst)    do if /i not [%%i]==[_cosme.lst] del %%i
