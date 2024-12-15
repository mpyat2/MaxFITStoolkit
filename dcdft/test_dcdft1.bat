@ECHO OFF
DEL eta_Aql_VisualData_2024_result.txt
dcdft.exe -t -l=0.001983109791903 -h=0.658392450911802 -s=0.001322073194602 eta_Aql_VisualData_2024.tsv eta_Aql_VisualData_2024-FP-result1.txt
IF ERRORLEVEL 1 GOTO :ERROR
notepad eta_Aql_VisualData_2024-FP-result.txt
GOTO :END
:ERROR
ECHO *** ERROR
:END
