@ECHO OFF
DEL eta_Aql_VisualData_2024_result.txt
dcdft.exe -t -l=0.1 -h=0.6 -n=10000 eta_Aql_VisualData_2024.tsv eta_Aql_VisualData_2024-FP-result.txt
IF ERRORLEVEL 1 GOTO :ERROR
notepad eta_Aql_VisualData_2024-FP-result.txt
GOTO :END
:ERROR
ECHO *** ERROR
:END
