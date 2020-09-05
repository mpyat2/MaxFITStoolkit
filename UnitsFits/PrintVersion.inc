procedure PrintVersion(const Name: string);
begin
  WriteLn(Name, ': FITS Utilities Package  Maksym Pyatnytskyy  2018-2020');
  WriteLn(GetVersionString(AnsiUpperCase(ParamStr(0))), ' ', {$I %FPCTARGETOS%}, ' ', {$I %DATE%}, ' ', {$I %TIME%});
  WriteLn;
end;
