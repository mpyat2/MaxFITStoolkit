procedure PrintVersion(const Name: string);
begin
  WriteLn(Name, '  Maksym Pyatnytskyy  2018');
  WriteLn(GetVersionString(AnsiUpperCase(ParamStr(0))), ' ', {$I %FPCTARGETOS%}, ' ', {$I %DATE%}, ' ', {$I %TIME%});
  WriteLn;
end;
