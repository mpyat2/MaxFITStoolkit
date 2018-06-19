{$APPTYPE CONSOLE}
{$MODE DELPHI}

program IPDAT;

uses
  SysUtils, CmdObj{, CmdObjStdSwitches}, Version, FitsUtilsHelp, CommonIni;

{$R *.res}

procedure PrintVersion;
begin
  WriteLn('IRIS photometry log parser  Maksym Pyatnytskyy  2017');
  WriteLn(GetVersionString(ParamStr(0)));
end;

procedure FileFormatError;
begin
  raise Exception.Create('File format error');
end;

procedure InvalidFloatingPointValueError(const Value: string);
begin
  raise Exception.Create(QuotedStr(Value) + ' is not a valid floating-point value');
end;

const PhotModeKey    = 'Phot mode';
const PixNumInnerKey = 'Pixel number in the inner circle =';
const PixNumBackKey  = 'Pixel number for background evaluation =';
const IntensityKey   = 'Intensity =';
const MagnitudeKey   = 'Magnitude =';
const BackLevelKey   = 'Background mean level = ';

const
  OutExt: array[Boolean] of string = ('.csv', '.txt');
  Delimiter: array[Boolean] of char = (';', ^I);

var
  InFileNamePrintable: string = '';
  TabbedOutput: Boolean = False;

procedure Progress(LineN: Integer);
var
  I: Integer;
  S: string;
begin
  Write(#13);
  S := '';
  for I := 1 to Length(InFileNamePrintable) + Length(IntToStr(MaxInt)) + 1 do begin
    S := S + ' ';
  end;
  Write(S);
  Write(#13);
  S := InFileNamePrintable + '(' + IntToStr(LineN) + ')';
  Write(S);
end;

procedure ProcessInput(const InFile: string; const OutFile: string; Quiet: boolean; Mode2: Boolean);
var
  InF: TextFile;
  OutF: TextFile;
  S, S2, TempS: string;
  PhotMode: Integer;
  X, Y: Integer;
  PixNumInner: Integer;
  PixNumBack: Integer;
  Intensity: Double;
  Magnitude: Double;
  BackLevel: Double;
  ValError: Integer;
  StringInBuffer: Boolean;
  N: Integer;
  StarN: integer;
  LineN: Integer;
  Mode2Header: string;
  Mode2Line: string;
begin
  AssignFile(InF, InFile);
  AssignFile(OutF, OutFile);
  try
    Reset(InF);
  except
    on E: Exception do begin
      WriteLn('**** Error while opening input file ', AnsiQuotedStr(InFile, '"'));
      WriteLn(E.Message);
      Halt(1);
    end;
  end;
  try
    Rewrite(OutF);
  except
    on E: Exception do begin
      WriteLn('**** Error while opening output file ', AnsiQuotedStr(OutFile, '"'));
      WriteLn(E.Message);
      Halt(1);
    end;
  end;
  try
    Mode2Header := '';
    Mode2Line := '';
    S := '';
    LineN := 0;
    StarN := 0;
    if not Mode2 then begin
      Write(OutF, 'StarN');
      Write(OutF, Delimiter[TabbedOutput], 'Magnitude');
      Write(OutF, Delimiter[TabbedOutput], '"Phot mode"');
      Write(OutF, Delimiter[TabbedOutput], 'X');
      Write(OutF, Delimiter[TabbedOutput], 'Y');
      Write(OutF, Delimiter[TabbedOutput], '"Pixel number in the inner circle"');
      Write(OutF, Delimiter[TabbedOutput], '"Pixel number for background evaluation"');
      Write(OutF, Delimiter[TabbedOutput], 'Intensity');
      Write(OutF, Delimiter[TabbedOutput], '"Background mean level"');
      WriteLn(OutF);
    end
    else begin
      //
    end;
    StringInBuffer := False;
    while not EOF(InF) or StringInBuffer do begin
      if not StringInBuffer then begin
        ReadLn(InF, S);
        S := TrimRight(S);
        Inc(LineN);
        if not Quiet then Progress(LineN);
      end
      else
        StringInBuffer := False;

      if AnsiSameStr(Copy(S, 1, Length(PhotModeKey)), PhotModeKey) then begin
        // Photometry block
        S := Copy(S, Length(PhotModeKey) + 1, MaxInt);
        N := Pos('-', S);
        if N = 0 then FileFormatError;
        TempS := Trim(Copy(S, 1, N - 1));
        PhotMode := StrToInt(TempS);
        S := Trim(Copy(S, N + 1, MaxInt));
        if (S = '') or (S[1] <> '(') or (S[Length(S)] <> ')') then FileFormatError;
        S := Copy(S, 2, Length(S) - 2);
        N := Pos(',', S);
        if N = 0 then FileFormatError;
        X := StrToInt(Trim(Copy(S, 1, N - 1)));
        Y := StrToInt(Trim(Copy(S, N + 1, MaxInt)));
        PixNumInner := 0;
        PixNumBack := 0;
        Magnitude := 0.0;
        Intensity := 0.0;
        BackLevel := 0.0;
        while not EOF(InF) do begin
          ReadLn(Inf, S);
          S := TrimRight(S);
          Inc(LineN);
          if not Quiet then Progress(LineN);
          if AnsiSameStr(Copy(S, 1, Length(PhotModeKey)), PhotModeKey) then begin
            StringInBuffer := True;
            Break;
          end;
          if AnsiSameStr(Copy(S, 1, Length(PixNumInnerKey)), PixNumInnerKey) then begin
            S := Copy(S, Length(PixNumInnerKey) + 1, MaxInt);
            PixNumInner := StrToInt(S);
          end;
          if AnsiSameStr(Copy(S, 1, Length(PixNumBackKey)), PixNumBackKey) then begin
            S := Copy(S, Length(PixNumBackKey) + 1, MaxInt);
            PixNumBack := StrToInt(S);
          end;
          if AnsiSameStr(Copy(S, 1, Length(IntensityKey)), IntensityKey) then begin
            S := Copy(S, Length(IntensityKey) + 1, MaxInt);
            N := Pos('-', S);
            if N = 0 then FileFormatError;
            TempS := Trim(Copy(S, 1, N - 1));
            Val(TempS, Intensity, ValError);
            if ValError <> 0 then InvalidFloatingPointValueError(TempS);
            S := Trim(Copy(S, N + 1, MaxInt));
            if AnsiSameStr(Copy(S, 1, Length(MagnitudeKey)), MagnitudeKey) then begin
              S := Copy(S, Length(MagnitudeKey) + 1, MaxInt);
              TempS := Trim(S);
              Val(TempS, Magnitude, ValError);
              if ValError <> 0 then InvalidFloatingPointValueError(TempS);
            end
            else
              FileFormatError;
          end;
          if AnsiSameStr(Copy(S, 1, Length(BackLevelKey)), BackLevelKey) then begin
            S := Copy(S, Length(BackLevelKey) + 1, MaxInt);
            Val(S, BackLevel, ValError);
            if ValError <> 0 then InvalidFloatingPointValueError(S);
          end;
        end;

        if not Mode2 then begin
          S2 := IntToStr(StarN);
          S2 := S2 + Delimiter[TabbedOutput] + FloatToStr(Magnitude);
          S2 := S2 + Delimiter[TabbedOutput] + IntToStr(PhotMode);
          S2 := S2 + Delimiter[TabbedOutput] + IntToStr(X);
          S2 := S2 + Delimiter[TabbedOutput] + IntToStr(Y);
          S2 := S2 + Delimiter[TabbedOutput] + IntToStr(PixNumInner);
          S2 := S2 + Delimiter[TabbedOutput] + IntToStr(PixNumBack);
          S2 := S2 + Delimiter[TabbedOutput] + FloatToStr(Intensity);
          S2 := S2 + Delimiter[TabbedOutput] + FloatToStr(BackLevel);
          WriteLn(OutF, S2);
        end
        else begin
          if StarN <> 0 then begin
            Mode2Header := Mode2Header + Delimiter[TabbedOutput];
            Mode2Line := Mode2Line + Delimiter[TabbedOutput];
          end;
          Mode2Header := Mode2Header + 'Star' + IntToStr(StarN);
          Mode2Line := Mode2Line + FloatToStr(Magnitude);
        end;
        Inc(StarN);
      end;
    end;
    if Mode2 then begin
      WriteLn(OutF, Mode2Header);
      WriteLn(OutF, Mode2Line);
    end;
    if not Quiet then WriteLn;
    CloseFile(OutF);
    CloseFile(InF);
  except
    on E: Exception do begin
      WriteLn;
      WriteLn('**** Error:');
      WriteLn(E.Message);
      Halt(1);
    end;
  end;
  if not Quiet then
    WriteLn(ExtractFileName(OutFile), ' created.');
end;

var
  Quiet: Boolean;
  PrintVer: Boolean;
  PrintHlp: Boolean;
  InputFileName: string;
  OutputFileName: string;
  Mode2: Boolean;
  ParamN: Integer;
  S: string;

begin
  PrintVer := CmdObj.CmdLine.IsCmdOption('V') or CmdObj.CmdLine.IsCmdOption('version');
  PrintHlp := CmdObj.CmdLine.IsCmdOption('?') or CmdObj.CmdLine.IsCmdOption('H') or CmdObj.CmdLine.IsCmdOption('help');
  if PrintVer then begin
    PrintVersion;
    if (CmdObj.CmdLine.FileCount = 0) and not PrintHlp then Halt(1);
  end;
  if PrintHlp or ((CmdObj.CmdLine.FileCount <> 2) and (CmdObj.CmdLine.FileCount <> 1)) then begin
    PrintHelp;
    Halt(1);
  end;

  Quiet := False;
  TabbedOutput := False;
  Mode2 := False;

  for ParamN := 1 to CmdObj.CmdLine.ParamCount do begin
    S := CmdObj.CmdLine.ParamStr(ParamN);
    if CmdObj.CmdLine.FirstCharIsSwitch(S) then begin
      if Length(S) = 1 then begin
        WriteLn('**** Invalid command-line parameter: ' + S);
        Halt(1);
      end;
      if CmdObj.CmdLine.ParamIsKey(S, 'V') or CmdObj.CmdLine.ParamIsKey(S, 'version') then begin
        // nothing: already processed.
      end
      else
      if CmdObj.CmdLine.ParamIsKey(S, 'Q') then
        Quiet := True
      else
      if CmdObj.CmdLine.ParamIsKey(S, 'T') then
        TabbedOutput := True
      else
      if CmdObj.CmdLine.ParamIsKey(S, '2') then
        Mode2 := CmdObj.CmdLine.IsCmdOption('2')
      else begin
        WriteLn('**** Invalid command-line parameter: ' + S);
        Halt(1);
      end;
    end;
  end;

  InputFileName := ExpandFileName(CmdObj.CmdLine.ParamFile(1));
  if ExtractFileExt(InputFileName) = '' then
    InputFileName := ChangeFileExt(InputFileName, '.dat');
  InFileNamePrintable := ExtractFileName(InputFileName);
  if CmdObj.CmdLine.FileCount < 2 then begin
    OutputFileName := ChangeFileExt(InputFileName, OutExt[TabbedOutput]);
  end
  else begin
    OutputFileName := ExpandFileName(CmdObj.CmdLine.ParamFile(2));
    if ExtractFileExt(OutputFileName) = '' then OutputFileName := OutputFileName + OutExt[TabbedOutput];
  end;
  if AnsiCompareFileName(InputFileName, OutputFileName) = 0 then begin
    WriteLn('Output file cannot be the same as input one.');
    Halt(1);
  end;
  ProcessInput(InputFileName, OutputFileName, Quiet, Mode2);
end.

