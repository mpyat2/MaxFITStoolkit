{$APPTYPE CONSOLE}

program APDAT;

uses
  Windows, SysUtils, Classes, CmdObj{, CmdObjStdSwitches}, CommonIni, Math;

procedure PrintVersion;
begin
  WriteLn('AIJ photometry log parser  Maksym Pyatnytskyy  2017');
  WriteLn('Version 2017.12.05.01');
end;

procedure PrintHelp;
begin
  WriteLn('Usage:');
  WriteLn(ExtractFileName(ParamStr(0)), ' [options] input_filename[.dat] [output_filename[.csv|.txt]]');
  WriteLn;
  WriteLn('  /T  Tabbed output instead of CSV (.txt output file)');
  WriteLn('  /Q  Quiet mode');
  WriteLn('  /V  Print version');
  WriteLn('  /H  Print this help and halt');
end;

//procedure FileFormatError;
//begin
//  raise Exception.Create('File format error');
//end;

procedure InvalidFloatingPointValueError(const Value: string);
begin
  raise Exception.Create(QuotedStr(Value) + ' is not a valid floating-point value');
end;

const
  JD_UTC_header = 'JD_UTC';
  SourceMinusSky_header = 'Source-Sky_';

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

function WordCount1(const S: string; const WordDelims: TSysCharSet): Integer;
var
  I: Integer;
begin
  Result := 0;
  if S = '' then Exit;
  Result := 1;
  for I := 1 to Length(S) do
    if S[I] in WordDelims then Inc(Result);
end;

function ExtractWord1(N: Integer; const S: string; const WordDelims: TSysCharSet): string;
var
  I, NN: Integer;
begin
  Result := '';
  if S = '' then Exit;
  NN := 1;
  for I := 1 to Length(S) do begin
    if S[I] in WordDelims then
      Inc(NN)
    else begin
      if NN > N then Exit;
      if NN = N then
        Result := Result + S[I];
    end;
  end;
end;


procedure ProcessInput(const InFile: string; const OutFile: string; Quiet: boolean);
var
  LineN: Integer;
  InputList: TStringList;
  OutF: TextFile;
  S, TempS, TempS2: string;
  ValError: Integer;
  TempF: Double;
  I, II: Integer;
  JD_UTC_Pos: Integer;
  IntensityPos: array of Integer;
begin
  try
    LineN := 0;
    JD_UTC_Pos := -1;
    IntensityPos := nil;
    AssignFile(OutF, OutFile);
    Rewrite(OutF);
    try
      InputList := TStringList.Create;
      try
        InputList.LoadFromFile(InFile);
        if InputList.Count > 0 then begin
          Inc(LineN);
          if not Quiet then Progress(LineN);
          TempS := InputList[0];
          for II := 1 to WordCount1(TempS, [^I]) do begin
            TempS2 := ExtractWord1(II, TempS, [^I]);
            if AnsiSameText(TempS2, JD_UTC_header) then begin
              JD_UTC_Pos := II;
            end
            else
            if AnsiSameText(Copy(TempS2, 1, Length(SourceMinusSky_header)), SourceMinusSky_header) then begin
              SetLength(IntensityPos, Length(IntensityPos) + 1);
              IntensityPos[Length(IntensityPos) - 1] := II;
            end;
          end;
          S := '';
          if JD_UTC_Pos >= 0 then
            S := S + ExtractWord1(JD_UTC_Pos, TempS, [^I]);
          for II := 0 to Length(IntensityPos) - 1 do begin
            if S <> '' then S := S + Delimiter[TabbedOutput];
            S := S + 'Magnitude(' + ExtractWord1(IntensityPos[II], TempS, [^I]) + ')';
          end;
          WriteLn(OutF, S);
          for I := 1 to InputList.Count - 1 do begin
            Inc(LineN);
            if not Quiet then Progress(LineN);
            TempS := InputList[I];
            S := '';
            if (JD_UTC_Pos >= 0) then begin
              TempS2 := Trim(ExtractWord1(JD_UTC_Pos, TempS, [^I]));
              Val(TempS2, TempF, ValError);
              if ValError <> 0 then
                InvalidFloatingPointValueError(TempS2);
              S := S + FloatToStr(TempF);
            end;
            for II := 0 to Length(IntensityPos) - 1 do begin
              if S <> '' then S := S + Delimiter[TabbedOutput];
              TempS2 := Trim(ExtractWord1(IntensityPos[II], TempS, [^I]));
              if TempS2 <> '' then begin
                Val(TempS2, TempF, ValError);
                if (ValError = 0) and (TempF > 0) then
                  S := S + FloatToStr(-2.5 * Log10(TempF))
                else
                  S := S + AnsiQuotedStr('ERR:[' + TempS2 + ']', '"');
              end;
            end;
            WriteLn(OutF, S);
          end;
        end;
      finally
        FreeAndNil(InputList);
      end;
    finally
      CloseFile(OutF);
    end;
    if not Quiet then begin
      WriteLn;
      WriteLn(ExtractFileName(OutFile), ' created.');
    end;
  except
    on E: Exception do begin
      WriteLn;
      WriteLn('**** Error:');
      WriteLn(E.Message);
      Halt(1);
    end;
  end;
end;

var
  Quiet: Boolean;
  PrintVer: Boolean;
  PrintHlp: Boolean;
  InputFileName: string;
  OutputFileName: string;

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
  Quiet := CmdObj.CmdLine.IsCmdOption('Q');
  TabbedOutput := CmdObj.CmdLine.IsCmdOption('T');
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
  ProcessInput(InputFileName, OutputFileName, Quiet);
end.

