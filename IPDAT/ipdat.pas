{$APPTYPE CONSOLE}

program IPDAT;

uses
  Windows, SysUtils, CmdObj, CmdObjStdSwitches;

procedure PrintVersion;
begin
  WriteLn('IRIS photometry log parser  Maksym Pyatnytskyy  2017');
  WriteLn('Version 2017.10.09.01');
end;
  
procedure PrintHelp;
begin
  WriteLn('Usage:');
  WriteLn(ExtractFileName(ParamStr(0)), ' [options] input_filename[.dat]  [output_filename[.csv]]');
  WriteLn;
  WriteLn('  -q  Quiet mode');
  WriteLn('  -v  Print version');
  WriteLn('  -h  Print this help and halt');
end;
  
procedure FileFormatError(N: integer);
begin
  raise Exception.Create('Line ' + IntToStr(N) + ': File format error');
end;

const PhotModeKey    = 'Phot mode';
const PixNumInnerKey = 'Pixel number in the inner circle =';
const PixNumBackKey  = 'Pixel number for background evaluation =';
const IntensityKey   = 'Intensity =';
const MagnitudeKey   = 'Magnitude =';
const BackLevelKey   = 'Background mean level = ';

var
  InFileNamePrintable: string = '';

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
  
procedure ProcessInput(const InFile: string; const OutFile: string; Quiet: boolean); 
var
  InF: TextFile;
  OutF: TextFile;
  S, S2, TempS: string;
  TempN: Integer;
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
    S := '';
    LineN := 0;
    StarN := 0;
    WriteLn(OutF, 'StarN;"Phot mode";X;Y;"Pixel number in the inner circle";"Pixel number for background evaluation";Intensity;Magnitude;"Background mean level"');
    StringInBuffer := False;
    while not EOF(InF) or StringInBuffer do begin
      if not StringInBuffer then begin
        ReadLn(InF, S);
        Inc(LineN);
        if not Quiet then Progress(LineN);
      end    
      else
        StringInBuffer := False;
    
      if AnsiSameStr(Copy(S, 1, Length(PhotModeKey)), PhotModeKey) then begin
        // Photometry block
        S2 := IntToStr(StarN);
        S := Copy(S, Length(PhotModeKey) + 1, MaxInt);
        N := Pos('-', S);
        if N = 0 then FileFormatError(LineN);
        TempS := Trim(Copy(S, 1, N - 1));
        TempN := StrToInt(TempS);
        S2 := S2 + ';' + IntToStr(TempN);
        S := Trim(Copy(S, N + 1, MaxInt));
        if (S = '') or (S[1] <> '(') or (S[Length(S)] <> ')') then FileFormatError(LineN);
        S := Copy(S, 2, Length(S) - 2);
        N := Pos(',', S);
        if N = 0 then FileFormatError(LineN);
        TempN := StrToInt(Trim(Copy(S, 1, N - 1)));
        S2 := S2 + ';' + IntToStr(TempN);
        TempN := StrToInt(Trim(Copy(S, N + 1, MaxInt)));
        S2 := S2 + ';' + IntToStr(TempN);
        PixNumInner := 0;
        PixNumBack := 0;
        Magnitude := 0.0;
        Intensity := 0.0;
        BackLevel := 0.0;
        while not EOF(InF) do begin
          ReadLn(Inf, S);
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
            if N = 0 then FileFormatError(LineN);
            TempS := Trim(Copy(S, 1, N - 1));
            Val(TempS, Intensity, ValError);
            if ValError <> 0 then FileFormatError(LineN);
            S := Trim(Copy(S, N + 1, MaxInt));
            if AnsiSameStr(Copy(S, 1, Length(MagnitudeKey)), MagnitudeKey) then begin
              S := Copy(S, Length(MagnitudeKey) + 1, MaxInt);
              TempS := Trim(S);
              Val(TempS, Magnitude, ValError);
              if ValError <> 0 then FileFormatError(LineN);
            end;
          end;
          if AnsiSameStr(Copy(S, 1, Length(BackLevelKey)), BackLevelKey) then begin
            S := Copy(S, Length(BackLevelKey) + 1, MaxInt);
            Val(S, BackLevel, ValError);
            if ValError <> 0 then FileFormatError(LineN);
          end;
        end;
        S2 := S2 + ';' + IntToStr(PixNumInner);
        S2 := S2 + ';' + IntToStr(PixNumBack);
        S2 := S2 + ';' + FloatToStr(Intensity);
        S2 := S2 + ';' + FloatToStr(Magnitude);
        S2 := S2 + ';' + FloatToStr(BackLevel);
        WriteLn(OutF, S2);
        Inc(StarN);
      end;
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
end;
  
var
  Quiet: Boolean;
  PrintVer: Boolean;
  PrintHlp: Boolean;
  InputFileName: string;
  OutputFileName: string;

begin
  PrintVer := CmdObj.CmdLine.IsCmdOption('v') or CmdObj.CmdLine.IsCmdOption('version');
  PrintHlp := CmdObj.CmdLine.IsCmdOption('?') or CmdObj.CmdLine.IsCmdOption('h') or CmdObj.CmdLine.IsCmdOption('help');
  if PrintVer then begin
    PrintVersion;
    if (CmdObj.CmdLine.FileCount = 0) and not PrintHlp then Halt(1);
  end;
  if PrintHlp or ((CmdObj.CmdLine.FileCount <> 2) and (CmdObj.CmdLine.FileCount <> 1)) then begin
    PrintHelp;
    Halt(1);
  end;
  Quiet := CmdObj.CmdLine.IsCmdOption('q');  
  InputFileName := ExpandFileName(CmdObj.CmdLine.ParamFile(1));
  if ExtractFileExt(InputFileName) = '' then
    InputFileName := ChangeFileExt(InputFileName, '.dat');
  InFileNamePrintable := ExtractFileName(InputFileName);
  if CmdObj.CmdLine.FileCount < 2 then begin
    OutputFileName := ChangeFileExt(InputFileName, '.csv');
  end
  else begin
    OutputFileName := ExpandFileName(CmdObj.CmdLine.ParamFile(2));
    if ExtractFileExt(OutputFileName) = '' then OutputFileName := OutputFileName + '.csv';
  end;
  if AnsiCompareFileName(InputFileName, OutputFileName) = 0 then begin
    WriteLn('Output file cannot be the same as input one.');
    Halt(1);
  end;
  ProcessInput(InputFileName, OutputFileName, Quiet);
end.
