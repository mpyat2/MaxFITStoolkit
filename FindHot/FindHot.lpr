{$APPTYPE CONSOLE}

program FindHot;

uses
  SysUtils, CmdObj{, CmdObjStdSwitches}, FITSUtils, FitsUtilsHelp, CommonIni;

procedure PrintVersion;
begin
  WriteLn('FindHot  Maksym Pyatnytskyy  2017');
  WriteLn('Version 2018.01.18.01');
  WriteLn;
end;

procedure FileError(const S: string);
begin
  raise Exception.Create(S);
end;

function GetFITSimage2D(const FITSfileName: string; out Width, Height, BitPix: Integer): PChar;
var
  FITSfile: FITSRecordFile;
  I, N, NblocksInHeader, StartOfImage, BytePix, NImagePoints, NrecordsToRead, ImageMemSize: Integer;
  NaxisN: TIntArray;
begin
  AssignFile(FITSfile, FITSfileName);
  Reset(FITSfile);
  try
    N := GetEndPosition(FITSfile);
    if N < 0 then
      FileError('Cannot find End of Header in file ' + AnsiQuotedStr(FITSfileName, '"'));
    NblocksInHeader := N div RecordsInBlock + 1;
    StartOfImage := NblocksInHeader * RecordsInBlock;
    GetBitPixAndNaxis(FITSfile, FITSfileName, BitPix, NaxisN);
    if Length(NAxisN) <> 2 then
      FileError('2D FITS expected. File ' + AnsiQuotedStr(FITSfileName, '"'));
    Width := NAxisN[0];
    Height := NAxisN[1];
    BytePix := Abs(BitPix) div 8;
    NImagePoints := 1;
    for I := 0 to Length(NaxisN) - 1 do
      NImagePoints := NImagePoints * NaxisN[I];
    NrecordsToRead := (NImagePoints * BytePix - 1) div FITSRecordLen + 1;
    ImageMemSize := NrecordsToRead * FITSRecordLen;
    GetMem(Result, ImageMemSize);
    try
      FillChar(Result^, ImageMemSize, 0);
      Seek(FITSfile, StartOfImage);
      BlockRead(FITSfile, Result^, NrecordsToRead);
    except
      FreeMem(Result);
      Result := nil;
      raise;
    end;
  finally
    CloseFile(FITSFile);
  end;
end;

procedure ProcessInput(const FITSFileName: string; Level: Double; MaxNumber: Integer; ExtendedOut: Boolean; const OutFileName: string; CheckExistense: Boolean);

  function PrintHot(Image: PChar; Width, Height, BitPix: Integer): Integer;
  var
    A: TFITSValue;
    X, Y, Addr: Integer;
    V: Double;
    BytePix: Integer;
    OutFile: TextFile;
  begin
    Result := 0;
    if OutFileName <> '' then begin
      if CheckExistense and FileExists(OutFileName) then
        FileError('File ' + AnsiQuotedStr(OutFileName, '"') + ' already exists. Use /F switch to overwrite.');
      AssignFile(OutFile, OutFileName);
      Rewrite(OutFile);
    end;
    try
      BytePix := Abs(BitPix) div 8;
      for Y := 3 to Height - 4 do begin
        for X := 3 to Width - 4 do begin
          Addr := (Y * Width + X) * BytePix;
          Move(Image[Addr], A, BytePix);
          RevertBytes(A, BitPix);
          case BitPix of
              8: V := A.B;
             16: V := A.I;
             32: V := A.L;
            -32: V := A.S;
            -64: V := A.D;
          end;
          if V > Level then begin
            if OutFileName <> '' then begin
              Write(OutFile, 'P', ' ', X + 1, ' ', Y + 1);
              if ExtendedOut then begin
                Write(OutFile, ' ');
                if Frac(V) = 0 then
                  Write(OutFile, V:0:0)
                else
                  Write(OutFile, V);
              end;
              WriteLn(OutFile);
            end;
            Inc(Result);
            if Result >= MaxNumber then
              Exit;
          end;
        end;
      end;
    finally
      if OutFileName <> '' then
        CloseFile(OutFile);
    end;
  end;

var
  Image: PChar;
  Width, Height, BitPix, N: Integer;
begin
  try
    Image := GetFITSimage2D(FITSFileName, Width, Height, BitPix);
    try
      N := PrintHot(Image, Width, Height, BitPix);
      WriteLn(N, ' hot pixels found');
    finally
      FreeMem(Image);
      Image := nil;
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
  PrintVer: Boolean;
  Overwrite: Boolean;
  ExtendedOut: Boolean;
  InputFileName: string;
  OutputFileName: string;
  Level: Double;
  MaxNumber: Integer;
  ErrorPos: Integer;
  S: string;

begin
  FileMode := fmOpenRead;

  PrintVer := (CmdObj.CmdLine.IsCmdOption('V') or CmdObj.CmdLine.IsCmdOption('version'));
  if PrintVer then PrintVersion;

  if (CmdObj.CmdLine.IsCmdOption('?') or CmdObj.CmdLine.IsCmdOption('H') or CmdObj.CmdLine.IsCmdOption('help')) then begin
    PrintHelp;
    Halt(1);
  end;

  if (CmdObj.CmdLine.FileCount <> 1) then begin
    if not PrintVer then begin
      WriteLn('**** Input File Name is expected');
      WriteLn;
      PrintHelp;
    end;
    Halt(1);
  end;

  Level := 0;
  S := CmdObj.CmdLine.KeyValue('L=');
  if S <> '' then begin
    Val(S, Level, ErrorPos);
    if ErrorPos <> 0 then begin
      WriteLn('**** Level must be integer of floating-point value');
      WriteLn;
      PrintHelp;
      Halt(1);
    end;
  end
  else begin
    WriteLn('**** Level is not specified');
    WriteLn;
    PrintHelp;
    Halt(1);
  end;

  MaxNumber := 1000;
  S := CmdObj.CmdLine.KeyValue('M=');
  if S <> '' then begin
    Val(S, MaxNumber, ErrorPos);
    if (ErrorPos <> 0) or (MaxNumber < 1)  then begin
      WriteLn('**** MaxNumber must be positive non-zero integer');
      WriteLn;
      PrintHelp;
      Halt(1);
    end;
  end;

  OutputFileName := CmdObj.CmdLine.KeyValue('N=');
  if OutputFileName <> '' then begin
    OutputFileName := ExpandFileName(OutputFileName);
    if ExtractFileExt(OutputFileName) = '' then
      OutputFileName := ChangeFileExt(OutputFileName, '.lst');
  end;

  Overwrite := CmdObj.CmdLine.IsCmdOption('F');
  ExtendedOut := CmdObj.CmdLine.IsCmdOption('E');

  InputFileName := ExpandFileName(CmdObj.CmdLine.ParamFile(1));
  if ExtractFileExt(InputFileName) = '' then InputFileName := ChangeFileExt(InputFileName, '.fit');

  ProcessInput(InputFileName, Level, MaxNumber, ExtendedOut, OutputFileName, not Overwrite);

end.

