{$APPTYPE CONSOLE}

program FITSstat;

uses
  SysUtils, CmdObj{, CmdObjStdSwitches}, FITSUtils, FitsUtilsHelp, CommonIni;

procedure PrintVersion;
begin
  WriteLn('FITSstat  Maksym Pyatnytskyy  2017');
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

////////////////////////////////////////////////////////////////////////////////
// http://wiki.freepascal.org/Functions_for_descriptive_statistics
// (modified)

type
  TDoubleArray = array of Double;

procedure SortDoubleArray(var data: TDoubleArray);
{ Based on Shell Sort - avoiding recursion allows for sorting of very
  large arrays, too }
var
  arrayLength, i, j, k: longint;
  h: double;
begin
  arrayLength := high(data);
  k := arrayLength div 2;
  while k > 0 do
  begin
    for i := 0 to arrayLength - k do
    begin
      j := i;
      while (j >= 0) and (data[j] > data[j + k]) do
      begin
        h := data[j];
        data[j] := data[j + k];
        data[j + k] := h;
        if j > k then
          dec(j, k)
        else
          j := 0;
      end;
    end;
    k := k div 2
  end;
end;

function median(var data: TDoubleArray): double;
var
  centralElement: integer;
begin
  SortDoubleArray(data);
  centralElement := length(data) div 2;
  if odd(length(data)) then
    result := data[centralElement]
  else
    result := (data[centralElement - 1] + data[centralElement]) / 2;
end;

function mean(var data: TDoubleArray): double;
var
  I: Integer;
  V: Extended;
begin
  Result := 0;
  V := 0;
  for I := 0 to High(data) do
    V := V + data[I];
  Result := V / High(data);
end;

procedure PrintV(Name: string; V: Double);
begin
  Write(Name);
  if Frac(V) = 0 then
    WriteLn(V:0:0)
  else
    WriteLn(V);
end;

procedure ProcessInput(const FITSFileName: string);
var
  Image: PChar;
  Width, Height, BitPix: Integer;
  A: TFITSValue;
  X, Y, Addr: Integer;
  BytePix: Integer;
  data: TDoubleArray;
  medianV, meanV, minV, maxV: Double;
begin
  try
    Image := GetFITSimage2D(FITSFileName, Width, Height, BitPix);
    if Width * Height = 0 then
      FileError('One of dimensions is zero. File ' + AnsiQuotedStr(FITSfileName, '"')); // should never occured...
    try
      BytePix := Abs(BitPix) div 8;
      SetLength(data, Height * Width);
      for Y := 3 to Height - 4 do begin
        for X := 3 to Width - 4 do begin
          Addr := (Y * Width + X) * BytePix;
          Move(Image[Addr], A, BytePix);
          RevertBytes(A, BitPix);
          case BitPix of
              8: data[Y * Width + X] := A.B;
             16: data[Y * Width + X] := A.I;
             32: data[Y * Width + X] := A.L;
            -32: data[Y * Width + X] := A.S;
            -64: data[Y * Width + X] := A.D;
          end;
        end;
      end;
      medianV := median(data); // sorts data!
      minV := data[0];
      maxV := data[High(data)];
      meanV := mean(data);
      WriteLn('File:'^I, ExtractFileName(FITSFileName));
      PrintV('Width:'^I, Width);
      PrintV('Height:'^I, Height);
      PrintV('Median:'^I, medianV);
      PrintV('Mean:'^I, meanV);
      PrintV('Min:'^I, minV);
      PrintV('Max:'^I, maxV);
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
  InputFileName: string;

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

  InputFileName := ExpandFileName(CmdObj.CmdLine.ParamFile(1));

  ProcessInput(InputFileName);

end.

