{$APPTYPE CONSOLE}

program CalcMed;

uses
  SysUtils, Classes, CmdObj{, CmdObjStdSwitches}, FITSUtils, EnumFiles, StringListNaturalSort, FitsUtilsHelp, CommonIni;

procedure PrintVersion;
begin
  WriteLn('Calc Median or Average  Maksym Pyatnytskyy  2017');
  WriteLn('Version 2017.12.27.01');
  WriteLn;
end;

////////////////////////////////////////////////////////////////////////////////
// http://wiki.freepascal.org/Functions_for_descriptive_statistics
// Extended replaced by Double

type
  TDoubleArray = array of Double;
  TLongIntArray = array of LongInt;

function SortDoubleArray(const data: TDoubleArray): TDoubleArray;
{ Based on Shell Sort - avoiding recursion allows for sorting of very
  large arrays, too }
var
  data2: TDoubleArray;
  arrayLength, i, j, k: longint;
  h: double;
begin
  arrayLength := high(data);
  data2 := copy(data, 0, arrayLength + 1);
  k := arrayLength div 2;
  while k > 0 do
  begin
    for i := 0 to arrayLength - k do
    begin
      j := i;
      while (j >= 0) and (data2[j] > data2[j + k]) do
      begin
        h := data2[j];
        data2[j] := data2[j + k];
        data2[j + k] := h;
        if j > k then
          dec(j, k)
        else
          j := 0;
      end;
    end;
    k := k div 2
  end;
  result := data2;
end;

function SortLongIntArray(const data: TLongIntArray): TLongIntArray;
{ Based on Shell Sort - avoiding recursion allows for sorting of very
  large arrays, too }
var
  data2: TLongIntArray;
  arrayLength, i, j, k: longint;
  h: LongInt;
begin
  arrayLength := high(data);
  data2 := copy(data, 0, arrayLength + 1);
  k := arrayLength div 2;
  while k > 0 do
  begin
    for i := 0 to arrayLength - k do
    begin
      j := i;
      while (j >= 0) and (data2[j] > data2[j + k]) do
      begin
        h := data2[j];
        data2[j] := data2[j + k];
        data2[j + k] := h;
        if j > k then
          dec(j, k)
        else
          j := 0;
      end;
    end;
    k := k div 2
  end;
  result := data2;
end;

function median(const data: TDoubleArray): double; overload;
var
  centralElement: integer;
  sortedData: TDoubleArray;
begin
  sortedData := SortDoubleArray(data);
  centralElement := length(sortedData) div 2;
  if odd(length(sortedData)) then
    result := sortedData[centralElement]
  else
    result := (sortedData[centralElement - 1] + sortedData[centralElement]) / 2;
end;

function median(const data: TLongIntArray): LongInt; overload;
var
  centralElement: integer;
  sortedData: TLongIntArray;
begin
  sortedData := SortLongIntArray(data);
  centralElement := length(sortedData) div 2;
  if odd(length(sortedData)) then
    result := sortedData[centralElement]
  else
    //result := Round((sortedData[centralElement - 1] + sortedData[centralElement]) / 2);
    result := (sortedData[centralElement - 1] + sortedData[centralElement]) div 2; // Like IRIS!
end;

function average(const data: TDoubleArray): double; overload;
var
  Sum: Extended;
  I: Integer;
begin
  Sum := 0;
  if Length(data) = 0 then Exit;
  for I := 0 to Length(data) - 1 do
    Sum := Sum + data[I];
  Result := Sum / Length(data);
end;

function average(const data: TLongIntArray): LongInt; overload;
var
  Sum: Int64;
  I: Integer;
begin
  Sum := 0;
  if Length(data) = 0 then Exit;
  for I := 0 to Length(data) - 1 do
    Sum := Sum + data[I];
  Result := Round(Sum / Length(data));
end;

////////////////////////////////////////////////////////////////////////////////

procedure ShowProgress(N, NBlocksInImage: Integer);
var
  S: string;
begin
  S := PadCh('', Round(N / NBlocksInImage * 70), '#');
  S := PadCh(S, 70, ' ');
  Write(#13, Round(N / NBlocksInImage * 100):3, '% ', S);
end;

type
  TStringArray = array of string;
  TFITSRecordArray = array of FITSRecordType;

// The function is identocal to iconvraw.StrToFITSRecord. Move it to FITSutils!
procedure StrToFITSRecord(const S: string; out FITSRecord: FITSRecordType);
var
  L: Integer;
begin
  FillChar(FITSRecord, SizeOf(FITSRecord), ' ');
  L := Length(S);
  if L > SizeOf(FITSRecord) then L := SizeOf(FITSRecord);
  Move(S[1], FITSRecord, L);
end;

// The function is identocal to iconvraw.MakeFITSHeader. Move it to FITSutils!
function MakeFITSHeader(BitPix: Integer;
                        const Axes: TIntArray;
                        DateObs: TDateTime;
                        Date: TDateTime;
                        Exposure: Double;
                        const Instrument: string;
                        const Comments: TStringArray): TFITSRecordArray;
var
  I, N: Integer;
  TempS: string;
begin
  N := 0;

  SetLength(Result, N + 1);
  StrToFITSRecord('SIMPLE  =                    T', Result[N]);
  Inc(N);

  SetLength(Result, N + 1);
  Str(BitPix:20, TempS);
  StrToFITSRecord('BITPIX  = ' + TempS, Result[N]);
  Inc(N);

  SetLength(Result, N + 1);
  Str(Length(Axes):20, TempS);
  StrToFITSRecord('NAXIS   = ' + TempS, Result[N]);
  Inc(N);

  for I := 0 to Length(Axes) - 1 do begin
    SetLength(Result, N + 1);
    Str(Axes[I]:20, TempS);
    StrToFITSRecord(PadCh('NAXIS' + IntToStr(I + 1), 8, ' ') + '= ' + TempS, Result[N]);
    Inc(N);
  end;
  if DateObs <> 0 then begin
    SetLength(Result, N + 1);
    TempS := '''' + FormatDateTime('YYYY-MM-DD"T"hh:nn:ss', DateObs) + '''';
    StrToFITSRecord('DATE-OBS= ' + TempS, Result[N]);
    Inc(N);
  end
  else begin
    SetLength(Result, N + 1);
    StrToFITSRecord('COMMENT **** DATE-OBS was not specified!', Result[N]);
    Inc(N);
  end;
  if Instrument <> '' then begin
    SetLength(Result, N + 1);
    TempS := FITSQuotedValue(Instrument);
    StrToFITSRecord('INSTRUME= ' + TempS, Result[N]);
    Inc(N);
  end;
  if Exposure > 0 then begin
    SetLength(Result, N + 1);
    Str(Exposure:20, TempS);
    StrToFITSRecord('EXPTIME = ' + TempS, Result[N]);
    Inc(N);
  end;
  if Date <> 0 then begin
    SetLength(Result, N + 1);
    TempS := '''' + FormatDateTime('YYYY-MM-DD"T"hh:nn:ss.zzz', Date) + '''';
    StrToFITSRecord('DATE    = ' + TempS, Result[N]);
    Inc(N);
  end;
  for I := 0 to Length(Comments) - 1 do begin
    SetLength(Result, N + 1);
    StrToFITSRecord('COMMENT ' + Comments[I], Result[N]);
    Inc(N);
  end;
  SetLength(Result, N + 1);
  Result[N] := recordEND;
  Inc(N);
  N := N mod RecordsInBlock;
  if N > 0 then begin
    for I := 1 to RecordsInBlock - N do begin
      SetLength(Result, N + 1);
      FillChar(Result[N], SizeOf(FITSRecordType), ' ');
      Inc(N);
    end;
  end;
end;

type
  TFileInfo = class
    StartOfImage: Integer;
    Chunk: array[0..RecordsInBlock-1] of FITSRecordType;
    constructor Create;
  end;

constructor TFileInfo.Create;
begin
  inherited Create;
  StartOfImage := -1;
  FillChar(Chunk, SizeOf(Chunk), 0);
end;

var
  FileList: TStringListNaturalSort;
  FileListFull: TStringList;

procedure FileError(const S: string);
begin
  raise Exception.Create(S);
end;

type
  TFileEnumClass = class(TObject)
    class function FileEnumProc(const Directory: string; const F: TSearchRec): Boolean;
  end;

class function TFileEnumClass.FileEnumProc(const Directory: string; const F: TSearchRec): Boolean;
begin
  FileList.Add(Directory + F.Name);
  Result := True;
end;

procedure ProcessInput(const FileMasks: array of string; const OutputFileName: string; CalcAverage: Boolean; CheckExistence: Boolean);
var
  I, II, III, N: Integer;
  FITSfile: FITSRecordFile;
  OutputFITSFile: FITSRecordFile;
  OutputFITSHeader: TFITSRecordArray;
  OutputImageBlock: array[0..RecordsInBlock-1] of FITSRecordType;
  FITSValueSum, FITSValue0: TFITSValue;
  Data: TDoubleArray;
  DataInt: TLongIntArray;
  P: PChar;
  Idx: Integer;
  BytePix: Integer;
  BitPix0, BitPix: Integer;
  NAxisN0, NAxisN: TIntArray;
  NblocksInHeader, NBlocksInImage: Integer;
  Comments: TStringArray;
begin
  try
    if CheckExistence and FileExists(OutputFileName) then
      FileError('Output file already exists. Use /F option to overwrite.');
    for N := 0 to Length(FileMasks) - 1 do begin
      WriteLn;
      WriteLn('[', FileMasks[N], ']');
      FileList.Clear;
      FileEnum(FileMasks[N], faArchive, False, TFileEnumClass.FileEnumProc);
      FileList.NaturalSort;
      for I := 0 to FileList.Count - 1 do
        FileListFull.AddObject(FileList[I], TFileInfo.Create);
    end;
    if FileListFull.Count < 1 then begin
      WriteLn;
      WriteLn('**** No files found.');
      Exit;
    end;

    WriteLn;

    AssignFile(FITSfile, FileListFull[0]);
    Reset(FITSfile);
    try
      GetBitPixAndNaxis(FITSfile, FileListFull[0], BitPix0, NAxisN0);
    finally
      CloseFile(FITSfile);
    end;
    case BitPix0 of
        8: ;
       16: ;
       32: ;
      -32: ;
      -64: ;
      else
        FileError('Invalid BITPIX');
    end;

    BytePix := Abs(BitPix0) div 8;
    //
    NBlocksInImage := 1;
    for I := 0 to Length(NAxisN0) - 1 do
      NBlocksInImage := NBlocksInImage * NAxisN0[I];
    NBlocksInImage := (NBlocksInImage * BytePix - 1) div (FITSRecordLen * RecordsInBlock) + 1;
    //
    for I := 0 to FileListFull.Count - 1 do begin
      AssignFile(FITSfile, FileListFull[I]);
      Reset(FITSfile);
      try
        GetBitPixAndNaxis(FITSfile, FileListFull[0], BitPix, NAxisN);
        if BitPix <> BitPix0 then
          FileError('BITPIX must be the same. File: ' + AnsiQuotedStr(FileListFull[I], '"'));
        if Length(NAxisN) <> Length(NAxisN0) then
          FileError('NAXIS must be the same. File: ' + AnsiQuotedStr(FileListFull[I], '"'));
        for II := 0 to Length(NAxisN0) - 1 do
          if NAxisN[II] <> NAxisN0[II] then
            FileError('Axes lengthes must be the same. File: ' + AnsiQuotedStr(FileListFull[I], '"'));
        N := GetEndPosition(FITSfile);
        if N < 0 then
          FileError('Cannot find End of Header in file ' + AnsiQuotedStr(FileListFull[I], '"'));
        NblocksInHeader := N div RecordsInBlock + 1;
        TFileInfo(FileListFull.Objects[I]).StartOfImage := NblocksInHeader * RecordsInBlock;
      finally
        CloseFile(FITSfile);
      end;
    end;

    SetLength(Comments, 1);
    if CalcAverage then
      Comments[0] := IntToStr(FileListFull.Count) + ' files; average stack'
    else
      Comments[0] := IntToStr(FileListFull.Count) + ' files; median stack';
    AssignFile(OutputFITSFile, OutputFileName);
    Rewrite(OutputFITSFile);
    try
      OutputFITSHeader := MakeFITSHeader(BitPix0, NAxisN0, 0, 0, 0, '', Comments);
      BlockWrite(OutputFITSFile, OutputFITSHeader[0], Length(OutputFITSHeader));
      for N := 0 to NBlocksInImage - 1 do begin
        ShowProgress(N, NBlocksInImage);
        for I := 0 to FileListFull.Count - 1 do begin
          AssignFile(FITSfile, FileListFull[I]);
          Reset(FITSfile);
          try
            Seek(FITSFile, TFileInfo(FileListFull.Objects[I]).StartOfImage + N * RecordsInBlock);
            //!! Check block count here. Ignore some errors???? or not?
            BlockRead(FITSFile, TFileInfo(FileListFull.Objects[I]).Chunk[0], RecordsInBlock);
          finally
            CloseFile(FITSfile);
          end;
        end;
        FillChar(OutputImageBlock, SizeOf(OutputImageBlock), 0);
        case BitPix0 of
            8: SetLength(DataInt, FileListFull.Count);
           16: SetLength(DataInt, FileListFull.Count);
           32: SetLength(DataInt, FileListFull.Count);
          -32: SetLength(Data, FileListFull.Count);
          -64: SetLength(Data, FileListFull.Count);
        end;
        for II := 0 to SizeOf(OutputImageBlock) div BytePix - 1 do begin
          for I := 0 to FileListFull.Count - 1 do begin
            P := PChar(@(TFileInfo(FileListFull.Objects[I]).Chunk));
            Idx := II * BytePix;
            for III := 0 to BytePix - 1 do
              FITSValue0.A[BytePix - 1 - III] := Byte(P[Idx + III]);
            case BitPix0 of
                8: DataInt[I] := FITSValue0.B;
               16: DataInt[I] := FITSValue0.I;
               32: DataInt[I] := FITSValue0.L;
              -32: Data[I] := FITSValue0.S;
              -64: Data[I] := FITSValue0.D;
            end;
          end;
          if CalcAverage then begin
            case BitPix0 of
                8: FITSValueSum.B := average(DataInt);
               16: FITSValueSum.I := average(DataInt);
               32: FITSValueSum.L := average(DataInt);
              -32: FITSValueSum.S := average(Data);
              -64: FITSValueSum.D := average(Data);
            end;
          end
          else begin
            case BitPix0 of
                8: FITSValueSum.B := median(DataInt);
               16: FITSValueSum.I := median(DataInt);
               32: FITSValueSum.L := median(DataInt);
              -32: FITSValueSum.S := median(Data);
              -64: FITSValueSum.D := median(Data);
            end;
          end;
          P := PChar(@OutputImageBlock);
          for III := 0 to BytePix - 1 do
            P[Idx + III] := Char(FITSValueSum.A[BytePix - 1 - III]);
        end;
        BlockWrite(OutputFITSFile, OutputImageBlock[0], Length(OutputImageBlock));
      end;
    finally
      CloseFile(OutputFITSFile);
    end;
    WriteLn;
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
  InputFileMasks: array of string;
  PrintVer: Boolean;
  OutputFileName: string;
  Overwrite: Boolean;
  CalcAverage: Boolean;
  N: Integer;
  I: Integer;

begin
  FileMode := fmOpenRead;

  PrintVer := (CmdObj.CmdLine.IsCmdOption('V') or CmdObj.CmdLine.IsCmdOption('version'));
  if PrintVer then PrintVersion;

  if (CmdObj.CmdLine.IsCmdOption('?') or CmdObj.CmdLine.IsCmdOption('H') or CmdObj.CmdLine.IsCmdOption('help')) then begin
    PrintHelp;
    Halt(1);
  end;

  N := CmdObj.CmdLine.FileCount;

  if (N < 1) then begin
    if not PrintVer then begin
      WriteLn('**** At least one filemask must be specified');
      WriteLn;
      PrintHelp;
    end;
    Halt(1);
  end;

  OutputFileName := CmdObj.CmdLine.KeyValue('N=');
  if OutputFileName = '' then begin
    WriteLn('**** Output file name must be specified');
    WriteLn;
    PrintHelp;
    Halt(1);
  end;
  OutputFileName := ExpandFileName(OutputFileName);
  if ExtractFileExt(OutputFileName) = '' then
    OutputFileName := ChangeFileExt(OutputFileName, '.fit');

  CalcAverage := CmdObj.CmdLine.IsCmdOption('A');

  Overwrite := CmdObj.CmdLine.IsCmdOption('F');

  SetLength(InputFileMasks, N);
  for I := 1 to N do begin
    InputFileMasks[I - 1] := ExpandFileName(CmdObj.CmdLine.ParamFile(I));
    if ExtractFileExt(InputFileMasks[I - 1]) = '' then InputFileMasks[I - 1] := ChangeFileExt(InputFileMasks[I - 1], '.fit');
  end;

  FileListFull := TStringList.Create;
  try
    FileList := TStringListNaturalSort.Create;
    try
      ProcessInput(InputFileMasks, OutputFileName, CalcAverage, not Overwrite);
    finally
      FreeAndNil(FileList);
    end;
  finally
    for I := FileListFull.Count - 1 downto 0 do begin
      FileListFull.Objects[I].Free;
      FileListFull.Objects[I] := nil;
    end;
    FreeAndNil(FileListFull);
  end;

end.
