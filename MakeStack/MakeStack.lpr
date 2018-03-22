{$APPTYPE CONSOLE}

program MakeStack;

uses
  SysUtils, Classes, Math, DateUtils, CmdObj{, CmdObjStdSwitches},
  Version, EnumFiles, FITSUtils, StringListNaturalSort, FitsUtilsHelp, CommonIni;

{$R *.res}

procedure PrintVersion;
begin
  WriteLn('FITS stack  Maksym Pyatnytskyy  2018');
  WriteLn(GetVersionString(ParamStr(0)));
  WriteLn;
end;

procedure FileError(const S: string);
begin
  raise Exception.Create(S);
end;

type

  { TFITSFileInfo }

  TFITSFileInfo = class(TObject)
    DateObs: TDateTime;
    BitPix: Integer;
    Naxis: TIntArray;
    BScale: Double;
    BZero: Double;
    StartOfImage: Integer;
    ImageMemSize: Integer; // Padded!
    ObjectName: string;
    Telescope: string;
    Instrument: string;
    Exposure: Double;
    destructor Destroy; override;
  end;

  { TFITSFileInfo }

  destructor TFITSFileInfo.Destroy;
  begin
    inherited Destroy;
  end;

type
  TStackMode = ( smAdd, smAvg, smMed );

var
  FileList: TStringListNaturalSort;
  FileListAllFiles: TStringList;

type
  TExtendedArray = array of Extended;
  TDoubleArray = array of Double;

////////////////////////////////////////////////////////////////////////////////
// http://wiki.freepascal.org/Functions_for_descriptive_statistics

procedure SortExtendedArray(var data: TExtendedArray);
{ Based on Shell Sort - avoiding recursion allows for sorting of very
  large arrays, too }
var
  arrayLength, i, j, k: longint;
  h: extended;
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

// modifies data!
function median(var data: TExtendedArray): extended;
var
  centralElement: integer;
begin
  SortExtendedArray(data);
  centralElement := length(data) div 2;
  if odd(length(data)) then
    result := data[centralElement]
  else
    result := (data[centralElement - 1] + data[centralElement]) / 2;
end;

////////////////////////////////////////////////////////////////////////////////

function sum(const data: TExtendedArray): extended;
var
  I: Integer;
begin
  Result := 0;
  if Length(data) = 0 then Exit;
  for I := 0 to Length(data) - 1 do
    Result := Result + data[I];
end;

function average(const data: TExtendedArray): extended;
begin
  Result := sum(data) / Length(data);
end;

function GetFITSpixelAsExtended(FITSdata: PChar; N, BitPix: Integer; BScale: Double; BZero: Double): Extended;
var
  FITSValue: TFITSValue;
  BytePix: Integer;
  Addr: Integer;
  I: Integer;
begin
  BytePix := Abs(BitPix) div 8;
  Addr := N * BytePix;
  for I := 0 to BytePix - 1 do
    FITSValue.A[BytePix - 1 - I] := Byte(FITSdata[Addr + I]);
  case BitPix of
      8: Result := FITSValue.B;
     16: Result := FITSValue.I;
     32: Result := FITSValue.L;
    -32: Result := FITSValue.S;
    -64: Result := FITSValue.D;
  end;
  Result := BScale * Result + BZero;
end;

procedure SetFITSpixel(FITSdata: PChar; N, BitPix: Integer; Value: Double);
var
  FITSValue: TFITSValue;
  BytePix: Integer;
  Addr: Integer;
  I: Integer;
begin
  case BitPix of
      8: FITSValue.B := Round(Value);
     16: FITSValue.I := Round(Value);
     32: FITSValue.L := Round(Value);
    -32: FITSValue.S := Value;
    -64: FITSValue.D := Value;
  end;
  BytePix := Abs(BitPix) div 8;
  Addr := N * BytePix;
  for I := 0 to BytePix - 1 do
    FITSdata[Addr + I] := Char(FITSValue.A[BytePix - 1 - I]);
end;

function StackModeToString(Mode: TStackMode): string;
begin
  case Mode of
    smAdd: Result := 'SUM';
    smAvg: Result := 'AVERAGE';
    smMed: Result := 'MEDIAN';
    else Result := '<UNKNOWN>';
  end;
end;

procedure ShowProgress(const Info: string; N, Nmax: Integer);
var
  S, S2: string;
begin
  S := PadCh('', Round(N / Nmax * 60), '#');
  S := PadCh(S, 60, ' ');
  S2 := PadCh(Copy(Info, 1, 9), 9, ' ');
  Write(#13, S2, Round(N / Nmax * 100):3, '% ', S);
end;

procedure DoStackProc(StackMode: TStackMode; const GenericName: string; const OutputDir: string; const OutputExt: string; Overwrite: Boolean; StackNumber: Integer; const StackList: TStringList);
var
  FileName: string;
  OutFileName: string;
  FileInfo: TFITSFileInfo;
  FITSfile: FITSRecordFile;
  Images: array of PChar;
  DestImage: PChar;
  DestImageMemSize: Integer;
  MinBitPix, MaxBitPix: Integer;
  DestBitPix: Integer;
  DestBytePix: Integer;
  DestNaxis: TIntArray;
  DestHeader: TFITSRecordArray;
  DestPixelArray: TDoubleArray;
  StackPixels: TExtendedArray;
  StackedResult, StackedResultMax, StackedResultMin: Extended;
  ScaledOrShifted: Boolean;
  TotalExposure: Double;
  DestDateObs: TDateTime;
  DestObject: string;
  DestTelescope: string;
  DestInstrument: string;
  S: string;
  Pixels: Integer;
  I, II: Integer;
  Comments: TStringArray;
begin
  if StackList.Count < 1 then Exit;
  OutFileName := IncludeTrailingPathDelimiter(OutputDir) + GenericName;
  if StackNumber >= 0 then OutFileName := OutFileName + IntToStr(StackNumber);
  OutFileName := OutFileName + OutputExt;
  WriteLn('Stacking ', StackList.Count, ' files. Output file: ' + ExtractFileName(OutFileName));
  FileInfo := TFITSFileInfo(StackList.Objects[0]);
  MinBitPix := FileInfo.BitPix;
  MaxBitPix := FileInfo.BitPix;
  DestNaxis := Copy(FileInfo.Naxis , 0, MaxInt);
  SetLength(Images, StackList.Count);
  for I := 0 to Length(Images) - 1 do
    Images[I] := nil;
  try
    SetLength(Comments, Length(Comments) + 1);
    Comments[Length(Comments) - 1] := 'Stack of ' + IntToStr(StackList.Count) + ' images';
    SetLength(Comments, Length(Comments) + 1);
    Comments[Length(Comments) - 1] := 'Stacking mode: ' + StackModeToString(StackMode);
    DestObject := TFITSFileInfo(StackList.Objects[0]).ObjectName;
    DestTelescope := TFITSFileInfo(StackList.Objects[0]).Telescope;
    DestInstrument := TFITSFileInfo(StackList.Objects[0]).Instrument;
    TotalExposure := 0;
    DestDateObs := 0;
    for I := 0 to StackList.Count - 1 do begin
      if DestObject <> TFITSFileInfo(StackList.Objects[I]).ObjectName then DestObject := '';
      if DestTelescope <> TFITSFileInfo(StackList.Objects[I]).Telescope then DestTelescope := '';
      if DestInstrument <> TFITSFileInfo(StackList.Objects[I]).Instrument then DestInstrument := '';
      TotalExposure := TotalExposure + TFITSFileInfo(StackList.Objects[I]).Exposure;
      DestDateObs := DestDateObs + TFITSFileInfo(StackList.Objects[I]).DateObs + TFITSFileInfo(StackList.Objects[I]).Exposure / (24.0*60.0*60.0) / 2.0;
    end;
    DestDateObs := DestDateObs / StackList.Count;
    SetLength(Comments, Length(Comments) + 1);
    Str(TotalExposure:0:2, S);
    Comments[Length(Comments) - 1] := 'Total Exposure = ' + S;

    ShowProgress('Reading', 0, StackList.Count);
    ScaledOrShifted := False;
    for I := 0 to StackList.Count - 1 do begin
      if not Overwrite and FileExists(OutFileName) then
        FileError('File ' + AnsiQuotedStr(OutFileName, '"') + ' already exists. Use /F switch to overwrite.');
      FileName := StackList[I];
      FileInfo := TFITSFileInfo(StackList.Objects[I]);
      if Length(FileInfo.Naxis) <> Length(DestNaxis) then
        FileError('All files in stack must have identical number of axes');
      for II := 0 to Length(DestNaxis) - 1 do
        if DestNaxis[II] <> FileInfo.Naxis[II] then
          FileError('All files in stack must have identical axes');

      if FileInfo.BitPix > MaxBitPix then MaxBitPix := FileInfo.BitPix;
      if FileInfo.BitPix < MinBitPix then MinBitPix := FileInfo.BitPix;

      if (FileInfo.BScale <> 1) or (FileInfo.BZero <> 0) then ScaledOrShifted := True;

      SetLength(Comments, Length(Comments) + 1);
      Comments[Length(Comments) - 1] := 'File: ' + ExtractFileName(FileName);

      AssignFile(FITSfile, FileName);
      Reset(FITSfile);
      try
        Seek(FITSFile, FileInfo.StartOfImage);
        GetMem(Images[I], FileInfo.ImageMemSize);
        FillChar(Images[I]^, FileInfo.ImageMemSize, 0);
        BlockRead(FITSFile, Images[I]^, FileInfo.ImageMemSize div FITSRecordLen);
      finally
        CloseFile(FitsFile);
      end;
      ShowProgress('Reading', I + 1, StackList.Count);
    end;
    ShowProgress('Reading', StackList.Count, StackList.Count);

    Pixels := 1;
    for I := 0 to Length(DestNaxis) - 1 do
      Pixels := Pixels * DestNaxis[I];
    SetLength(DestPixelArray, Pixels);
    SetLength(StackPixels, StackList.Count);
    ShowProgress('Stacking', 0, Pixels);

    for II := 0 to Pixels - 1 do begin
      for I := 0 to StackList.Count - 1 do
        StackPixels[I] := GetFITSpixelAsExtended(Images[I], II, TFITSFileInfo(StackList.Objects[I]).BitPix, TFITSFileInfo(StackList.Objects[I]).BScale, TFITSFileInfo(StackList.Objects[I]).BZero);
      case StackMode of
        smAdd: StackedResult := sum(StackPixels);
        smAvg: StackedResult := average(StackPixels);
        smMed: StackedResult := median(StackPixels);
        else FileError('Internal error: invalid Stack Mode');
      end;
      if II = 0 then begin
        StackedResultMax := StackedResult;
        StackedResultMin := StackedResult;
      end
      else begin
        if StackedResult > StackedResultMax then StackedResultMax := StackedResult;
        if StackedResult < StackedResultMin then StackedResultMin := StackedResult;
      end;
      try
        DestPixelArray[II] := StackedResult;
      except
        on E: Exception do
          FileError('Stack error: ' + E.Message);
      end;
      if ((II + 1) mod (Pixels div 100) = 0) then
        ShowProgress('Stacking', II + 1, Pixels);
    end;

    ShowProgress('Stacking', Pixels, Pixels);
    WriteLn;
    WriteLn('Done.');

    for I := Length(Images) - 1 downto 0 do begin
      if Images[I] <> nil then begin
        FreeMem(Images[I]);
        Images[I] := nil;
      end;
    end;

    if (MaxBitPix = MinBitPix) and (StackMode <> smAdd) and not ScaledOrShifted then
      DestBitPix := MaxBitPix
    else
    if MinBitPix = -64 then
      DestBitPix := -64
    else
    if MinBitPix = -32 then begin
      if (StackedResultMax < MaxSingle) and (StackedResultMin > MinSingle) then
        DestBitPix := -32
      else
        DestBitPix := -64
    end
    else
      DestBitPix := -32;

    DestBytePix := Abs(DestBitPix) div 8;
    DestImageMemSize := Pixels * DestBytePix;
    DestImageMemSize := ((DestImageMemSize - 1) div (FITSRecordLen * RecordsInBlock) + 1) * FITSRecordLen * RecordsInBlock;
    GetMem(DestImage, DestImageMemSize);
    try
      FillChar(DestImage^, DestImageMemSize, 0);

      for I := 0 to Length(DestPixelArray) - 1 do
        SetFITSpixel(DestImage, I, DestBitPix, DestPixelArray[I]);

      DestHeader := MakeFITSHeader(DestBitPix, DestNaxis, 0, 1,
                                   DestDateObs, 'Stack Mid Time, fixed by Exposure Time',
                                   LocalTimeToUniversal(Now()), 'FITS creation time (UTC)',
                                   0, '',
                                   DestObject,
                                   DestTelescope,
                                   DestInstrument,
                                   Comments);
      AssignFile(FITSfile, OutFileName);
      Rewrite(FITSfile);
      try
        BlockWrite(FITSfile, DestHeader[0], Length(DestHeader));
        BlockWrite(FITSfile, DestImage^, DestImageMemSize div FITSRecordLen);
      finally
        CloseFile(FITSfile);
      end;
    finally
      FreeMem(DestImage);
      DestImage := nil;
    end;
  finally
    for I := Length(Images) - 1 downto 0 do begin
      if Images[I] <> nil then begin
        FreeMem(Images[I]);
        Images[I] := nil;
      end;
    end;
  end;

  WriteLn;
end;

procedure DoStacking(StackMode: TStackMode; const GenericName: string; const OutputDir: string; const OutputExt: string; Overwrite: Boolean; BaseNumber: Integer; StackSize: Integer);
var
  StackList: TStringList;
  StackNumber: Integer;
  I: Integer;
begin
  StackNumber := BaseNumber;
  StackList := TStringList.Create;
  try
    for I := 0 to FileListAllFiles.Count - 1 do begin
      StackList.AddObject(FileListAllFiles[I], FileListAllFiles.Objects[I]);
      if (StackSize > 0) and (StackList.Count = StackSize) then begin
        DoStackProc(StackMode, GenericName, OutputDir, OutputExt, Overwrite, StackNumber, StackList);
        StackList.Clear;
        Inc(StackNumber);
      end;
    end;
    if StackList.Count > 0 then begin
      DoStackProc(StackMode, GenericName, OutputDir, OutputExt, Overwrite, StackNumber, StackList);
      StackList.Clear;
      Inc(StackNumber);
    end;
  finally
    FreeAndNil(StackList);
  end;
end;

function GetFITSstringValue(var FITSfile: FITSRecordFile; const Keyword: string): string;
begin
  GetKeywordValue(FITSfile, Keyword, Result, True, True);
  Result := Trim(StripQuotes(Result));
end;

procedure ProcessFile(const FileName: string);
var
  FITSfile: FITSRecordFile;
  NblocksInHeader: Integer;
  ImageMemSizeTemp: Integer;
  FitsInfo: TFITSFileInfo;
  BytePix: Integer;
  N, I: Integer;
begin
  AssignFile(FITSfile, FileName);
  Reset(FITSfile);
  try
    if not IsFits(FITSfile) then
      FileError('Not a valid FITS file: ' + AnsiQuotedStr(FileName, '"'));
    N := GetEndPosition(FITSfile);
    if N < 0 then
      FileError('Cannot find End of Header in file ' + AnsiQuotedStr(FileName, '"'));
    NblocksInHeader := N div RecordsInBlock + 1;

    FitsInfo := TFITSFileInfo.Create;
    FileListAllFiles.AddObject(FileName, FitsInfo);
    GetBitPixAndNaxis(FITSfile, FitsInfo.BitPix, FitsInfo.Naxis);
    if (FitsInfo.BitPix <> 8) and (FitsInfo.BitPix <> 16) and (FitsInfo.BitPix <> 32) and (FitsInfo.BitPix <> -32) and (FitsInfo.BitPix <> -64) then
      FileError('Nonstandard BitPix = ' + IntToStr(FitsInfo.BitPix) + ' in file ' + AnsiQuotedStr(FileName, '"'));
    BytePix := Abs(FitsInfo.BitPix) div 8;
    GetBscaleBzero(FITSfile, FitsInfo.BScale, FitsInfo.BZero);
    FitsInfo.DateObs := GetDateObs(FITSfile);
    FitsInfo.Exposure := GetExposureTime(FITSfile);
    FitsInfo.ObjectName := GetFITSstringValue(FITSfile, 'OBJECT');
    FitsInfo.Telescope  := GetFITSstringValue(FITSfile, 'TELESCOP');
    FitsInfo.Instrument := GetFITSstringValue(FITSfile, 'INSTRUME');
    FitsInfo.StartOfImage := NblocksInHeader * RecordsInBlock;
    ImageMemSizeTemp := 1;
    for I := 0 to Length(FitsInfo.Naxis) - 1 do
      ImageMemSizeTemp := ImageMemSizeTemp * FitsInfo.Naxis[I];
    ImageMemSizeTemp := ImageMemSizeTemp * BytePix;
    ImageMemSizeTemp := ((ImageMemSizeTemp - 1) div (FITSRecordLen * RecordsInBlock) + 1) * FITSRecordLen * RecordsInBlock;
    FitsInfo.ImageMemSize := ImageMemSizeTemp;
  finally
    CloseFile(FITSfile);
  end;
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

procedure ProcessInput(const FileMasks: array of string; StackMode: TStackMode; const GenericName: string; const OutputDir: string; const OutputExt: string; Overwrite: Boolean; BaseNumber: Integer; StackSize: Integer);
var
  I, N, Ntotal: Integer;
begin
  try
    WriteLn;
    Ntotal := 0;
    for N := 0 to Length(FileMasks) - 1 do begin
      Write('[', FileMasks[N], ']');
      FileList.Clear;
      FileEnum(FileMasks[N], faArchive, False, TFileEnumClass.FileEnumProc);
      FileList.NaturalSort;
      for I := 0 to FileList.Count - 1 do begin
        ProcessFile(FileList[I]);
        Inc(Ntotal);
      end;
      WriteLn(': ', FileList.Count, ' file(s).');
    end;
    WriteLn;
    if Ntotal < 1 then begin
      WriteLn('**** No files found.');
    end
    else begin
      WriteLn(Ntotal, ' files to process. Mode: ', StackModeToString(StackMode));
      WriteLn;
      DoStacking(StackMode, GenericName, OutputDir, OutputExt, Overwrite, BaseNumber, StackSize);
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
  InputFileMasks: array of string;
  PrintVer: Boolean;
  OutputDir: string;
  GenericName: string;
  OutputExt: string;
  Overwrite: Boolean;
  BaseNumber: Integer;
  StackSize: Integer;
  StackMode: TStackMode;
  S, S2: string;
  ErrorPos: Integer;
  ParamN: Integer;
  I: Integer;

begin
  FileMode := fmOpenRead;

  PrintVer := (CmdObj.CmdLine.IsCmdOption('V') or CmdObj.CmdLine.IsCmdOption('version'));
  if PrintVer then PrintVersion;

  if (CmdObj.CmdLine.IsCmdOption('?') or CmdObj.CmdLine.IsCmdOption('H') or CmdObj.CmdLine.IsCmdOption('help')) then begin
    PrintHelp;
    Halt(1);
  end;

  if (CmdObj.CmdLine.FileCount < 1) then begin
    if not PrintVer then begin
      WriteLn('**** At least one filemask must be specified');
      WriteLn;
      PrintHelp;
    end;
    Halt(1);
  end;

  // Other options
  InputFileMasks := nil;
  OutputDir := '';
  GenericName := '';
  OutputExt := '.fit';
  StackMode := smAdd;
  Overwrite := False;
  BaseNumber := 1;
  StackSize := 0; // default value: all files to stack

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
      if CmdObj.CmdLine.ExtractParamValue(S, 'O=', OutputDir) then begin
        //
      end
      else
      if CmdObj.CmdLine.ExtractParamValue(S, 'G=', GenericName) then begin
        if GenericName <> '' then begin
          // \/:*?
          if (Pos('\', GenericName) <> 0) or
             (Pos('/', GenericName) <> 0) or
             (Pos(':', GenericName) <> 0) or
             (Pos('*', GenericName) <> 0) or
             (Pos('?', GenericName) <> 0)
          then begin
            WriteLn('**** Generic name must not contain \/:*?');
            Halt(1);
          end;
        end;
      end
      else
      if CmdObj.CmdLine.ExtractParamValue(S, 'X=', S2) then begin
        if S2 <> '' then OutputExt := S2;
        if OutputExt[1] <> '.' then OutputExt := '.' + OutputExt;
      end
      else
      if CmdObj.CmdLine.ExtractParamValue(S, 'M=', S2) then begin
        if S2 <> '' then begin
          S2 := AnsiUpperCase(S2);
          if S2 = 'S' then StackMode := smAdd
          else
          if S2 = 'A' then StackMode := smAvg
          else
          if S2 = 'M' then StackMode := smMed
          else begin
            WriteLn('**** Invalid stack mode. Can be S, A or M');
            Halt(1);
          end;
        end;
      end
      else
      if CmdObj.CmdLine.ExtractParamValue(S, 'B=', S2) then begin
        if S2 <> '' then begin
          Val(S2, BaseNumber, ErrorPos);
          if (ErrorPos <> 0) or (BaseNumber < 0) then begin
            WriteLn('**** Base filenumber must be an integer >= 0');
            Halt(1);
          end;
        end;
      end
      else
      if CmdObj.CmdLine.ParamIsKey(S, 'F') then
        Overwrite := True
      else
      if CmdObj.CmdLine.ExtractParamValue(S, 'N=', S2) then begin
        Val(S2, StackSize, ErrorPos);
        if StackSize < 0 then begin
          WriteLn('**** Stack size must be integer number >= 0 (0 stands for "all files")');
          Halt(1);
        end;
      end
      else begin
        WriteLn('**** Invalid command-line parameter: ' + S);
        Halt(1);
      end;
    end
    else begin
      if S <> '' then begin
        S := ExpandFileName(S);
        if ExtractFileExt(S) = '' then S := ChangeFileExt(S, '.fit');
        SetLength(InputFileMasks, Length(InputFileMasks) + 1);
        InputFileMasks[Length(InputFileMasks) - 1] := S;
      end;
    end;
  end;

  if GenericName = '' then begin
    WriteLn('**** Generic name must be specified (by /G=<name> parameter)');
    Halt(1);
  end;

  if OutputDir = '' then OutputDir := GetCurrentDir;
  if OutputDir <> '' then
    OutputDir := IncludeTrailingPathDelimiter(ExpandFileName(OutputDir));

  FileListAllFiles := TStringList.Create;
  try
    FileList := TStringListNaturalSort.Create;
    try
      ProcessInput(InputFileMasks, StackMode, GenericName, OutputDir, OutputExt, Overwrite, BaseNumber, StackSize);
    finally
      FreeAndNil(FileList);
    end;
  finally
    for I := FileListAllFiles.Count - 1 downto 0 do
      FileListAllFiles.Objects[I].Free;
    FreeAndNil(FileListAllFiles);
  end;
end.

