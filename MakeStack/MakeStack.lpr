{*****************************************************************************}
{                                                                             }
{ MakeStack                                                                   }
{ (c) 2017 Maksym Pyatnytskyy                                                 }
{                                                                             }
{ This program is distributed                                                 }
{ WITHOUT ANY WARRANTY; without even the implied warranty of                  }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                        }
{                                                                             }
{*****************************************************************************}

{$APPTYPE CONSOLE}
{$MODE DELPHI}
{$INCLUDE FITSUtils.inc}

{.ASSERTIONS ON}

program MakeStack;

uses
  SysUtils, Classes, Math, DateUtils, CmdObj{, CmdObjStdSwitches}, Version, EnumFiles, 
  FITScompatibility, FITSUtils, FITSStatUtils, FITSTimeUtils, StringListNaturalSort,
  FitsUtilsHelp, CalcThread, CommonIni;

{$R *.res}

{$INCLUDE PrintVersion.inc}

procedure FileError(const S: string);
begin
  raise Exception.Create(S);
end;

type
  TOutFITSbitpix = (bitpixDefault, bitpixU8, bitpixI16, bitpixI32, bitpixF32, bitpixF64);

const
  MIN_PIXELS_PER_THREAD = 1024;
  MAX_THREADS           = 32;

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
  S, S2, SBuf: string;
begin
  S := PadCh('', Round(N / Nmax * 60), '#');
  S := PadCh(S, 60, ' ');
  S2 := PadCh(Copy(Info, 1, 13), 13, ' ');
  SBuf := #13 + S2 + LeftPadCh(IntToStr(Round(N / Nmax * 100)), 3, ' ') + '% ' + S;
  Write(SBuf);
end;

var
  FileList: TStringListNaturalSort;
  FileListAllFiles: TStringList;
  ProgressProcCriticalSection: TRTLCriticalSection;

type
  TProgressProcWrapper = class(TObject)
    private
      FItems: Integer;
      FThreadCounters: array of Integer;
      FInfo: string;
    public
      function ThreadProgressProc(ThreadNo, Counter, FStartIndex, FNumberOfItems: Integer): Boolean;
      constructor Create(NumberOfThreads: Integer; Items: Integer; const Info: string);
  end;

  { TProgressProcWrapper }

  constructor TProgressProcWrapper.Create(NumberOfThreads: Integer; Items: Integer; const Info: string);
  begin
    inherited Create;
    SetLength(FThreadCounters, NumberOfThreads);
    FItems := Items;
    FInfo := Info;
  end;

  function TProgressProcWrapper.ThreadProgressProc(ThreadNo, Counter, FStartIndex, FNumberOfItems: Integer): Boolean;
  var
    N, I: Integer;
  begin
    Result := True;
    EnterCriticalSection(ProgressProcCriticalSection);
    try
      N := 0;
      if (ThreadNo >= 0) and (ThreadNo < Length(FThreadCounters)) then
        FThreadCounters[ThreadNo] := Counter;
      for I := 0 to Length(FThreadCounters) - 1 do
        N := N + FThreadCounters[I];
      ShowProgress(FInfo, N, FItems);
    finally
      LeaveCriticalSection(ProgressProcCriticalSection);
    end;
  end;

procedure GetNumberOfThreadsAndItemsInFirstThread(ItemCount: Integer; CmdLineNumberOfThreads: Integer; MinItemsPerThread: Integer; out NumberOfThreads: Integer; out ItemsToProcessInFirstThread: Integer);
begin
  if CmdLineNumberOfThreads <= 0 then
    NumberOfThreads := Min(Max(1, GetLogicalCpuCount), MAX_THREADS)
  else
    NumberOfThreads := CmdLineNumberOfThreads;
  if NumberOfThreads = 1 then
    ItemsToProcessInFirstThread := ItemCount
  else
    ItemsToProcessInFirstThread := Min(ItemCount, Max(MinItemsPerThread, ItemCount div NumberOfThreads));
end;

procedure DoStackProc(StackMode: TStackMode;
                      NormalizeMVal: Double;
                      OutFITSbitpix: TOutFITSbitpix;
                      const GenericName: string;
                      const OutputDir: string;
                      const OutputExt: string;
                      Overwrite: Boolean;
                      StackNumber: Integer;
                      const StackList: TStringList;
                      CmdLineNumberOfThreads: Integer;
                      const NormalizeFactorsFileName: string);
var
  FileName: string;
  OutFileName: string;
  FileInfo: TFITSFileInfo;
  FITSfile: FITSRecordFile;
  Images: TPCharArray;
  NormalizationFactors: TExtendedArray;
  DestImage: PChar;
  DestImageMemSize: PtrUInt;
  MinBitPix, MaxBitPix: Integer;
  DestBitPix: Integer;
  DestBytePix: Integer;
  DestNaxis: TIntArray;
  DestHeader: TFITSRecordArray;
  DestPixelArray: TDoubleArray;
  NormalizeRangeErrorCount: Integer;
  ScaledOrShifted: Boolean;
  UseFast16bitProcs: Boolean;
  TotalExposure: Double;
  DestDateObs: TDateTime;
  DestObject: string;
  DestTelescope: string;
  DestInstrument: string;
  S: string;
  Pixels: Integer;
  I, II: Integer;
  Comments: TStringArray;
  Threads: array of TPrimitiveThread;
  StackedResultMax, StackedResultMin: Extended;
  NumberOfThreads: Integer;
  StartIndex: Integer;
  ItemsToProcessInThread: Integer;
  ItemsRemained: Integer;
  InitialValuesSet: Boolean;
  ProgressProcWrapper: TProgressProcWrapper;
  InfoStr: string;
  TimeStart: TDateTime;
  OutOfRangeErrorCount: Integer;
  FactorsFile: TextFile;
begin
  if StackList.Count < 1 then Exit;
  NormalizationFactors := nil;
  OutFileName := IncludeTrailingPathDelimiter(OutputDir) + GenericName;
  if StackNumber >= 0 then OutFileName := OutFileName + IntToStr(StackNumber);
  OutFileName := OutFileName + OutputExt;
  WriteLn('Stacking ', StackList.Count, ' files. Output file: ' + ExtractFileName(OutFileName));
  if not Overwrite and FileExists(OutFileName) then
    FileError('File ' + AnsiQuotedStr(OutFileName, '"') + ' already exists. Use /F switch to overwrite.');
  FileInfo := TFITSFileInfo(StackList.Objects[0]);
  MinBitPix := FileInfo.BitPix;
  MaxBitPix := FileInfo.BitPix;
  DestNaxis := Copy(FileInfo.Naxis, 0, MaxInt);
  Comments := nil;
  SetLength(Images, StackList.Count);
  for I := 0 to Length(Images) - 1 do
    Images[I] := nil;
  try
    SetLength(Comments, Length(Comments) + 1);
    Comments[Length(Comments) - 1] := 'Stack of ' + IntToStr(StackList.Count) + ' images';
    SetLength(Comments, Length(Comments) + 1);
    Comments[Length(Comments) - 1] := 'Stacking mode: ' + StackModeToString(StackMode);
    if NormalizeMVal > 0 then begin
      SetLength(Comments, Length(Comments) + 1);
      Str(NormalizeMVal:0:2, S);
      Comments[Length(Comments) - 1] := 'Normalization value: ' + S;
    end;
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

    InfoStr := 'Reading';
    ShowProgress(InfoStr, 0, StackList.Count);
    ScaledOrShifted := False;
    UseFast16bitProcs := False;
    for I := 0 to StackList.Count - 1 do begin
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
      ShowProgress(InfoStr, I + 1, StackList.Count);
    end;
    ShowProgress(InfoStr, StackList.Count, StackList.Count);

    UseFast16bitProcs := (not ScaledOrShifted) and (MinBitPix = 16) and (MaxBitPix = 16);

    TimeStart := Now();

    Pixels := 1;

{$IFNDEF range_check}{$R+}{$ENDIF}
{$IFNDEF overflow_check}{$Q+}{$ENDIF}
    for I := 0 to Length(DestNaxis) - 1 do
      Pixels := Pixels * DestNaxis[I];
{$IFNDEF range_check}{$R-}{$ENDIF}
{$IFNDEF overflow_check}{$Q-}{$ENDIF}

    if NormalizeMVal > 0 then begin
      SetLength(NormalizationFactors, StackList.Count);
      InitCriticalSection(ProgressProcCriticalSection);
      try
        GlobalTerminateAllThreads := False;
        GetNumberOfThreadsAndItemsInFirstThread(StackList.Count, CmdLineNumberOfThreads, 1, NumberOfThreads, ItemsToProcessInThread);
        SetLength(Threads, NumberOfThreads);
        for I := 0 to Length(Threads) - 1 do Threads[I] := nil; // not nesessary, already initialized
        try
          InfoStr := 'N:' + LeftPadCh(IntToStr(NumberOfThreads), 2, ' ') + ' threads';
          ShowProgress(InfoStr, 0, Pixels);
          ProgressProcWrapper := TProgressProcWrapper.Create(NumberOfThreads, StackList.Count, InfoStr);
          try
            StartIndex := 0;
            ItemsRemained := StackList.Count;
            for I := 0 to NumberOfThreads - 1 do begin
              if I = NumberOfThreads - 1 then
                ItemsToProcessInThread := ItemsRemained;
              Threads[I] := TNormThread.Create(I,
                                               StartIndex,
                                               ItemsToProcessInThread,
                                               Pixels,
                                               StackList,
                                               Images,
                                               @NormalizationFactors,
                                               NormalizeMVal,
                                               UseFast16bitProcs,
                                               ProgressProcWrapper.ThreadProgressProc);
              // check for exception while creation (see FPC docs)
              if Assigned(Threads[I].FatalException) then begin
                if Assigned(Threads[I].FatalException) then begin
                  if Threads[I].FatalException is Exception then
                    FileError(Exception(Threads[I].FatalException).Message)
                  else
                    FileError('Unknown Exception');
                end;
              end;

              ItemsRemained := ItemsRemained - ItemsToProcessInThread;
              StartIndex := StartIndex + ItemsToProcessInThread;
            end;

            if ItemsRemained <> 0 then FileError('Internal error while creating normalization threads');

            for I := 0 to NumberOfThreads - 1 do
              Threads[I].Start;

            for I := 0 to NumberOfThreads - 1 do
              Threads[I].WaitFor;

            for I := 0 to NumberOfThreads - 1 do begin
              if Assigned(Threads[I].FatalException) then begin
                if Threads[I].FatalException is Exception then
                  FileError(Exception(Threads[I].FatalException).Message)
                else
                  FileError('Unknown Exception');
              end;
            end;
          finally
            FreeAndNil(ProgressProcWrapper);
          end;
        finally
          for I := Length(Threads) - 1 downto 0 do
            FreeAndNil(Threads[I]);
          Threads := nil;
        end;
      finally
        DoneCriticalSection(ProgressProcCriticalSection);
      end;

      if NormalizeFactorsFileName <> '' then begin
        Assign(FactorsFile, NormalizeFactorsFileName);
        try
          if FileExists(NormalizeFactorsFileName) then begin
            Append(FactorsFile);
            WriteLn(FactorsFile);
          end
          else
            Rewrite(FactorsFile);
          for I := 0 to StackList.Count - 1 do
            WriteLn(FactorsFile, NormalizationFactors[I]);
        finally
          CloseFile(FactorsFile);
        end;
      end;
    end;

    // Stacking

    // DestNaxis[*] cannot be zero, see FITSUtils.GetBitPixAndNaxis
    SetLength(DestPixelArray, Pixels);
    NormalizeRangeErrorCount := 0;

    InitCriticalSection(ProgressProcCriticalSection);
    try
      GlobalTerminateAllThreads := False;
      GetNumberOfThreadsAndItemsInFirstThread(Pixels, CmdLineNumberOfThreads, MIN_PIXELS_PER_THREAD, NumberOfThreads, ItemsToProcessInThread);
      SetLength(Threads, NumberOfThreads);
      for I := 0 to Length(Threads) - 1 do Threads[I] := nil; // not nesessary, already initialized
      try
        InfoStr := 'S:' + LeftPadCh(IntToStr(NumberOfThreads), 2, ' ') + ' threads';
        ShowProgress(InfoStr, 0, Pixels);
        ProgressProcWrapper := TProgressProcWrapper.Create(NumberOfThreads, Pixels, InfoStr);
        try
          StartIndex := 0;
          ItemsRemained := Pixels;
          for I := 0 to NumberOfThreads - 1 do begin
            if I = NumberOfThreads - 1 then
              ItemsToProcessInThread := ItemsRemained;
            Threads[I] := TCalcThread.Create(I,
                                             StartIndex,
                                             ItemsToProcessInThread,
                                             StackMode,
                                             StackList,
                                             Images,
                                             NormalizationFactors,
                                             @DestPixelArray,
                                             @NormalizeRangeErrorCount,
                                             UseFast16bitProcs,
                                             ProgressProcWrapper.ThreadProgressProc);
            // check for exception while creation (see FPC docs)
            if Assigned(Threads[I].FatalException) then begin
              if Assigned(Threads[I].FatalException) then begin
                if Threads[I].FatalException is Exception then
                  FileError(Exception(Threads[I].FatalException).Message)
                else
                  FileError('Unknown Exception');
              end;
            end;

            ItemsRemained := ItemsRemained - ItemsToProcessInThread;
            StartIndex := StartIndex + ItemsToProcessInThread;
          end;

          if ItemsRemained <> 0 then FileError('Internal error whle creating stacking threads');

          for I := 0 to NumberOfThreads - 1 do
            Threads[I].Start;

          for I := 0 to NumberOfThreads - 1 do
            Threads[I].WaitFor;

          for I := 0 to NumberOfThreads - 1 do begin
            if Assigned(Threads[I].FatalException) then begin
              if Threads[I].FatalException is Exception then
                FileError(Exception(Threads[I].FatalException).Message)
              else
                FileError('Unknown Exception');
            end;
          end;
        finally
          FreeAndNil(ProgressProcWrapper);
        end;

        StackedResultMin := 0;
        StackedResultMax := 0;
        InitialValuesSet := False;
        for I := 0 to NumberOfThreads - 1 do begin
          if Threads[I].ExecuteCompleted then begin // ExecuteCompleted is not set if PixelsToStackInThread was 0, however this should never occur
            if not InitialValuesSet then begin
              StackedResultMin := (Threads[I] as TCalcThread).StackedResultMin;
              StackedResultMax := (Threads[I] as TCalcThread).StackedResultMax;
              InitialValuesSet := True;
            end
            else begin
              if (Threads[I] as TCalcThread).StackedResultMin < StackedResultMin then StackedResultMin := (Threads[I] as TCalcThread).StackedResultMin;
              if (Threads[I] as TCalcThread).StackedResultMax > StackedResultMax then StackedResultMax := (Threads[I] as TCalcThread).StackedResultMax;
            end;
          end;
        end;
      finally
        for I := Length(Threads) - 1 downto 0 do
          FreeAndNil(Threads[I]);
        Threads := nil;
      end;
    finally
      DoneCriticalSection(ProgressProcCriticalSection);
    end;

    ShowProgress(InfoStr, Pixels, Pixels);
    WriteLn;
    WriteLn('Done. Calculation time: ', (Now() - TimeStart)*24*60*60:0:1, 's');
    if NormalizeRangeErrorCount > 0 then
      WriteLn('**** WARNING: overflow for ', NormalizeRangeErrorCount, ' pixels while normalization of ', StackList.Count, ' images');

    for I := Length(Images) - 1 downto 0 do begin
      if Images[I] <> nil then begin
        FreeMem(Images[I]);
        Images[I] := nil;
      end;
    end;

    // Default output bitpix
    if (MaxBitPix = MinBitPix) and (StackMode <> smAdd) and not ScaledOrShifted then
      DestBitPix := MaxBitPix
    else
    if MinBitPix = -64 then
      DestBitPix := -64
    else
    if MinBitPix = -32 then begin
      if (StackedResultMax < MaxSingle) and (StackedResultMin > -MaxSingle) then
        DestBitPix := -32
      else
        DestBitPix := -64
    end
    else
      DestBitPix := -32;

    case OutFITSbitpix of
      bitpixU8:  DestBitPix := 8;
      bitpixI16: DestBitPix := 16;
      bitpixI32: DestBitPix := 32;
      bitpixF32: DestBitPix := -32;
      bitpixF64: DestBitPix := -64;
      else ; // default value.
    end;

    DestBytePix := Abs(DestBitPix) div 8;
    DestImageMemSize := Pixels * DestBytePix;
    // Align DestImageMemSize to FITSRecordLen * RecordsInBlock to simpliry writing.
    DestImageMemSize := ((DestImageMemSize - 1) div (FITSRecordLen * RecordsInBlock) + 1) * FITSRecordLen * RecordsInBlock;
    GetMem(DestImage, DestImageMemSize);
    try
      FillChar(DestImage^, DestImageMemSize, 0);

      OutOfRangeErrorCount := 0;
      for I := 0 to Length(DestPixelArray) - 1 do
        SetFITSpixel(DestImage, I, DestBitPix, DestPixelArray[I], OutOfRangeErrorCount);
      if OutOfRangeErrorCount > 0 then
        WriteLn('**** WARNING: overflow for ', OutOfRangeErrorCount, ' pixels (output BITPIX=', DestBitPix, ')');

      DestHeader := MakeFITSHeader(DestBitPix, DestNaxis, 0, 1,
                                   DestDateObs, 'Stack Mid Time, fixed by Exposure Time',
                                   LocalTimeToUniversal(Now()), 'Stacked FITS creation time (by MakeStack)',
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

procedure DoStacking(StackMode: TStackMode;
                     NormalizeMVal: Double;
                     OutFITSbitpix: TOutFITSbitpix;
                     const GenericName: string;
                     const OutputDir: string;
                     const OutputExt: string;
                     Overwrite: Boolean;
                     BaseNumber: Integer;
                     StackSize: Integer;
                     CmdLineNumberOfThreads: Integer;
                     DontAddNumberIfStackAll: Boolean;
                     const NormalizeFactorsFileName: string);
var
  StackList: TStringList;
  StackNumber: Integer;
  I: Integer;
begin
  StackNumber := BaseNumber;
  if (StackSize = 0) and DontAddNumberIfStackAll then StackNumber := -1;
  StackList := TStringList.Create;
  try
    for I := 0 to FileListAllFiles.Count - 1 do begin
      StackList.AddObject(FileListAllFiles[I], FileListAllFiles.Objects[I]);
      if (StackSize > 0) and (StackList.Count = StackSize) then begin
        DoStackProc(StackMode, NormalizeMVal, OutFITSbitpix, GenericName, OutputDir, OutputExt, Overwrite, StackNumber, StackList, CmdLineNumberOfThreads, NormalizeFactorsFileName);
        StackList.Clear;
        Inc(StackNumber);
      end;
    end;
    if StackList.Count > 0 then begin
      DoStackProc(StackMode, NormalizeMVal, OutFITSbitpix, GenericName, OutputDir, OutputExt, Overwrite, StackNumber, StackList, CmdLineNumberOfThreads, NormalizeFactorsFileName);
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
  FitsInfo: TFITSFileInfo;
begin
  AssignFile(FITSfile, FileName);
  Reset(FITSfile);
  try
    if not IsFits(FITSfile) then
      FileError('Not a valid FITS file: ' + AnsiQuotedStr(FileName, '"'));

    FitsInfo := TFITSFileInfo.Create;
    FileListAllFiles.AddObject(FileName, FitsInfo);
    GetFITSproperties(FITSfile, FitsInfo.BitPix, FitsInfo.Naxis, FitsInfo.StartOfImage, FitsInfo.ImageMemSize); // ImageMemSize is padded to FITSRecordLen!
    if (FitsInfo.BitPix <> 8) and (FitsInfo.BitPix <> 16) and (FitsInfo.BitPix <> 32) and (FitsInfo.BitPix <> -32) and (FitsInfo.BitPix <> -64) then
      FileError('Nonstandard BitPix = ' + IntToStr(FitsInfo.BitPix) + ' in file ' + AnsiQuotedStr(FileName, '"'));
    GetBscaleBzero(FITSfile, FitsInfo.BScale, FitsInfo.BZero);
    FitsInfo.DateObs := GetDateObs(FITSfile);
    FitsInfo.Exposure := GetExposureTime(FITSfile);
    FitsInfo.ObjectName := GetFITSstringValue(FITSfile, 'OBJECT');
    FitsInfo.Telescope  := GetFITSstringValue(FITSfile, 'TELESCOP');
    FitsInfo.Instrument := GetFITSstringValue(FITSfile, 'INSTRUME');
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

procedure ProcessInput(const FileMasks: array of string;
                       StackMode: TStackMode;
                       NormalizeMVal: Double;
                       OutFITSbitpix: TOutFITSbitpix;
                       const GenericName: string;
                       const OutputDir: string;
                       const OutputExt: string;
                       Overwrite: Boolean;
                       BaseNumber: Integer;
                       StackSize: Integer;
                       CmdLineNumberOfThreads: Integer;
                       DontAddNumberIfStackAll: Boolean;
                       const NormalizeFactorsFileName: string);
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
      Write(Ntotal, ' files to process. Mode: ', StackModeToString(StackMode));
      if NormalizeMVal > 0 then Write(' with Normalization');
      WriteLn;
      WriteLn;
      DoStacking(StackMode, NormalizeMVal, OutFITSbitpix, GenericName, OutputDir, OutputExt, Overwrite, BaseNumber, StackSize, CmdLineNumberOfThreads, DontAddNumberIfStackAll, NormalizeFactorsFileName);
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
  OutFITSbitpix: TOutFITSbitpix;
  CmdLineNumberOfThreads: Integer;
  DontAddNumberIfStackAll: Boolean;
  NormalizeMVal: Double;
  NormalizeFactorsFileName: string;
  S, S2: string;
  ParamN: Integer;
  I: Integer;

begin
  FileMode := fmOpenRead;

  PrintVer := (CmdObj.CmdLine.IsCmdOption('V') or CmdObj.CmdLine.IsCmdOption('version'));
  if PrintVer then PrintVersion('FITS stack (multithreaded)');

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
  OutFITSbitpix := bitpixDefault;
  Overwrite := False;
  BaseNumber := 1;
  StackSize := 0; // default value: all files to stack
  CmdLineNumberOfThreads := 0;
  DontAddNumberIfStackAll := False;
  NormalizeMVal := 0;
  NormalizeFactorsFileName := '';

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
             (Pos('?', GenericName) <> 0) or
             (Pos('<', GenericName) <> 0) or
             (Pos('>', GenericName) <> 0)
          then begin
            WriteLn('**** Generic name must not contain \/:*?<>');
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
      if CmdObj.CmdLine.ExtractParamValue(S, 'FORMAT=', S2) then begin
        if S2 <> '' then begin
          S2 := AnsiUpperCase(S2);
          if S2 = 'U8'  then OutFITSbitpix := bitpixU8
          else
          if S2 = 'I16' then OutFITSbitpix := bitpixI16
          else
          if S2 = 'I32' then OutFITSbitpix := bitpixI32
          else
          if S2 = 'F32' then OutFITSbitpix := bitpixF32
          else
          if S2 = 'F64' then OutFITSbitpix := bitpixF64
          else begin
            WriteLn('**** Invalid output FORMAT. Allowed values: U8, I16, I32, F32, F64');
            Halt(1);
          end;
        end;
      end
      else
      if CmdObj.CmdLine.ExtractParamValue(S, 'NORM=', S2) then begin
        if S2 <> '' then begin
          if (not GetDouble(S2, NormalizeMVal)) or (NormalizeMVal <= 0) then begin
            WriteLn('**** Normalization value be > 0');
            Halt(1);
          end;
        end;
      end
      else
      if CmdObj.CmdLine.ExtractParamValue(S, 'B=', S2) then begin
        if S2 <> '' then begin
          if (not GetInt(S2, BaseNumber)) or (BaseNumber < 0) then begin
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
        if (not GetInt(S2, StackSize)) or (StackSize < 0) then begin
          WriteLn('**** Stack size must be integer number >= 0 (0 stands for "all files")');
          Halt(1);
        end;
      end
      else
      if CmdObj.CmdLine.ParamIsKey(S, 'A') then
        DontAddNumberIfStackAll := True
      else
      if CmdObj.CmdLine.ExtractParamValue(S, 'T=', S2) then begin
        if (not GetInt(S2, CmdLineNumberOfThreads)) or (CmdLineNumberOfThreads <= 0) or (CmdLineNumberOfThreads > MAX_THREADS) then begin
          WriteLn('**** Number of threads must be in a range [1..' + IntToStr(MAX_THREADS) + ']');
          Halt(1);
        end;
      end
      else
      if CmdObj.CmdLine.ExtractParamValue(S, 'NORMFACTORS=', NormalizeFactorsFileName) then begin
        //
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
      ProcessInput(InputFileMasks, StackMode, NormalizeMVal, OutFITSbitpix, GenericName, OutputDir, OutputExt, Overwrite, BaseNumber, StackSize, CmdLineNumberOfThreads, DontAddNumberIfStackAll, NormalizeFactorsFileName);
    finally
      FreeAndNil(FileList);
    end;
  finally
    for I := FileListAllFiles.Count - 1 downto 0 do
      FileListAllFiles.Objects[I].Free;
    FreeAndNil(FileListAllFiles);
  end;
end.

