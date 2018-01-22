{
 To-do!
 1) Time: shifted by UnixToDateTime()
}

{$APPTYPE CONSOLE}

program iconvraw;

uses Windows, SysUtils, DateUtils, Classes, CmdObj, EnumFiles, StringListNaturalSort, LibRawMxWrapper, FITSUtils, FITSTimeUtils, FitsUtilsHelp, CommonIni;

procedure PrintVersion;
begin
  WriteLn('RAW -> CFA converter  Maksym Pyatnytskyy  2017');
  WriteLn('Version 2018.01.21.01');
  WriteLn;
end;

procedure FileError(const S: string);
begin
  raise Exception.Create(S);
end;

type
  TFileEnumClass = class(TObject)
    class function FileEnumProc(const Directory: string; const F: TSearchRec): Boolean;
  end;

var
  FileList: TStringListNaturalSort;

class function TFileEnumClass.FileEnumProc(const Directory: string; const F: TSearchRec): Boolean;
begin
  FileList.Add(Directory + F.Name);
  Result := True;
end;

procedure CheckLibRawError(RawProcessor: Pointer; LibRawError: Integer);
begin
  if LibRawError <> 0 then
    FileError(RawProcessorStrError(RawProcessor, LibRawError));
end;

function MMMtoMonth(const MMM: string): Word;
begin
  if MMM = 'Jan' then Result :=  1 else
  if MMM = 'Feb' then Result :=  2 else
  if MMM = 'Mar' then Result :=  3 else
  if MMM = 'Apr' then Result :=  4 else
  if MMM = 'May' then Result :=  5 else
  if MMM = 'Jun' then Result :=  6 else
  if MMM = 'Jul' then Result :=  7 else
  if MMM = 'Aug' then Result :=  8 else
  if MMM = 'Sep' then Result :=  9 else
  if MMM = 'Oct' then Result := 10 else
  if MMM = 'Nov' then Result := 11 else
  if MMM = 'Dec' then Result := 12 else
  raise EConvertError.Create('Cannot convert Month string to a number');
end;

procedure ConvertFile(const FileName: string;
                      PrintInfo: Boolean;
                      const NewFileName: string;
                      CheckExistence: Boolean;
                      TimeByExposureCorrection: Boolean;
                      DontTruncate: Boolean;
                      TimeShiftInSeconds: Integer;
                      PixelRealNumber: Boolean;
                      BzeroShift: Boolean;
                      FITSParams: TStrings;
                      out TimeCorrected, TimeShifted: Boolean);
var
  RawProcessor: Pointer;
  //LibRawError: Integer;
  RawFrameLeft, RawFrameTop: Word;
  RawFrameWidth, RawFrameHeight: Word;
  RawFrameWidth0, RawFrameHeight0: Word;
  _width, _height: Word;
  year, month, day, hour, min, sec: Word;
  bits: PChar;
  scanline: PChar;
  FITSbpp: Integer;
  BytePerPix: Byte;
  Pixel: Word;
  PixelBytes: array[0..1] of Char absolute Pixel;
  PixelAsSignedInt: SmallInt;
  PixelAsSignedIntBytes: array[0..1] of Char absolute PixelAsSignedInt;
  PixelR: Single;
  PixelRBytes: array[0..3] of Char absolute PixelR;
  X, Y: Word;
  FirstPixel: Boolean;
  MinValue, MaxValue: Word;
  PixelCount: LongWord;
  SumValue: Double;
  FITSFile: FITSRecordFile;
  Image: PChar;
  ImageSize: LongWord; // including padding
  ImageSizeInBlocks: LongWord;
  AverageValue: Double;
  N: LongWord;
  Make, Model: string;
  Instrument: string;
  Software: string;
  ISO: Double;
  ExposureTimeFloat: Double;
  //Timestamp: Int64;
  TimeStr: array[0..25] of Char;
  DateTime: TDateTime;
  DateTimeFile: TDateTime;
  TempS: string;
  FITSHeader: TFITSRecordArray;
  Comments: TStringArray;
  DateTimeComment: string;
  Axes: TIntArray;
  Success: Boolean;
  S: string;
  I, P: Integer;
  Name, Value, TempValue: string;
  Bzero: Integer;
begin
  TimeShifted := False;
  TimeCorrected := False;
  RawFrameLeft := 0;
  RawFrameTop := 0;
  RawFrameWidth := 0;
  RawFrameHeight := 0;
  RawFrameWidth0 := 0;
  RawFrameHeight0 := 0;

  if not PrintInfo then begin
    if CheckExistence and FileExists(NewFileName) then
      FileError('File already exists. Use /F switch to overwrite existing files.');
  end;

  RawProcessor := RawProcessorCreate;
  if RawProcessor = nil then FileError('Cannot create RawProcessor');
  try
    CheckLibRawError(RawProcessor, RawProcessorOpenFile(RawProcessor, PChar(FileName)));
    RawProcessorSizes(RawProcessor, RawFrameWidth0, RawFrameHeight0, _width, _height, RawFrameTop, RawFrameLeft);
    if PrintInfo then begin
      WriteLn;
      WriteLn('Image Size:  ', RawFrameWidth0, 'x', RawFrameHeight0);
      WriteLn('Raw Size:    ', _width, 'x', _height);
      WriteLn('Left Margin: ', RawFrameLeft);
      WriteLn('Top Margin:  ', RawFrameTop);
      Exit;
    end;

    if DontTruncate then begin
      RawFrameLeft := 0;
      RawFrameTop := 0;
      RawFrameWidth := _width;
      RawFrameHeight := _height;
    end
    else begin
      RawFrameWidth := _width - RawFrameLeft;
      RawFrameHeight := _height - RawFrameTop;
    end;

    CheckLibRawError(RawProcessor, RawProcessorUnpack(RawProcessor));
    if RawProcessorCheck(RawProcessor) <> 0 then
      FileError('Not a Bayer-pattern RAW');

    if BzeroShift then begin
      Bzero := 32768;
    end
    else
      Bzero := 0;

    bits := PChar(RawProcessorRawImage(RawProcessor));
    FirstPixel := True;
    SumValue := 0;
    MinValue := 0;
    MaxValue := 0;
    PixelCount := 0;
    if PixelRealNumber then begin
      FITSbpp := -32;
      BytePerPix := 4;
    end
    else begin
      FITSbpp := 16;
      BytePerPix := 2;
    end;
    ImageSize := RawFrameWidth * RawFrameHeight * BytePerPix;
    ImageSizeInBlocks := ImageSize div (RecordsInBlock * SizeOf(FITSRecordType));
    if ImageSize mod (RecordsInBlock * SizeOf(FITSRecordType)) > 0 then Inc(ImageSizeInBlocks);
    ImageSize := ImageSizeInBlocks * RecordsInBlock * SizeOf(FITSRecordType);
    GetMem(Image, ImageSize);
    try
      FillChar(Image[0], ImageSize, 0);
      N := 0;
      for Y := RawFrameTop + RawFrameHeight - 1 downto RawFrameTop do begin
        scanline := @bits[Y * _width * 2];
        for X := RawFrameLeft to RawFrameLeft + RawFrameWidth - 1 do begin
          Pixel := PWord(@scanline[X * 2])^;
          if FirstPixel then begin
            MinValue := Pixel;
            MaxValue := Pixel;
            FirstPixel := False;
          end
          else begin
            if Pixel < MinValue then MinValue := Pixel;
            if Pixel > MaxValue then MaxValue := Pixel;
          end;
          SumValue := SumValue + Pixel;
          Inc(PixelCount);

          if not PixelRealNumber then begin
            if not BzeroShift then begin
              Image[N    ] := PixelBytes[1];
              Image[N + 1] := PixelBytes[0];
            end
            else begin
              PixelAsSignedInt := Pixel - Bzero;
              Image[N    ] := PixelAsSignedIntBytes[1];
              Image[N + 1] := PixelAsSignedIntBytes[0];
            end;
          end
          else begin
            PixelR := Pixel;
            PixelR := PixelR - Bzero;
            Image[N    ] := PixelRBytes[3];
            Image[N + 1] := PixelRBytes[2];
            Image[N + 2] := PixelRBytes[1];
            Image[N + 3] := PixelRBytes[0];
          end;

          Inc(N, BytePerPix);
        end;
      end;

      if not PixelRealNumber and not BzeroShift and (MaxValue > High(SmallInt)) then begin
        FileError('UINT pixel value too big: ' + IntToStr(MaxValue) +
                  ^M^J'Use /R option to make floating-point FITS' +
                  ^M^J'or' +
                  ^M^J'Use /S option to specify BZERO = 32768');
      end;

      if PixelCount > 0 then
        AverageValue := SumValue / PixelCount
      else
        AverageValue := 0;

      //Timestamp := RawProcessorTimestamp(RawProcessor);
      //DateTime := UnixToDateTime(Timestamp);
      DateTime := 0;
      RawProcessorTime(RawProcessor, TimeStr, SizeOf(TimeStr));
      if StrLen(TimeStr) = 25 then begin
        try
           //Www Mmm dd hh:mm:ss yyyy\n
           year := StrToInt(Copy(TimeStr, 21, 4));
           month := MMMtoMonth(Copy(TimeStr, 5, 3));
           day := StrToInt(Copy(TimeStr, 9, 2));
           hour := StrToInt(Copy(TimeStr, 12, 2));
           min := StrToInt(Copy(TimeStr, 15, 2));
           sec := StrToInt(Copy(TimeStr, 18, 2));
           DateTime := EncodeDateTime(year, month, day, hour, min, sec, 0);
        except
          on E: EConvertError do DateTime := 0;
        end;
      end;

      Make := RawProcessorMake(RawProcessor);
      Model := RawProcessorModel(RawProcessor);
      ExposureTimeFloat := RawProcessorShutter(RawProcessor);
      if ExposureTimeFloat < 1 then begin
        // Round it slightly....
        ExposureTimeFloat := ExposureTimeFloat * 1e7;
        ExposureTimeFloat := Round(ExposureTimeFloat);
        ExposureTimeFloat := ExposureTimeFloat / 1e7;
      end;
      ISO := RawProcessorISOspeed(RawProcessor);
      Instrument := '';
      Software := '';
      if (Make <> '') or (Model <> '') then
        Instrument := Trim(Make + ' ' + Model);
      Software := RawProcessorVersion + ' / ' + ExtractFileName(ParamStr(0));

      if DateTime <> 0 then begin
        DateTime := DateTime + TimeShiftInSeconds / (24.0*60.0*60.0);
        TimeShifted := TimeShiftInSeconds <> 0;
        if (ExposureTimeFloat > 0) and TimeByExposureCorrection then begin
          DateTime := DateTime + ExposureTimeFloat / (24.0*60.0*60.0) / 2.0;
          TimeCorrected := True;
        end;
      end;

      SetLength(Axes, 2);
      Axes[0] := RawFrameWidth;
      Axes[1] := RawFrameHeight;

      N := 0;

      SetLength(Comments, N + 1);
      Comments[N] := 'Original filename: ' + ExtractFileName(FileName);
      Inc(N);

      SetLength(Comments, N + 1);
      Comments[N] := 'Original EXIF time: ' + TimeStr;
      Inc(N);

      if TimeShifted then begin
        SetLength(Comments, N + 1);
        Comments[N] := 'DATE-OBS = Original EXIF time shifted by ' + IntToStr(TimeShiftInSeconds) + ' seconds';
        Inc(N);
      end;

      if TimeCorrected then begin
        SetLength(Comments, N + 1);
        Comments[N] := 'DATE-OBS corrected by exposure';
        Inc(N);
      end;

      if ISO <> 0 then begin
        SetLength(Comments, N + 1);
        Comments[N] := 'ISO ' + FloatToStr(ISO);
        Inc(N);
      end;

      SetLength(Comments, N + 1);
      Comments[N] := 'MIN  PIXEL VALUE = ' + IntToStr(MinValue);
      Inc(N);
      SetLength(Comments, N + 1);
      Comments[N] := 'MAX  PIXEL VALUE = ' + IntToStr(MaxValue);
      Inc(N);
      SetLength(Comments, N + 1);
      Str(AverageValue:0:1, TempS);
      Comments[N] := 'MEAN PIXEL VALUE = ' + TempS;
      Inc(N);
      SetLength(Comments, N + 1);
      Comments[N] := 'SOFTWARE: ' + Software;
      Inc(N);

      DateTimeFile := Now();

      DateTimeComment := '';
      if TimeCorrected then DateTimeComment := 'corrected by exposure';

      FITSHeader := MakeFITSHeader(
        FITSbpp,
        Axes,
        Bzero, 1,
        DateTime,          DateTimeComment,
        DateTimeFile,      'FITS creation time (reported by OS)',
        ExposureTimeFloat, 'ExposureTime EXIF value',
        Instrument,
        Comments);

      AssignFile(FITSFile, NewFileName);
      Rewrite(FITSFile);
      try
        BlockWrite(FITSFile, FITSHeader[0], Length(FITSHeader));
        //
        for I := 0 to FITSParams.Count - 1 do begin
          Success := False;
          S := FITSParams[I];
          P := Pos('=', S);
          if P > 0 then begin
            Name := AnsiUpperCase(Trim(Copy(S, 1, P - 1)));
            if (Length(Name) <= FITSKeywordLen) and (Name <> KeywordEND) and (Name <> KeywordHierarch) then begin
              Value := Copy(S, P + 1, MaxInt);
              if (Name = '') or (Name = KeywordComment) or (Name = KeywordHistory) then begin
                if AddCommentLikeKeyword(FITSfile, NewFileName, Name, Value, True) then begin
                  WriteLn;
                  Write('Added ', Name, ' ', TrimRight(Value));
                  Success := True;
                end;
              end
              else begin
                if SetKeywordValue(FITSfile, NewFileName, Name, Value, True, '', True) then begin
                  TempValue := '';
                  if GetKeywordValue(FITSfile, Name, TempValue, False, False) < 0 then begin
                    if (Value <> '') or (TempValue <> '') then
                      FileError('Internal Error: setting value of keyword ' + Name + ' failed.');
                  end;
                  WriteLn;
                  Write('Keyword ', Name, ' set to ', TrimRight(TempValue));
                  Success := True;
                end;
              end;
            end;
            if not Success then
              WriteLn('**** Keyword ', Name, ' cannot be set');
          end;
        end;
        if FITSParams.Count <> 0 then WriteLn else Write(' ');
        Seek(FITSFile, FileSize(FITSFile));
        //
        BlockWrite(FITSFile, Image[0], ImageSize div SizeOf(FITSRecordType));
      finally
        CloseFile(FITSFile);
      end;

    finally
      FreeMem(Image);
      Image := nil;
    end;
  finally
    RawProcessorFree(RawProcessor);
  end;
end;

procedure ProcessInput(const FileMasks: array of string;
                       PrintInfo: Boolean;
                       const GenericName: string;
                       const OutputDir: string;
                       Overwrite: Boolean;
                       BaseNumber: Integer;
                       TimeByExposureCorrection: Boolean;
                       DontTruncate: Boolean;
                       TimeShiftInSeconds: Integer;
                       PixelRealNumber: Boolean;
                       BzeroShift: Boolean;
                       FITSParams: TStrings;
                       const OutputExt: string);
var
  I, N: Integer;
  FileNumber: Integer;
  FileName: string;
  NewFileName: string;
  TempOutputDir: string;
  TimeShifted: Boolean;
  TimeCorrected: Boolean;
begin
  try
    FileNumber := 0;
    for N := 0 to Length(FileMasks) - 1 do begin
      WriteLn;
      WriteLn('[', FileMasks[N], ']');
      FileList.Clear;
      FileEnum(FileMasks[N], faArchive, False, TFileEnumClass.FileEnumProc);
      FileList.NaturalSort;
      for I := 0 to FileList.Count - 1 do begin
        FileName := ExtractFileName(FileList[I]);
        TempOutputDir := OutputDir;
        if TempOutputDir = '' then TempOutputDir := ExtractFilePath(FileList[I]);
        if GenericName <> '' then
          NewFileName := TempOutputDir + GenericName + IntToStr(FileNumber + BaseNumber)
        else
          NewFileName := TempOutputDir + FileName;
        NewFileName := ChangeFileExt(NewFileName, OutputExt);
        Write(FileName);
        if not PrintInfo then
          Write(^I'->'^I, NewFileName);
        ConvertFile(FileList[I], PrintInfo, NewFileName, not Overwrite, TimeByExposureCorrection, DontTruncate, TimeShiftInSeconds, PixelRealNumber, BzeroShift, FITSParams, TimeCorrected, TimeShifted);
        if TimeShifted then Write('DATE-OBS shifted by ', TimeShiftInSeconds, ' seconds');
        if TimeCorrected then Write(' DATE-OBS corrected by exposure');
        WriteLn;
        Inc(FileNumber);
      end;
    end;
    if FileNumber < 1 then begin
      WriteLn;
      WriteLn('**** No files found');
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
  GenericName: string;
  OutputDir: string;
  Overwrite: Boolean;
  TimeByExposureCorrection: Boolean;
  BaseNumber: Integer;
  DontTruncate: Boolean;
  TimeShiftInSeconds: Integer;
  PixelRealNumber: Boolean;
  BzeroShift: Boolean;
  OutputExt: string;
  ErrorPos: Integer;
  PrintVer: Boolean;
  PrintInfo: Boolean;
  FITSparams: TStringList;
  S: string;
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

  PrintInfo := CmdObj.CmdLine.IsCmdOption('I');

  GenericName := CmdObj.CmdLine.KeyValue('G=');
  // \/:*?
  if (Pos('\', GenericName) <> 0) or
     (Pos('/', GenericName) <> 0) or
     (Pos(':', GenericName) <> 0) or
     (Pos('*', GenericName) <> 0) or
     (Pos('?', GenericName) <> 0)
  then begin
    WriteLn('**** Generic name must not contain \/:*?');
    WriteLn;
    PrintHelp;
    Halt(1);
  end;

  OutputDir := CmdObj.CmdLine.KeyValue('O=');
  if OutputDir <> '' then
    OutputDir := IncludeTrailingPathDelimiter(ExpandFileName(OutputDir));

  BaseNumber := 1;
  S := CmdObj.CmdLine.KeyValue('B=');
  if S <> '' then begin
    Val(S, BaseNumber, ErrorPos);
    if (ErrorPos <> 0) or (BaseNumber < 0) then begin
      WriteLn('**** Base filenumber must be integer >= 0');
      WriteLn;
      PrintHelp;
      Halt(1);
    end;
  end;

  Overwrite := CmdObj.CmdLine.IsCmdOption('F');

  TimeByExposureCorrection := CmdObj.CmdLine.IsCmdOption('E');

  DontTruncate := CmdObj.CmdLine.IsCmdOption('L');

  PixelRealNumber := CmdObj.CmdLine.IsCmdOption('R');
  BzeroShift := CmdObj.CmdLine.IsCmdOption('S');

  OutputExt := CmdObj.CmdLine.KeyValue('X=');
  if OutputExt = '' then
    OutputExt := '.fit';
  if OutputExt[1] <> '.' then OutputExt := '.' + OutputExt;

  TimeShiftInSeconds := 0;
  S := CmdObj.CmdLine.KeyValue('TS');
  if S <> '' then begin
    Val(S, TimeShiftInSeconds, ErrorPos);
    if ErrorPos <> 0 then begin
      WriteLn('**** Time Shift must be integer number');
      WriteLn;
      PrintHelp;
      Halt(1);
    end;
  end;

  SetLength(InputFileMasks, N);
  for I := 1 to N do begin
    InputFileMasks[I - 1] := ExpandFileName(CmdObj.CmdLine.ParamFile(I));
  end;

  FITSparams := TStringList.Create;
  try
    for I := 1 to CmdObj.CmdLine.ParamCount do begin
      S := CmdObj.CmdLine.ParamStr(I);
      if CmdObj.CmdLine.FirstCharIsSwitch(S) and (Copy(S, 2, 1) = '$') and (Pos('=', S) <> 0) then begin
          Delete(S, 1, 2);
          FITSparams.Add(S);
      end;
    end;
    FileList := TStringListNaturalSort.Create;
    try
      ProcessInput(InputFileMasks, PrintInfo, GenericName, OutputDir, Overwrite, BaseNumber, TimeByExposureCorrection, DontTruncate, TimeShiftInSeconds, PixelRealNumber, BzeroShift, FITSParams, OutputExt);
      WriteLn;
    finally
      FreeAndNil(FileList);
    end;
  finally
    FreeAndNil(FITSparams);
  end;
end.

