{$APPTYPE CONSOLE}

program iconvraw;

uses Windows, SysUtils, DateUtils, Classes, CmdObj, EnumFiles, StringListNaturalSort, FreeImage, FITSUtils, FITSTimeUtils, FitsUtilsHelp, CommonIni;

procedure PrintVersion;
begin
  WriteLn('RAW -> CFA converter  Maksym Pyatnytskyy  2017');
  WriteLn('Version 2017.12.18.01');
  WriteLn;
end;

procedure FileError(const S: string);
begin
  raise Exception.Create(S);
end;

procedure FreeImageErrorHandler(fif: FREE_IMAGE_FORMAT; Msg: PChar); stdcall;
begin
  FileError(Msg);
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

const
  RAW_UNPROCESSED = 8; // missing in FreeImage.pas

type
  TRational = array[0..1] of LongInt;
  PRational = ^TRational;
  PSmallInt = ^SmallInt;

procedure ConvertFile(const FileName: string;
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
  //fif: FREE_IMAGE_FORMAT;
  dib: PFIBITMAP;
  lpTag: PFITAG;
  bpp: LongWord;
  RawFrameLeft, RawFrameTop, RawFrameWidth, RawFrameHeight: LongWord;
  //BayerPattern: string;
  _width: LongWord;
  _height: LongWord;
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
  X, Y: LongWord;
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
  SoftwareTag: string;
  Software: string;
  ISO: SmallInt;
  ExposureTime: TRational;
  ExposureTimeFloat: Double;
  DateTimeString: string;
  DateTime: TDateTime;
  DateTimeFile: TDateTime;
  FormatSettings: TFormatSettings;
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
  if CheckExistence and FileExists(NewFileName) then
    FileError('File already exists. Use /F switch to overwrite existing files.');
  //fif := FreeImage_GetFileType(PChar(FileName), 0);
  //if (fif <> FIF_RAW) or not FreeImage_FIFSupportsReading(fif) then
  //  FileError('Unsupported file.');
  dib := FreeImage_Load(FIF_RAW, PChar(FileName), RAW_UNPROCESSED); // Output a FIT_UINT16 raw Bayer image
  if (dib = nil) then
    FileError('Error loading bitmap.');
  try
    bpp := FreeImage_GetBPP(dib);
    if bpp <> 16 then
      FileError('Internal error: RAW IMAGE BITPIX <> 16');
    _width := FreeImage_GetWidth(dib);
    _height := FreeImage_GetHeight(dib);

    if DontTruncate then begin
      RawFrameLeft := 0;
      RawFrameTop := 0;
      RawFrameWidth := _width;
      RawFrameHeight := _height;
    end
    else begin
      FreeImage_GetMetadata(FIMD_COMMENTS, dib, 'Raw.Frame.Left', lpTag);
      if (lpTag = nil) then FileError('Metadata error');
      RawFrameLeft := StrToInt(PChar(FreeImage_GetTagValue(lpTag)));
      FreeImage_GetMetadata(FIMD_COMMENTS, dib, 'Raw.Frame.Top', lpTag);
      if (lpTag = nil) then FileError('Metadata error');
      RawFrameTop := StrToInt(PChar(FreeImage_GetTagValue(lpTag)));
      FreeImage_GetMetadata(FIMD_COMMENTS, dib, 'Raw.Frame.Width', lpTag);
      if (lpTag = nil) then FileError('Metadata error');
      RawFrameWidth := StrToInt(PChar(FreeImage_GetTagValue(lpTag)));
      FreeImage_GetMetadata(FIMD_COMMENTS, dib, 'Raw.Frame.Height', lpTag);
      if (lpTag = nil) then FileError('Metadata error');
      RawFrameHeight := StrToInt(PChar(FreeImage_GetTagValue(lpTag)));
    end;

    //FreeImage_GetMetadata(FIMD_COMMENTS, dib, 'Raw.BayerPattern', lpTag);
    //if (lpTag = nil) then FileError('Metadata error');
    //BayerPattern := PChar(FreeImage_GetTagValue(lpTag));

    if BzeroShift then begin
      Bzero := 32768;
    end
    else
      Bzero := 0;

    bits := PChar(FreeImage_GetBits(dib));
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
      for Y := _height - RawFrameHeight - RawFrameTop to _height - RawFrameTop - 1 do begin
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

      Make := '';
      Model := '';
      Instrument := '';
      Software := '';
      ISO := 0;
      FillChar(ExposureTime, SizeOf(ExposureTime), 0);
      ExposureTimeFloat := 0;
      DateTimeString := '';
      DateTime := 0;
      if FreeImage_GetMetadata(FIMD_EXIF_MAIN, dib, 'DateTime', lpTag) then
        DateTimeString := PChar(FreeImage_GetTagValue(lpTag));
      if DateTimeString <> '' then begin
        FillChar(FormatSettings, SizeOf(FormatSettings), 0);
        FormatSettings.DateSeparator := ':';
        FormatSettings.TimeSeparator := ':';
        FormatSettings.ShortDateFormat := 'y/m/d';
        FormatSettings.ShortTimeFormat := 'hh:nn';
        FormatSettings.LongTimeFormat := 'hh:nn:ss';
        try
          DateTime := StrToDateTime(DateTimeString, FormatSettings);
        except
          on E: EConvertError do DateTime := 0;
        end;
      end;
      if FreeImage_GetMetadata(FIMD_EXIF_EXIF, dib, 'ISOSpeedRatings', lpTag) then
        ISO := PSmallInt(FreeImage_GetTagValue(lpTag))^;
      if FreeImage_GetMetadata(FIMD_EXIF_EXIF, dib, 'ExposureTime', lpTag) then begin
        ExposureTime := PRational(FreeImage_GetTagValue(lpTag))^;
        if ExposureTime[1] <> 0 then begin
          ExposureTimeFloat := ExposureTime[0] / ExposureTime[1];
        end;
      end;
      if DateTime <> 0 then begin
        DateTime := DateTime + TimeShiftInSeconds / (24.0*60.0*60.0);
        TimeShifted := TimeShiftInSeconds <> 0;
        if (ExposureTimeFloat > 0) and TimeByExposureCorrection then begin
          DateTime := DateTime + ExposureTimeFloat / (24.0*60.0*60.0) / 2.0;
          TimeCorrected := True;
        end;
      end;
      if FreeImage_GetMetadata(FIMD_EXIF_MAIN, dib, 'Make', lpTag) then
        Make := PChar(FreeImage_GetTagValue(lpTag));
      if FreeImage_GetMetadata(FIMD_EXIF_MAIN, dib, 'Model', lpTag) then
        Model := PChar(FreeImage_GetTagValue(lpTag));
      if (Make <> '') or (Model <> '') then
        Instrument := Trim(Make + ' ' + Model);
      Software := 'FreeImage ' + FreeImage_GetVersion() + ' / ' + ExtractFileName(ParamStr(0));
      if FreeImage_GetMetadata(FIMD_EXIF_MAIN, dib, 'Software', lpTag) then begin
        SoftwareTag := PChar(FreeImage_GetTagValue(lpTag));
        if SoftwareTag <> '' then
          Software := SoftwareTag + ' / ' + Software;
      end;

      SetLength(Axes, 2);
      Axes[0] := RawFrameWidth;
      Axes[1] := RawFrameHeight;

      N := 0;

      SetLength(Comments, N + 1);
      Comments[N] := 'Original filename: ' + ExtractFileName(FileName);
      Inc(N);

      SetLength(Comments, N + 1);
      Comments[N] := 'Original EXIF time string: ' + DateTimeString;
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
        Comments[N] := 'ISO ' + IntToStr(ISO);
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
    FreeImage_Unload(dib);
  end;
end;

procedure ProcessInput(const FileMasks: array of string;
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
        Write(FileName, ^I'->'^I, NewFileName);
        ConvertFile(FileList[I], NewFileName, not Overwrite, TimeByExposureCorrection, DontTruncate, TimeShiftInSeconds, PixelRealNumber, BzeroShift, FITSParams, TimeCorrected, TimeShifted);
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

  FreeImage_SetOutputMessageStdCall(FreeImageErrorHandler);

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
      ProcessInput(InputFileMasks, GenericName, OutputDir, Overwrite, BaseNumber, TimeByExposureCorrection, DontTruncate, TimeShiftInSeconds, PixelRealNumber, BzeroShift, FITSParams, OutputExt);
      WriteLn;
    finally
      FreeAndNil(FileList);
    end;
  finally
    FreeAndNil(FITSparams);
  end;
end.
