{$APPTYPE CONSOLE}

program iconvraw;

uses Windows, SysUtils, DateUtils, Classes, CmdObj, EnumFiles, StringListNaturalSort, LibRawMxWrapper, FITSUtils, FITSTimeUtils, FitsUtilsHelp, CommonIni;

procedure PrintVersion;
begin
  WriteLn('RAW -> CFA converter  Maksym Pyatnytskyy  2017');
  WriteLn('Version 2018.01.26.01');
  WriteLn('Libraw version: ', RawProcessorVersion);
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
                      DontTruncate: Boolean;
                      TimeShiftInSeconds: Integer;
                      PixelRealNumber: Boolean;
                      BzeroShift: Boolean;
                      FITSParams: TStrings;
                      out TimeShifted: Boolean);
var
  RawProcessor: Pointer;
  RawFrameLeft, RawFrameTop: Word;
  RawFrameWidth, RawFrameHeight: Word;
  _width, _height: Word;
  Iwidth, Iheight: Word;
  RawPitch: LongWord;
  PixelAspect: Double;
  ImageFlip: Integer;
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
  Make, Model, Software: string;
  Instrument: string;
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
  RawFrameLeft := 0;
  RawFrameTop := 0;
  RawFrameWidth := 0;
  RawFrameHeight := 0;
  _width := 0;
  _height := 0;
  Iwidth := 0;
  Iheight := 0;
  RawPitch := 0;
  PixelAspect := 0;
  ImageFlip := 0;

  if not PrintInfo then begin
    if CheckExistence and FileExists(NewFileName) then
      FileError('File already exists. Use /F switch to overwrite existing files.');
  end;

  RawProcessor := RawProcessorCreate;
  if RawProcessor = nil then FileError('Cannot create RawProcessor');
  try
    CheckLibRawError(RawProcessor, RawProcessorOpenFile(RawProcessor, PChar(FileName)));
    if not PrintInfo then
      CheckLibRawError(RawProcessor, RawProcessorUnpack(RawProcessor))
    else
      CheckLibRawError(RawProcessor, RawProcessorAdjustSizesInfoOnly(RawProcessor));

    RawProcessorSizes(
      RawProcessor,
      _width, _height,                // raw_width, raw_height
      RawFrameWidth, RawFrameHeight,  // width, height
      RawFrameLeft, RawFrameTop,      // left_margin, top_marhin
      Iwidth, Iheight,                // iwidth, iheight
      RawPitch,                       // raw_pitch -- will not be assigned if RawProcessorUnpack is not called
      PixelAspect,                    // pixel_aspect -- equal 1 even for FUJI_S5000 RAF (strange...)
      ImageFlip                       // flip
    );

    Make := RawProcessorMake(RawProcessor);
    Model := RawProcessorModel(RawProcessor);
    Software := RawProcessorSoftware(RawProcessor);
    ExposureTimeFloat := RawProcessorShutter(RawProcessor);
    ISO := RawProcessorISOspeed(RawProcessor);
    RawProcessorTime(RawProcessor, TimeStr, SizeOf(TimeStr));

    if PrintInfo then begin
      WriteLn;
      WriteLn('Make        : ', Make);
      WriteLn('Model       : ', Model);
      WriteLn('Software    : ', Software);
      WriteLn('Time        : ', TrimRight(TimeStr));
      WriteLn('ISO         : ', Round(ISO));
      WriteLn('Exposure    : ', ExposureTimeFloat:9:7);
      WriteLn('Raw Size    : ', _width, 'x', _height);
      WriteLn('Image Size  : ', RawFrameWidth, 'x', RawFrameHeight);
      WriteLn('Left Margin : ', RawFrameLeft);
      WriteLn('Top Margin  : ', RawFrameTop);
      WriteLn('Output Size : ', Iwidth, 'x', Iheight);
      Exit;
    end;

    if (2 * _width <> RawPitch) then
      FileError('RAW image must be 16-bit.');

    if DontTruncate then begin
      RawFrameLeft := 0;
      RawFrameTop := 0;
      RawFrameWidth := _width;
      RawFrameHeight := _height;
    end
    else begin
      if ((RawFrameLeft + RawFrameWidth) > _width) or ((RawFrameTop + RawFrameHeight) > _height) then
        FileError('Don''t know how to work with the image: Raw sizes less than image sizes. Use /L switch to convert.');
    end;

    if RawProcessorCheck(RawProcessor) <> 0 then
      FileError('Don''t know how to work with non-Bayer RAW.');

    //Timestamp := RawProcessorTimestamp(RawProcessor);
    //DateTime := UnixToDateTime(Timestamp);
    DateTime := 0;
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
    // FITS image
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

      Instrument := '';
      if (Make <> '') or (Model <> '') then
        Instrument := Trim(Make + ' ' + Model);
      if Software <> '' then
        Software := Software + ' / ';
      Software := 'libraw ' + RawProcessorVersion + ' / ' + ExtractFileName(ParamStr(0));

      if DateTime <> 0 then begin
        DateTime := DateTime + TimeShiftInSeconds / (24.0*60.0*60.0);
        TimeShifted := TimeShiftInSeconds <> 0;
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

      FITSHeader := MakeFITSHeader(
        FITSbpp,
        Axes,
        Bzero, 1,
        DateTime,          DateTimeComment,
        DateTimeFile,      'FITS creation time (reported by OS)',
        ExposureTimeFloat, '',
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
                if AddCommentLikeKeyword(FITSfile, Name, Value, True) then begin
                  WriteLn;
                  Write('Added ', Name, ' ', TrimRight(Value));
                  Success := True;
                end;
              end
              else begin
                if SetKeywordValue(FITSfile, Name, Value, True, '', True) then begin
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
        ConvertFile(FileList[I], PrintInfo, NewFileName, not Overwrite, DontTruncate, TimeShiftInSeconds, PixelRealNumber, BzeroShift, FITSParams, TimeShifted);
        if TimeShifted then Write('DATE-OBS shifted by ', TimeShiftInSeconds, ' seconds');
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
  BaseNumber: Integer;
  DontTruncate: Boolean;
  TimeShiftInSeconds: Integer;
  PixelRealNumber: Boolean;
  BzeroShift: Boolean;
  OutputExt: string;
  PrintInfo: Boolean;
  FITSparams: TStringList;
  PrintVer: Boolean;
  ErrorPos: Integer;
  S, S2: string;
  ParamN: Integer;

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
  GenericName := '';
  OutputDir := '';
  Overwrite := False;
  BaseNumber := 1;
  DontTruncate := False;
  TimeShiftInSeconds := 0;
  PixelRealNumber := False;
  BzeroShift := False;
  OutputExt := '.fit';
  PrintInfo := False;
  FITSparams := TStringList.Create;
  try
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
        if CmdObj.CmdLine.ParamIsKey(S, 'I') then
          PrintInfo := True
        else
        if CmdObj.CmdLine.ParamIsKey(S, 'F') then
          Overwrite := True
        else
        if CmdObj.CmdLine.ParamIsKey(S, 'L') then
          DontTruncate := True
        else
        if CmdObj.CmdLine.ParamIsKey(S, 'R') then
          PixelRealNumber := True
        else
        if CmdObj.CmdLine.ParamIsKey(S, 'S') then
          BzeroShift := True
        else
        if (Copy(S, 2, 1) = '$') and (Pos('=', S) <> 0) then
          FITSparams.Add(Copy(S, 3, MaxInt))
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
        if CmdObj.CmdLine.ExtractParamValue(S, 'O=', OutputDir) then begin
          if OutputDir <> '' then
            OutputDir := IncludeTrailingPathDelimiter(ExpandFileName(OutputDir));
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
        if CmdObj.CmdLine.ExtractParamValue(S, 'X=', S2) then begin
          if S2 <> '' then OutputExt := S2;
          if OutputExt[1] <> '.' then OutputExt := '.' + OutputExt;
        end
        else
        if CmdObj.CmdLine.ExtractParamValue(S, 'TS', S2) then begin
          if S2 <> '' then begin
            Val(S2, TimeShiftInSeconds, ErrorPos);
            if ErrorPos <> 0 then begin
              WriteLn('**** Time Shift must be an integer number');
              Halt(1);
            end;
          end;
        end
        else begin
          WriteLn('**** Invalid command-line parameter: ' + S);
          Halt(1);
        end;
      end
      else begin
        if S <> '' then begin
          SetLength(InputFileMasks, Length(InputFileMasks) + 1);
          InputFileMasks[Length(InputFileMasks) - 1] := ExpandFileName(S);
        end;
      end;
    end;

    FileList := TStringListNaturalSort.Create;
    try
      ProcessInput(InputFileMasks, PrintInfo, GenericName, OutputDir, Overwrite, BaseNumber, DontTruncate, TimeShiftInSeconds, PixelRealNumber, BzeroShift, FITSParams, OutputExt);
      WriteLn;
    finally
      FreeAndNil(FileList);
    end;
  finally
    FreeAndNil(FITSparams);
  end;
end.

