{*****************************************************************************}
{                                                                             }
{ ICONVRAW                                                                    }
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

program iconvraw;

uses 
  SysUtils, Variants, DateUtils, Classes, CmdObj, Version, EnumFiles,
  FITScompatibility, MiscUtils, StringListNaturalSort, LibRawMxWrapper,
  FITSUtils, FITSTimeUtils, ConvUtils, FitsUtilsHelp, CommonIni;

{$R *.res}

{$INCLUDE PrintVersion.inc}

type
  TPrintInfoMode = (infoNo, infoNormal, infoTable);

var
  LibRawLoaded: Boolean = False;
  LibRawWrapperName: string = '';

procedure PrintVersion2;
begin
  PrintVersion('RAW -> CFA converter');
  if LibRawLoaded then begin
    WriteLn('Libraw Wrapper: ', LibRawWrapperName);
    WriteLn('Libraw Version: ', RawProcessorVersion);
  end;
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

procedure CheckLibRawError(LibRawError: Integer);
begin
  if LibRawError <> 0 then
    FileError(RawProcessorStrError(nil, LibRawError));
end;

procedure ConvertFile(const FileName: string;
                      PrintInfo: TPrintInfoMode;
                      PrintTiming: Boolean;
                      const NewFileName: string;
                      CheckExistence: Boolean;
                      DontTruncate: Boolean;
                      DoFlip: Boolean;
                      PixelRealNumber: Boolean;
                      BzeroShift: Boolean;
                      FITSParams: TStrings;
                      var TimeShiftInSecondsV: Variant;
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
  bits: PChar;
  BayerPattern: array[0..16] of Char;
  MinValue, MaxValue: Word;
  AverageValue: Double;
  Image: PChar;
  ImageSize: PtrUInt; // including padding
  Make, Model, Software, Software2: string;
  Instrument: string;
  ISO: Double;
  ExposureTimeFloat: Double;
  TimeStr: TUnixTimeStr;
  TimeStamp: Int64; // !!
  TimeStampD: TDateTime;
  DateTime: TDateTime;
  TimeShift: Int64;
  TempS: string;
  FITSHeader: TFITSRecordArray;
  Comments: TStringArray;
  Axes: TIntArray;
  Success: Boolean;
  FITSFile: FITSRecordFile;
  N, I, P: Integer;
  S: string;
  Name, Value, TempValue: string;
  TimeProcStart: TDateTime;
begin
  TimeProcStart := Now();

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

  if PrintInfo = infoNo then begin
    if CheckExistence and FileExists(NewFileName) then
      FileError('File already exists. Use /F switch to overwrite existing files.');
  end;

  RawProcessor := RawProcessorCreate;
  if RawProcessor = nil then FileError('Cannot create RawProcessor');
  try
    CheckLibRawError(RawProcessorOpenFile(RawProcessor, PChar(FileName)));
    if PrintInfo = infoNo then
      CheckLibRawError(RawProcessorUnpack(RawProcessor))
    else
      CheckLibRawError(RawProcessorAdjustSizesInfoOnly(RawProcessor));

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
    TimeStamp := RawProcessorTimestamp(RawProcessor);
    TimeStampD := UnixToDateTime(TimeStamp);
    RawProcessorTime(RawProcessor, TimeStr, SizeOf(TimeStr));
    RawProcessorBayerPattern(RawProcessor, BayerPattern, SizeOf(BayerPattern));
    for I := 0 to StrLen(BayerPattern) - 1 do
      if (Ord(BayerPattern[I]) < 32) or (Ord(BayerPattern[I]) > 126) then BayerPattern[I] := '?';

    if PrintInfo = infoNormal then begin
      WriteLn;
      WriteLn('File          : ', ExtractFileName(FileName));
      WriteLn('Make          : ', Make);
      WriteLn('Model         : ', Model);
      WriteLn('Software      : ', Software);
      WriteLn('Time (local)  : ', TrimRight(TimeStr));
      WriteLn('TimeStamp (UT): ', GetDayOfWeek(TimeStampD), ' ', GetMonth(TimeStampD), ' ', FormatDateTime('dd hh:nn:ss yyyy', TimeStampD));
      WriteLn('ISO           : ', ISO:0:0);
      WriteLn('Exposure      : ', ExposureTimeFloat:9:7);
      WriteLn('Raw Size      : ', _width, 'x', _height);
      WriteLn('Image Size    : ', RawFrameWidth, 'x', RawFrameHeight);
      WriteLn('Left Margin   : ', RawFrameLeft);
      WriteLn('Top Margin    : ', RawFrameTop);
      WriteLn('Output Size   : ', Iwidth, 'x', Iheight);
      WriteLn('Bayer Pattern : ', BayerPattern);
      //WriteLn;
      //WriteLn('Note: Time is a value of EXIF TimeStamp reported by LibRaw formatted by ctime()');
      Exit;
    end
    else
    if PrintInfo = infoTable then begin
      Write(AnsiQuotedStr(ExtractFileName(FileName), '"'));
      Write(^I);
      Write(AnsiQuotedStr(Make, '"'));
      Write(^I);
      Write(AnsiQuotedStr(Model, '"'));
      Write(^I);
      Write(AnsiQuotedStr(Software, '"'));
      Write(^I);
      Write(AnsiQuotedStr(TrimRight(TimeStr), '"'));
      Write(^I);
      Write(AnsiQuotedStr(GetDayOfWeek(TimeStampD) + ' ' + GetMonth(TimeStampD) + ' ' + FormatDateTime('dd hh:nn:ss yyyy', TimeStampD), '"'));
      Write(^I);
      Str(ISO:0:0, S);
      Write(AnsiQuotedStr(S, '"'));
      Write(^I);
      Str(ExposureTimeFloat:9:7, S);
      Write(AnsiQuotedStr(S, '"'));
      Write(^I);
      Write(AnsiQuotedStr(IntToStr(_width) + 'x' + IntToStr(_height), '"'));
      Write(^I);
      Write(AnsiQuotedStr(IntToStr(RawFrameWidth) + 'x' + IntToStr(RawFrameHeight), '"'));
      Write(^I);
      Write(AnsiQuotedStr(IntToStr(RawFrameLeft), '"'));
      Write(^I);
      Write(AnsiQuotedStr(IntToStr(RawFrameTop), '"'));
      Write(^I);
      Write(AnsiQuotedStr(IntToStr(Iwidth) + 'x' + IntToStr(Iheight), '"'));
      Write(^I);
      Write(AnsiQuotedStr(BayerPattern, '"'));
      WriteLn;
      Exit;
    end;

    if (2 * LongWord(_width) <> RawPitch) then
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

    DateTime := DateTimeFromUnixTimeStringSafe(TimeStr);

    if VarIsNull(TimeShiftInSecondsV) then begin
      TimeShift := TimeStamp - DateTimeToUnix(DateTime); // in seconds
      TimeShiftInSecondsV := TimeShift;
    end
    else
      TimeShift := TimeShiftInSecondsV;

    bits := PChar(RawProcessorRawImage(RawProcessor));

    if PrintTiming then begin
      WriteLn;
      Write('[decoded , elapsed: ', ((Now() - TimeProcStart) * (24 * 60 * 60)):0:2, ' s]');
    end;

    // FITS image: ConvertRawBytes returns FITS image padded to block size!
    Image :=  ConvertRawBytes(bits, _width,
                             RawFrameWidth, RawFrameHeight, RawFrameLeft, RawFrameTop,
                             PixelRealNumber, BzeroShift, DoFlip,
                             MinValue, MaxValue, AverageValue, ImageSize);
    try
      if PrintTiming then begin
        WriteLn;
        Write('[prepared, elapsed: ', ((Now() - TimeProcStart) * (24 * 60 * 60)):0:2, ' s]');
      end;

      if not PixelRealNumber and not BzeroShift and (MaxValue > High(SmallInt)) then begin
        FileError('UINT pixel value too big: ' + IntToStr(MaxValue) +
                  ^M^J'Use /R option to make floating-point FITS' +
                  ^M^J'or' +
                  ^M^J'Use /S option to specify BZERO = ' + IntToStr(defBZeroShift[True]));
      end;

      Instrument := '';
      if (Make <> '') or (Model <> '') then
        Instrument := Trim(Make + ' ' + Model);
      Software2 := 'libraw ' + RawProcessorVersion + ' / ' + ExtractFileName(ParamStr(0)) + ' ' + GetVersionString2(ParamStr(0));

      if DateTime <> 0 then begin
        DateTime := DateTime + TimeShift / (24.0*60.0*60.0);
        TimeShifted := TimeShift <> 0;
      end;

      SetLength(Axes, 2);
      Axes[0] := RawFrameWidth;
      Axes[1] := RawFrameHeight;

      N := 0;

      SetLength(Comments, N + 1);
      Comments[N] := 'RAW: Original filename: ' + ExtractFileName(FileName);
      Inc(N);

      SetLength(Comments, N + 1);
      Comments[N] := 'RAW: Original EXIF time: ' + TrimRight(TimeStr); // To remove '\n'!
      Inc(N);

      if TimeShifted then begin
        SetLength(Comments, N + 1);
        Comments[N] := 'RAW: DATE-OBS: Original EXIF time shifted by ' + IntToStr(TimeShift) + ' seconds';
        Inc(N);
      end;

      if ISO <> 0 then begin
        Str(ISO:0:0, TempS);
        SetLength(Comments, N + 1);
        Comments[N] := 'RAW: ISO ' + TempS;
        Inc(N);
      end;

      if not DoFlip and not DontTruncate then begin
        SetLength(Comments, N + 1);
        Comments[N] := 'RAW: Bayer Pattern (8 rows x 2 pixels): ' + BayerPattern;
        Inc(N);
      end;

      if DoFlip then begin
        SetLength(Comments, N + 1);
        Comments[N] := 'RAW: Y-axis is reversed';
        Inc(N);
      end;

      SetLength(Comments, N + 1);
      Comments[N] := 'RAW: MIN  PIXEL VALUE = ' + IntToStr(MinValue);
      Inc(N);
      SetLength(Comments, N + 1);
      Comments[N] := 'RAW: MAX  PIXEL VALUE = ' + IntToStr(MaxValue);
      Inc(N);
      SetLength(Comments, N + 1);
      Str(AverageValue:0:1, TempS);
      Comments[N] := 'RAW: MEAN PIXEL VALUE = ' + TempS;
      Inc(N);
      if Software <> '' then begin
        SetLength(Comments, N + 1);
        Comments[N] := 'RAW: ' + Software;
        Inc(N);
      end;
      SetLength(Comments, N + 1);
      Comments[N] := 'RAW: ' + Software2;
      Inc(N);

      FITSHeader := MakeFITSHeader(
        defFITSbpp[PixelRealNumber],
        Axes,
        defBZeroShift[BzeroShift], 1,
        DateTime, '',
        LocalTimeToUniversal(Now()), '(UTC): RAW->FITS conversion time',
        ExposureTimeFloat, '',
        '',
        '',
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
        if FITSParams.Count <> 0 then WriteLn;
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

  if PrintTiming then begin
    WriteLn;
    WriteLn('[saved   , elapsed: ', ((Now() - TimeProcStart) * (24 * 60 * 60)):0:2, ' s]');
  end;
end;

procedure ProcessInput(const FileMasks: array of string;
                       PrintInfo: TPrintInfoMode;
                       PrintTiming: Boolean;
                       const GenericName: string;
                       const OutputDir: string;
                       Overwrite: Boolean;
                       BaseNumber: Integer;
                       DontTruncate: Boolean;
                       DoFlip: Boolean;
                       PixelRealNumber: Boolean;
                       BzeroShift: Boolean;
                       FITSParams: TStrings;
                       TimeShiftInSecondsV: Variant;
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
      if PrintInfo <> infoTable then begin
        WriteLn;
        WriteLn('[', FileMasks[N], ']');
      end;
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
        if PrintInfo = infoNo then begin
          Write(FileName, ^I'->'^I, NewFileName, ^I);
        end;
        ConvertFile(FileList[I],
                    PrintInfo,
                    PrintTiming,
                    NewFileName,
                    not Overwrite,
                    DontTruncate,
                    DoFlip,
                    PixelRealNumber,
                    BzeroShift,
                    FITSParams,
                    TimeShiftInSecondsV,
                    TimeShifted);
        if TimeShifted then Write('DATE-OBS shifted by ', VarToStrDef(TimeShiftInSecondsV, 'NULL'), ' seconds');
        if PrintInfo <> infoTable then begin
          WriteLn;
        end;
        Inc(FileNumber);
      end;
    end;
    if FileNumber < 1 then begin
      WriteLn;
      PrintWarning('**** No files found.'^M^J);
    end;
  except
    on E: Exception do begin
      PrintError(^M^J'**** Error:'^M^J + E.Message + ^M^J);
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
  TimeShiftInSecondsV: Variant;
  TimeShift: Int64;
  PixelRealNumber: Boolean;
  BzeroShift: Boolean;
  DoFlip: Boolean;
  OutputExt: string;
  PrintInfo: TPrintInfoMode;
  PrintTiming: Boolean;
  FITSparams: TStringList;
  PrintVer: Boolean;
  ErrorPos: Integer;
  S, S2: string;
  ParamN: Integer;

begin
  FileMode := fmOpenRead;
  LibRawLoaded := False;

  LibRawWrapperName := CmdObj.CmdLine.KeyValue('DLL=');
  if LibRawWrapperName = '' then begin
    LibRawWrapperName := CommonIni.Ini.ReadString('SETTINGS', {$IFDEF WIN64}'LIBRAWWRAPPER64'{$ELSE}'LIBRAWWRAPPER'{$ENDIF}, '');
    if LibRawWrapperName = '' then LibRawWrapperName := LibRawWrapper;
  end;
  if ExtractFilePath(LibRawWrapperName) = '' then LibRawWrapperName := ExtractFilePath(ParamStr(0)) + LibRawWrapperName;

  try
    InitLibrary(LibRawWrapperName);
    LibRawLoaded := True;
  except
    on E: Exception do
      PrintError(^M^J'**** Problem loading ' + LibRawWrapperName + '.'^M^J'**** Error: ' + E.Message + ^M^J);
  end;

  PrintVer := (CmdObj.CmdLine.IsCmdOption('V') or CmdObj.CmdLine.IsCmdOption('version'));
  if PrintVer then PrintVersion2;

  if (CmdObj.CmdLine.IsCmdOption('?') or CmdObj.CmdLine.IsCmdOption('H') or CmdObj.CmdLine.IsCmdOption('help')) then begin
    PrintHelp;
    Halt(1);
  end;

  if not LibRawLoaded then begin
    PrintError('**** LibRaw not loaded.'^M^J);
    Halt(1);
  end;

  if (CmdObj.CmdLine.FileCount < 1) then begin
    if not PrintVer then begin
      PrintWarning('**** At least one filemask must be specified'^M^J);
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
  TimeShiftInSecondsV := 0;
  TimeShift := 0;
  PixelRealNumber := False;
  BzeroShift := False;
  DoFlip := False;
  OutputExt := '.fit';
  PrintInfo := infoNo;
  PrintTiming := False;
  FITSparams := TStringList.Create;
  try
    for ParamN := 1 to CmdObj.CmdLine.ParamCount do begin
      S := CmdObj.CmdLine.ParamStr(ParamN);
      if CmdObj.CmdLine.FirstCharIsSwitch(S) then begin
        if Length(S) = 1 then begin
          PrintError('**** Invalid command-line parameter: ' + S + ^M^J);
          Halt(1);
        end;
        if CmdObj.CmdLine.ParamIsKey(S, 'V') or CmdObj.CmdLine.ParamIsKey(S, 'version') then begin
          // nothing: already processed.
        end
        else
        if CmdObj.CmdLine.ExtractParamValue(S, 'DLL=', S2) then begin
          // nothing: already processed.
        end
        else
        if CmdObj.CmdLine.ParamIsKey(S, 'I') then
          PrintInfo := infoNormal
        else
        if CmdObj.CmdLine.ParamIsKey(S, 'I2') then
          PrintInfo := infoTable
        else
        if CmdObj.CmdLine.ParamIsKey(S, 'TIMING') then
          PrintTiming := True
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
        if CmdObj.CmdLine.ParamIsKey(S, 'Y') then
          DoFlip := True
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
               (Pos('?', GenericName) <> 0) or
               (Pos('<', GenericName) <> 0) or
               (Pos('>', GenericName) <> 0)
            then begin
              PrintError('**** Generic name must not contain \/:*?<>'^M^J);
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
              PrintError('**** Base filenumber must be an integer >= 0'^M^J);
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
            if AnsiSameText(S2, 'A') then begin
              TimeShiftInSecondsV := Null;
            end
            else begin
             Val(S2, TimeShift, ErrorPos);
              if ErrorPos <> 0 then begin
                PrintError('**** Time Shift must be an integer number or ''A'' for autoshift'^M^J);
                Halt(1);
              end;
              TimeShiftInSecondsV := TimeShift;
            end;
          end;
        end
        else begin
          PrintError('**** Invalid command-line parameter: ' + S + ^M^J);
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
      if PrintInfo = infoTable then begin
        Write('"File"'^I);
        Write('"Make"'^I);
        Write('"Model"'^I);
        Write('"Software"'^I);
        Write('"Time (local)"'^I);
        Write('"TimeStamp (UT)"'^I);
        Write('"ISO"'^I);
        Write('"Exposure"'^I);
        Write('"Raw Size"'^I);
        Write('"Image Size"'^I);
        Write('"Left Margin"'^I);
        Write('"Top Margin"'^I);
        Write('"Output Size"'^I);
        Write('"Bayer Pattern"');
        Writeln;
      end;
      ProcessInput(InputFileMasks,
                   PrintInfo,
                   PrintTiming,
                   GenericName,
                   OutputDir,
                   Overwrite,
                   BaseNumber,
                   DontTruncate,
                   DoFlip,
                   PixelRealNumber,
                   BzeroShift,
                   FITSParams,
                   TimeShiftInSecondsV,
                   OutputExt);
      WriteLn;
    finally
      FreeAndNil(FileList);
    end;
  finally
    FreeAndNil(FITSparams);
  end;
end.

