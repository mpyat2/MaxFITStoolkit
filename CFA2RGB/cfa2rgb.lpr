{*****************************************************************************}
{                                                                             }
{ cfa2rgb                                                                     }
{ (c) 2017 Maksym Pyatnytskyy                                                 }
{                                                                             }
{ This program is distributed                                                 }
{ WITHOUT ANY WARRANTY; without even the implied warranty of                  }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                        }
{                                                                             }
{*****************************************************************************}

{$APPTYPE CONSOLE}
{$MODE DELPHI}

{$ASSERTIONS ON}          // can be disabled in release mode

{$INCLUDE FITSUtils.inc}

program cfa2rgb;

uses
  SysUtils, CmdObj{, CmdObjStdSwitches}, Version, FITScompatibility,
  FITSUtils, EnumFiles, StringListNaturalSort, FitsUtilsHelp, CommonIni;

{$R *.res}

procedure PrintVersion;
begin
  WriteLn('CFA(16-bit) -> RGB converter  Maksym Pyatnytskyy  2018');
  WriteLn(GetVersionString(AnsiUpperCase(ParamStr(0))){$IFDEF WIN64}, ' WIN64'{$ENDIF}, ' ', {$I %DATE%}, ' ', {$I %TIME%});
  WriteLn;
end;

procedure FileError(const S: string);
begin
  raise Exception.Create(S);
end;

type
  TBayerPattern = array[0..3] of Char;

// 16-bit FITSes only are supported in the current version! (for efficiency)

procedure GetPixelValue16bit(Layer: PChar; C, R: Integer; Naxis1: Integer; out A: SmallInt); inline;
var
  ABytes: array[0..1] of Char absolute A;
  Addr: LongInt;
begin
  Addr := (R * Naxis1 + C) * 2;
  ABytes[1] := Layer[Addr];
  ABytes[0] := Layer[Addr + 1];
end;

procedure SetPixelValue16bit(Layer: PChar; C, R: Integer; Naxis1: Integer; A: SmallInt); inline;
var
  ABytes: array[0..1] of Char absolute A;
  Addr: LongInt;
begin
  Addr := (R * Naxis1 + C) * 2;
  Layer[Addr] := ABytes[1];
  Layer[Addr + 1] := ABytes[0];
end;

procedure InterpolateLayer16bit(Layer: PChar; Color: Char; Naxis1, Naxis2: Integer; const BayerPattern: TBayerPattern; BlackLevel: SmallInt);
var
  A1, A2, A3, A4: SmallInt;
  C, R: Integer;
  OddColumn: Boolean;
  ActiveRow: Boolean;
begin
  if Color = 'G' then begin
    OddColumn := BayerPattern[2] = 'G'; // column to process (one row skipped)
    for R := Naxis2 - 2 downto 1 do begin // start from the end of Naxis2, for "correct" order of pixels
      for C := 1 to Naxis1 - 2 do begin
        if not (Odd(C) xor OddColumn) then begin
          GetPixelValue16bit(Layer, C - 1, R,     Naxis1, A1);
          GetPixelValue16bit(Layer, C,     R - 1, Naxis1, A2);
          GetPixelValue16bit(Layer, C + 1, R,     Naxis1, A3);
          GetPixelValue16bit(Layer, C,     R + 1, Naxis1, A4);
          SetPixelValue16bit(Layer, C,     R,     Naxis1, Round((LongInt(A1) + LongInt(A2) + LongInt(A3) + LongInt(A4)) / 4));
        end;
      end;
      OddColumn := not OddColumn;
    end;
  end
  else begin
    // 'R' or 'B'
    // First pass: process rows with 'R' or 'B' pixels
    ActiveRow := (BayerPattern[2] = Color) or (BayerPattern[3] = Color); // one row skipped.
    OddColumn := (BayerPattern[0] = Color) or (BayerPattern[2] = Color); // column to process.
    for R := Naxis2 - 2 downto 1 do begin // start from the end of Naxis2, for "correct" order of pixels
      if ActiveRow then begin
        for C := 1 to Naxis1 - 2 do begin
          if not (Odd(C) xor OddColumn) then begin
            GetPixelValue16bit(Layer, C - 1, R, Naxis1, A1);
            GetPixelValue16bit(Layer, C + 1, R, Naxis1, A2);
            SetPixelValue16bit(Layer, C,     R, Naxis1, Round((LongInt(A1) + LongInt(A2)) / 2));
          end;
        end;
      end;
      ActiveRow := not ActiveRow;
    end;
    // Second pass: process dark rows
    ActiveRow := not ((BayerPattern[2] = Color) or (BayerPattern[3] = Color)); // one row skipped.
    for R := Naxis2 - 2 downto 1 do begin // start from the end of Naxis2, for "correct" order of pixels
      if ActiveRow then begin
        for C := 1 to Naxis1 - 2 do begin
          if not (Odd(C) xor OddColumn) then begin
            GetPixelValue16bit(Layer, C - 1, R - 1, Naxis1, A1);
            GetPixelValue16bit(Layer, C + 1, R - 1, Naxis1, A2);
            GetPixelValue16bit(Layer, C - 1, R + 1, Naxis1, A3);
            GetPixelValue16bit(Layer, C + 1, R + 1, Naxis1, A4);
            SetPixelValue16bit(Layer, C,     R,     Naxis1, Round((LongInt(A1) + LongInt(A2) + LongInt(A3) + LongInt(A4)) / 4));
          end
          else begin
            GetPixelValue16bit(Layer, C, R - 1, Naxis1, A1);
            GetPixelValue16bit(Layer, C, R + 1, Naxis1, A2);
            SetPixelValue16bit(Layer, C, R,     Naxis1, Round((LongInt(A1) + LongInt(A2)) / 2));
          end;
        end;
      end;
      ActiveRow := not ActiveRow;
    end;
  end;
  // Zero border
  for R := 0 to Naxis2 - 1 do begin
    SetPixelValue16bit(Layer, 0,          R, Naxis1, BlackLevel);
    SetPixelValue16bit(Layer, Naxis1 - 1, R, Naxis1, BlackLevel);
  end;
  for C := 0 to Naxis1 - 1 do begin
    SetPixelValue16bit(Layer, C, 0,          Naxis1, BlackLevel);
    SetPixelValue16bit(Layer, C, Naxis2 - 1, Naxis1, BlackLevel);
  end;
end;

procedure CFAtoRGB(var FITSfile: FITSRecordFile; const OutFileName: string; const BayerPattern: TBayerPattern; Overwrite: Boolean; Linear: Boolean; PrintTiming: Boolean);
var
  StartOfImage: Integer;
  Bscale, Bzero: Double;
  BlackLevel: SmallInt;
  BitPix: Integer;
  NaxisN: TIntArray;
  Naxis1, Naxis2: Integer;
  Naxis1new, Naxis2new: Integer;
  NRecordsToWrite: Int64;
  Buf: FITSRecordType;
  Header: TFITSRecordArray;
  HeaderNew: TFITSRecordArray;
  EndOfHeaderFound: Boolean;
  Image: PChar;
  ImageC: array[0..3] of PChar;
  ImageToAverage: TPCharArray;
  ImageMemSize: PtrUInt;
  ImageLayerMemSize: PtrUInt;
  ColorL: Integer;
  ShiftV, ShiftH: Integer;
  C, R, C2, R2, X, Y: Integer;
  PixAddr, PixAddr2: Integer;
  RedL, GreenL, BlueL: PChar;
  OutFile: FITSRecordfile;
  FileModeSaved: Integer;
  S: string;
  I: Integer;
  N: Integer;
  TimeProcStart: TDateTime;
begin
  TimeProcStart := Now();

  GetFITSproperties(FITSfile, BitPix, NaxisN, StartOfImage, ImageMemSize); // ImageMemSize is padded!
  if (Length(NaxisN) <> 2) then
    FileError('Cannot work with NAXIS other than 2, got ' + IntToStr(Length(NaxisN)) + '. File ' + AnsiQuotedStr(FITSRecordTypeFileName(FITSfile), '"'));
  Naxis1 := NaxisN[0];
  Naxis2 := NaxisN[1];
  Write(' [', Naxis1, 'x', Naxis2, '] [BITPIX=', BitPix, '] -> ');
  if BitPix <> 16 then
    FileError('Only BITPIX=16 is currently supported. File ' + AnsiQuotedStr(FITSRecordTypeFileName(FITSfile), '"'));

  BlackLevel := 0;
  GetBscaleBzero(FITSfile, Bscale, Bzero); // Bzero -- for border pixels

{$IFNDEF range_check}{$R+}{$ENDIF}
{$IFNDEF overflow_check}{$Q+}{$ENDIF}
  // BlackLevel is used for setting zero-pixel border only in case of Linear interpolation.
  // So we can almost safely set it to zero if its value is out of supported range.
  if Bzero <> 0 then begin
    if (Round(-Bzero) <= High(SmallInt)) and (Round(-Bzero) >= Low(SmallInt)) then
      BlackLevel := Round(-Bzero);
  end;
{$IFNDEF range_check}{$R-}{$ENDIF}
{$IFNDEF overflow_check}{$Q-}{$ENDIF}

  if not Linear then begin
    // Superpixel debayering
    Naxis1new := Naxis1 div 2;
    Naxis2new := Naxis2 div 2;
  end
  else begin
    Naxis1new := Naxis1;
    Naxis2new := Naxis2;
  end;

  GetMem(Image, ImageMemSize);
  try
    FillChar(Image^, ImageMemSize, 0);
    Seek(FITSfile, StartOfImage);
    BlockRead(FITSfile, Image^, ImageMemSize div FITSRecordLen);
    ImageLayerMemSize := Naxis1new * Naxis2new * 2;

    for ColorL := 0 to 3 do ImageC[ColorL] := nil;
    RedL := nil;
    GreenL := nil;
    BlueL := nil;

    try

      if not Linear then begin
        // Superpixel
        // Reading 4 color planes
        for ShiftV := 0 to 1 do begin
          for ShiftH := 0 to 1 do begin
            ColorL := ShiftH + ShiftV * 2;
            GetMem(ImageC[ColorL], ImageLayerMemSize);
            FillChar(ImageC[ColorL]^, ImageLayerMemSize, 0);
            for C := 0 to Naxis1 - 1 do begin
              if not Odd(C) then begin
                X := C div 2;
                if X < Naxis1new then begin
                  for R := 0 to Naxis2 - 1 do begin
                    if not Odd(R) then begin
                      Y := R div 2;
                      if Y < Naxis2new then begin
                        C2 := C + ShiftH;
                        R2 := Naxis2 - 1 - (R + ShiftV); // start from the end of Naxis2, for "correct" order of pixels
                        if (C2 >= 0) and (C2 < Naxis1) and (R2 >= 0) and (R2 < Naxis2) then begin
                          PixAddr := (R2 * Naxis1 + C2) * 2;
                          PixAddr2 := ((Naxis2new - 1 - Y) * Naxis1new + X) * 2;
                          Move(Image[PixAddr], ImageC[ColorL][PixAddr2], 2);
                        end;
                      end;
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
        // Averaging 2 G-planes
        ImageToAverage := nil;
        for ColorL := 0 to 4 do begin
          if BayerPattern[ColorL] = 'G' then begin
            SetLength(ImageToAverage, Length(ImageToAverage) + 1);
            ImageToAverage[Length(ImageToAverage) - 1] := ImageC[ColorL];
          end
          else
          if BayerPattern[ColorL] = 'R' then
            RedL := ImageC[ColorL]
          else
          if BayerPattern[ColorL] = 'B' then
            BlueL := ImageC[ColorL];
        end;
        Assert(RedL <> nil);
        Assert(BlueL <> nil);
        Assert(Length(ImageToAverage) = 2);
        Average16bitLayers(ImageToAverage[0], ImageToAverage[1], Naxis1new * Naxis2new);
        GreenL := ImageToAverage[0];
      end
      else begin
        // Linear interpolation
        // Reading 4 color planes
        // Step1: allocating memory and making three copy of original layer
        for ColorL := 0 to 3 do begin
          if BayerPattern[ColorL] = 'R' then begin
            GetMem(ImageC[ColorL], ImageLayerMemSize);
            Move(Image^, ImageC[ColorL]^, ImageLayerMemSize);
            RedL := ImageC[ColorL];
          end
          else
          if BayerPattern[ColorL] = 'G' then begin
            if GreenL = nil then begin
              GetMem(ImageC[ColorL], ImageLayerMemSize);
              Move(Image^, ImageC[ColorL]^, ImageLayerMemSize);
              GreenL := ImageC[ColorL];
            end;
          end
          else
          if BayerPattern[ColorL] = 'B' then begin
            GetMem(ImageC[ColorL], ImageLayerMemSize);
            Move(Image^, ImageC[ColorL]^, ImageLayerMemSize);
            BlueL := ImageC[ColorL];
          end
        end;
        Assert(GreenL <> nil);
        Assert(RedL <> nil);
        Assert(BlueL <> nil);
        // Interpolating
        InterpolateLayer16bit(GreenL, 'G', Naxis1, Naxis2, BayerPattern, BlackLevel);
        InterpolateLayer16bit(RedL,   'R', Naxis1, Naxis2, BayerPattern, BlackLevel);
        InterpolateLayer16bit(BlueL,  'B', Naxis1, Naxis2, BayerPattern, BlackLevel);
      end;

      if Image <> nil then begin
        FreeMem(Image);
        Image := nil;
      end;

      // Saving file
      EndOfHeaderFound := False;
      // Creating new header with NAXIS=3 and new sizes
      HeaderNew := nil;
      GetHeader(FITSFile, Header);
      for I := 0 to Length(Header) - 1 do begin
        Move(Header[I], Buf, FITSRecordLen);
        if Copy(Buf, 1, FITSKeywordLen) = PadCh('NAXIS', FITSKeywordLen, ' ') then begin
          // Fix NAXIS
          Str(3:FITSNumericAlign - FITSKeywordLen - 2, S);
          StrToFITSRecord(PadCh('NAXIS', FITSKeywordLen, ' ') + '= ' + S, Buf);
        end
        else
        if Copy(Buf, 1, FITSKeywordLen) = PadCh('NAXIS1', FITSKeywordLen, ' ') then begin
          // Fix NAXIS1
          Str(Naxis1new:FITSNumericAlign - FITSKeywordLen - 2, S);
          StrToFITSRecord(PadCh('NAXIS1', FITSKeywordLen, ' ') + '= ' + S, Buf);
        end
        else
        if Copy(Buf, 1, FITSKeywordLen) = PadCh('NAXIS2', FITSKeywordLen, ' ') then begin
          // Fix NAXIS2 ...
          Str(Naxis2new:FITSNumericAlign - FITSKeywordLen - 2, S);
          StrToFITSRecord(PadCh('NAXIS2', FITSKeywordLen, ' ') + '= ' + S, Buf);
        end;
        SetLength(HeaderNew, Length(HeaderNew) + 1);
        Move(Buf, HeaderNew[Length(HeaderNew) - 1], FITSRecordLen);
        if Copy(Buf, 1, FITSKeywordLen) = PadCh('NAXIS2', FITSKeywordLen, ' ') then begin
          // ... and add NAXIS3=3 after NAXIS2.
          Str(3:FITSNumericAlign - FITSKeywordLen - 2, S);
          StrToFITSRecord(PadCh('NAXIS3', FITSKeywordLen, ' ') + '= ' + S, Buf);
          SetLength(HeaderNew, Length(HeaderNew) + 1);
          Move(Buf, HeaderNew[Length(HeaderNew) - 1], FITSRecordLen);
        end
        else
        if Buf = recordEND then begin
          // adding comment before END
          S := PadCh('COMMENT', FITSKeywordLen, ' ') + 'Debayered by ' + ChangeFileExt(ExtractFileName(ParamStr(0)), '');
          if not Linear then S := S + ' (superpixel mode)' else S := S + ' (linear interpolation)';
          StrToFITSRecord(S, Buf);
          Move(Buf, HeaderNew[Length(HeaderNew) - 1], FITSRecordLen);
          SetLength(HeaderNew, Length(HeaderNew) + 1);
          Move(recordEND, HeaderNew[Length(HeaderNew) - 1], FITSRecordLen);
          EndOfHeaderFound := True;
          Break;
        end;
      end;
      Header := nil;

      Assert(EndOfHeaderFound);

      // Padding new header...
      N := Length(HeaderNew) mod RecordsInBlock;
      if N > 0 then begin
        for I := 1 to RecordsInBlock - N do begin
          SetLength(HeaderNew, Length(HeaderNew) + 1);
          FillChar(HeaderNew[Length(HeaderNew) - 1], SizeOf(FITSRecordType), ' ');
        end;
      end;
      // Updating StartOfImage
      StartOfImage := Length(HeaderNew);

      // Reallocating Image
      NRecordsToWrite := (3 * Int64(ImageLayerMemSize) - 1) div FITSRecordLen + 1;
      NRecordsToWrite := ((NRecordsToWrite - 1) div RecordsInBlock + 1) * RecordsInBlock; // padding
      GetMem(Image, NRecordsToWrite * FITSRecordLen);
      FillChar(Image^, NRecordsToWrite * FITSRecordLen, 0);
      Move(RedL[0], Image[0], ImageLayerMemSize);
      Move(GreenL[0], Image[ImageLayerMemSize], ImageLayerMemSize);
      Move(BlueL[0], Image[ImageLayerMemSize * 2], ImageLayerMemSize);

      // Writing file
      Write(ExtractFileName(OutFileName));
      if not Overwrite and FileExists(OutFileName) then
        FileError('File ' + AnsiQuotedStr(OutFileName, '"') + ' already exists. Use /F switch to overwrite.');
      AssignFile(OutFile, OutFileName);
      FileModeSaved := FileMode;
      FileMode := fmOpenReadWrite;
      try
        Rewrite(OutFile);
        try
          BlockWrite(OutFile, HeaderNew[0], StartOfImage);
          BlockWrite(OutFile, Image^, NRecordsToWrite);
        finally
          CloseFile(OutFile);
        end;
      finally
        FileMode := FileModeSaved;
      end;

    finally
      for ColorL := 3 downto 0 do begin
        if ImageC[ColorL] <> nil then FreeMem(ImageC[ColorL]);
        ImageC[ColorL] := nil;
      end;
    end;
  finally
    FreeMem(Image);
    Image := nil;
  end;

  if PrintTiming then begin
    Write(' [elapsed: ', ((Now() - TimeProcStart) * (24 * 60 * 60)):0:2, ' s] ');
  end;

end;

procedure ProcessFile(const FileName: string; const OutFileName: string; const BayerPattern: TBayerPattern; Overwrite: Boolean; Linear: Boolean; PrintTiming: Boolean);
var
  FITSfile: FITSRecordFile;
begin
  Write('Processing ', ExtractFileName(FileName));
  AssignFile(FITSfile, FileName);
  Reset(FITSfile);
  try
    if not IsFits(FITSfile) then
      FileError('Not a valid FITS file: ' + AnsiQuotedStr(FileName, '"'));
    CFAtoRGB(FITSfile, OutFileName, BayerPattern, Overwrite, Linear, PrintTiming);
  finally
    CloseFile(FITSfile);
  end;
  WriteLn(': done.');
end;


var
  FileList: TStringListNaturalSort;

type
  TFileEnumClass = class(TObject)
    class function FileEnumProc(const Directory: string; const F: TSearchRec): Boolean;
  end;

class function TFileEnumClass.FileEnumProc(const Directory: string; const F: TSearchRec): Boolean;
begin
  FileList.Add(Directory + F.Name);
  Result := True;
end;

procedure ProcessInput(const FileMasks: array of string; const OutputDir: string; const Prefix: string; const GenericName: string; const BayerPattern: TBayerPattern; Overwrite: Boolean; Linear: Boolean; BaseNumber: Integer; PrintTiming: Boolean);
var
  I, N, Ntotal: Integer;
  OutFileName: string;
  FileNumber: Integer;
begin
  try
    Ntotal := 0;
    for N := 0 to Length(FileMasks) - 1 do begin
      WriteLn;
      WriteLn('[', FileMasks[N], ']');
      FileList.Clear;
      FileEnum(FileMasks[N], faArchive, False, TFileEnumClass.FileEnumProc);
      FileList.NaturalSort;
      for I := 0 to FileList.Count - 1 do begin
        FileNumber := BaseNumber + NTotal;
        if GenericName <> '' then
          OutFileName := GenericName + IntToStr(FileNumber) + ExtractFileExt(FileList[I])
        else
          OutFileName := Prefix + ExtractFileName(FileList[I]);
        if OutputDir = '' then
          OutFileName := ExtractFilePath(FileList[I]) + OutFileName
        else
          OutFileName := IncludeTrailingPathDelimiter(OutputDir) + OutFileName;
        ProcessFile(FileList[I], OutFileName, BayerPattern, Overwrite, Linear, PrintTiming);
        Inc(Ntotal);
      end;
    end;
    if Ntotal < 1 then begin
      WriteLn;
      WriteLn('**** No files found.');
    end
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
  OutputDir: string;
  Prefix: string;
  GenericName: string;
  BaseNumber: Integer;
  BayerPatternStr: string;
  Overwrite: Boolean;
  Linear: Boolean;
  PrintVer: Boolean;
  PrintTiming: Boolean;
  S, S2: string;
  ParamN: Integer;
  I: Integer;

var
  BayerPattern: TBayerPattern = (#0, #0, #0, #0);

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
  Prefix := '';
  GenericName := '';
  BaseNumber := 1;
  BayerPatternStr := '';
  Overwrite := False;
  Linear := False;
  PrintTiming := False;

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
        if OutputDir <> '' then
          OutputDir := IncludeTrailingPathDelimiter(ExpandFileName(OutputDir));
      end
      else
      if CmdObj.CmdLine.ExtractParamValue(S, 'M=', BayerPatternStr) then begin
        //
      end
      else
      if CmdObj.CmdLine.ExtractParamValue(S, 'P=', Prefix) then begin
        if Prefix <> '' then begin
          // \/:*?
          if (Pos('\', Prefix) <> 0) or
             (Pos('/', Prefix) <> 0) or
             (Pos(':', Prefix) <> 0) or
             (Pos('*', Prefix) <> 0) or
             (Pos('?', Prefix) <> 0) or
             (Pos('<', Prefix) <> 0) or
             (Pos('>', Prefix) <> 0)
          then begin
            WriteLn('**** Output file prefix must not contain \/:*?<>');
            Halt(1);
          end;
        end;
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
      if CmdObj.CmdLine.ExtractParamValue(S, 'B=', S2) then begin
        if S2 <> '' then begin
          if not GetInt(S2, BaseNumber) or (BaseNumber < 0) then begin
            WriteLn('**** Base filenumber must be an integer >= 0');
            Halt(1);
          end;
        end;
      end
      else
      if CmdObj.CmdLine.ParamIsKey(S, 'F') then
        Overwrite := True
      else
      if CmdObj.CmdLine.ParamIsKey(S, '2') then
        Linear := True
      else
      if CmdObj.CmdLine.ParamIsKey(S, 'TIMING') then
        PrintTiming := True
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

  if (BayerPatternStr = '') then begin
    WriteLn('**** Bayer pattern is not defined: use /M=<pattent> parameter');
    Halt(1);
  end;
  //if Length(BayerPatternStr) <> 4 then begin
  //  WriteLn('**** Invalid Bayer pattern definition');
  //  Halt(1);
  //end;
  BayerPatternStr := AnsiUpperCase(BayerPatternStr);
  if (BayerPatternStr <> 'RGGB') and (BayerPatternStr <> 'BGGR') and (BayerPatternStr <> 'GBRG') and (BayerPatternStr <> 'GRBG') then begin
    WriteLn('**** Invalid or unsupported Bayer pattern');
    Halt(1);
  end;
  for I := 0 to 3 do BayerPattern[I] := BayerPatternStr[I + 1];

  if (GenericName = '') and (Prefix = '') then
    Prefix := 'rgb-';

  if not((Prefix <> '') xor (GenericName <> '')) then begin
    WriteLn('**** You should define output file prefix (/P=<prefix>) or generic name (/G=<name>) but not both.');
    Halt(1);
  end;

  FileList := TStringListNaturalSort.Create;
  try
    ProcessInput(InputFileMasks, OutputDir, Prefix, GenericName, BayerPattern, Overwrite, Linear, BaseNumber, PrintTiming);
  finally
    FreeAndNil(FileList);
  end;

end.

