{$APPTYPE CONSOLE}
{$ASSERTIONS ON}
{.DEFINE ROUND_C}

program cfa2rgb;

uses
  SysUtils, CmdObj{, CmdObjStdSwitches}, Version, FITSUtils, EnumFiles, StringListNaturalSort, FitsUtilsHelp, CommonIni;

{$R *.res}

procedure PrintVersion;
begin
  WriteLn('CFA(16-bit) -> RGB converter  Maksym Pyatnytskyy  2018');
  WriteLn(GetVersionString(ParamStr(0)));
  WriteLn;
end;

procedure FileError(const S: string);
begin
  raise Exception.Create(S);
end;

{$IFDEF ROUND_C}
function Round_C(R: Double): Int64; inline;
begin
  if R > 0.0 then Result := Trunc(R + 0.5) else Result := Trunc(R - 0.5);
end;
{$ENDIF}

type
  TBayerPattern = array[0..3] of Char;

type
  TPCharArray = array of PChar;

// 16-bit FITSes only are supported in the current version! (for efficiency)

procedure AverageIntLayers(Layer1, Layer2: PChar; Len: Integer);
// result -> 1st layer
var
  A1, A2: SmallInt;
  A1Bytes: array[0..1] of Char absolute A1;
  A2Bytes: array[0..1] of Char absolute A2;
  ASum: SmallInt;
  ASumBytes: array[0..1] of Char absolute ASum;
  NN, Addr: Integer;
begin
  for NN := 0 to Len do begin
    Addr := NN * 2;
    A1Bytes[1] := Layer1[Addr];
    A1Bytes[0] := Layer1[Addr + 1];
    A2Bytes[1] := Layer2[Addr];
    A2Bytes[0] := Layer2[Addr + 1];
    ASum := {$IFDEF ROUND_C}Round_C{$ELSE}Round{$ENDIF}((A1 + A2) / 2);
    Layer1[Addr]     := ASumBytes[1];
    Layer1[Addr + 1] := ASumBytes[0];
  end;
end;

procedure GetPixelValue16bit(Layer: PChar; C, R: Integer; Naxis1: Integer; out A: SmallInt); inline;
var
  ABytes: array[0..1] of Char absolute A;
  Addr: Integer;
begin
  Addr := (R * Naxis1 + C) * 2;
  ABytes[1] := Layer[Addr];
  ABytes[0] := Layer[Addr + 1];
end;

procedure SetPixelValue16bit(Layer: PChar; C, R: Integer; Naxis1: Integer; A: SmallInt); inline;
var
  ABytes: array[0..1] of Char absolute A;
  Addr: Integer;
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
          GetPixelValue16bit(Layer, C - 1, R, Naxis1, A1);
          GetPixelValue16bit(Layer, C,     R - 1, Naxis1, A2);
          GetPixelValue16bit(Layer, C + 1, R, Naxis1, A3);
          GetPixelValue16bit(Layer, C,     R + 1, Naxis1, A4);
          SetPixelValue16bit(Layer, C,     R, Naxis1, {$IFDEF ROUND_C}Round_C{$ELSE}Round{$ENDIF}((A1 + A2 + A3 + A4) / 4));
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
            SetPixelValue16bit(Layer, C,     R, Naxis1, {$IFDEF ROUND_C}Round_C{$ELSE}Round{$ENDIF}((A1 + A2) / 2));
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
            SetPixelValue16bit(Layer, C,     R, Naxis1, {$IFDEF ROUND_C}Round_C{$ELSE}Round{$ENDIF}((A1 + A2 + A3 + A4) / 4));
          end
          else begin
            GetPixelValue16bit(Layer, C, R - 1, Naxis1, A1);
            GetPixelValue16bit(Layer, C, R + 1, Naxis1, A2);
            SetPixelValue16bit(Layer, C, R, Naxis1, {$IFDEF ROUND_C}Round_C{$ELSE}Round{$ENDIF}((A1 + A2) / 2));
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

function CFAtoRGB(var FITSfile: FITSRecordFile; const FITSFileName: string; const OutputDir: string; const Prefix: string; const BayerPattern: TBayerPattern; Overwrite: Boolean; Linear: Boolean): Boolean;
var
  N: Integer;
  NblocksInHeader: Integer;
  StartOfImage: Integer;
  Bscale, Bzero: Double;
  BlackLevel: SmallInt;
  BitPix: Integer;
  BytePix: Integer;
  NaxisN: TIntArray;
  Naxis1, Naxis2: Integer;
  Naxis1new, Naxis2new: Integer;
  NrecordsToRead: Integer;
  NRecordsToWrite: Integer;
  Header: PChar;
  Buf: FITSRecordType;
  HeaderNew: TFITSRecordArray;
  EndOfHeaderFound: Boolean;
  Image: PChar;
  ImageC: array[0..3] of PChar;
  ImageToAverage: TPCharArray;
  ImageMemSize: Integer;
  ImageLayerMemSize: Integer;
  ColorL: Integer;
  ShiftV, ShiftH: Integer;
  C, R, C2, R2, X, Y: Integer;
  PixAddr, PixAddr2: Integer;
  RedL, GreenL, BlueL: PChar;
  OutFile: FITSRecordfile;
  OutFileName: string;
  FileModeSaved: Integer;
  S: string;
  I: Integer;
begin
  Result := False;
  N := GetEndPosition(FITSfile);
  if N < 0 then
    FileError('Cannot find End of Header in file ' + AnsiQuotedStr(FITSfileName, '"'));
  NblocksInHeader := N div RecordsInBlock + 1;
  StartOfImage := NblocksInHeader * RecordsInBlock;
  GetBitPixAndNaxis(FITSfile, BitPix, NaxisN);
  if (Length(NaxisN) <> 2) then
    FileError('Cannot work with NAXIS other than 2, got ' + IntToStr(Length(NaxisN)) + '. File ' + AnsiQuotedStr(FITSfileName, '"'));
  Naxis1 := NaxisN[0];
  Naxis2 := NaxisN[1];
  Write(' [', Naxis1, 'x', Naxis2, '] [BITPIX=', BitPix, '] -> ');
  if BitPix <> 16 then begin
    Write(' Only BITPIX=16 is currently supported.');
    Exit;
  end;
  BytePix := Abs(BitPix) div 8;

  BlackLevel := 0;
  GetBscaleBzero(FITSfile, Bscale, Bzero); // Bzero -- for border pixels
  if Bzero <> 0 then begin
    if ({$IFDEF ROUND_C}Round_C{$ELSE}Round{$ENDIF}(-Bzero) <= High(SmallInt)) and
       ({$IFDEF ROUND_C}Round_C{$ELSE}Round{$ENDIF}(-Bzero) >= Low(SmallInt))
    then
      BlackLevel := {$IFDEF ROUND_C}Round_C{$ELSE}Round{$ENDIF}(-Bzero);
  end;


  if not Linear then begin
    // Superpixel debayering
    Naxis1new := Naxis1 div 2;
    Naxis2new := Naxis2 div 2;
  end
  else begin
    Naxis1new := Naxis1;
    Naxis2new := Naxis2;
  end;

  NrecordsToRead := ((Naxis1 * Naxis2 * BytePix - 1) div FITSRecordLen + 1);
  GetMem(Header, StartOfImage * FITSRecordLen);
  try
    FillChar(Header^, StartOfImage * FITSRecordLen, 0);
    Seek(FITSFile, 0);
    BlockRead(FITSFile, Header^, StartOfImage);
    ImageMemSize := NrecordsToRead * FITSRecordLen;
    GetMem(Image, ImageMemSize);
    try
      FillChar(Image^, ImageMemSize, 0);
      Seek(FITSfile, StartOfImage);
      BlockRead(FITSfile, Image^, NrecordsToRead);
      ImageLayerMemSize := Naxis1new * Naxis2new * BytePix;

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
                for R := 0 to Naxis2 - 1 do begin
                  C2 := C + ShiftH;
                  R2 := Naxis2 - 1 - (R + ShiftV); // start from the end of Naxis2, for "correct" order of pixels
                  if (C2 >= 0) and (C2 < Naxis1) and (R2 >= 0) and (R2 < Naxis2) then begin
                    PixAddr := (R2 * Naxis1 + C2) * BytePix;
                    if (C mod 2 = 0) and (R mod 2 = 0) then begin
                      X := C div 2;
                      Y := R div 2;
                      if (X >= 0) and (X < Naxis1new) and (Y >= 0) and (Y < Naxis2new) then begin
                        PixAddr2 := ((Naxis2new - 1 - Y) * Naxis1new + X) * BytePix;
                        Move(Image[PixAddr], ImageC[ColorL][PixAddr2], BytePix);
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
          AverageIntLayers(ImageToAverage[0], ImageToAverage[1], Naxis1new * Naxis2new);
          GreenL := ImageToAverage[0];
        end
        else begin
          // Linear interpolation
          // Reading 4 color planes
          // Step1: allocating memory
          for ColorL := 0 to 3 do begin
            if BayerPattern[ColorL] = 'R' then begin
              GetMem(ImageC[ColorL], ImageLayerMemSize);
              FillChar(ImageC[ColorL]^, ImageLayerMemSize, 0);
              RedL := ImageC[ColorL];
            end
            else
            if BayerPattern[ColorL] = 'G' then begin
              if GreenL = nil then begin
                GetMem(ImageC[ColorL], ImageLayerMemSize);
                FillChar(ImageC[ColorL]^, ImageLayerMemSize, 0);
                GreenL := ImageC[ColorL];
              end;
            end
            else
            if BayerPattern[ColorL] = 'B' then begin
              GetMem(ImageC[ColorL], ImageLayerMemSize);
              FillChar(ImageC[ColorL]^, ImageLayerMemSize, 0);
              BlueL := ImageC[ColorL];
            end
          end;
          Assert(GreenL <> nil);
          Assert(RedL <> nil);
          Assert(BlueL <> nil);
          // Moving exisiting pixels to color planes
          for C := 0 to Naxis1 - 1 do begin
            for R := Naxis2 - 1 downto 0 do begin // start from the end of Naxis2, for "correct" order of pixels
              PixAddr := (R * Naxis1 + C) * BytePix;
              ColorL := C mod 2 + 2 * ((Naxis2 - 1 - R) mod 2);
              if BayerPattern[ColorL] = 'R' then
                Move(Image[PixAddr], RedL[PixAddr], BytePix)
              else
              if BayerPattern[ColorL] = 'G' then
                Move(Image[PixAddr], GreenL[PixAddr], BytePix)
              else
              if BayerPattern[ColorL] = 'B' then
                Move(Image[PixAddr], BlueL[PixAddr], BytePix);
            end;
          end;
          // Interpolating
          InterpolateLayer16bit(GreenL, 'G', Naxis1, Naxis2, BayerPattern, BlackLevel);
          InterpolateLayer16bit(RedL,   'R', Naxis1, Naxis2, BayerPattern, BlackLevel);
          InterpolateLayer16bit(BlueL,  'B', Naxis1, Naxis2, BayerPattern, BlackLevel);
        end;

        // Saving file
        EndOfHeaderFound := False;
        // Creating new header with NAXIS=3 and new sizes
        HeaderNew := nil;
        for I := 0 to StartOfImage - 1 do begin
          Move(Header[I * FITSRecordLen], Buf, FITSRecordLen);
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
            S := PadCh('COMMENT', FITSKeywordLen, ' ') + 'Debayered by CFA2RGB';
            if not Linear then S := S + ' (superpixel mode)' else S := S + ' (linear interpolation)';
            StrToFITSRecord(S, Buf);
            Move(Buf, HeaderNew[Length(HeaderNew) - 1], FITSRecordLen);
            SetLength(HeaderNew, Length(HeaderNew) + 1);
            Move(recordEND, HeaderNew[Length(HeaderNew) - 1], FITSRecordLen);
            EndOfHeaderFound := True;
            Break;
          end;
        end;

        Assert(EndOfHeaderFound);

        // Padding header...
        N := Length(HeaderNew) mod RecordsInBlock;
        if N > 0 then begin
          for I := 1 to RecordsInBlock - N do begin
            SetLength(HeaderNew, Length(HeaderNew) + 1);
            FillChar(HeaderNew[Length(HeaderNew) - 1], SizeOf(FITSRecordType), ' ');
          end;
        end;
        StartOfImage := Length(HeaderNew);

        // Reallocating Image
        FreeMem(Image);
        NRecordsToWrite := (3 * ImageLayerMemSize - 1) div FITSRecordLen + 1;
        NRecordsToWrite := ((NRecordsToWrite - 1) div RecordsInBlock + 1) * RecordsInBlock;
        GetMem(Image, NRecordsToWrite * FITSRecordLen);
        FillChar(Image^, NRecordsToWrite * FITSRecordLen, 0);
        Move(RedL[0], Image[0], ImageLayerMemSize);
        Move(GreenL[0], Image[ImageLayerMemSize], ImageLayerMemSize);
        Move(BlueL[0], Image[ImageLayerMemSize * 2], ImageLayerMemSize);

        // Writing file
        OutFileName := Prefix + ExtractFileName(FITSfileName);
        if OutputDir = '' then
          OutFileName := ExtractFilePath(FITSfileName) + OutFileName
        else
          OutFileName := IncludeTrailingPathDelimiter(OutputDir) + OutFileName;
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
  finally
    FreeMem(Header);
    Header := nil;
  end;
  Result := True;
end;

procedure ProcessFile(const FileName: string; const OutputDir: string; const Prefix: string; const BayerPattern: TBayerPattern; Overwrite: Boolean; Linear: Boolean);
var
  FITSfile: FITSRecordFile;
  R: Boolean;
begin
  Write('Processing ', ExtractFileName(FileName));
  AssignFile(FITSfile, FileName);
  Reset(FITSfile);
  try
    if not IsFits(FITSfile) then
      FileError('Not a valid FITS file: ' + AnsiQuotedStr(FileName, '"'));
    R := CFAtoRGB(FITSfile, FileName, OutputDir, Prefix, BayerPattern, Overwrite, Linear);
  finally
    CloseFile(FITSfile);
  end;
  if R then WriteLn(': done.') else WriteLn(' Skipped.');
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

procedure ProcessInput(const FileMasks: array of string; const OutputDir: string; const Prefix: string; const BayerPattern: TBayerPattern; Overwrite: Boolean; Linear: Boolean);
var
  I, N, Ntotal: Integer;
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
        ProcessFile(FileList[I], OutputDir, Prefix, BayerPattern, Overwrite, Linear);
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
  BayerPatternStr: string;
  Overwrite: Boolean;
  Linear: Boolean;
  PrintVer: Boolean;
  S: string;
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
  Prefix := 'rgb-';
  BayerPatternStr := '';
  Overwrite := False;
  Linear := False;

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
        //
      end
      else
      if CmdObj.CmdLine.ParamIsKey(S, 'F') then
        Overwrite := True
      else
      if CmdObj.CmdLine.ParamIsKey(S, '2') then
        Linear := True
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


  if (Prefix = '') then begin
    WriteLn('**** Output file prefix is not defined: use /P=<prefix> parameter');
    Halt(1);
  end;
  // \/:*?
  if (Pos('\', Prefix) <> 0) or
     (Pos('/', Prefix) <> 0) or
     (Pos(':', Prefix) <> 0) or
     (Pos('*', Prefix) <> 0) or
     (Pos('?', Prefix) <> 0)
  then begin
    WriteLn('**** Output file prefix must not contain \/:*?');
    Halt(1);
  end;


  FileList := TStringListNaturalSort.Create;
  try
    ProcessInput(InputFileMasks, OutputDir, Prefix, BayerPattern, Overwrite, Linear);
  finally
    FreeAndNil(FileList);
  end;

end.

