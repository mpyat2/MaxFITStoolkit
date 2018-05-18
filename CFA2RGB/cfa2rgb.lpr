{$APPTYPE CONSOLE}
{$ASSERTIONS ON}

program cfa2rgb;

uses
  SysUtils, CmdObj{, CmdObjStdSwitches}, Version, FITSUtils, EnumFiles, StringListNaturalSort, FitsUtilsHelp, CommonIni;

{$R *.res}

procedure PrintVersion;
begin
  WriteLn('CFA -> RGB superpixel converter  Maksym Pyatnytskyy  2018');
  WriteLn(GetVersionString(ParamStr(0)));
  WriteLn;
end;

procedure FileError(const S: string);
begin
  raise Exception.Create(S);
end;

type
  TBayerPattern = array[0..3] of Char;

type
  TPCharArray = array of PChar;

procedure AverageLayers(const Layers: TPCharArray; Len: Integer; BitPix: Integer);
// result -> [0] layer
var
  A1: TFITSValue;
  Asum: TFITSValue;
  N, NN, Addr: Integer;
  NLayers: Integer;
  BytePix: Integer;
begin
  BytePix := Abs(BitPix) div 8;
  NLayers := Length(Layers);
  for NN := 0 to Len div BytePix do begin
    FillChar(Asum, SizeOf(Asum), 0);
    Addr := NN * BytePix;
    for N := 0 to NLayers - 1 do begin
      Move(Layers[N][Addr], A1, BytePix);
      RevertBytes(A1, BitPix);
      case BitPix of
          8: Asum.H := Asum.H + A1.B;
         16: Asum.H := Asum.H + A1.I;
         32: Asum.H := Asum.H + A1.L;
        -32: Asum.E := Asum.E + A1.S;
        -64: Asum.E := Asum.E + A1.D;
      end;
    end;
    case BitPix of
        8: Asum.B := Round(Asum.H / NLayers);
       16: Asum.I := Round(Asum.H / NLayers);
       32: Asum.L := Round(Asum.H / NLayers);
      -32: Asum.S := Asum.E / NLayers;
      -64: Asum.D := Asum.E / NLayers;
    end;
    RevertBytes(Asum, BitPix);
    Move(Asum, Layers[0][Addr], BytePix);
  end;
end;

procedure InterpolateLayer(Layer: PChar; Color: Char; Naxis1, Naxis2: Integer; BitPix: Integer; const BayerPattern: TBayerPattern);
var
  A1: TFITSValue;
  Asum: TFITSValue;
  PixelCountToAverage: Integer;
  PixAddr: Integer;
  C, R, C2, R2, X, Y: Integer;
  ColorL: Integer;
  BytePix: Integer;
begin
  BytePix := Abs(BitPix) div 8;
  for C := 0 to Naxis1 - 1 do begin
    for R := Naxis2 - 1 downto 0 do begin // start from the end of Naxis2, for "correct" order of pixels
      ColorL := C mod 2 + 2 * ((Naxis2 - 1 - R) mod 2);
      if BayerPattern[ColorL] <> Color then begin
        FillChar(Asum, SizeOf(Asum), 0);
        PixelCountToAverage := 0;
        for X := -1 to 1 do begin
          for Y := -1 to 1 do begin
            R2 := R + X;
            C2 := C + Y;
            if (R2 >= 0) and (R2 < Naxis2) and (C2 >= 0) and (C2 < Naxis1) then begin
              PixAddr := (R2 * Naxis1 + C2) * BytePix;
              ColorL := C2 mod 2 + 2 * ((Naxis2 - 1 - R2) mod 2);
              if BayerPattern[ColorL] = Color then begin
                Move(Layer[PixAddr], A1, BytePix);
                RevertBytes(A1, BitPix);
                case BitPix of
                    8: Asum.H := Asum.H + A1.B;
                   16: Asum.H := Asum.H + A1.I;
                   32: Asum.H := Asum.H + A1.L;
                  -32: Asum.E := Asum.E + A1.S;
                  -64: Asum.E := Asum.E + A1.D;
                end;
                Inc(PixelCountToAverage);
              end;
            end;
          end;
        end;
        if PixelCountToAverage > 0 then begin
          case BitPix of
              8: Asum.B := Round(Asum.H / PixelCountToAverage);
             16: Asum.I := Round(Asum.H / PixelCountToAverage);
             32: Asum.L := Round(Asum.H / PixelCountToAverage);
            -32: Asum.S := Asum.E / PixelCountToAverage;
            -64: Asum.D := Asum.E / PixelCountToAverage;
          end;
          RevertBytes(Asum, BitPix);
          PixAddr := (R * Naxis1 + C) * BytePix;
          Move(Asum, Layer[PixAddr], BytePix);
        end;
      end;
    end;
  end;
end;

procedure CFAtoRGB(var FITSfile: FITSRecordFile; const FITSFileName: string; const OutputDir: string; const Prefix: string; const BayerPattern: TBayerPattern; Overwrite: Boolean; Linear: Boolean);
var
  N: Integer;
  NblocksInHeader: Integer;
  StartOfImage: Integer;
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
  N := GetEndPosition(FITSfile);
  if N < 0 then
    FileError('Cannot find End of Header in file ' + AnsiQuotedStr(FITSfileName, '"'));
  NblocksInHeader := N div RecordsInBlock + 1;
  StartOfImage := NblocksInHeader * RecordsInBlock;
  GetBitPixAndNaxis(FITSfile, BitPix, NaxisN);
  if (Length(NaxisN) <> 2) then
    FileError('Cannot work with NAXIS other than 2, got ' + IntToStr(Length(NaxisN)) + '. File ' + AnsiQuotedStr(FITSfileName, '"'));
  if (BitPix <> 8) and (BitPix <> 16) and (BitPix <> 32) and (BitPix <> -32) and (BitPix <> -64) then
    FileError('Unsupported BITPIX value: ' + IntToStr(BitPix) + '. File ' + AnsiQuotedStr(FITSfileName, '"'));
  BytePix := Abs(BitPix) div 8;
  Naxis1 := NaxisN[0];
  Naxis2 := NaxisN[1];
  Write(' [', Naxis1, 'x', Naxis2, '] [BITPIX=', BitPix, '] -> ');

  if not Linear then begin
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
          AverageLayers(ImageToAverage, Naxis1new * Naxis2new * BytePix, BitPix);
          GreenL := ImageToAverage[0];
        end
        else begin
          // Linear interpolation
          // Reading 4 color planes
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
          InterpolateLayer(GreenL, 'G', Naxis1, Naxis2, BitPix, BayerPattern);
          InterpolateLayer(RedL,   'R', Naxis1, Naxis2, BitPix, BayerPattern);
          InterpolateLayer(BlueL,  'B', Naxis1, Naxis2, BitPix, BayerPattern);
        end;

        OutFileName := Prefix + ExtractFileName(FITSfileName);
        if OutputDir = '' then
          OutFileName := ExtractFilePath(FITSfileName) + OutFileName
        else
          OutFileName := IncludeTrailingPathDelimiter(OutputDir) + OutFileName;
        Write(' ', ExtractFileName(OutFileName));
        if not Overwrite and FileExists(OutFileName) then
          FileError('File ' + AnsiQuotedStr(OutFileName, '"') + ' already exists. Use /F switch to overwrite.');

        N := -1;
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
          end;
          if Buf = recordEND then begin
            N := I;
            Break;
          end;
        end;

        Assert(N >= 0);

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
end;

procedure ProcessFile(const FileName: string; const OutputDir: string; const Prefix: string; const BayerPattern: TBayerPattern; Overwrite: Boolean; Linear: Boolean);
var
  FITSfile: FITSRecordFile;
begin
  Write('Processing ', ExtractFileName(FileName));
  AssignFile(FITSfile, FileName);
  Reset(FITSfile);
  try
    if not IsFits(FITSfile) then
      FileError('Not a valid FITS file: ' + AnsiQuotedStr(FileName, '"'));
    CFAtoRGB(FITSfile, FileName, OutputDir, Prefix, BayerPattern, Overwrite, Linear);
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

function CCount(C: Char; BayerPattern: TBayerPattern): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Length(BayerPattern) - 1 do
    if BayerPattern[I] = C then
      Inc(Result);
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
  I: Integer;
  ParamN: Integer;

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
  if Length(BayerPatternStr) <> 4 then begin
    WriteLn('**** Invalid Bayer pattern definition');
    Halt(1);
  end;
  BayerPatternStr := AnsiUpperCase(BayerPatternStr);
  for I := 0 to 3 do begin
    if ((BayerPatternStr[I+1] in ['R', 'B']) and (CCount(BayerPatternStr[I+1], BayerPattern) = 0)) or
       ((BayerPatternStr[I+1] = 'G') and (CCount(BayerPatternStr[I+1], BayerPattern) < 2)) then
      BayerPattern[I] := BayerPatternStr[I+1]
    else begin
      WriteLn('**** Invalid Bayer pattern definition');
      Halt(1);
    end;
  end;

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

