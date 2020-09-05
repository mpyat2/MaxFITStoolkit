{*****************************************************************************}
{                                                                             }
{ Convert FITS                                                                }
{ (c) 2018-2020 Maksym Pyatnytskyy                                            }
{                                                                             }
{ This program is distributed                                                 }
{ WITHOUT ANY WARRANTY; without even the implied warranty of                  }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                        }
{                                                                             }
{*****************************************************************************}

{$APPTYPE CONSOLE}
{$MODE DELPHI}
{$INCLUDE FITSUtils.inc}

program iconvfits;

uses
  SysUtils, CmdObj{, CmdObjStdSwitches}, Version, EnumFiles, MiscUtils, 
  FITScompatibility, FITSUtils, StringListNaturalSort, FitsUtilsHelp, CommonIni,
  DateUtils, ConvUtils, FITSStatUtils;

{$R *.res} // include version info!

{$INCLUDE PrintVersion.inc}

{.DEFINE DEBUG_LOG}

var
  FileList: TStringListNaturalSort;

procedure MakeNewHeader(Header: TFITSRecordArray; DestBitPix: Integer; Offset, Dark, Flat, Cosme: Boolean; out HeaderNew: TFITSRecordArray);
var
  KeyBITPIX, KeyBZERO, KeyBSCALE: string;
  Buf: FITSRecordType;
  TempS: string;
  I: Integer;
begin
  KeyBITPIX := PadCh('BITPIX', FITSKeywordLen, ' ');
  KeyBZERO  := PadCh('BZERO', FITSKeywordLen, ' ');
  KeyBSCALE := PadCh('BSCALE', FITSKeywordLen, ' ');

  HeaderNew := nil;
  for I := 0 to Length(Header) - 1 do begin
    Buf := Header[I];
    if Buf = recordEND then begin
      if Offset then begin
        StrToFITSRecord('HISTORY OFFSET applied', Buf);
        SetLength(HeaderNew, Length(HeaderNew) + 1);
        HeaderNew[Length(HeaderNew) - 1] := Buf;
      end;
      if Dark then begin
        StrToFITSRecord('HISTORY DARK applied', Buf);
        SetLength(HeaderNew, Length(HeaderNew) + 1);
        HeaderNew[Length(HeaderNew) - 1] := Buf;
      end;
      if Flat then begin
        StrToFITSRecord('HISTORY FLAT applied', Buf);
        SetLength(HeaderNew, Length(HeaderNew) + 1);
        HeaderNew[Length(HeaderNew) - 1] := Buf;
      end;
      if Cosme then begin
        StrToFITSRecord('HISTORY HOT PIXEL map applied', Buf);
        SetLength(HeaderNew, Length(HeaderNew) + 1);
        HeaderNew[Length(HeaderNew) - 1] := Buf;
      end;
      TempS := 'HISTORY Converted by iconvfits UT ' + FormatDateTime('YYYY-MM-DD"T"hh:nn:ss', LocalTimeToUniversal(Now()));
      StrToFITSRecord(TempS, Buf);
      SetLength(HeaderNew, Length(HeaderNew) + 1);
      HeaderNew[Length(HeaderNew) - 1] := Buf;
      SetLength(HeaderNew, Length(HeaderNew) + 1);
      HeaderNew[Length(HeaderNew) - 1] := recordEND;
      Break;
    end
    else
    if Copy(Buf, 1, FITSKeywordLen) = KeyBITPIX then begin
      Str(DestBitPix : FITSNumericAlign - FITSKeywordLen - 2, TempS);
      TempS := KeyBITPIX + '= ' + TempS + ' / Converted by iconvfits';
      StrToFITSRecord(TempS, Buf);
      SetLength(HeaderNew, Length(HeaderNew) + 1);
      HeaderNew[Length(HeaderNew) - 1] := Buf;
    end
    else
    if Copy(Buf, 1, FITSKeywordLen) = KeyBZERO then begin
      // remove this value
    end
    else
    if Copy(Buf, 1, FITSKeywordLen) = KeyBSCALE then begin
      // remove this value
    end
    else begin
      // add record 'as is'
      SetLength(HeaderNew, Length(HeaderNew) + 1);
      HeaderNew[Length(HeaderNew) - 1] := Buf;
    end;
  end;
  PadHeader(HeaderNew);
end;

procedure CreateAndSaveOutputImage(const OutFileName: string;
                                   DestBitPix: Integer;
                                   const PixelArray: TExtendedArray;
                                   const HeaderNew: TFITSRecordArray);
var
  DestBytePix: Integer;
  DestImage: PChar;
  DestImageMemSize: PtrUInt;
  OutOfRangeErrorCount: Integer;
  PixelNumber: Integer;
  I: Integer;
begin
  DestBytePix := Abs(DestBitPix) div 8;
  PixelNumber := Length(PixelArray);
  DestImageMemSize := PixelNumber * DestBytePix;
  // Align DestImageMemSize to FITSRecordLen * RecordsInBlock to simplify writing.
  DestImageMemSize := ((DestImageMemSize - 1) div (FITSRecordLen * RecordsInBlock) + 1) * FITSRecordLen * RecordsInBlock;
  GetMem(DestImage, DestImageMemSize);
  try
    FillChar(DestImage^, DestImageMemSize, 0);

    OutOfRangeErrorCount := 0;
    for I := 0 to Length(PixelArray) - 1 do
      SetFITSpixel(DestImage, I, DestBitPix, PixelArray[I], OutOfRangeErrorCount);
    if OutOfRangeErrorCount > 0 then
      PrintWarning('**** WARNING: overflow for ' + IntToStr(OutOfRangeErrorCount) + ' pixels (output BITPIX=' + IntToStr(DestBitPix) + ')'^M^J);

    WriteFITS(OutFileName, HeaderNew, DestImage, DestImageMemSize);
  finally
    FreeMem(DestImage);
    DestImage := nil;
  end;
end;

procedure MakeLocalMedian(const PixelArray, TempPixelArray: TExtendedArray; Width, Height, X, Y: Integer);
// excluding border pixels (like IRIS does)
var
  I, J: Integer;
  A: array[0..7] of Extended;
  E: Extended;
  N: Integer;
  BorderWidth: Integer;
begin
  BorderWidth := 1;
  // TEST ADJACENT pixels!
  if (X >= BorderWidth) and (X < Width - BorderWidth) and (Y >= BorderWidth) and (Y < Height - BorderWidth) then begin
{$IFDEF DEBUG_LOG}
    WriteLn(^M^J'>>>>Bad Pixel'^I, X, '-', Y, ^I, PixelArray[Y * Width + X]);
{$ENDIF}
    N := 0;
    FillChar(A, SizeOf(A), 0);
    for I := X - 1 to X + 1 do begin
      for J := Y - 1 to Y + 1 do begin
        if (I <> X) or (J <> Y) then begin
          A[N] := PixelArray[J * Width + I];
{$IFDEF DEBUG_LOG}
          WriteLn('       Pixel'^I, I, '-', J, ^I, A[N]);
{$ENDIF}
          Inc(N);
        end;
      end;
    end;
    E := TStatHelper<Extended>.WirthMedian(A); // array is reordered!
    TempPixelArray[Y * Width + X] := E;
{$IFDEF DEBUG_LOG}
    WriteLn('<<<<Fix Pixel'^I, X, '-', Y, ^I, TempPixelArray[Y * Width + X]);
{$ENDIF}
  end;
end;

procedure MakeLocalMeanForColumn(const PixelArray: TExtendedArray; Width, Height, X: Integer);
var
  Y: Integer;
begin
  if (X > 0) and (X < Width - 1) then begin
    for Y := 0 to Height - 1 do
      PixelArray[Y * Width + X] := (PixelArray[Y * Width + X - 1] + PixelArray[Y * Width + X + 1]) / 2;
  end;
end;

procedure MakeLocalMeanForRow(const PixelArray: TExtendedArray; Width, Height, Y: Integer);
var
  X: Integer;
begin
  if (Y > 0) and (Y < Height - 1) then begin
    for X := 0 to Width - 1 do
      PixelArray[Y * Width + X] := (PixelArray[(Y - 1) * Width + X] + PixelArray[(Y + 1) * Width + X]) / 2;
  end;
end;

procedure SubtractArray(const PixelArray, Array2: TExtendedArray);
var
  I: Integer;
begin
  for I := 0 to Length(PixelArray) - 1 do
    PixelArray[I] := PixelArray[I] - Array2[I];
end;

procedure DivArray(const PixelArray, Array2: TExtendedArray; MultBy: Extended);
var
  I: Integer;
begin
  for I := 0 to Length(PixelArray) - 1 do begin
    PixelArray[I] := MultBy * PixelArray[I] / Array2[I];
  end;
end;

procedure ProcessCalibration(const PixelArray, BiasArray, DarkArray, FlatArray: TExtendedArray; FlatMedian: Extended);
begin
  if not(Assigned(BiasArray)) and not(Assigned(DarkArray)) and not(Assigned(FlatArray)) then
    Exit;
  if Assigned(BiasArray) then
    SubtractArray(PixelArray, BiasArray);
  if Assigned(DarkArray) then
    SubtractArray(PixelArray, DarkArray);
  if Assigned(FlatArray) then
    DivArray(PixelArray, FlatArray, FlatMedian);
end;

procedure ProcessCosme(const PixelArray: TExtendedArray; Width: Integer; const CosmeArray: T3IntegerArray);
var
  TempPixelArray: TExtendedArray;
  Height, I: Integer;
begin
  if not Assigned(CosmeArray) then
    Exit;
  SetLength(TempPixelArray, Length(PixelArray));
  Move(PixelArray[0], TempPixelArray[0], Length(PixelArray) * SizeOf(PixelArray[1]));
  Height := Length(PixelArray) div Width;
  for I := 0 to Length(CosmeArray) do begin
    if CosmeArray[I][2] = 0 then
      MakeLocalMedian(PixelArray, TempPixelArray, Width, Height, CosmeArray[I][0], CosmeArray[I][1])
    else
    if CosmeArray[I][2] = 1 then
      MakeLocalMeanForColumn(TempPixelArray, Width, Height, CosmeArray[I][0])
    else
      MakeLocalMeanForRow(TempPixelArray, Width, Height, CosmeArray[I][1])
  end;
  Move(TempPixelArray[0], PixelArray[0], Length(PixelArray) * SizeOf(PixelArray[1]));
  TempPixelArray := nil;
end;

procedure ProcessFile(const FileName: string;
                      OutFITSbitpix: TOutFITSbitpix;
                      const GenericName: string;
                      const OutputDir: string;
                      const OutputExt: string;
                      Overwrite: Boolean;
                      Number: Integer;
                      const BiasArray, DarkArray, FlatArray: TExtendedArray;
                      FlatMedian: Extended;
                      const CosmeArray: T3IntegerArray;
                      RequiredPixelNumber, RequiredWidth: Integer);
var
  PixelArray: TExtendedArray;
  Header: TFITSRecordArray;
  BitPix: Integer;
  Width, PixelNumber: Integer;
  OutFileName: string;
  HeaderNew: TFITSRecordArray;
  DestBitPix: Integer;
begin
  OutFileName := ConstructOutputName(FileName, GenericName, OutputDir, OutputExt, Number);

  Write('Converting ', ExtractFileName(FileName), ' -> ', ExtractFileName(OutFileName), '... ');

  if not Overwrite and FileExists(OutFileName) then
    FileError('File ' + AnsiQuotedStr(OutFileName, '"') + ' already exists. Use /F switch to overwrite.');

  GetFITSPixelArray(FileName, PixelArray, Header, BitPix, PixelNumber, Width);
  if (RequiredPixelNumber <> 0) or (RequiredWidth <> 0) then begin
    if (PixelNumber <> RequiredPixelNumber) or (Width <> RequiredWidth) then
      FileError('Dimensions of image file must be the same as calibration file(s) dimensions');
  end;

  // make additional image transformation here! (i.e. calibration, cosmetic correction etc.)
  ProcessCalibration(PixelArray, BiasArray, DarkArray, FlatArray, FlatMedian);
  ProcessCosme(PixelArray, Width, CosmeArray);

  DestBitPix := GetDestBitPix(BitPix, OutFITSbitpix);

  MakeNewHeader(Header, DestBitPix, Assigned(BiasArray), Assigned(DarkArray), Assigned(FlatArray), Assigned(CosmeArray), HeaderNew);

  CreateAndSaveOutputImage(OutFileName, DestBitPix, PixelArray, HeaderNew);

  WriteLn('Done.');
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

function ReadCosmeMap(const CosmeFile: string; out CosmeArray: T3IntegerArray): Integer;
var
  Cosme: TextFile;
  X, Y, P: Integer;
  ColMode: Boolean;
  S: string;
begin
  Result := 0;
  CosmeArray := nil;
  Assign(Cosme, CosmeFile);
  Reset(Cosme);
  try
    while not EOF(Cosme) do begin
      ReadLn(Cosme, S);
      S := AnsiUpperCase(Trim(S));
      if S <> '' then begin
        if Copy(S, 1, 2) = 'P ' then begin
          S := Trim(Copy(S, 3, Length(S)));
          P := Pos(' ', S);
          if P > 1 then begin
            try
              X := StrToInt(Copy(S, 1, P-1)) - 1;
              Y := StrToInt(Trim(Copy(S, P+1, Length(S)))) - 1;
            except
              on EConvertError do begin
                Inc(Result);
                Break;
              end;
            end;
            SetLength(CosmeArray, Length(CosmeArray) + 1);
            CosmeArray[Length(CosmeArray) - 1][0] := X;
            CosmeArray[Length(CosmeArray) - 1][1] := Y;
            CosmeArray[Length(CosmeArray) - 1][2] := 0;
          end
          else
           Inc(Result); // invalid pixel specification
        end
        else
        if (Copy(S, 1, 2) = 'L ') or (Copy(S, 1, 2) = 'C ') then begin
          ColMode := Copy(S, 1, 2) = 'C ';
          S := Trim(Copy(S, 3, Length(S)));
          P := Pos(' ', S);
          if P > 1 then
            S := Copy(S, 1, P-1);
          try
            if ColMode then begin
              X := StrToInt(S) - 1;
              Y := 0;
            end
            else begin
              X := 0;
              Y := StrToInt(S) - 1;
            end;
          except
            on EConvertError do begin
              Inc(Result);
              Break;
            end;
          end;
          SetLength(CosmeArray, Length(CosmeArray) + 1);
          CosmeArray[Length(CosmeArray) - 1][0] := X;
          CosmeArray[Length(CosmeArray) - 1][1] := Y;
          if ColMode then
            CosmeArray[Length(CosmeArray) - 1][2] := 1
          else
            CosmeArray[Length(CosmeArray) - 1][2] := 2
        end
        else
          Inc(Result); // invalid command (only P, L, C currently supported)
      end;
    end;
  finally
    CloseFile(Cosme);
  end;
end;

procedure ProcessInput(const FileMasks: array of string;
                       OutFITSbitpix: TOutFITSbitpix;
                       const GenericName: string;
                       const OutputDir: string;
                       const OutputExt: string;
                       Overwrite: Boolean;
                       BaseNumber: Integer;
                       const BiasFile: string;
                       const DarkFile: string;
                       const FlatFile: string;
                       const CosmeFile: string);
var
  BiasArray: TExtendedArray;
  DarkArray: TExtendedArray;
  FlatArray: TExtendedArray;
  TempPixelArray: TExtendedArray;
  FlatMedian: Extended;
  TempHeader: TFITSRecordArray;
  BitPix: Integer;
  RequiredPixelNumber, BiasPixelNumber, DarkPixelNumber, FlatPixelNumber: Integer;
  RequiredWidth, BiasWidth, DarkWidth, FlatWidth: Integer;
  CosmeArray: T3IntegerArray;
  CosmeFileErrorCount: Integer;
  I, N, Ntotal: Integer;
begin
  try
    BiasArray := nil;
    DarkArray := nil;
    FlatArray := nil;
    CosmeArray := nil;
    BitPix := 0;
    RequiredPixelNumber := 0;
    BiasPixelNumber := 0;
    DarkPixelNumber := 0;
    FlatPixelNumber := 0;
    RequiredWidth := 0;
    BiasWidth := 0;
    DarkWidth := 0;
    FlatWidth := 0;
    FlatMedian := 0;


    if BiasFile <> '' then begin
      GetFITSPixelArray(BiasFile, BiasArray, TempHeader, BitPix, BiasPixelNumber, BiasWidth);
      RequiredPixelNumber := BiasPixelNumber;
      RequiredWidth := BiasWidth;
    end;

    if DarkFile <> '' then begin
      GetFITSPixelArray(DarkFile, DarkArray, TempHeader, BitPix, DarkPixelNumber, DarkWidth);
      if (RequiredPixelNumber <> 0) or (RequiredWidth <> 0) then begin
        if (DarkPixelNumber <> RequiredPixelNumber) or (DarkWidth <> RequiredWidth) then
          FileError('Offset, Dark, Flat dimensions must be the same');
      end
      else begin
        RequiredPixelNumber := DarkPixelNumber;
        RequiredWidth := DarkWidth;
      end;
    end;

    if FlatFile <> '' then begin
      GetFITSPixelArray(FlatFile, FlatArray, TempHeader, BitPix, FlatPixelNumber, FlatWidth);
      if (RequiredPixelNumber <> 0) or (RequiredWidth <> 0) then begin
        if (FlatPixelNumber <> RequiredPixelNumber) or (FlatWidth <> RequiredWidth) then
          FileError('Offset, Dark, Flat dimensions must be the same');
      end
      else begin
        RequiredPixelNumber := FlatPixelNumber;
        RequiredWidth := FlatWidth;
      end;
      // WirthMedian reorders its array, so we need a copy.
      SetLength(TempPixelArray, Length(FlatArray));
      Move(FlatArray[0], TempPixelArray[0], Length(FlatArray) * SizeOf(FlatArray[1]));
      FlatMedian := TStatHelper<Extended>.WirthMedian(TempPixelArray);
      TempPixelArray := nil;
    end;

    if (CosmeFile <> '') then begin
      CosmeFileErrorCount := ReadCosmeMap(CosmeFile, CosmeArray);
      if CosmeFileErrorCount <> 0 then
        PrintWarning(^M^J'**** Warning: cosmetic file has ' + IntToStr(CosmeFileErrorCount) + ' invalid or unsupported line(s)'^M^J);
    end;

    Ntotal := 0;
    for N := 0 to Length(FileMasks) - 1 do begin
      WriteLn;
      WriteLn('[', FileMasks[N], ']');
      FileList.Clear;
      FileEnum(FileMasks[N], faArchive, False, TFileEnumClass.FileEnumProc);
      FileList.NaturalSort;
      for I := 0 to FileList.Count - 1 do begin
        ProcessFile(FileList[I], OutFITSbitpix, GenericName, OutputDir, OutputExt, Overwrite, BaseNumber + Ntotal,
                    BiasArray, DarkArray, FlatArray,
                    FlatMedian,
                    CosmeArray, RequiredPixelNumber, RequiredWidth);
        Inc(Ntotal);
      end;
    end;
    if Ntotal < 1 then begin
      WriteLn;
      PrintWarning('**** No files found.'^M^J);
    end
  except
    on E: Exception do begin
      PrintError(^M^J'**** Error:'^M^J + E.Message + ^M^J);
      Halt(1);
    end;
  end;
end;

var
  InputFileMasks: array of string;
  PrintVer: Boolean;
  S, S2: string;
  ParamN: Integer;
  OutFITSbitpix: TOutFITSbitpix;
  OutputDir: string;
  GenericName: string;
  OutputExt: string;
  Overwrite: Boolean;
  BaseNumber: Integer;
  BiasFile: string;
  DarkFile: string;
  FlatFile: string;
  CosmeFile: string;

begin
  FileMode := fmOpenRead;

  PrintVer := (CmdObj.CmdLine.IsCmdOption('V') or CmdObj.CmdLine.IsCmdOption('version'));
  if PrintVer then PrintVersion('FITS Converter');

  if (CmdObj.CmdLine.IsCmdOption('?') or CmdObj.CmdLine.IsCmdOption('H') or CmdObj.CmdLine.IsCmdOption('help')) then begin
    PrintHelp;
    Halt(1);
  end;

  OutFITSbitpix := bitpixDefault;
  OutputDir := '';
  GenericName := '';
  OutputExt := '';
  Overwrite := False;
  BaseNumber := 1;
  BiasFile := '';
  DarkFile := '';
  FlatFile := '';
  CosmeFile := '';

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
            PrintError('**** Generic name must not contain \/:*?<>'^M^J);
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
      if CmdObj.CmdLine.ExtractParamValue(S, 'B=', S2) then begin
        if S2 <> '' then begin
          if (not GetInt(S2, BaseNumber)) or (BaseNumber < 0) then begin
            PrintError('**** Base filenumber must be an integer >= 0'^M^J);
            Halt(1);
          end;
        end;
      end
      else
      if CmdObj.CmdLine.ParamIsKey(S, 'F') then
        Overwrite := True
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
            PrintError('**** Invalid output FORMAT. Allowed values: U8, I16, I32, F32, F64'^M^J);
            Halt(1);
          end;
        end;
      end
      else
      if CmdObj.CmdLine.ExtractParamValue(S, 'OFFSET=', S2) then begin
        if S2 <> '' then begin
           BiasFile := ExpandFileName(S2);
        end;
      end
      else
      if CmdObj.CmdLine.ExtractParamValue(S, 'DARK=', S2) then begin
        if S2 <> '' then begin
           DarkFile := ExpandFileName(S2);
        end;
      end
      else
      if CmdObj.CmdLine.ExtractParamValue(S, 'FLAT=', S2) then begin
        if S2 <> '' then begin
           FlatFile := ExpandFileName(S2);
        end;
      end
      else
      if CmdObj.CmdLine.ExtractParamValue(S, 'COSME=', S2) then begin
        if S2 <> '' then begin
           CosmeFile := ExpandFileName(S2);
        end;
      end
      else begin
        PrintError('**** Invalid command-line parameter: ' + S + ^M^J);
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

  // GenericName can be empty: use original name
  //if GenericName = '' then begin
  //  PrintError('**** Generic name must be specified (by /G=<name> parameter)'^M^J);
  //  Halt(1);
  //end;

  if OutputDir = '' then OutputDir := GetCurrentDir;
  if OutputDir <> '' then
    OutputDir := IncludeTrailingPathDelimiter(ExpandFileName(OutputDir));

  FileList := TStringListNaturalSort.Create;
  try
    ProcessInput(InputFileMasks, OutFITSbitpix, GenericName, OutputDir, OutputExt, Overwrite, BaseNumber, BiasFile, DarkFile, FlatFile, CosmeFile);
  finally
    FreeAndNil(FileList);
  end;

end.

