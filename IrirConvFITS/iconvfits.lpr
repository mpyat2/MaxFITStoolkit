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
  DateUtils, ConvUtils;

{$R *.res} // include version info!

{$INCLUDE PrintVersion.inc}

var
  FileList: TStringListNaturalSort;

procedure MakeNewHeader(Header: TFITSRecordArray; DestBitPix: Integer; out HeaderNew: TFITSRecordArray);
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

procedure ProcessFile(const FileName: string;
                      OutFITSbitpix: TOutFITSbitpix;
                      const GenericName: string;
                      const OutputDir: string;
                      const OutputExt: string;
                      Overwrite: Boolean;
                      Number: Integer);
var
  PixelArray: TExtendedArray;
  Header: TFITSRecordArray;
  BitPix: Integer;
  PixelNumber: Integer;
  OutFileName: string;
  HeaderNew: TFITSRecordArray;
  DestBitPix: Integer;
begin
  OutFileName := ConstructOutputName(FileName, GenericName, OutputDir, OutputExt, Number);

  Write('Converting ', ExtractFileName(FileName), ' -> ', ExtractFileName(OutFileName), '... ');

  if not Overwrite and FileExists(OutFileName) then
    FileError('File ' + AnsiQuotedStr(OutFileName, '"') + ' already exists. Use /F switch to overwrite.');

  GetFITSPixelArray(FileName, PixelArray, Header, BitPix, PixelNumber);

  // make additional image transformation here! (i.e. calibration, cosmetic correction etc.)

  DestBitPix := GetDestBitPix(BitPix, OutFITSbitpix);

  MakeNewHeader(Header, DestBitPix, HeaderNew);

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

procedure ProcessInput(const FileMasks: array of string;
                       OutFITSbitpix: TOutFITSbitpix;
                       const GenericName: string;
                       const OutputDir: string;
                       const OutputExt: string;
                       Overwrite: Boolean;
                       BaseNumber: Integer);
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
        ProcessFile(FileList[I], OutFITSbitpix, GenericName, OutputDir, OutputExt, Overwrite, BaseNumber + Ntotal);
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

begin
  FileMode := fmOpenReadWrite;

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
    ProcessInput(InputFileMasks, OutFITSbitpix, GenericName, OutputDir, OutputExt, Overwrite, BaseNumber);
  finally
    FreeAndNil(FileList);
  end;

end.

