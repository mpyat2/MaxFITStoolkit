{*****************************************************************************}
{                                                                             }
{ FITSCFA                                                                     }
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

program FITSCFA;

uses
  SysUtils, CmdObj{, CmdObjStdSwitches}, Version, EnumFiles, FITScompatibility,
  FITSUtils, StringListNaturalSort, IniFiles, FitsUtilsHelp, CommonIni;

{$R *.res}

procedure PrintVersion;
begin
  WriteLn('FITS CFA splitter  Maksym Pyatnytskyy  2017');
  WriteLn(GetVersionString(AnsiUpperCase(ParamStr(0))){$IFDEF WIN64}, ' WIN64'{$ENDIF}, ' ', {$I %DATE%}, ' ', {$I %TIME%});
  WriteLn;
end;

procedure PrintIni;
var
  IniName: string;
  Ini: Text;
  S: string;
begin
  IniName := ChangeFileExt(ParamStr(0), '.INI');
  WriteLn('; **** [' + IniName + '] ****');
  WriteLn;
  try
    AssignFile(Ini, IniName);
    Reset(Ini);
    try
      while not EOF(Ini) do begin
        ReadLn(Ini, S);
        WriteLn(S);
      end;
    finally
      CloseFile(Ini);
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
  FileList: TStringListNaturalSort;
  Ini: TMemIniFile;

procedure FileError(const S: string);
begin
  raise Exception.Create(S);
end;

procedure FITSSplit(var FITSfile: FITSRecordfile; const FITSfileName: string; const Profile: string; const OutputDir: string; Overwrite: Boolean);
var
  StartOfImage: Integer;
  BitPix: Integer;
  BytePix: Integer;
  NaxisN: TIntArray;
  Naxis1, Naxis2: Integer;
  Naxis1new, Naxis2new: Integer;
  NRecordsToWrite: Integer;
  Header: TFITSRecordArray;
  HeaderNew: TFITSRecordArray;
  EndOfHeaderFound: Boolean;
  Buf: FITSRecordType;
  Image: PChar;
  ImageC: array[0..3] of PChar;
  ImageToAverage: TPCharArray;
  ImageNames: array[0..3] of string;
  ImageMemSize: PtrUInt;
  Image2MemSize: PtrUInt;
  C, R, C2, R2, X, Y: Integer;
  ShiftV, ShiftH: Integer;
  ColorL, ColorL2, LL: Integer;
  PixAddr, PixAddr2: Integer;
  OutFile: FITSRecordfile;
  OutFileName: string;
  FileModeSaved: Integer;
  S: string;
  N, I: Integer;
begin
  GetFITSproperties(FITSfile, BitPix, NaxisN, StartOfImage, ImageMemSize);
  if (Length(NaxisN) <> 2) then
    FileError('Cannot work with NAXIS other than 2, got ' + IntToStr(Length(NaxisN)) + '. File ' + AnsiQuotedStr(FITSfileName, '"'));
  if (BitPix <> 8) and (BitPix <> 16) and (BitPix <> 32) and (BitPix <> -32) and (BitPix <> -64) then
    FileError('Unsupported BITPIX value: ' + IntToStr(BitPix) + '. File ' + AnsiQuotedStr(FITSfileName, '"'));
  BytePix := Abs(BitPix) div 8;
  Naxis1 := NaxisN[0];
  Naxis2 := NaxisN[1];
  Write(' [', Naxis1, 'x', Naxis2, '] [BITPIX=', BitPix, '] -> ');
  GetHeader(FITSFile, Header);
  Naxis1new := Naxis1 div 2;
  Naxis2new := Naxis2 div 2;
  NRecordsToWrite := ((Naxis1new * Naxis2new * BytePix - 1) div FITSRecordLen + 1);
  NRecordsToWrite := ((NRecordsToWrite - 1) div RecordsInBlock + 1) * RecordsInBlock; // padding
  GetMem(Image, ImageMemSize);
  try
    FillChar(Image^, ImageMemSize, 0);
    Seek(FITSfile, StartOfImage);
    BlockRead(FITSfile, Image^, ImageMemSize div FITSRecordLen);
    Image2MemSize := NrecordsToWrite * FITSRecordLen;
    for ColorL := 0 to 3 do ImageC[ColorL] := nil;
    try
      for ShiftV := 0 to 1 do begin
        for ShiftH := 0 to 1 do begin
          ColorL := ShiftH + ShiftV * 2;
          ImageNames[ColorL] := Trim(Ini.ReadString(Profile, IntToStr(ColorL + 1), ''));
          if ImageNames[ColorL] = '' then ImageNames[ColorL] := 'p' + IntToStr(ColorL + 1);
          GetMem(ImageC[ColorL], Image2MemSize);
          FillChar(ImageC[ColorL]^, Image2MemSize, 0);
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

      for ColorL := 0 to 3 do begin
        if ImageNames[ColorL] <> '' then begin
          SetLength(ImageToAverage, 1);
          ImageToAverage[0] := ImageC[ColorL];
          for ColorL2 := ColorL + 1 to 3 do begin
            if ImageNames[ColorL] = ImageNames[ColorL2] then begin
              SetLength(ImageToAverage, Length(ImageToAverage) + 1);
              ImageToAverage[Length(ImageToAverage) - 1] := ImageC[ColorL2];
            end;
          end;
          if Length(ImageToAverage) > 1 then begin
            AverageFITSlayers(ImageToAverage, Naxis1new * Naxis2new * BytePix, BitPix);
            for ColorL2 := 1 to Length(ImageToAverage) - 1 do begin
              for LL := ColorL + 1 to 3 do begin
                if ImageC[LL] = ImageToAverage[ColorL2] then begin
                  ImageNames[LL] := '';
                  FreeMem(ImageC[LL]);
                  ImageC[LL] := nil;
                end;
              end;
            end;
          end;
        end;
      end;

      for ColorL := 0 to 3 do begin
        if ImageNames[ColorL] <> '' then begin
          EndOfHeaderFound := False;
          // We should update NAXIS1 and NAXIS2
          HeaderNew := nil;
          for I := 0 to Length(Header) - 1 do begin
            Move(Header[I], Buf, FITSRecordLen);
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
            if Buf = recordEND then begin
              // adding FILTER keyword
              S := PadCh('FILTER', FITSKeywordLen, ' ') + '= ' + FITSQuotedValue(' ' + ImageNames[ColorL]) + ' / Color Layer';
              StrToFITSRecord(S, Buf);
              Move(Buf, HeaderNew[Length(HeaderNew) - 1], FITSRecordLen);
              SetLength(HeaderNew, Length(HeaderNew) + 1);
              Move(recordEND, HeaderNew[Length(HeaderNew) - 1], FITSRecordLen);
              EndOfHeaderFound := True;
              Break;
            end;
          end;

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

          // Writing file
          OutFileName := ImageNames[ColorL] + '-' + ExtractFileName(FITSfileName);
          if OutputDir = '' then
            OutFileName := ExtractFilePath(FITSfileName) + OutFileName
          else
            OutFileName := IncludeTrailingPathDelimiter(OutputDir) + OutFileName;
          Write(' ', ExtractFileName(OutFileName));
          if not Overwrite and FileExists(OutFileName) then
            FileError('File ' + AnsiQuotedStr(OutFileName, '"') + ' already exists. Use /F switch to overwrite.');
          AssignFile(OutFile, OutFileName);
          FileModeSaved := FileMode;
          FileMode := fmOpenReadWrite;
          try
            Rewrite(OutFile);
            try
              BlockWrite(OutFile, HeaderNew[0], StartOfImage);
              BlockWrite(OutFile, ImageC[ColorL]^, NrecordsToWrite);
            finally
              CloseFile(OutFile);
            end;
          finally
            FileMode := FileModeSaved;
          end;
        end;
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
end;

procedure ProcessFile(const FileName: string; const Profile: string; const OutputDir: string; Overwrite: Boolean);
var
  FITSfile: FITSRecordFile;
begin
  Write('Processing ', ExtractFileName(FileName));
  AssignFile(FITSfile, FileName);
  Reset(FITSfile);
  try
    if not IsFits(FITSfile) then
      FileError('Not a valid FITS file: ' + AnsiQuotedStr(FileName, '"'));
    FITSSplit(FITSfile, FileName, Profile, OutputDir, Overwrite);
  finally
    CloseFile(FITSfile);
  end;
  WriteLn(': done.');
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

procedure ProcessInput(const FileMasks: array of string; const Profile: string; const OutputDir: string; Overwrite: Boolean);
var
  I, N, Ntotal: Integer;
begin
  try
    WriteLn('Profile: ', Profile);
    Ntotal := 0;
    for N := 0 to Length(FileMasks) - 1 do begin
      WriteLn;
      WriteLn('[', FileMasks[N], ']');
      FileList.Clear;
      FileEnum(FileMasks[N], faArchive, False, TFileEnumClass.FileEnumProc);
      FileList.NaturalSort;
      for I := 0 to FileList.Count - 1 do begin
        ProcessFile(FileList[I], Profile, OutputDir, Overwrite);
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
  PrintVer: Boolean;
  Overwrite: Boolean;
  Profile: string;
  S, S2: string;
  ParamN: Integer;
  I: Integer;

begin
  FileMode := fmOpenRead;

  PrintVer := (CmdObj.CmdLine.IsCmdOption('V') or CmdObj.CmdLine.IsCmdOption('version'));
  if PrintVer then PrintVersion;

  if (CmdObj.CmdLine.IsCmdOption('?') or CmdObj.CmdLine.IsCmdOption('H') or CmdObj.CmdLine.IsCmdOption('help')) then begin
    PrintHelp;
    Halt(1);
  end;

  if (CmdObj.CmdLine.IsCmdOption('PI')) then begin
    PrintIni;
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
  Profile := 'DEFAULT';
  OutputDir := '';
  Overwrite := False;

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
      if CmdObj.CmdLine.ExtractParamValue(S, 'P=', S2) then begin
        if S2 <> '' then
          Profile := S2;
      end
      else
      if CmdObj.CmdLine.ParamIsKey(S, 'F') then
        Overwrite := True
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

  Ini := TMemIniFile.Create(ChangeFileExt(ParamStr(0), '.INI'));
  try
    // Check section
    for I := 0 to 3 do begin
      S := Trim(Ini.ReadString(Profile, IntToStr(I+1), ''));
      if S = '' then begin
        WriteLn('**** Error: Profile ' + Profile + ' is not defined or partially undefined!');
        Halt(1);
      end;
    end;
    FileList := TStringListNaturalSort.Create;
    try
      ProcessInput(InputFileMasks, Profile, OutputDir, Overwrite);
    finally
      FreeAndNil(FileList);
    end;
  finally
    FreeAndNil(Ini);
  end;
end.

