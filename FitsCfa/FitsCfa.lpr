{$APPTYPE CONSOLE}

program FITSCFA;

uses
  SysUtils, CmdObj{, CmdObjStdSwitches}, FITSUtils, EnumFiles, StringListNaturalSort, IniFiles, FitsUtilsHelp, CommonIni;

procedure PrintVersion;
begin
  WriteLn('FITS CFA splitter  Maksym Pyatnytskyy  2017');
  WriteLn('Version 2018.02.22.01');
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

procedure FITSSplit(var FITSfile: FITSRecordfile; const FITSfileName: string; const Profile: string; const OutputDir: string; Overwrite: Boolean);
var
  N: Integer;
  BitPix: Integer;
  BytePix: Integer;
  NaxisN: TIntArray;
  Naxis1, Naxis2: Integer;
  NblocksInHeader: Integer;
  NrecordsToRead: Integer;
  NRecordsToWrite: Integer;
  StartOfImage: Integer;
  Header: PChar;
  Image: PChar;
  ImageC: array[0..3] of PChar;
  ImageToAverage: TPCharArray;
  ImageNames: array[0..3] of string;
  ImageMemSize: Integer;
  Image2MemSize: Integer;
  C, R, C2, R2, X, Y: Integer;
  ShiftV, ShiftH: Integer;
  ColorL, ColorL2, LL: Integer;
  PixAddr, PixAddr2: Integer;
  OutFile: FITSRecordfile;
  OutFileName: string;
  FileModeSaved: Integer;
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
  NrecordsToRead := ((Naxis1 * Naxis2 * BytePix - 1) div FITSRecordLen + 1);
  NRecordsToWrite := (((Naxis1 div 2) * (Naxis2 div 2) * BytePix - 1) div FITSRecordLen + 1);
  NRecordsToWrite := ((NRecordsToWrite - 1) div RecordsInBlock + 1) * RecordsInBlock;
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
      Image2MemSize := NrecordsToWrite * FITSRecordLen;
      for ColorL := 0 to 3 do ImageC[ColorL] := nil;
      try
        for ShiftV := 0 to 1 do begin
          for ShiftH := 0 to 1 do begin
            ColorL := ShiftH + ShiftV * 2;
            ImageNames[ColorL] := Trim(Ini.ReadString(Profile, IntToStr(ColorL + 1), ''));
            if ImageNames[ColorL] = '' then ImageNames[ColorL] := 'p' + IntToStr(ColorL + 1);
            GetMem(ImageC[ColorL], Image2MemSize);
            //FillChar(ImageC[ColorL]^, Image2MemSize, ' ');
            //FillChar(ImageC[ColorL]^, (Naxis1 div 2) * (Naxis2 div 2) * BytePix, 0);
            FillChar(ImageC[ColorL]^, Image2MemSize, 0);
            for C := 0 to Naxis1 - 1 do begin
              for R := 0 to Naxis2 - 1 do begin
                C2 := C + ShiftH;
                R2 := R + ShiftV;
                PixAddr := (R2 * Naxis1 + C2) * BytePix;
                if (C mod 2 = 0) and (R mod 2 = 0) then begin
                  X := C div 2;
                  Y := R div 2;
                  if (X < Naxis1 div 2) and (Y < Naxis2 div 2) then begin
                    PixAddr2 := (Y * (Naxis1 div 2) + X) * BytePix;
                    Move(Image[PixAddr], ImageC[ColorL][PixAddr2], BytePix);
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
              AverageLayers(ImageToAverage, (Naxis1 div 2) * (Naxis2 div 2) * BytePix, BitPix);
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
                BlockWrite(OutFile, Header^, StartOfImage);
                SetKeywordValue(OutFile, 'NAXIS1', IntToStr(Naxis1 div 2), True, '', False);
                SetKeywordValue(OutFile, 'NAXIS2', IntToStr(Naxis2 div 2), True, '', False);
                Seek(OutFile, StartOfImage);
                BlockWrite(OutFile, ImageC[ColorL]^, NrecordsToWrite);
                SetKeywordValue(OutFile, 'FILTER', FITSQuotedValue(' ' + ImageNames[ColorL]), False, 'Color Layer', True);
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
  finally
    FreeMem(Header);
    Header := nil;
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
  ProfileDefined: Boolean;
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

  if (CmdObj.CmdLine.IsCmdOption('PI')) then begin
    PrintIni;
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

  SetLength(InputFileMasks, N);
  for I := 1 to N do begin
    InputFileMasks[I - 1] := ExpandFileName(CmdObj.CmdLine.ParamFile(I));
    if ExtractFileExt(InputFileMasks[I - 1]) = '' then InputFileMasks[I - 1] := ChangeFileExt(InputFileMasks[I - 1], '.fit');
  end;

  Profile := Trim(CmdObj.CmdLine.KeyValue('P='));
  if Profile = '' then Profile := 'DEFAULT';

  OutputDir := Trim(CmdObj.CmdLine.KeyValue('O='));
  if OutputDir <> '' then OutputDir := ExpandFileName(OutputDir);

  Overwrite := CmdObj.CmdLine.IsCmdOption('F');

  Ini := TMemIniFile.Create(ChangeFileExt(ParamStr(0), '.INI'));
  try
    // Check section
    ProfileDefined := True;
    for I := 0 to 3 do begin
      S := Trim(Ini.ReadString(Profile, IntToStr(I+1), ''));
      if S = '' then begin
        ProfileDefined := False;
        Break;
      end;
    end;
    if not ProfileDefined then begin
      WriteLn('**** Error: Profile ' + Profile + ' is not defined or partially undefined!');
      Halt(1);
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

