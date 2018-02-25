{$APPTYPE CONSOLE}

program CalcSub;

uses
  SysUtils, Classes, CmdObj{, CmdObjStdSwitches}, FITSUtils, EnumFiles, StringListNaturalSort, FitsUtilsHelp, CommonIni;

procedure PrintVersion;
begin
  WriteLn('Subtract FITS  Maksym Pyatnytskyy  2017');
  WriteLn('Version 2018.01.17.01');
  WriteLn;
end;

var
  FileList: TStringListNaturalSort;

procedure FileError(const S: string);
begin
  raise Exception.Create(S);
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

procedure SubImages(Image, Image2: PChar; Offset: Double; BitPix: Integer; NImagePoints: Integer);
var
  BytePix: Integer;
  A1, A2, Aresult: TFITSValue;
  I, Addr: Integer;
begin
  BytePix := Abs(BitPix) div 8;
  for I := 0 to NImagePoints - 1 do begin
    FillChar(Aresult, SizeOf(Aresult), 0);
    Addr := I * BytePix;
    Move(Image[Addr], A1, BytePix);
    RevertBytes(A1, BitPix);
    if Image2 <> nil then begin
      Move(Image2[Addr], A2, BytePix);
      RevertBytes(A2, BitPix);
    end
    else
      FillChar(A2, SizeOf(A2), 0);
    case BitPix of
        8: Aresult.H := A1.B - A2.B - Round(Offset);
       16: Aresult.H := A1.I - A2.I - Round(Offset);
       32: Aresult.H := A1.L - A2.L - Round(Offset);
      -32: Aresult.E := A1.S - A2.S - Offset;
      -64: Aresult.E := A1.D - A2.D - Offset;
    end;
    RevertBytes(Aresult, BitPix);
    Move(Aresult, Image[Addr], BytePix);
  end;
end;

procedure DoSubtraction(const InputFileName: string; const FileToSubName: string; Offset: Double; const Prefix: string; const OutputDir: string; CheckExistence: Boolean);
var
  FITSfile: FITSRecordfile;
  FITSfileToSub: FITSRecordfile;
  FITSfileOut: FITSRecordfile;
  OutFileName: string;
  I, N: Integer;
  S: string;
  NblocksInHeader, StartOfImage: Integer;
  NImagePoints: Integer;
  NrecordsToRead: Integer;
  ImageMemSize: Integer;
  BitPix, BytePix: Integer;
  NaxisN: TIntArray;
  Header: PChar;
  Image: PChar;
  Image2: PChar;

  NblocksInHeader2, StartOfImage2: Integer;
  BitPix2: Integer;
  NaxisN2: TIntArray;

  FileModeSaved: Integer;
begin
  Header := nil;
  Image := nil;
  Image2 := nil;
  AssignFile(FITSfile, InputFileName);
  Reset(FITSfile);
  try
    N := GetEndPosition(FITSfile);
    if N < 0 then
      FileError('Cannot find End of Header in file ' + AnsiQuotedStr(InputFileName, '"'));
    NblocksInHeader := N div RecordsInBlock + 1;
    StartOfImage := NblocksInHeader * RecordsInBlock;
    GetBitPixAndNaxis(FITSfile, InputFileName, BitPix, NaxisN);
    BytePix := Abs(BitPix) div 8;
    NImagePoints := 1;
    for I := 0 to Length(NaxisN) - 1 do
      NImagePoints := NImagePoints * NaxisN[I];
    NrecordsToRead := (NImagePoints * BytePix - 1) div FITSRecordLen + 1;

    OutFileName := Prefix + ExtractFileName(InputFileName);
    if OutputDir = '' then
      OutFileName := ExtractFilePath(InputFileName) + OutFileName
    else
      OutFileName := IncludeTrailingPathDelimiter(OutputDir) + OutFileName;
    if CheckExistence and FileExists(OutFileName) then
      FileError('File ' + AnsiQuotedStr(OutFileName, '"') + ' already exists. Use /F switch to overwrite.');

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

        if FileToSubName <> '' then begin
          AssignFile(FITSfileToSub, FileToSubName);
          Reset(FITSfileToSub);
        end;
        try
          if FileToSubName <> '' then begin
            N := GetEndPosition(FITSfileToSub);
            if N < 0 then
              FileError('Cannot find End of Header in file ' + AnsiQuotedStr(FileToSubName, '"'));
            NblocksInHeader2 := N div RecordsInBlock + 1;
            StartOfImage2 := NblocksInHeader2 * RecordsInBlock;
            GetBitPixAndNaxis(FITSfileToSub, FileToSubName, BitPix2, NaxisN2);
            if BitPix <> BitPix2 then
              FileError('Input file and file to subtract must have identical BITPIX');
            if Length(NaxisN) <> Length(NaxisN2) then
              FileError('Input file and file to subtract must have identical number of axes');
            for I := 0 to Length(NaxisN) - 1 do
              if NaxisN[I] <> NaxisN2[I] then
                FileError('Input file and file to subtract must have identical lengthes of axes');
            GetMem(Image2, ImageMemSize);
          end;
          try
            if FileToSubName <> '' then begin
              FillChar(Image2^, ImageMemSize, 0);
              Seek(FITSfileToSub, StartOfImage2);
              BlockRead(FITSfileToSub, Image2^, NrecordsToRead);
            end;
            AssignFile(FITSfileOut, OutFileName);
            FileModeSaved := FileMode;
            FileMode := fmOpenReadWrite;
            try
              Rewrite(FITSfileOut);
              try
                SubImages(Image, Image2, Offset, BitPix, NImagePoints);
                BlockWrite(FITSfileOut, Header^, StartOfImage);
                BlockWrite(FITSfileOut, Image^, NrecordsToRead);
                if FileToSubName <> '' then
                  AddCommentLikeKeyword(FITSfileOut, OutFileName, 'HISTORY', 'File subtracted: ' + ExtractFileName(FileToSubName), True);
                if Offset <> 0 then begin
                  if Frac(Offset) = 0 then
                    Str(Offset:0:0, S)
                  else
                    Str(Offset, S);
                  AddCommentLikeKeyword(FITSfileOut, OutFileName, 'HISTORY', 'Offset subtracted: ' + S, True);
                end;
              finally
                CloseFile(FITSfileOut);
              end;
            finally
              FileMode := FileModeSaved;
            end;
          finally
            if FileToSubName <> '' then begin
              FreeMem(Image2);
              Image2 := nil;
            end;
          end;
        finally
          if FileToSubName <> '' then
            CloseFile(FITSfileToSub);
        end;
      finally
        FreeMem(Image);
        Image := nil;
      end;
    finally
      FreeMem(Header);
      Header := nil;
    end;
  finally
    CloseFile(FITSfile);
  end;
end;

procedure ProcessInput(const FileMasks: array of string; const FileToSubName: string; Offset: Double; const Prefix: string; const OutputDir: string; CheckExistence: Boolean);
var
  I, N, Count: Integer;
begin
  try
    Count := 0;
    for N := 0 to Length(FileMasks) - 1 do begin
      WriteLn;
      WriteLn('[', FileMasks[N], ']');
      FileList.Clear;
      FileEnum(FileMasks[N], faArchive, False, TFileEnumClass.FileEnumProc);
      FileList.NaturalSort;
      for I := 0 to FileList.Count - 1 do begin
        Write(ExtractFileName(FileList[I]));
        DoSubtraction(FileList[I], FileToSubName, Offset, Prefix, OutputDir, CheckExistence);
        if FileToSubName <> '' then Write(' minus ', ExtractFileName(FileToSubName));
        if Offset <> 0 then begin
          Write(' minus ');
          if Frac(Offset) = 0 then
            Write(Offset:0:0)
          else
            Write(Offset:0:0);
        end;
        WriteLn;
        Inc(Count);
      end;
      WriteLn;
    end;
    if Count < 1 then begin
      WriteLn;
      WriteLn('**** No files found.');
      Exit;
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
  PrintVer: Boolean;
  Overwrite: Boolean;
  I, N: Integer;
  S: string;
  Prefix: string;
  OutputDir: string;
  FileToSubName: string;
  Offset: Double;
  ErrorPos: Integer;

begin
  FileMode := fmOpenRead;

  WriteLn('**** WARNING!!! THIS VERSION DOES NOT SUPPORT BSCALE/BZERO YET! SHOULD BE IMPLEMENTED BEFORE RELEASE!');

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

  OutputDir := Trim(CmdObj.CmdLine.KeyValue('O='));
  if OutputDir <> '' then OutputDir := ExpandFileName(OutputDir);

  Overwrite := CmdObj.CmdLine.IsCmdOption('F');

  FileToSubName := CmdObj.CmdLine.KeyValue('S=');
  if FileToSubName <> '' then begin
    FileToSubName := ExpandFileName(FileToSubName);
    if ExtractFileExt(FileToSubName) = '' then FileToSubName := ChangeFileExt(FileToSubName, '.fit');
  end;

  Prefix := CmdObj.CmdLine.KeyValue('P=');
  if Prefix = '' then begin
    WriteLn('**** Output file prefix not specified');
    WriteLn;
    PrintHelp;
    Halt(1);
  end;

  Offset := 0;
  S := CmdObj.CmdLine.KeyValue('V=');
  if S <> '' then begin
    Val(S, Offset, ErrorPos);
    if ErrorPos <> 0 then begin
      WriteLn('**** Offset must be integer of floating-point value');
      WriteLn;
      PrintHelp;
      Halt(1);
    end;
  end;

  SetLength(InputFileMasks, N);
  for I := 1 to N do begin
    InputFileMasks[I - 1] := ExpandFileName(CmdObj.CmdLine.ParamFile(I));
    if ExtractFileExt(InputFileMasks[I - 1]) = '' then InputFileMasks[I - 1] := ChangeFileExt(InputFileMasks[I - 1], '.fit');
  end;

  FileList := TStringListNaturalSort.Create;
  try
    ProcessInput(InputFileMasks, FileToSubName, Offset, Prefix, OutputDir, not Overwrite);
  finally
    FreeAndNil(FileList);
  end;

end.

