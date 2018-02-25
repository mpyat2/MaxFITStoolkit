{$APPTYPE CONSOLE}

program FITSRGB;

uses
  SysUtils, CmdObj{, CmdObjStdSwitches}, FITSUtils, EnumFiles, StringListNaturalSort, FitsUtilsHelp, CommonIni;

procedure PrintVersion;
begin
  WriteLn('FITS RGB splitter  Maksym Pyatnytskyy  2018');
  WriteLn('Version 2018.02.23.01');
  WriteLn;
end;

var
  FileList: TStringListNaturalSort;

procedure FileError(const S: string);
begin
  raise Exception.Create(S);
end;

const
  ColorNames: array[0..2] of string = ('R', 'G', 'B');

procedure FITSSplitRGB(var FITSfile: FITSRecordfile; const FITSfileName: string; const OutputDir: string; Overwrite: Boolean);
var
  N: Integer;
  BitPix: Integer;
  BytePix: Integer;
  NaxisN: TIntArray;
  Naxis1, Naxis2, Naxis3: Integer;
  NblocksInHeader: Integer;
  StartOfImage: Integer;
  Header: PChar;
  Buf: FITSRecordType;
  HeaderNew: TFITSRecordArray;
  Image: PChar;
  ImageLayer: PChar;
  ImageLayerSize: Integer;
  ImageLayerSizePadded: Integer;
  ImageMemSize: Integer;
  ColorL: Integer;
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
  if (Length(NaxisN) <> 3) then
    FileError('Cannot work with NAXIS other than 3, got ' + IntToStr(Length(NaxisN)) + '. File ' + AnsiQuotedStr(FITSfileName, '"'));
  BytePix := Abs(BitPix) div 8;
  Naxis1 := NaxisN[0];
  Naxis2 := NaxisN[1];
  Naxis3 := NaxisN[2];
  if (Naxis3 <> 3) then
    FileError('Cannot work with NAXIS3 other than 3, got ' + IntToStr(Naxis3) + '. File ' + AnsiQuotedStr(FITSfileName, '"'));
  Write(' [', Naxis1, 'x', Naxis2, 'x', Naxis3, '] [BITPIX=', BitPix, '] -> ');
  GetMem(Header, StartOfImage * FITSRecordLen);
  try
    FillChar(Header^, StartOfImage * FITSRecordLen, 0);
    Seek(FITSFile, 0);
    BlockRead(FITSFile, Header^, StartOfImage);
    ImageLayerSize := Naxis1 * Naxis2 * BytePix;
    ImageLayerSizePadded := ((ImageLayerSize - 1) div (FITSRecordLen * RecordsInBlock) + 1) * FITSRecordLen * RecordsInBlock;
    ImageMemSize := ((3 * ImageLayerSize - 1) div (FITSRecordLen * RecordsInBlock) + 1) * FITSRecordLen * RecordsInBlock;
    GetMem(Image, ImageMemSize);
    try
      FillChar(Image^, ImageMemSize, 0);
      GetMem(ImageLayer, ImageLayerSizePadded);
      try
        FillChar(ImageLayer^, ImageLayerSizePadded, 0);
        BlockRead(FITSFile, Image^, ImageMemSize div FITSRecordLen);
        for ColorL := 0 to 2 do begin
           OutFileName := ColorNames[ColorL] + '-' + ExtractFileName(FITSfileName);
          if OutputDir = '' then
            OutFileName := ExtractFilePath(FITSfileName) + OutFileName
          else
            OutFileName := IncludeTrailingPathDelimiter(OutputDir) + OutFileName;
          Write(' ', ExtractFileName(OutFileName));
          if not Overwrite and FileExists(OutFileName) then
            FileError('File ' + AnsiQuotedStr(OutFileName, '"') + ' already exists. Use /F switch to overwrite.');
          Move(Image[ImageLayerSize * ColorL], ImageLayer[0], ImageLayerSize);

          // We should remove NAXIS3 record and update NAXIS.
          HeaderNew := nil;
          for I := 0 to StartOfImage - 1 do begin
            Move(Header[I * FITSRecordLen], Buf, FITSRecordLen);
            if Copy(Buf, 1, FITSKeywordLen) = PadCh('NAXIS', FITSKeywordLen, ' ') then begin
              Str(2:FITSNumericAlign - FITSKeywordLen - 2, S);
              StrToFITSRecord(PadCh('NAXIS', FITSKeywordLen, ' ') + '= ' + S, Buf);
            end;
            if Copy(Buf, 1, FITSKeywordLen) <> PadCh('NAXIS3', FITSKeywordLen, ' ') then begin
              SetLength(HeaderNew, Length(HeaderNew) + 1);
              Move(Buf, HeaderNew[Length(HeaderNew) - 1], FITSRecordLen);
            end;
            if Buf = recordEND then
              Break;
          end;

          // Padding
          N := Length(HeaderNew) mod RecordsInBlock;
          if N > 0 then begin
            for I := 1 to RecordsInBlock - N do begin
              SetLength(HeaderNew, Length(HeaderNew) + 1);
              FillChar(HeaderNew[Length(HeaderNew) - 1], SizeOf(FITSRecordType), ' ');
            end;
          end;

          // Updating StartOfImage
          StartOfImage := Length(HeaderNew);

          AssignFile(OutFile, OutFileName);
          FileModeSaved := FileMode;
          FileMode := fmOpenReadWrite;
          try
            Rewrite(OutFile);
            try
              BlockWrite(OutFile, HeaderNew[0], StartOfImage);
              BlockWrite(OutFile, ImageLayer^, ImageLayerSizePadded div FITSRecordLen);
              AddCommentLikeKeyword(OutFile, 'COMMENT', 'Color Layer: ' + ColorNames[ColorL], True);
            finally
              CloseFile(OutFile);
            end;
          finally
            FileMode := FileModeSaved;
          end;
        end;
      finally
        FreeMem(ImageLayer);
        ImageLayer := nil;
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

procedure ProcessFile(const FileName: string; const OutputDir: string; Overwrite: Boolean);
var
  FITSfile: FITSRecordFile;
begin
  Write('Processing ', ExtractFileName(FileName));
  AssignFile(FITSfile, FileName);
  Reset(FITSfile);
  try
    if not IsFits(FITSfile) then
      FileError('Not a valid FITS file: ' + AnsiQuotedStr(FileName, '"'));
    FITSSplitRGB(FITSfile, FileName, OutputDir, Overwrite);
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

procedure ProcessInput(const FileMasks: array of string; const OutputDir: string; Overwrite: Boolean);
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
        ProcessFile(FileList[I], OutputDir, Overwrite);
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
  PrintVer: Boolean;
  OutputDir: string;
  Overwrite: Boolean;
  S: string;
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

  FileList := TStringListNaturalSort.Create;
  try
    ProcessInput(InputFileMasks, OutputDir, Overwrite);
  finally
    FreeAndNil(FileList);
  end;
end.

