{$APPTYPE CONSOLE}

program FITSRGB;

uses
  SysUtils, CmdObj{, CmdObjStdSwitches}, FITSUtils, EnumFiles, StringListNaturalSort, FitsUtilsHelp, CommonIni;

procedure PrintVersion;
begin
  WriteLn('FITS RGB splitter  Maksym Pyatnytskyy  2018');
  WriteLn('Version 2018.02.22.01');
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
  Image: PChar;
  ImageLayer: PChar;
  ImageLayerSize: Integer;
  ImageLayerSizePadded: Integer;
  ImageMemSize: Integer;
  ColorL: Integer;
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
          AssignFile(OutFile, OutFileName);
          FileModeSaved := FileMode;
          FileMode := fmOpenReadWrite;
          try
            Rewrite(OutFile);
            try
              BlockWrite(OutFile, Header^, StartOfImage);
              SetKeywordValue(OutFile, 'NAXIS', '2', True, '', False);
              Seek(OutFile, StartOfImage);
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
  OutputDir: string;
  PrintVer: Boolean;
  Overwrite: Boolean;
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

  OutputDir := Trim(CmdObj.CmdLine.KeyValue('O='));
  if OutputDir <> '' then OutputDir := ExpandFileName(OutputDir);

  Overwrite := CmdObj.CmdLine.IsCmdOption('F');

  FileList := TStringListNaturalSort.Create;
  try
    ProcessInput(InputFileMasks, OutputDir, Overwrite);
  finally
    FreeAndNil(FileList);
  end;
end.

