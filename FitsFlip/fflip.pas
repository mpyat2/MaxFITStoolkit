{$APPTYPE CONSOLE}

program FFLIP;

uses
  SysUtils, CmdObj{, CmdObjStdSwitches}, FITSUtils, EnumFiles, StringListNaturalSort;

procedure PrintVersion;
begin
  WriteLn('FITS Flip  Maksym Pyatnytskyy  2017');
  WriteLn('Version 2017.11.22.01');
  WriteLn;
end;

procedure PrintHelp;
begin
  WriteLn('Usage:');
  WriteLn(ExtractFileName(ParamStr(0)), ' file_mask1[.fit] [file_mask2[.fit] ...] [/2]');
  WriteLn;
  WriteLn('/2         Flip horizontally, if not specified -- vertically');
  WriteLn('/V         print version');
  WriteLn('/H         print this help and halt');
end;

var
  FileList: TStringListNaturalSort;

procedure FileError(S: string);
begin
  raise Exception.Create(S);
end;

type
  TIntArray = array of Integer;

procedure GetBitPixAndNaxis({$IFDEF FPC} var {$ELSE} const {$ENDIF} FITSfile: FITSRecordfile; const FITSfileName: string; out BitPix: Integer; out NaxisN: TIntArray);
var
  Value: string;
  Naxis: Integer;
  N: Integer;
  I: Integer;
  ErrorPos: Integer;
begin
  BitPix := 0;
  NaxisN := nil;
  if GetKeywordValue(FITSfile, 'BITPIX', Value, True, True) < 0 then
    FileError('Cannot get value of BITPIX. File ' + AnsiQuotedStr(FITSfileName, '"'));
  Val(Value, BitPix, ErrorPos);
  if (ErrorPos <> 0) or (BitPix = 0) or (BitPix mod 8 <> 0) then
    FileError('BITPIX has invalid value. File ' + AnsiQuotedStr(FITSfileName, '"'));
  if GetKeywordValue(FITSfile, 'NAXIS', Value, True, True) < 0 then
    FileError('Cannot get value of NAXIS. File ' + AnsiQuotedStr(FITSfileName, '"'));
  Val(Value, Naxis, ErrorPos);
  if (ErrorPos <> 0) or (Naxis < 0) or (Naxis > 999) then
    FileError('NAXIS has invalid value. File ' + AnsiQuotedStr(FITSfileName, '"'));
  if Naxis = 0 then Exit;
  SetLength(NaxisN, Naxis);
  for I := 0 to Naxis - 1 do begin
    if GetKeywordValue(FITSfile, 'NAXIS' + IntToStr(I + 1), Value, True, True) < 0 then
      FileError('Cannot get value of NAXIS' + IntToStr(I + 1) + '. File ' + AnsiQuotedStr(FITSfileName, '"'));
    Val(Value, N, ErrorPos);
    if (ErrorPos <> 0) or (N < 0) then
      FileError('NAXIS' + IntToStr(I + 1) + ' has invalid value. File ' + AnsiQuotedStr(FITSfileName, '"'));
    NaxisN[I] := N;
  end;
end;

procedure FITSflip({$IFDEF FPC} var {$ELSE} const {$ENDIF} FITSfile: FITSRecordfile; const FITSfileName: string; Vertically: Boolean);
var
  BitPix: Integer;
  BytePix: Integer;
  NaxisN: TIntArray;
  Naxis1, Naxis2, Naxis3: Integer;
  NblocksInHeader: Integer;
  NrecordsToRead: Integer;
  StartOfImage: Integer;
  Image: PChar;
  Chunk: PChar;
  Chunk1addr: Integer;
  Chunk2addr: Integer;
  Buf: array[0..31] of Char; // more than enough
  Pix1Addr: Integer;
  Pix2Addr: Integer;
  N, I, II, Planes: Integer;
  Offset: Integer;
  IOcount: Integer;
begin
  N := GetEndPosition(FITSfile);
  if N < 0 then
    FileError('Cannot find End of Header in file ' + AnsiQuotedStr(FITSfileName, '"'));
  NblocksInHeader := N div RecordsInBlock + 1;
  StartOfImage := NblocksInHeader * RecordsInBlock;
  GetBitPixAndNaxis(FITSfile, FITSfileName, BitPix, NaxisN);
  if (Length(NaxisN) < 2) or (Length(NaxisN) > 3) then
    FileError('Cannot work with NAXIS other than 2 or 3, got ' + IntToStr(Length(NaxisN)) + '. File ' + AnsiQuotedStr(FITSfileName, '"'));
  BytePix := Abs(BitPix) div 8;
  if BytePix > SizeOf(Buf) then
    FileError('Cannot work with BITPIX=' + IntToStr(BitPix) + '. File ' + AnsiQuotedStr(FITSfileName, '"'));

  Naxis1 := NaxisN[0];
  Naxis2 := NaxisN[1];
  Write(' [', Naxis1, 'x', Naxis2);
  if Length(NaxisN) = 3 then begin
    Naxis3 := NaxisN[2];
    Write('x', Naxis3);
  end
  else 
    Naxis3 := 1;
  Write(']');  
  NrecordsToRead := ((Naxis1 * Naxis2 * Naxis3 * BytePix - 1) div FITSRecordLen + 1);
  GetMem(Image, NrecordsToRead * FITSRecordLen);
  try
    FillChar(Image^, NrecordsToRead * FITSRecordLen, 0);
    Seek(FITSfile, StartOfImage);
    BlockRead(FITSfile, Image^, NrecordsToRead, IOcount);
    //if IOcount <> NrecordsToRead then WriteLn('Actual number of records in file differs from expected number');
    
    for Planes := 0 to Naxis3 - 1 do begin
      Offset := Naxis1 * Naxis2 * BytePix * Planes; 
      
      if Vertically then begin
        GetMem(Chunk, Naxis1 * BytePix);
        try
          for I := 0 to Naxis2 div 2 - 1 do begin
            Chunk1addr := Offset + I * Naxis1 * BytePix;
            Chunk2addr := Offset + (Naxis2 - I - 1) * Naxis1 * BytePix;
            Move(Image[Chunk1addr], Chunk^, Naxis1 * BytePix);
            Move(Image[Chunk2addr], Image[Chunk1addr], Naxis1 * BytePix);
            Move(Chunk^, Image[Chunk2addr], Naxis1 * BytePix);
          end;
        finally
          FreeMem(Chunk);
          Chunk := nil;
         end;
      end
      else begin
        for I := 0 to Naxis2 - 1 do begin
          for II := 0 to Naxis1 div 2 - 1 do begin
            Pix1Addr := Offset + (I * Naxis1 * BytePix) + II * BytePix;
            Pix2Addr := Offset + (I * Naxis1 * BytePix) + (Naxis1 - II - 1) * BytePix;
            Move(Image[Pix1Addr], Buf, BytePix);
            Move(Image[Pix2Addr], Image[Pix1Addr], BytePix);
            Move(Buf, Image[Pix2Addr], BytePix);
          end;
        end;
      end;
    end;
    
    Seek(FITSfile, StartOfImage);
    BlockWrite(FITSfile, Image^, NrecordsToRead, IOcount);
    if IOcount <> NrecordsToRead then FileError('Error writing file');
  finally
    FreeMem(Image);
    Image := nil;
  end;
end;

procedure ProcessFile(const FileName: string; Vertically: Boolean);
var
  FITSfile: FITSRecordFile;
  Value: string;
begin
  Write('Processing ', ExtractFileName(FileName));
  AssignFile(FITSfile, FileName);
  Reset(FITSfile);
  try
    if (GetKeywordValue(FITSfile, KeywordSimple, Value, True, True) <> 0) or ((Value <> 'T') and (Value <> 'F')) then
      FileError('Not a valid FITS file: ' + AnsiQuotedStr(FileName, '"'));
    FITSflip(FITSfile, FileName, Vertically);
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

procedure ProcessInput(const FileMasks: array of string; Vertically: Boolean);
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
        ProcessFile(FileList[I], Vertically);
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
  Vertically: Boolean;
  N: Integer;
  I: Integer;

begin
  FileMode := fmOpenReadWrite;

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

  Vertically := not CmdObj.CmdLine.IsCmdOption('2');

  FileList := TStringListNaturalSort.Create;
  try
    ProcessInput(InputFileMasks, Vertically);
  finally
    FreeAndNil(FileList);
  end;

end.

