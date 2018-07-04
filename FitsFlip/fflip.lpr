{*****************************************************************************}
{                                                                             }
{ FIHED                                                                       }
{ (c) 2017 Maksym Pyatnytskyy                                                 }
{                                                                             }
{ This program is distributed                                                 }
{ WITHOUT ANY WARRANTY; without even the implied warranty of                  }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                        }
{                                                                             }
{*****************************************************************************}

{$APPTYPE CONSOLE}
{$MODE DELPHI}
{$INCLUDE FITSUtils.inc}

program FFLIP;

uses
  SysUtils, CmdObj{, CmdObjStdSwitches}, Version, EnumFiles, FITScompatibility,
  FITSUtils, StringListNaturalSort, FitsUtilsHelp, CommonIni;

{$R *.res}

procedure PrintVersion;
begin
  WriteLn('FITS Flip  Maksym Pyatnytskyy  2017');
  WriteLn(GetVersionString(AnsiUpperCase(ParamStr(0))){$IFDEF WIN64}, ' WIN64'{$ENDIF}, ' ', {$I %DATE%}, ' ', {$I %TIME%});
  WriteLn;
end;

var
  FileList: TStringListNaturalSort;

procedure FileError(const S: string);
begin
  raise Exception.Create(S);
end;

procedure FITSflip(var FITSfile: FITSRecordfile; const FITSfileName: string; Vertically: Boolean);
var
  ImageMemSize: PtrUInt;
  BitPix: Integer;
  BytePix: Integer;
  NaxisN: TIntArray;
  Naxis1, Naxis2, Naxis3: Integer;
  StartOfImage: Integer;
  Image: PChar;
  Chunk: PChar;
  Chunk1addr: Integer;
  Chunk2addr: Integer;
  Buf: array[0..31] of Char; // more than enough
  Pix1Addr: Integer;
  Pix2Addr: Integer;
  I, II, Planes: Integer;
  Offset: Integer;
begin
  GetFITSproperties(FITSfile, BitPix, NaxisN, StartOfImage, ImageMemSize);
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
  GetMem(Image, ImageMemSize);
  try
    FillChar(Image^, ImageMemSize, 0);
    Seek(FITSfile, StartOfImage);
    BlockRead(FITSfile, Image^, ImageMemSize div FITSRecordLen);
    
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
    BlockWrite(FITSfile, Image^, ImageMemSize div FITSRecordLen);
  finally
    FreeMem(Image);
    Image := nil;
  end;
end;

procedure ProcessFile(const FileName: string; Vertically: Boolean);
var
  FITSfile: FITSRecordFile;
begin
  Write('Processing ', ExtractFileName(FileName));
  AssignFile(FITSfile, FileName);
  Reset(FITSfile);
  try
    if not IsFits(FITSfile) then
      FileError('Not a valid FITS file: ' + AnsiQuotedStr(FileName, '"'));
    FITSflip(FITSfile, FileName, Vertically);
  finally
    CloseFile(FITSfile);
  end;
  Write(': ');
  if Vertically then Write('V') else Write('H');
  WriteLn('-flip done.');
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
  S: string;
  ParamN: Integer;

begin
  FileMode := fmOpenReadWrite;

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
  Vertically := True;

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
      if CmdObj.CmdLine.ParamIsKey(S, '1') then
        Vertically := False
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
    ProcessInput(InputFileMasks, Vertically);
  finally
    FreeAndNil(FileList);
  end;

end.

