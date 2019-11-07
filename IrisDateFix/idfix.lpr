{*****************************************************************************}
{                                                                             }
{ Date Fix                                                                    }
{ (c) 2019 Maksym Pyatnytskyy                                                 }
{                                                                             }
{ This program is distributed                                                 }
{ WITHOUT ANY WARRANTY; without even the implied warranty of                  }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                        }
{                                                                             }
{*****************************************************************************}

{$APPTYPE CONSOLE}
{$MODE DELPHI}
{$INCLUDE FITSUtils.inc}

program idfix;

uses
  SysUtils, CmdObj{, CmdObjStdSwitches}, Version, EnumFiles, MiscUtils,
  FITScompatibility, FITSUtils, StringListNaturalSort, FitsUtilsHelp, CommonIni;

{$R *.res} // include version info!

{$INCLUDE PrintVersion.inc}

var
  FileList: TStringListNaturalSort;

procedure FileError(const S: string);
begin
  raise Exception.Create(S);
end;

procedure ProcessFITSfile(var FITSfile: FITSRecordfile; TimeShift: Double; ShiftByExposureBackward: Boolean);
var
  BitPix: Integer;
  NaxisN: TIntArray;
  StartOfImage: Integer;
  FITSImageSize: Integer;
  ImageMemSize: PtrUInt;
  DateObs: TDateTime;
  Exposure: Double;
  Header, HeaderNew: TFITSRecordArray;
  FITSfileSize: Int64;
  FileImageMinusHeader: array of FITSRecordType;
  Buf: FITSRecordType;
  DateObsHistory: string;
  DateObsKeyword: string;
  TimeObsKeyword: string;
  UTstartKeyword: string; // IRIS-specific
  TempS: string;
  I, N, N2: Integer;
  ErrorPos: Integer;
begin
  GetFITSproperties(FITSfile, BitPix, NaxisN, StartOfImage, ImageMemSize);
  DateObs := GetDateObs(FITSfile);
  Exposure := GetExposureTime(FITSfile);
  if ShiftByExposureBackward then
    TimeShift := TimeShift - Exposure;
  DateObs := DateObs + TimeShift / (24.0*60.0*60.0);
  Str(TimeShift:0:1, TempS);
  DateObsHistory := 'DATE-OBS changed by ' + TempS + ' seconds';
  GetHeader(FITSfile, Header);
  FITSfileSize := FileSize(FITSfile);
  FITSImageSize := FITSfileSize - StartOfImage;
  SetLength(FileImageMinusHeader, FITSImageSize);
  FillChar(FileImageMinusHeader[0], FITSImageSize * FITSRecordLen, 0);
  Seek(FITSfile, StartOfImage);
  BlockRead(FITSfile, FileImageMinusHeader[0], FITSImageSize);

  // Set new DATE-OBS.
  // Remove TIME-OBS, UT-START (IRIS-specific) if exist
  DateObsKeyword := PadCh('DATE-OBS', FITSKeywordLen, ' ');
  TimeObsKeyword := PadCh('TIME-OBS', FITSKeywordLen, ' ');
  UTstartKeyword := PadCh('UT-START', FITSKeywordLen, ' ');
  HeaderNew := nil;
  for I := 0 to Length(Header) - 1 do begin
    Buf := Header[I];
   if Buf = recordEND then begin
      TempS := 'HISTORY ' + DateObsHistory;
      StrToFITSRecord(TempS, Buf);
      SetLength(HeaderNew, Length(HeaderNew) + 1);
      HeaderNew[Length(HeaderNew) - 1] := Buf;
      SetLength(HeaderNew, Length(HeaderNew) + 1);
      HeaderNew[Length(HeaderNew) - 1] := recordEND;
    end
    else
    if Copy(Buf, 1, FITSKeywordLen) = DateObsKeyword then begin
      // New DATE-OBS
      TempS := 'DATE-OBS= ' + '''' + FormatDateTime('YYYY-MM-DD"T"hh:nn:ss.zzz', DateObs) + '''';
      TempS := TempS + ' / Value changed. See HISTORY.';
      StrToFITSRecord(TempS, Buf);
      SetLength(HeaderNew, Length(HeaderNew) + 1);
      HeaderNew[Length(HeaderNew) - 1] := Buf;
    end
    else
    if Copy(Buf, 1, FITSKeywordLen) = TimeObsKeyword then begin
      // ignore this value
    end
    else
    if Copy(Buf, 1, FITSKeywordLen) = UTstartKeyword then begin
      // ignore this value
    end
    else begin
      // add record 'as is'
      SetLength(HeaderNew, Length(HeaderNew) + 1);
      HeaderNew[Length(HeaderNew) - 1] := Buf;
    end;
  end;
  // Pad HeaderNew
  // Padding to FITS block size
  N := Length(HeaderNew);
  N2 := N mod RecordsInBlock;
  if N2 > 0 then begin
    for I := 1 to RecordsInBlock - N2 do begin
      SetLength(HeaderNew, N + 1);
      FillChar(HeaderNew[N], SizeOf(FITSRecordType), ' ');
      Inc(N);
    end;
  end;
  Seek(FITSfile, 0);
  Truncate(FITSfile);
  BlockWrite(FITSfile, HeaderNew[0], N);
  BlockWrite(FITSfile, FileImageMinusHeader[0], FITSImageSize);
  Write(DateObsHistory);
end;

procedure ProcessFile(const FileName: string; TimeShift: Double; ShiftByExposureBackward: Boolean);
var
  FITSfile: FITSRecordFile;
begin
  Write('Processing ', ExtractFileName(FileName) + ': ');
  AssignFile(FITSfile, FileName);
  Reset(FITSfile);
  try
    if not IsFits(FITSfile) then
      FileError('Not a valid FITS file: ' + AnsiQuotedStr(FileName, '"'));
    ProcessFITSfile(FITSfile, TimeShift, ShiftByExposureBackward);
  finally
    CloseFile(FITSfile);
  end;
  WriteLn('.');
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

procedure ProcessInput(const FileMasks: array of string; TimeShift: Double; ShiftByExposureBackward: Boolean);
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
        ProcessFile(FileList[I], TimeShift, ShiftByExposureBackward);
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
  TimeShift: Double;
  TimeShiftExposureBackward: boolean;
  ErrorPos: Integer;

begin
  FileMode := fmOpenReadWrite;

  PrintVer := (CmdObj.CmdLine.IsCmdOption('V') or CmdObj.CmdLine.IsCmdOption('version'));
  if PrintVer then PrintVersion('Fix DATE-OBS inplace');

  if (CmdObj.CmdLine.IsCmdOption('?') or CmdObj.CmdLine.IsCmdOption('H') or CmdObj.CmdLine.IsCmdOption('help')) then begin
    PrintHelp;
    Halt(1);
  end;

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
  TimeShift := 0;
  TimeShiftExposureBackward := False;

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
      if CmdObj.CmdLine.ExtractParamValue(S, 'TS', S2) then begin
        if S2 <> '' then begin
          Val(S2, TimeShift, ErrorPos);
          if ErrorPos <> 0 then begin
            PrintError('**** Time Shift must be a number'^M^J);
            Halt(1);
          end;
        end;
      end
      else
      if CmdObj.CmdLine.ExtractParamValue(S, '-E', S2) then begin
        TimeShiftExposureBackward := True;
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

  if (TimeShift = 0) and (not TimeShiftExposureBackward) then begin
    PrintWarning('**** Nothing to do: /TS is missing or equals to 0, /-E is not specified'^M^J);
  end
  else begin
    FileList := TStringListNaturalSort.Create;
    try
      ProcessInput(InputFileMasks, TimeShift, TimeShiftExposureBackward);
    finally
      FreeAndNil(FileList);
    end;
  end;
end.

