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

program FIHED;

uses
  SysUtils, Classes, CmdObj, Version, FihedSwitchChars, EnumFiles,
  FITScompatibility, FITSUtils, StringListNaturalSort, FitsUtilsHelp, CommonIni;
// Do not include CmdObjStdSwitches in the current version, instead, use FihedSwitchChars.
// Currently slash '/' is used to distinguish options (double slash) and FITS keywords (single slash)

{$R *.res}

procedure PrintVersion;
begin
  WriteLn('FITS Header Viewer/Editor  Maksym Pyatnytskyy  2018');
  WriteLn(GetVersionString(AnsiUpperCase(ParamStr(0))){$IFDEF WIN64}, ' WIN64'{$ENDIF}, ' ', {$I %DATE%}, ' ', {$I %TIME%});
  WriteLn;
end;  
  
procedure FileError(const S: string);
begin
  raise Exception.Create(S);
end;

const
  Delimiter: array[Boolean] of char = (';', ^I);

type
  TFihedMode = (fihedNONE, fihedTAB, fihedCSV, fihedSET);

procedure ProcessFile(const FileName: string; Keywords: TStrings; FihedMode: TFihedMode);
var
  Header: TStringList;
  I: Integer;
  FITSFile: FITSRecordFile;
  Name: string;
  Value: string;
  TempValue: string;
  S: string;
  P: Integer;
  Success: Boolean;
  FileModeSaved: Integer;
begin
  if not (FihedMode in [fihedTAB, fihedCSV]) then
    WriteLn(PadCh('File', FITSKeywordLen, ' ') + '= ', QuotedStr(ExtractFilename(FileName)))
  else
    Write(QuotedStr(ExtractFilename(FileName)));

  FileModeSaved := FileMode;
  if FihedMode = fihedSET then FileMode := fmOpenReadWrite; // Edit mode
  try
    AssignFile(FITSFile, FileName);
    Reset(FITSFile);
    try
      if IsFITS(FITSFile) then begin
        if FihedMode <> fihedSET then begin
          // Print header and exit
          if not (FihedMode in [fihedTAB, fihedCSV]) then begin
            Header := TStringList.Create;
            try
              GetHeader(FITSFile, Header);
              for I := 0 to Header.Count - 1 do begin
                if (Keywords.Count < 1) or (Keywords.IndexOf(TrimRight(Copy(Header[I], 1, FITSKeywordLen))) >= 0) then
                  WriteLn(TrimRight(Header[I]));
              end;
            finally
              FreeAndNil(Header);
            end;
            WriteLn;
          end
          else begin
            // Table-like output
            for I := 0 to Keywords.Count - 1 do begin
              GetKeywordValue(FITSFile, Keywords[I], Value, True, True);
              Write(Delimiter[FihedMode = fihedTAB]);
              Write(Value);
            end;
            WriteLn;
          end;
        end
        else begin
          // Edit mode
          for I := 0 to Keywords.Count - 1 do begin
            Success := False;
            S := Keywords[I];
            P := Pos('=', S);
            if P > 0 then begin
              Name := AnsiUpperCase(Trim(Copy(S, 1, P - 1)));
              if (Length(Name) <= FITSKeywordLen) and (Name <> KeywordEND) and (Name <> KeywordHierarch) and (Name <> KeywordContinue) and (Name <> KeywordSIMPLE) then begin
                Value := Copy(S, P + 1, MaxInt);
                if (Name = '') or (Name = KeywordComment) or (Name = KeywordHistory) then begin
                  if AddCommentLikeKeyword(FITSfile, Name, Value, True) then begin
                    WriteLn('Added ', Name, ' ', TrimRight(Value));
                    Success := True;
                  end;
                end
                else begin
                  if SetKeywordValue(FITSfile, Name, Value, True, '', True) then begin
                    TempValue := '';
                    if GetKeywordValue(FITSfile, Name, TempValue, False, False) < 0 then begin
                      if (Value <> '') or (TempValue <> '') then
                        FileError('Internal Error: setting value of keyword ' + Name + ' failed.');
                    end;
                    WriteLn('Keyword "', Name, '" set to "', TrimRight(TempValue) + '"');
                    Success := True;
                  end;
                end;
              end;
              if not Success then
                WriteLn('**** Keyword "', Name, '" cannot be set');
            end
            else begin
              WriteLn('**** No value specified for keyword "', AnsiUpperCase(S), '"');
            end;
          end;
          WriteLn;
        end;
      end
      else begin
        if not (FihedMode in [fihedTAB, fihedCSV]) then begin
          WriteLn('**** Not a FITS file');
          WriteLn;
        end;
      end;
    finally
      CloseFile(FITSFile);
    end;
  finally
    FileMode := FileModeSaved;
  end;
end;

type
  TFileEnumClass = class(TObject)
    class function FileEnumProc(const Directory: string; const F: TSearchRec): Boolean; 
  end;  

var
  FileList: TStringListNaturalSort;

class function TFileEnumClass.FileEnumProc(const Directory: string; const F: TSearchRec): Boolean;
begin
  FileList.Add(Directory + F.Name);
  Result := True;
end;

procedure ProcessInput(const FileMasks: array of string; Keywords: TStrings; FihedMode: TFihedMode);
var
  I, N: Integer;
begin
  if FihedMode in [fihedTAB, fihedCSV] then begin
    Write('File');
    for I := 0 to Keywords.Count - 1 do begin
      Write(Delimiter[FihedMode = fihedTAB]);
      Write(Keywords[I]);
    end;
    WriteLn;
  end;
  try
    for N := 0 to Length(FileMasks) - 1 do begin  
      FileList.Clear;
      FileEnum(FileMasks[N], faArchive, False, TFileEnumClass.FileEnumProc);
      FileList.NaturalSort;
      for I := 0 to FileList.Count - 1 do
        ProcessFile(FileList[I], Keywords, FihedMode);
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

procedure ParameterConflict;
begin
  WriteLn('**** Error: ' + defFihedfSwitchChar + defFihedfSwitchChar + 'CSV, ' +
                           defFihedfSwitchChar + defFihedfSwitchChar + 'TAB, and ' +
                           defFihedfSwitchChar + defFihedfSwitchChar + 'SET are mutually exclusive');
  Halt(1);
end;

var
  InputFileMasks: array of string;
  Keywords: TStringList;
  PrintVer: Boolean;
  S: string;
  ParamN: Integer;
  FihedMode: TFihedMode;

begin
  FileMode := fmOpenRead + fmShareDenyNone;

  // Command-line options in this program are distinguished by DOUBLE slashes: i.e //V or //TAB
  // SINGLE slash is used to distinguish FITS keyword

  PrintVer := (CmdObj.CmdLine.IsCmdOption(defFihedfSwitchChar + 'V') or CmdObj.CmdLine.IsCmdOption(defFihedfSwitchChar + 'version'));
  if PrintVer then PrintVersion;
   
  if (CmdObj.CmdLine.IsCmdOption(defFihedfSwitchChar + '?') or
      CmdObj.CmdLine.IsCmdOption(defFihedfSwitchChar + 'H') or
      CmdObj.CmdLine.IsCmdOption(defFihedfSwitchChar + 'help')) then
  begin
    PrintHelp;
    Halt(1);
  end;

  if (CmdObj.CmdLine.FileCount < 1) then begin
    if not PrintVer then begin
      WriteLn;
      WriteLn('Use');
      WriteLn(AnsiUpperCase(ExtractFileName(ParamStr(0))) + ' ' + defFihedfSwitchChar + defFihedfSwitchChar + 'V to print version');
      WriteLn(AnsiUpperCase(ExtractFileName(ParamStr(0))) + ' ' + defFihedfSwitchChar + defFihedfSwitchChar + '? to print help');
    end;
    Halt(1);
  end;

  InputFileMasks := nil;
  FihedMode := fihedNONE;

  Keywords := TStringList.Create;
  try
    for ParamN := 1 to CmdObj.CmdLine.ParamCount do begin
      S := CmdObj.CmdLine.ParamStr(ParamN);
      if CmdObj.CmdLine.FirstCharIsSwitch(S) then begin
        Delete(S, 1, 1);
        if CmdObj.CmdLine.FirstCharIsSwitch(S) then begin
          if CmdObj.CmdLine.ParamIsKey(S, 'V') or CmdObj.CmdLine.ParamIsKey(S, 'version') then begin
            // nothing: already processed.
          end
          else
          if CmdObj.CmdLine.ParamIsKey(S, 'CSV') then begin
            if FihedMode = fihedNONE then
              FihedMode := fihedCSV
            else
              ParameterConflict;
          end
          else
          if CmdObj.CmdLine.ParamIsKey(S, 'TAB') then begin
            if FihedMode = fihedNONE then
              FihedMode := fihedTAB
            else
              ParameterConflict;
          end
          else
          if CmdObj.CmdLine.ParamIsKey(S, 'SET') then begin
            if FihedMode = fihedNONE then
              FihedMode := fihedSET
            else
              ParameterConflict;
          end
          else begin
            WriteLn('**** Invalid command-line parameter: ' + defFihedfSwitchChar + S);
            Halt(1);
          end;
        end
        else begin
          // There can be multiply /comment or /hystory parameters... (also several identical columns in CSV or TAB modes)
          // Empty keyword is allowed!
          Keywords.Add(TrimRight(S));
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
      ProcessInput(InputFileMasks, Keywords, FihedMode);
    finally
      FreeAndNil(FileList);
    end;
  finally
    FreeAndNil(Keywords);
  end;
end.
