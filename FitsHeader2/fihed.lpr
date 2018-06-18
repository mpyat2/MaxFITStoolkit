{$APPTYPE CONSOLE}

{$IFDEF FPC} {$MODE DELPHI} {$ENDIF}

program FIHED;

// https://fits.gsfc.nasa.gov/fits_primer.html

uses
  SysUtils, Classes, CmdObj, Version, FITSUtils, EnumFiles,
  StringListNaturalSort, FitsUtilsHelp, CommonIni;
// do not include CmdObjStdSwitches!

{$R *.res}

procedure PrintVersion;
begin
  WriteLn('FITS Header Viewer/Editor  Maksym Pyatnytskyy  2018');
  WriteLn(GetVersionString(ParamStr(0)));
  WriteLn;
end;  
  
procedure FileError(const S: string);
begin
  raise Exception.Create(S);
end;

var
  CSVmode: Boolean;
  TABmode: Boolean;
  SETmode: Boolean;
  Keywords: TStringList;
  FileList: TStringListNaturalSort;  
  
procedure ProcessFile(const FileName: string); 
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
  if not (CSVmode or TABmode) then
    WriteLn('File    = ', QuotedStr(ExtractFilename(FileName)))
  else
    Write(QuotedStr(ExtractFilename(FileName)));

  FileModeSaved := FileMode;
  if SETmode then FileMode := fmOpenReadWrite; // Edit mode
  try
    AssignFile(FITSFile, FileName);
    Reset(FITSFile);
    try
      if IsFITS(FITSFile) then begin
        if not SETmode then begin
          // Print header and exit
          if not (CSVmode or TABmode) then begin
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
              if CSVmode then Write(';') else Write(^I);
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
        if not (CSVmode or TABmode) then begin
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

class function TFileEnumClass.FileEnumProc(const Directory: string; const F: TSearchRec): Boolean;
begin
  FileList.Add(Directory + F.Name);
  Result := True;
end;

procedure ProcessInput(const FileMasks: array of string);
var
  I, N: Integer;
begin
  if (CSVmode or TABmode) and SETmode then begin
    WriteLn('**** //CSV (or //TAB) and //SET are mutually exclusive');
  Halt(1);
  end;
  if (CSVmode or TABmode) then begin
    Write('File');
    for I := 0 to Keywords.Count - 1 do begin
      if CSVmode then Write(';') else Write(^I);
      Write(Keywords[I]);
    end;
    WriteLn;
  end;
  try
    for N := 0 to Length(FileMasks) - 1 do begin  
      FileList.Clear;
      FileEnum(FileMasks[N], faArchive, False, TFileEnumClass.FileEnumProc);
      FileList.NaturalSort;
      for I := 0 to FileList.Count - 1 do begin
        ProcessFile(FileList[I]);
      end;
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
  SwitchChar: Char;
  S: string;  
  ParamN: Integer;

begin
  FileMode := fmOpenRead;
  
  PrintVer := (CmdObj.CmdLine.IsCmdOption('/V') or CmdObj.CmdLine.IsCmdOption('/version'));
  if PrintVer then PrintVersion;
   
  if (CmdObj.CmdLine.IsCmdOption('/?') or CmdObj.CmdLine.IsCmdOption('/H') or CmdObj.CmdLine.IsCmdOption('/help')) then begin
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

  InputFileMasks := nil;
  CSVmode := False;
  TABmode := False;
  SETmode := False;

  Keywords := TStringList.Create;
  try
    for ParamN := 1 to CmdObj.CmdLine.ParamCount do begin
      S := CmdObj.CmdLine.ParamStr(ParamN);
      if CmdObj.CmdLine.FirstCharIsSwitch(S) then begin
        SwitchChar := S[1];
        Delete(S, 1, 1);
        if CmdObj.CmdLine.FirstCharIsSwitch(S) then begin
          if CmdObj.CmdLine.ParamIsKey(S, 'V') or CmdObj.CmdLine.ParamIsKey(S, 'version') then begin
            // nothing: already processed.
          end
          else
          if CmdObj.CmdLine.ParamIsKey(S, 'CSV') then
            CSVmode := True
          else
          if CmdObj.CmdLine.ParamIsKey(S, 'TAB') then
            TABmode := True
          else
          if CmdObj.CmdLine.ParamIsKey(S, 'SET') then
            SETmode := True
          else begin
            WriteLn('**** Invalid command-line parameter: ' + SwitchChar + S);
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
      ProcessInput(InputFileMasks);
    finally
      FreeAndNil(FileList);
    end;
  finally
    FreeAndNil(Keywords);
  end;
end.
