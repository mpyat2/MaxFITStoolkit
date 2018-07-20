{*****************************************************************************}
{                                                                             }
{                                                                             }
{ (c) 2018 Maksym Pyatnytskyy                                                 }
{                                                                             }
{ This program is distributed                                                 }
{ WITHOUT ANY WARRANTY; without even the implied warranty of                  }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                        }
{                                                                             }
{*****************************************************************************}

{$APPTYPE CONSOLE}
{$MODE DELPHI}
{$INCLUDE FITSUtils.inc}

program Template;

uses
  SysUtils, CmdObj{, CmdObjStdSwitches}, Version, EnumFiles, FITScompatibility,
  FITSUtils, StringListNaturalSort, FitsUtilsHelp, CommonIni;

//{$R *.res} // include version info!

procedure PrintVersion;
begin
  WriteLn(ChangeFileExt({$I %FILE%}, ''), '  Maksym Pyatnytskyy  2018');
  WriteLn(GetVersionString(AnsiUpperCase(ParamStr(0))), ' ', {$I %FPCTARGETOS%}, ' ', {$I %DATE%}, ' ', {$I %TIME%});
  WriteLn;
end;

var
  FileList: TStringListNaturalSort;

procedure FileError(const S: string);
begin
  raise Exception.Create(S);
end;

procedure ProcessFITSfile(var FITSfile: FITSRecordfile);
begin
  WriteLn;
  WriteLn(FITSRecordTypeFileName(FITSfile));
end;

procedure ProcessFile(const FileName: string);
var
  FITSfile: FITSRecordFile;
begin
  Write('Processing ', ExtractFileName(FileName));
  AssignFile(FITSfile, FileName);
  Reset(FITSfile);
  try
    if not IsFits(FITSfile) then
      FileError('Not a valid FITS file: ' + AnsiQuotedStr(FileName, '"'));
    ProcessFITSfile(FITSfile);
  finally
    CloseFile(FITSfile);
  end;
  WriteLn('done.');
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
        ProcessFile(FileList[I]);
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
    ProcessInput(InputFileMasks);
  finally
    FreeAndNil(FileList);
  end;

end.

