{*****************************************************************************}
{                                                                             }
{ IREN                                                                        }
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

program IREN;

uses
  Windows, SysUtils, Classes, CmdObj{, CmdObjStdSwitches}, Version, EnumFiles,
  FITScompatibility, StringListNaturalSort, FITSUtils, FitsUtilsHelp, CommonIni;

{$R *.res}

procedure PrintVersion;
begin
  WriteLn('Rename files accorting to IRIS standard  Maksym Pyatnytskyy  2017-2021');
  WriteLn(GetVersionString(AnsiUpperCase(ParamStr(0))){$IFDEF WIN64}, ' WIN64'{$ENDIF}, ' ', {$I %DATE%}, ' ', {$I %TIME%});
  WriteLn;
end;

procedure FileError(const S: string);
begin
  raise Exception.Create(S);
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
  
procedure ProcessInput(const FileMasks: array of string; const GenericName: string; const OutputDir: string; Overwrite: Boolean; Recursively: Boolean; ShowSrcDirNames: Boolean; BaseNumber: Integer; const OutputExt: string);
var
  I, N: Integer;
  FileNumber: Integer;
  FileName: string;
  FileExt: string;
  NewFileName: string;
begin
  try
    FileNumber := BaseNumber - 1;
    for N := 0 to Length(FileMasks) - 1 do begin
      WriteLn;    
      WriteLn('[', FileMasks[N], ']');
      FileList.Clear;
      FileEnum(FileMasks[N], faArchive, Recursively, TFileEnumClass.FileEnumProc);
      FileList.NaturalSort;
      for I := 0 to FileList.Count - 1 do begin
{$IFNDEF range_check}{$R+}{$ENDIF}
{$IFNDEF overflow_check}{$Q+}{$ENDIF}
        try
          Inc(FileNumber);
        except
          on E: Exception do
            FileError('Cannot increment FileNumber: ' + E.Message);
        end;
{$IFNDEF range_check}{$R-}{$ENDIF}
{$IFNDEF overflow_check}{$Q-}{$ENDIF}
        FileName := ExtractFileName(FileList[I]);
        if OutputExt = '' then
          FileExt := ExtractFileExt(FileList[I])
        else
          FileExt := OutputExt;
        NewFileName := OutputDir + GenericName + IntToStr(FileNumber) + FileExt;
        if ShowSrcDirNames then
          Write(FileList[I])
        else
          Write(FileName);
        WriteLn(^I'->'^I, NewFileName);
        if not CopyFile(PChar(FileList[I]), PChar(NewFileName), not Overwrite) then
          RaiseLastOSError;
      end;
    end;  
    if FileNumber < BaseNumber then begin
      WriteLn;
      WriteLn('**** No files found');
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
  GenericName: string;
  OutputDir: string;
  OutputExt: string;
  Overwrite: Boolean;
  Recursively: Boolean;
  ShowSrcDirNames: Boolean;
  BaseNumber: Integer;
  PrintVer: Boolean;
  S, S2: string;
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
  OutputExt := '';
  GenericName := '';
  Overwrite := False;
  Recursively := False;
  ShowSrcDirNames := False;
  BaseNumber := 1;

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
      if CmdObj.CmdLine.ExtractParamValue(S, 'G=', GenericName) then begin
        if GenericName <> '' then begin
          // \/:*?
          if (Pos('\', GenericName) <> 0) or
             (Pos('/', GenericName) <> 0) or
             (Pos(':', GenericName) <> 0) or
             (Pos('*', GenericName) <> 0) or
             (Pos('?', GenericName) <> 0) or
             (Pos('<', GenericName) <> 0) or
             (Pos('>', GenericName) <> 0)
          then begin
            WriteLn('**** Generic name must not contain \/:*?<>');
            Halt(1);
          end;
        end;
      end
      else
      if CmdObj.CmdLine.ExtractParamValue(S, 'B=', S2) then begin
        if S2 <> '' then begin
          if not GetInt(S2, BaseNumber) or (BaseNumber < 0) then begin
            WriteLn('**** Base filenumber must be an integer >= 0');
            Halt(1);
          end;
        end;
      end
      else
      if CmdObj.CmdLine.ParamIsKey(S, 'F') then
        Overwrite := True
      else
      if CmdObj.CmdLine.ExtractParamValue(S, 'X=', S2) then begin
        if S2 <> '' then begin
          OutputExt := S2;
          if OutputExt[1] <> '.' then OutputExt := '.' + OutputExt;
        end;
      end
      else
      if CmdObj.CmdLine.ParamIsKey(S, 'S') then
        Recursively := True
      else
      if CmdObj.CmdLine.ParamIsKey(S, 'D') then
        ShowSrcDirNames := True
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

  if OutputDir = '' then begin
    WriteLn('**** Output directory must be specified (by /O=<dir> parameter)');
    Halt(1);
  end;

  if GenericName = '' then begin
    WriteLn('**** Generic name must be specified (by /G=<name> parameter)');
    Halt(1);
  end;

  FileList := TStringListNaturalSort.Create;
  try
    ProcessInput(InputFileMasks, GenericName, OutputDir, Overwrite, Recursively, ShowSrcDirNames, BaseNumber, OutputExt);
  finally
    FreeAndNil(FileList);
  end;
end.
