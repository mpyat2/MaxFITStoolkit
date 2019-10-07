{*****************************************************************************}
{                                                                             }
{ IDOBS                                                                       }
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

program IDOBS;

uses
  SysUtils, Classes, CmdObj{, CmdObjStdSwitches}, Version, EnumFiles,
  FITScompatibility, MiscUtils, StringListNaturalSort, FITSUtils, FitsUtilsHelp,
  CommonIni;

{$R *.res}

{$INCLUDE PrintVersion.inc}

procedure FileError(const S: string);
begin
  raise Exception.Create(S);
end;

var
  FileList: TStringListNaturalSort;  

type
  TFileEnumClass = class(TObject)
    class function FileEnumProc(const Directory: string; const F: TSearchRec): Boolean; 
  end;  

class function TFileEnumClass.FileEnumProc(const Directory: string; const F: TSearchRec): Boolean;
begin
  FileList.Add(Directory + F.Name); 
  Result := True;
end;
  
procedure ProcessInput(const FileMasks: array of string; CorrectByExposure: Boolean; IgnoreTimeError: Boolean);
var
  I, II: Integer;
  FileName: string;
  FITSFile: FITSRecordFile;
  DateTimeObs, DateTimeObs0: TDateTime;
  ExpTime: Double;
  N: Integer;
  SumDate: Double;
  SumExp: Double;
  TimeError: Boolean;
begin
  try
    SumExp := 0;
    SumDate := 0;
    N := 0;
    
    for II := 0 to Length(FileMasks) - 1 do begin
      WriteLn;
      WriteLn('[', FileMasks[II], ']');
      FileList.Clear;
      FileEnum(FileMasks[II], faArchive, False, TFileEnumClass.FileEnumProc);
      FileList.NaturalSort;
      for I := 0 to FileList.Count - 1 do begin
        FileName := ExtractFileName(FileList[I]);
        Write('File:'^I, FileName);
        AssignFile(FITSFile, FileList[I]);
        Reset(FITSFile);
        try
          if not IsFits(FITSfile) then
            FileError('Not a FITS file: ' + AnsiQuotedStr(FileList[I], '"'));
          TimeError := False;
          try
            DateTimeObs := GetDateObs(FITSfile);
            ExpTime := GetExposureTime(FITSFile);
          except
            on E: EFITSerror do begin
              if not IgnoreTimeError then
                raise
              else
                TimeError := True;
            end;
          end;
          if not TimeError then begin
            DateTimeObs0 := DateTimeObs;
            SumExp := SumExp + ExpTime;
            if CorrectByExposure and (ExpTime > 0) then begin
              DateTimeObs := DateTimeObs + ExpTime / (24.0*60.0*60.0) / 2.0;
            end;
            Write(^I, FormatDateTime('"Date:'^I'"YYYY-MM-DD" "hh:nn:ss', DateTimeObs));
            Write(^I'Exposure: '^I, ExpTime:10:2);
            if (ExpTime <> 0) and CorrectByExposure then begin
              Write(^I'[Fixed by EXPTIME. Original Time: ', FormatDateTime('hh:nn:ss', DateTimeObs0), ']');
            end;
            WriteLn;
            SumDate := SumDate + DateTimeObs;
            Inc(N);
          end
          else begin
            WriteLn(^I'Cannot get DATE-OBS and/or EXPTIME. File is ignored.');
          end;
        finally
          CloseFile(FITSFile);
        end;
      end;
    end;  
    
    if N < 1 then begin
      PrintWarning(^M^J'**** No files found.'^M^J);
    end
    else begin
      WriteLn;
      SumDate := SumDate / N;
      WriteLn(^I'Middle  ', ^I, FormatDateTime('"Date:'^I'"YYYY-MM-DD" "hh:nn:ss', SumDate), ^I'Sum Exp :'^I, SumExp:10:2);
    end;
  except
    on E: Exception do begin
      PrintError(^M^J'**** Error:'^M^J + E.Message + ^M^J);
      Halt(1);
    end;
  end;
end;

var
  InputFileMasks: array of string;
  CorrectByExposure: Boolean;
  IgnoreTimeError: Boolean;
  PrintVer: Boolean;
  S: string;
  ParamN: Integer;
  
begin
  FileMode := fmOpenRead;
  
  PrintVer := (CmdObj.CmdLine.IsCmdOption('V') or CmdObj.CmdLine.IsCmdOption('version'));
  if PrintVer then PrintVersion('Calculate mean DATE-OBS');
   
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
  CorrectByExposure := False;
  IgnoreTimeError := False;

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
      if CmdObj.CmdLine.ParamIsKey(S, 'NoTimeError') then
        IgnoreTimeError := True
      else
      if CmdObj.CmdLine.ParamIsKey(S, 'E') then
        CorrectByExposure := True
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

  FileList := TStringListNaturalSort.Create;
  try
    ProcessInput(InputFileMasks, CorrectByExposure, IgnoreTimeError);
  finally
    FreeAndNil(FileList);
  end;
end.
