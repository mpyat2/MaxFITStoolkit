{$APPTYPE CONSOLE}

{$IFDEF FPC} {$MODE DELPHI} {$ENDIF}

program IDOBS;

uses Windows, SysUtils, Classes, CmdObj{, CmdObjStdSwitches}, Version, EnumFiles, StringListNaturalSort, FITSUtils, FitsUtilsHelp, CommonIni;

{$R *.res}

procedure PrintVersion;
begin
  WriteLn('Calculate mean DATE-OBS  Maksym Pyatnytskyy  2017');
  WriteLn(GetVersionString(ParamStr(0)));
  WriteLn;
end;

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
  
procedure ProcessInput(const FileMasks: array of string; CorrectByExposure: Boolean);
var
  I, II: Integer;
  FileName: string;
  FITSFile: FITSRecordFile;
  DateTimeObs, DateTimeObs0: TDateTime;
  ExpTime: Double;
  N: Integer;
  SumDate: Double;
  SumExp: Double;
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

          DateTimeObs := GetDateObs(FITSfile);
          DateTimeObs0 := DateTimeObs;
          ExpTime := GetExposureTime(FITSFile);
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
        finally
          CloseFile(FITSFile);
        end;
      end;
    end;  
    
    if N < 1 then begin
      WriteLn;
      WriteLn('**** No files found.');
    end
    else begin
      WriteLn;
      SumDate := SumDate / N;
      Write(^I'Middle  ', ^I, FormatDateTime('"Date:'^I'"YYYY-MM-DD" "hh:nn:ss', SumDate), ^I'Sum Exp :'^I, SumExp:10:2);
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
  CorrectByExposure: Boolean;
  PrintVer: Boolean;
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
  CorrectByExposure := False;

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
      if CmdObj.CmdLine.ParamIsKey(S, 'E') then
        CorrectByExposure := True
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
    ProcessInput(InputFileMasks, CorrectByExposure);
  finally
    FreeAndNil(FileList);
  end;
end.
