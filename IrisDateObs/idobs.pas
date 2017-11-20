{$APPTYPE CONSOLE}

program IDOBS;

uses Windows, SysUtils, Classes, CmdObj, CmdObjStdSwitches, EnumFiles, StringListNaturalSort, FITSUtils, FITSTimeUtils;

procedure PrintHelp;
begin
  WriteLn('Calculate mean DATE-OBS  Maksym Pyatnytskyy  2017');
  WriteLn('Version 2017.11.17.01');
  WriteLn;
  WriteLn('Date/Time is extracted from DATE-OBS first,');
  WriteLn('if DATE-OBS does contain time part, time is extracted from it,');
  WriteLn('otherwise TIME-OBS is used.');
  WriteLn('If TIME-OBS is missing, UT-START (IRIS-specific keyword) is searched.');
  WriteLn;
  WriteLn('Usage:');
  WriteLn(ExtractFileName(ParamStr(0)), ' input_file_mask[.fit] [/E] [/P=filename]');
  WriteLn;
  WriteLn('Where:');
  WriteLn('  input_file_mask    mask for input files to be processed');
  WriteLn('  /E                 correct time by exposure');
  WriteLn('  /P=filename        print FIHED command for filename (stacked image)');
end;

procedure FileError(S: string);
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
  
procedure ProcessInput(const FileMask: string; PFH2file: string; CorrectByExposure: Boolean);
var
  I: Integer;
  P: Integer;
  FileName: string;
  FITSFile: FITSRecordFile;
  DateObsStr: string;
  TimeObsStr: string;
  ExpTimeStr: string;
  ExpTime: Double;
  TempExpTime: Double;
  ExposureKeywordExists: Boolean;
  ErrorPos: Integer;
  DateTimeObs: TDateTime;
  N: Integer;
  SumDate: Double;
  SumExp: Double;
  TimeObsKeyUsed: Boolean;
  UtStartKeyUsed: Boolean;  
begin
  try
    SumExp := 0;
    SumDate := 0;
    N := 0;
    FileEnum(FileMask, faArchive, False, TFileEnumClass.FileEnumProc);
    FileList.NaturalSort;
    for I := 0 to FileList.Count - 1 do begin
      FileName := ExtractFileName(FileList[I]);
      if PFH2file <> ''then Write('REM ');
      Write('File:'^I, FileName);
      AssignFile(FITSFile, FileList[I]);
      Reset(FITSFile);
      try
        DateObsStr := '';
        TimeObsStr := '';
        ExpTimeStr := '';
        ExpTime := 0;
        TimeObsKeyUsed := False;
        UtStartKeyUsed := False;
        if (GetKeywordValue(FITSFile, 'DATE-OBS', DateObsStr, True, True) < 0) or (DateObsStr = '') then
          FileError('DATE-OBS keyword is not found or is empty.');
        DateObsStr := StripQuotes(DateObsStr);
        P := Pos('T', DateObsStr);
        if P <> 0 then begin
          TimeObsStr := Copy(DateObsStr, P + 1, MaxInt);
          DateObsStr := Copy(DateObsStr, 1, P - 1);
        end;
        if TimeObsStr = '' then begin
          // There is no time part in DATE-OBS.
          // Try to get time from TIME-OBS
          TimeObsKeyUsed := (GetKeywordValue(FITSFile, 'TIME-OBS', TimeObsStr, True, True) >= 0) and (TimeObsStr <> '');
          if not TimeObsKeyUsed then begin
            // There is no TIME-OBS...
            // Try to get time from UT-START (IRIS-specific)
            UtStartKeyUsed := (GetKeywordValue(FITSFile, 'UT-START', TimeObsStr, True, True) >= 0) and (TimeObsStr <> '');
            if not UtStartKeyUsed then
              FileError('DATE-OBS value does not contain time part and there is no TIME-OBS nor UT-START keywords.');
          end;
          TimeObsStr := StripQuotes(TimeObsStr);
        end;

        ExposureKeywordExists := (GetKeywordValue(FITSFile, 'EXPTIME', ExpTimeStr, True, True) >= 0) or (GetKeywordValue(FITSFile, 'EXPOSURE', ExpTimeStr, True, True) >= 0);
        if (ExpTimeStr <> '') then begin
          Val(ExpTimeStr, ExpTime, ErrorPos);
          if (ErrorPos <> 0) or (ExpTime < 0) then
            FileError('EXPTIME/EXPOSURE keyword does exist however it contains invalid value: ' + ExpTimeStr);
        end;
        SumExp := SumExp + ExpTime;

        TempExpTime := 0;
        if CorrectByExposure then TempExpTime := ExpTime;
        if not MakeDateObsFromIrisDate(DateTimeObs, DateObsStr, TimeObsStr, TempExpTime) then
          FileError('Cannot determine date/time of observation. Date part = ' + DateObsStr + '; Time part = ' + TimeObsStr);
        Write(^I, FormatDateTime('"Date:'^I'"YYYY-MM-DD" "hh:nn:ss', DateTimeObs));
        Write(^I'Exposure: '^I, ExpTime:10:1);
        if (ExpTime <> 0) and CorrectByExposure then begin
          Write(^I'[Fixed by EXPTIME');
          if UtStartKeyUsed then 
            Write('. Original UT-START=', TimeObsStr)
          else
            Write('. Original TIME-OBS=', TimeObsStr);
          Write(']');
        end;
        WriteLn;
        
        SumDate := SumDate + DateTimeObs;
        Inc(N);
      finally
        CloseFile(FITSFile);
      end;
    end;
    if N < 1 then begin
      WriteLn('No files found.');
    end
    else begin
      WriteLn;
      SumDate := SumDate / N;
      if PFH2file <> ''then Write('REM ');
      Write(^I'Middle  ', ^I, FormatDateTime('"Date:'^I'"YYYY-MM-DD" "hh:nn:ss', SumDate), ^I'Sum Exp :'^I, SumExp:10:1);
      if PFH2file <> '' then begin
        WriteLn;
        WriteLn;
        if PFH2file <> '' then Write('REM '); WriteLn('To set DATE-OBS in FITS header of a stacked image (mean time),');
        if PFH2file <> '' then Write('REM '); WriteLn('use the following command');
        if PFH2file <> '' then Write('REM '); WriteLn('(parameters UT-START, TIME-OBS, EXPTIME/EXPOSURE will be set to empty values):');
        WriteLn;
        Write('FIHED.EXE //SET /DATE-OBS=', FormatDateTime('YYYY-MM-DD"T"hh:nn:ss', SumDate), 
               ' /UT-START= /TIME-OBS= /EXPTIME= /EXPOSURE=',
               ' /COMMENT="', IntToStr(N), ' individual frames"',
               ' /COMMENT="DATE-OBS is set to mean time"');
        if CorrectByExposure then Write(' /COMMENT="DATE-OBS for each individual frame was corrected by exposure"');
        Write(' /COMMENT="Total exposure = ', SumExp:10:1, '"');
        WriteLn(' ', AnsiQuotedStr(PFH2file, '"'));
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
  InputFileMask: string;
  PFH2file: string;
  CorrectByExposure: Boolean;
  
begin
  FileMode := fmOpenRead;
  if (CmdObj.CmdLine.FileCount <> 1) then begin
    PrintHelp;
    Halt(1);
  end;
  InputFileMask := ExpandFileName(CmdObj.CmdLine.ParamFile(1));
  if ExtractFileExt(InputFileMask) = '' then InputFileMask := InputFileMask + '.fit';
  PFH2file := CmdObj.CmdLine.KeyValue('P=');
  CorrectByExposure := CmdObj.CmdLine.IsCmdOption('E'); 
  FileList := TStringListNaturalSort.Create;
  try
    ProcessInput(InputFileMask, PFH2file, CorrectByExposure);
  finally
    FreeAndNil(FileList);
  end;
end.
