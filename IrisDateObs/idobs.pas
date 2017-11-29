{$APPTYPE CONSOLE}

program IDOBS;

uses Windows, SysUtils, Classes, CmdObj{, CmdObjStdSwitches}, EnumFiles, StringListNaturalSort, FITSUtils, FITSTimeUtils, CommonIni;

procedure PrintVersion;
begin
  WriteLn('Calculate mean DATE-OBS  Maksym Pyatnytskyy  2017');
  WriteLn('Version 2017.11.22.01');
  WriteLn;
end;

procedure PrintHelp;
begin
  WriteLn('Usage:');
  WriteLn(ExtractFileName(ParamStr(0)), ' file_mask1[.fit] [file_mask2[.fit] ...] [/E] [/P=filename]');
  WriteLn;
  WriteLn('Where:');
  WriteLn('  file_maskX   mask of input files to be processed (several masks are allowed)');
  WriteLn('  /E           correct time by exposure');
  WriteLn('  /P=filename  print FIHED command for filename (stacked image)');
  WriteLn('  /V           print version');  
  WriteLn('  /H           print this help and halt');
  WriteLn;
  WriteLn;  
  WriteLn('Date/Time is extracted from DATE-OBS first,');
  WriteLn('if DATE-OBS does contain time part, time is extracted from it,');
  WriteLn('otherwise TIME-OBS is used.');
  WriteLn('If TIME-OBS is missing, UT-START (IRIS-specific keyword) is searched.');
  WriteLn;
  WriteLn;
  WriteLn('Example:');
  WriteLn(ExtractFileName(ParamStr(0)), ' ser1\calib* ser2\calib* ser3\calib* /E');  
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
  
procedure ProcessInput(const FileMasks: array of string; PFH2file: string; CorrectByExposure: Boolean);
var
  I, II: Integer;
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
    
    for II := 0 to Length(FileMasks) - 1 do begin
      WriteLn;
      if PFH2file <> ''then Write('REM ');      
      WriteLn('[', FileMasks[II], ']');
      FileList.Clear;
      FileEnum(FileMasks[II], faArchive, False, TFileEnumClass.FileEnumProc);
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
    end;  
    
    if N < 1 then begin
      WriteLn;
      WriteLn('**** No files found.');
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
  InputFileMasks: array of string;
  PFH2file: string;
  CorrectByExposure: Boolean;
  PrintVer: Boolean;  
  N: Integer;
  I: Integer;
  
begin
  FileMode := fmOpenRead;
  
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

  CorrectByExposure := CmdObj.CmdLine.IsCmdOption('E');   
  PFH2file := CmdObj.CmdLine.KeyValue('P=');
  
  SetLength(InputFileMasks, N);
  for I := 1 to N do begin
    InputFileMasks[I - 1] := ExpandFileName(CmdObj.CmdLine.ParamFile(I));
    if ExtractFileExt(InputFileMasks[I - 1]) = '' then InputFileMasks[I - 1] := ChangeFileExt(InputFileMasks[I - 1], '.fit');
  end;

  FileList := TStringListNaturalSort.Create;
  try
    ProcessInput(InputFileMasks, PFH2file, CorrectByExposure);
  finally
    FreeAndNil(FileList);
  end;
end.
