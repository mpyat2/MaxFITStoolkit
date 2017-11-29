{$APPTYPE CONSOLE}

program IREN;

uses Windows, SysUtils, Classes, CmdObj{, CmdObjStdSwitches}, EnumFiles, StringListNaturalSort, CommonIni;

procedure PrintVersion;
begin
  WriteLn('Rename files accorting to IRIS standard  Maksym Pyatnytskyy  2017');
  WriteLn('Version 2017.11.22.01');
  WriteLn;
end;

procedure PrintHelp;
begin
  WriteLn('Usage:');
  WriteLn(ExtractFileName(ParamStr(0)), ' in_file_mask1[.fit] [in_file_mask2[.fit] ...] /G=generic_name /O=out_dir [/B=nn] [/F]');
  WriteLn;
  WriteLn('Where:');
  WriteLn('  in_file_maskX   masks of input files to be processed, i.e. IMG*.CR2');
  WriteLn('  generic_name    prefix to be used to construct new file name');
  WriteLn('  out_dir         directory for files having new names');
  WriteLn('  nn              base file number (default = 1); must be >= 0');
  WriteLn('  /F              overwrite existing files in output directory');
  WriteLn('  /V              print version');
  WriteLn('  /H              print this help and halt');
  
  WriteLn;
  WriteLn('Example:');
  WriteLn(ExtractFileName(ParamStr(0)), ' dir1\IMG*.CR2 dir2\IMG*.CR2 dir3\IMG*.CR2 /G=src /O=C:\SKY\ /F /B=1');  
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
  
procedure ProcessInput(const FileMasks: array of string; const GenericName: string; const OutputDir: string; Overwrite: Boolean; BaseNumber: Integer);
var
  I, N: Integer;
  FileNumber: Integer;
  FileName: string;
  FileExt: string;
  NewFileName: string;
begin
  try
    FileNumber := 0;
    for N := 0 to Length(FileMasks) - 1 do begin
      WriteLn;    
      WriteLn('[', FileMasks[N], ']');
      FileList.Clear;
      FileEnum(FileMasks[N], faArchive, False, TFileEnumClass.FileEnumProc);
      FileList.NaturalSort;
      for I := 0 to FileList.Count - 1 do begin
        FileName := ExtractFileName(FileList[I]);
        FileExt := ExtractFileExt(FileList[I]);
        NewFileName := OutputDir + GenericName + IntToStr(FileNumber + BaseNumber) + FileExt;
        WriteLn(FileName, ^I'->'^I, NewFileName);
        if not CopyFile(PChar(FileList[I]), PChar(NewFileName), not Overwrite) then begin
          RaiseLastOSError;
        end;
        Inc(FileNumber);
      end;
    end;  
    if FileNumber < 1 then begin
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
  Overwrite: Boolean;
  BaseNumber: Integer;
  ErrorPos: Integer;
  PrintVer: Boolean;
  S: string;
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

  GenericName := CmdObj.CmdLine.KeyValue('G=');
  if GenericName = '' then begin
    WriteLn('**** Generic name must be specified');
    WriteLn;
    PrintHelp;
    Halt(1);
  end;
  // \/:*?
  if (Pos('\', GenericName) <> 0) or 
     (Pos('/', GenericName) <> 0) or 
     (Pos(':', GenericName) <> 0) or 
     (Pos('*', GenericName) <> 0) or 
     (Pos('?', GenericName) <> 0)
  then begin
    WriteLn('**** Generic name must not contain \/:*?');
    WriteLn;
    PrintHelp;
    Halt(1);
  end;
  
  OutputDir := CmdObj.CmdLine.KeyValue('O=');
  if OutputDir = '' then begin
    WriteLn('**** Output directory must be specified');
    WriteLn;
    PrintHelp;
    Halt(1);
  end;
  OutputDir := IncludeTrailingPathDelimiter(ExpandFileName(OutputDir));

  BaseNumber := 1;
  S := CmdObj.CmdLine.KeyValue('B=');
  if S <> '' then begin
    Val(S, BaseNumber, ErrorPos);
    if (ErrorPos <> 0) or (BaseNumber < 0) then begin
      WriteLn('**** Base filenumber must be >= 0');
      WriteLn;
      PrintHelp;
      Halt(1);
    end;
  end;
  
  Overwrite := CmdObj.CmdLine.IsCmdOption('F');  
  
  SetLength(InputFileMasks, N);
  for I := 1 to N do begin
    InputFileMasks[I - 1] := ExpandFileName(CmdObj.CmdLine.ParamFile(I));
    if ExtractFileExt(InputFileMasks[I - 1]) = '' then InputFileMasks[I - 1] := ChangeFileExt(InputFileMasks[I - 1], '.fit');
  end;

  FileList := TStringListNaturalSort.Create; 
  try
    ProcessInput(InputFileMasks, GenericName, OutputDir, Overwrite, BaseNumber);
  finally
    FreeAndNil(FileList);
  end;
end.
