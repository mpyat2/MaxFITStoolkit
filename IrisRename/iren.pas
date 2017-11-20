{$APPTYPE CONSOLE}

program IREN;

uses Windows, SysUtils, Classes, CmdObj, CmdObjStdSwitches, EnumFiles, StringListNaturalSort;

procedure PrintHelp;
begin
  WriteLn('Rename files accorting to IRIS standard  Maksym Pyatnytskyy  2017');
  WriteLn('Version 2017.11.17.01');
  WriteLn('Usage:');
  WriteLn(ExtractFileName(ParamStr(0)), ' input_file_mask[.fit] generic_name output_dir [/F]');
  WriteLn;
  WriteLn('Where:');
  WriteLn('  input_file_mask    mask for input files to be processed, i.e. IMG*');
  WriteLn('  generic_name       prefix to be used to construct new file name');
  WriteLn('  output_dir         directory for files having new names');
  WriteLn('  /F                 overwrite existing files in output directory');
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
  
procedure ProcessInput(const FileMask: string; const GenericName: string; const OutputDir: string; Overwrite: Boolean);
var
  I: Integer;
  FileName: string;
  FileExt: string;
  NewFileName: string;
begin
  try
    FileEnum(FileMask, faArchive, False, TFileEnumClass.FileEnumProc);
    FileList.NaturalSort;
    for I := 0 to FileList.Count - 1 do begin
      FileName := ExtractFileName(FileList[I]);
      FileExt := ExtractFileExt(FileList[I]);
      NewFileName := OutputDir + GenericName + IntToStr(I + 1) + FileExt;
      WriteLn(FileName, ^I'->'^I, NewFileName);
      if not CopyFile(PChar(FileList[I]), PChar(NewFileName), not Overwrite) then begin
        RaiseLastOSError;
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
  GenericName: string;
  OutputDir: string;
  Overwrite: Boolean;
  
begin
  if (CmdObj.CmdLine.FileCount <> 3) then begin
    PrintHelp;
    Halt(1);
  end;
  InputFileMask := ExpandFileName(CmdObj.CmdLine.ParamFile(1));
  if ExtractFileExt(InputFileMask) = '' then InputFileMask := InputFileMask + '.fit';
  GenericName := CmdObj.CmdLine.ParamFile(2);
  OutputDir := IncludeTrailingPathDelimiter(ExpandFileName(CmdObj.CmdLine.ParamFile(3)));
  Overwrite := CmdObj.CmdLine.IsCmdOption('F');
  FileList := TStringListNaturalSort.Create;
  try
    ProcessInput(InputFileMask, GenericName, OutputDir, Overwrite);
  finally
    FreeAndNil(FileList);
  end;
end.
