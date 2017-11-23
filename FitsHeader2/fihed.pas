{$APPTYPE CONSOLE}

program FIHED;

// https://fits.gsfc.nasa.gov/fits_primer.html

uses
  Windows, SysUtils, CmdObj, Classes, FITSUtils, EnumFiles, StringListNaturalSort;
// do not include CmdObjStdSwitches!

procedure PrintVersion;
begin
  WriteLn('FITS Header Viewer/Editor  Maksym Pyatnytskyy  2017');
  WriteLn('Version 2017.11.22.01');
  WriteLn;
end;  
  
procedure PrintHelp;
begin
  WriteLn('Usage:');
  WriteLn(ExtractFileName(ParamStr(0)), ' file_mask1[.fit] [file_mask2[.fit] ...] [//CSV|//TAB] [/keyword1 [/keyword2 ...]]');
  WriteLn('or');  
  WriteLn(ExtractFileName(ParamStr(0)), ' file_mask1[.fit] [file_mask2[.fit] ...] [//SET] [/keyword1=value [/keyword2=value ...]]');  
  WriteLn;
  WriteLn('//CSV       prints header values as CSV table.');
  WriteLn('            COMMENT, HISTORY and similar keywords without ''=''');
  WriteLn('            are ignored in this mode.');  
  WriteLn('            When /CSV parameter exists, at least one keyword must be specified');  
  WriteLn;
  WriteLn('//SET       edit mode');
  WriteLn;  
  WriteLn('//CSV and //SET or //TAB and //SET parameters are mutually exclusive');
  WriteLn('If both //CSV and //TAB specified, //CSV is used');
  WriteLn;
  WriteLn('//V         print version');  
  WriteLn('//H         print this help and halt');
end;
 
procedure FileError(S: string);
begin
  raise Exception.Create(S);
end;

var
  Keywords: TStringList;
  CSVmode: Boolean;
  TABmode: Boolean;
  SETmode: Boolean;
  FileList: TStringListNaturalSort;  
  
procedure ProcessFile(const FileName: string); 
var
  Header: TStringList;
  I: Integer;
  FITSFile: FITSRecordFile;
  Name: string;
  Value: string;
  S: string;
  P: Integer;
  Success: Boolean;
  FileModeSaved: Integer;
begin
  if not (CSVmode or TABmode) then
    WriteLn('File    = ', QuotedStr(ExtractFilename(FileName)))
  else
    Write(QuotedStr(ExtractFilename(FileName)));
    
  if IsFITS(FileName) then begin
    if not (CSVmode or TABmode) then begin
      if not SETmode then begin
        // Print header and exit
        Header := TStringList.Create;
        try
          GetHeader(FileName, Header);
          for I := 0 to Header.Count - 1 do begin
            if (Keywords.Count < 1) or (Keywords.IndexOf(TrimRight(Copy(Header[I], 1, FITSKeywordLen))) >= 0) then
              WriteLn(Header[I]);
          end;
        finally
          FreeAndNil(Header);
        end;    
        WriteLn;
      end
      else begin
        // Edit mode
        FileModeSaved := FileMode;
        FileMode := fmOpenReadWrite;
        try
          AssignFile(FITSFile, FileName);
          Reset(FITSFile);
          try
            for I := 0 to Keywords.Count - 1 do begin
              Success := False;
              S := Keywords[I];
              P := Pos('=', S);
              if P > 0 then begin
                Name := AnsiUpperCase(Trim(Copy(S, 1, P - 1)));
                if (Length(Name) <= FITSKeywordLen) and (Name <> KeywordEND) and (Name <> KeywordHierarch) then begin
                  Value := Copy(S, P + 1, MaxInt);
                  if (Name = '') or (Name = KeywordComment) or (Name = KeywordHistory) then begin
                    if AddCommentLikeKeyword(FITSfile, FileName, Name, Value, True) then begin
                      WriteLn('Added ', Name, ' ', Value);
                      Success := True;
                    end;  
                  end
                  else begin
                    if SetKeywordValue(FITSfile, FileName, Name, Value, True, '', True) then begin
                      if (GetKeywordValue(FITSfile, Name, Value, False, False) < 0) and (Value <> '') then  
                        FileError('Cannot get value of keyword ' + Name);
                      WriteLn('Keyword ', Name, ' set to ', Value);
                      Success := True;
                    end;
                  end;  
                end;
                if not Success then
                  WriteLn('**** Keyword ', Name, ' cannot be set');
              end;
            end;
          finally
            CloseFile(FITSfile);
          end;          
          WriteLn;
        finally
          FileMode := FileModeSaved;
        end;        
      end;
    end
    else begin
      // Table-like output
      AssignFile(FITSFile, FileName);
      Reset(FITSFile);
      try
        for I := 0 to Keywords.Count - 1 do begin
          GetKeywordValue(FITSFile, Keywords[I], Value, True, True);
          if CSVmode then Write(';') else Write(^I);
          Write(Value);
        end;
        WriteLn;
      finally
        CloseFile(FITSFile);
      end;
    end;
  end
  else begin
    if not (CSVmode or TABmode) then begin
      WriteLn('**** Not a FITS file');
      WriteLn;
    end;
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
    WriteLn('**** //CSV and //SET are mutually exclusive');
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
  ParamList: TStringList;
  InputFileMasks: array of string;
  PrintVer: Boolean;  
  S: string;  
  N: Integer;
  I: Integer;

begin
  FileMode := fmOpenRead;
  
  PrintVer := (CmdObj.CmdLine.IsCmdOption('/V') or CmdObj.CmdLine.IsCmdOption('/version'));
  if PrintVer then PrintVersion;
   
  if (CmdObj.CmdLine.IsCmdOption('/?') or CmdObj.CmdLine.IsCmdOption('/H') or CmdObj.CmdLine.IsCmdOption('/help')) then begin
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

  SetLength(InputFileMasks, N);
  for I := 1 to N do begin
    InputFileMasks[I - 1] := ExpandFileName(CmdObj.CmdLine.ParamFile(I));
    if ExtractFileExt(InputFileMasks[I - 1]) = '' then InputFileMasks[I - 1] := ChangeFileExt(InputFileMasks[I - 1], '.fit');
  end;

  ParamList := TStringList.Create;
  try
    Keywords := TStringList.Create;
    try
      for I := 1 to CmdObj.CmdLine.ParamCount do begin
        S := CmdObj.CmdLine.ParamStr(I);
        if CmdObj.CmdLine.FirstCharIsSwitch(S) then begin
            Delete(S, 1, 1);
            // There can be multiply /comment or /hystory parameters... (also several identical columns in CSV or TAB modes)
            {if ParamList.IndexOf(S) < 0 then }ParamList.Add(S);
        end;
      end;
      for I := 0 to ParamList.Count - 1 do begin
        S := TrimRight(ParamList[I]);
        // Empty keyword is allowed!
        if not CmdObj.CmdLine.FirstCharIsSwitch(S) then begin
          {if Keywords.IndexOf(S) < 0 then }Keywords.Add(S);
        end;  
      end;
      CSVmode := ParamList.IndexOf('/CSV') >= 0;
      TABmode := ParamList.IndexOf('/TAB') >= 0;
      SETmode := ParamList.IndexOf('/SET') >= 0;
      FileList := TStringListNaturalSort.Create;
      try    
        ProcessInput(InputFileMasks);
      finally
        FreeAndNil(FileList);
      end;
    finally
      FreeAndNil(Keywords);
    end;  
  finally
    FreeAndNil(ParamList);
  end;  
end.
