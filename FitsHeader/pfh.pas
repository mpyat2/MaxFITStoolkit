{$APPTYPE CONSOLE}

program PFH;

// https://fits.gsfc.nasa.gov/fits_primer.html

uses
  Windows, SysUtils, CmdObj, CmdObjStdSwitches, Classes;

procedure PrintHelp;
begin
  WriteLn('Print FITS Header  Maksym Pyatnytskyy  2017');
  WriteLn('Version 2017.11.17.01');
  WriteLn('Usage:');
  WriteLn(ExtractFileName(ParamStr(0)), ' input_filename[.fit] [/keyword1 [/keyword2 ...]]');
end;
  
procedure FileError(const S: string);
begin
  raise Exception.Create(S);
end;

function PadCh(const S: string; L: Integer; Ch: Char): string;
begin
  Result := S;
  while Length(Result) < L do Result := Result + Ch;
end;

const recordEND     = 'END                                                                             ';
const HeaderRecordLen = Length(recordEND);
const FITSKeywordLen = 8;

type
  BufType = array[1..HeaderRecordLen] of Char;

var
  InFileNamePrintable: string = '';
  
procedure ProcessInput(const InFile: string; const ParamList: TStrings); 
var
  InF: File of char;
  Buf: BufType;
  NReadWrite: Integer;
begin
  AssignFile(InF, InFile);
  try
    Reset(InF);
  except
    on E: Exception do begin
      WriteLn('**** Error while opening input file ', AnsiQuotedStr(InFile, '"'));
      WriteLn(E.Message);
      Halt(1);
    end;
  end;
  try
    while not EOF(InF) do begin
      BlockRead(InF, Buf, SizeOf(Buf), NReadWrite);
      if NReadWrite <> HeaderRecordLen then FileError('Invalid FITS record in file ' + AnsiQuotedStr(InFile, '"'));
      if (ParamList.Count < 1) or (ParamList.IndexOf(Copy(Buf, 1, FITSKeywordLen)) >= 0) then
        WriteLn(TrimRight(Buf));
      if Buf = recordEND then Break;
    end;
    CloseFile(InF);
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
  InputFileName: string;
  ParamList: TStringList;
  S: string;
  I: Integer;

begin
  if (CmdObj.CmdLine.FileCount <> 1) then begin
    PrintHelp;
    Halt(1);
  end;
  FileMode := fmOpenRead;  
  InputFileName := ExpandFileName(CmdObj.CmdLine.ParamFile(1));
  if ExtractFileExt(InputFileName) = '' then InputFileName := InputFileName + '.fit';
  InFileNamePrintable := ExtractFileName(InputFileName);
  ParamList := TStringList.Create;
  try
    for I := 1 to CmdObj.CmdLine.ParamCount do begin
      S := CmdObj.CmdLine.ParamStr(I);
      if CmdObj.CmdLine.FirstCharIsSwitch(S) then begin
        Delete(S, 1, 1);
        // 'Empty' keyword is allowed: according to FITS standard!
        S := AnsiUpperCase(PadCh(Trim(S), FITSKeywordLen, ' '));
        if Length(S) > FITSKeywordLen then begin
          WriteLn('**** Warning: parameter "', S, '" is ignored: according to FITS standard length must be less or equal ', FITSKeywordLen);
        end;
        if ParamList.IndexOf(S) < 0 then ParamList.Add(S);
      end;
    end;
  
    WriteLn('File    = ', QuotedStr(InFileNamePrintable));
    ProcessInput(InputFileName, ParamList);
    WriteLn;
  finally
    FreeAndNil(ParamList);
  end;  
end.
