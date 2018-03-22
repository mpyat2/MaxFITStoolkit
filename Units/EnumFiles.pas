{*****************************************************************************}
{                                                                             }
{ EnumFiles                                                                   }
{ (c) 2000 Maksym Pyatnytskyy                                                 }
{                                                                             }
{*****************************************************************************}

{$IFDEF FPC} {$MODE DELPHI} {$ENDIF}

unit EnumFiles;

interface

uses
  Windows, SysUtils;

type
  TFileEnumProc = function(const Directory: string; const F: TSearchRec): Boolean of object;

type
  EFileEnumError = class(Exception)
  public
    ErrorCode: Integer;
  end;

function FileEnum(const Mask: string; Attr: Integer; SubDirs: Boolean; FileEnumProc: TFileEnumProc): Boolean;

implementation

resourcestring
  sErrFindFailed = 'FindFirst/FindNext failed. Error code: %d';


procedure CheckFoundResult(R: Integer);
var
  E: EFileEnumError;
begin
  if (R <> 0) and (R <> Windows.ERROR_NO_MORE_FILES) and (R <> Windows.ERROR_FILE_NOT_FOUND) then begin
    E := EFileEnumError.CreateFmt(sErrFindFailed, [R]);
    E.ErrorCode := R;
    raise E;
  end;
end;


function FileEnum(const Mask: string; Attr: Integer; SubDirs: Boolean; FileEnumProc: TFileEnumProc): Boolean;

  function DoFileEnumProc(const Directory: string; const F: TSearchRec): Boolean;
  begin
    if Assigned(FileEnumProc) then
      Result := FileEnumProc(Directory, F)
    else
      Result := False;
  end;

var
  TempRootDir: string;
  TempMask: string;
  F: TSearchRec;
  FoundResult: Integer;

begin
  Result := True;
  if Mask = '' then
    TempMask := '.' + SysUtils.PathDelim
  else
    TempMask := Mask;
  TempRootDir := ExtractFilePath(TempMask);
  if TempRootDir <> '' then
    TempRootDir := IncludeTrailingPathDelimiter(TempRootDir);
  TempMask := ExtractFileName(TempMask);
  if TempMask = '' then TempMask := '*.*';

  // Process files in the current directory (including subdirectories if faDirectory is in Attr)
  FoundResult := FindFirst(TempRootDir + TempMask, Attr, F);
  try
    while FoundResult = 0 do begin
      Result := DoFileEnumProc(TempRootDir, F);
      if not Result then Exit;
      FoundResult := FindNext(F);
    end;
    CheckFoundResult(FoundResult);
  finally
    FindClose(F);
  end;

  if SubDirs then begin
    // get list of subdirectories and process them
    FoundResult := FindFirst(TempRootDir + '*.*', Attr or faDirectory, F);
    try
      while FoundResult = 0 do begin
        if ((F.Attr and faDirectory) = faDirectory) and (F.Name <> '.') and (F.Name <> '..') then begin
          Result := FileEnum(TempRootDir + F.Name + SysUtils.PathDelim + TempMask, Attr, SubDirs, FileEnumProc);
          if not Result then Exit;
        end;  
        FoundResult := FindNext(F);
      end;
      CheckFoundResult(FoundResult);
    finally
      FindClose(F);
    end;
  end;
end;

end.


