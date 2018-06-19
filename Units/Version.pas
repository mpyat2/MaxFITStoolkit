{*****************************************************************************}
{                                                                             }
{ CmdObj                                                                      }
{ (c) 2000 Maksym Pyatnytskyy                                                 }
{                                                                             }
{*****************************************************************************}

{$MODE DELPHI}

unit Version;

interface

uses Windows, SysUtils;

procedure GetVersionValues(const ModuleName: String; out V1, V2, V3, V4: Word);
function GetVersionString(const ModuleName: String): String;
function GetVersionString2(const ModuleName: String): String;

implementation

procedure GetVersionValues(const ModuleName: String; out V1, V2, V3, V4: Word);
var
  VerInfoSize:  DWORD;
  VerInfo:      Pointer;
  VerValueSize: DWORD;
  VerValue:     PVSFixedFileInfo;
  Dummy:        DWORD;
begin
  V1 := 0;
  V2 := 0;
  V3 := 0;
  V4 := 0;
  VerInfoSize := GetFileVersionInfoSize(PChar(ModuleName), Dummy);
  if VerInfoSize = 0 then Exit;
  GetMem(VerInfo, VerInfoSize);
  try
    if not GetFileVersionInfo(PChar(ModuleName), 0, VerInfoSize, VerInfo) then Exit;
    if not VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize) then Exit;
    with VerValue^ do begin
      V1 := dwFileVersionMS shr 16;
      V2 := dwFileVersionMS and $FFFF;
      V3 := dwFileVersionLS shr 16;
      V4 := dwFileVersionLS and $FFFF;
   end;
 finally
   FreeMem(VerInfo, VerInfoSize);
 end;
end;

function GetVersionString(const ModuleName: String): String;
var
  V1, V2, V3, V4: Word;
begin
  Result := '';
  GetVersionValues(ModuleName, V1, V2, V3, V4);
  Result := ExtractFileName(ModuleName) + ' Version ' + IntToStr(V1) + '.' + IntToStr(V2) + '.' + IntToStr(V3) + ' (Build ' + IntToStr(V4) + ')';
end;

function GetVersionString2(const ModuleName: String): String;
var
  V1, V2, V3, V4: Word;
begin
  Result := '';
  GetVersionValues(ModuleName, V1, V2, V3, V4);
  Result := IntToStr(V1) + '.' + IntToStr(V2) + '.' + IntToStr(V3) + '.' + IntToStr(V4);
end;

end.
