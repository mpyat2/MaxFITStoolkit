{*****************************************************************************}
{                                                                             }
{ CmdObj                                                                      }
{ (c) 2000 Maksym Pyatnytskyy                                                 }
{                                                                             }
{*****************************************************************************}

{$IFDEF FPC} {$MODE DELPHI} {$ENDIF}

unit CmdObj;

interface

uses SysUtils;

type

  { TCmdObj }

  TCmdObj = class(TObject)
  private
    FSwitches: TSysCharSet;
    procedure SetSwitches(Chars: TSysCharSet);
  public
    constructor Create;
    function FirstCharIsSwitch(const S: String): Boolean;
    function IsCmdOption(const Key: String): Boolean;
    function KeyValue(const Key: String): String;
    function ParamFile(No: Integer): String;
    function FileCount: Integer;
    function ParamCount: Integer;
    function ParamStr(No: Integer): String;
    function ParamIsKey(const Param: string; const Key: string): Boolean;
    function ExtractParamValue(const Param: string; const Key: string; var Value: string): Boolean;
    property Switches: TSysCharSet read FSwitches write SetSwitches;
  end;

function CmdLine: TCmdObj;

implementation

var
  CmdLineVar: TCmdObj = nil;

function CmdLine: TCmdObj;
begin
  if CmdLineVar = nil then
    CmdLineVar := TCmdObj.Create;
  Result := CmdLineVar;
end;

constructor TCmdObj.Create;
begin
  inherited Create;
  FSwitches := ['/'];
end;

function TCmdObj.FirstCharIsSwitch(const S: String): Boolean;
begin
  Result := False;
  if S = '' then Exit;
  Result := S[1] in FSwitches;
end;

procedure TCmdObj.SetSwitches(Chars: TSysCharSet);
begin
  if Chars <> [] then FSwitches := Chars;
end;

function TCmdObj.IsCmdOption(const Key: String): Boolean;
var
  I: Integer;
  S: String;
begin
  Result := False;
  if Key = '' then Exit;
  for I := 1 to ParamCount do begin
    S := ParamStr(I);
    if FirstCharIsSwitch(S) and (AnsiCompareText(Key, Copy(S, 2, MaxInt)) = 0) then begin
      Result := True;
      Exit;
    end;
  end;
end;

function TCmdObj.KeyValue(const Key: String): String;
var
  I: Integer;
  S: String;
begin
  Result := '';
  if Key = '' then Exit;
  for I := 1 to ParamCount do begin
    S := ParamStr(I);
    if FirstCharIsSwitch(S) and (AnsiCompareText(Key, Copy(S, 2, Length(Key))) = 0) then begin
      S := Copy(S, Length(Key)+2, MaxInt);
      if S <> '' then begin
        Result := S;
        Exit;
      end;
    end;
  end;
end;

function TCmdObj.ParamFile(No: Integer): String;
var
  I, N: Integer;
  S: String;
begin
  Result := '';
  if No < 1 then Exit; // #FIX#20030421
  N := 0;
  for I := 1 to ParamCount do begin
    S := ParamStr(I);
    if (S <> '') and (not FirstCharIsSwitch(S)) then Inc(N);
    if N = No then begin
      Result := S;
      Exit;
    end;
  end;
end;

function TCmdObj.FileCount: Integer;
var
  I: Integer;
  S: String;
begin
  Result := 0;
  for I := 1 to ParamCount do begin
    S := ParamStr(I);
    if (S <> '') and (not FirstCharIsSwitch(S)) then Inc(Result);
  end;
end;

function TCmdObj.ParamCount: Integer;
begin
  Result := System.ParamCount;
end;

function TCmdObj.ParamStr(No: Integer): String;
begin
  Result := System.ParamStr(No);
end;

function TCmdObj.ParamIsKey(const Param: string; const Key: string): Boolean;
begin
  Result := FirstCharIsSwitch(Param) and (AnsiCompareText(Key, Copy(Param, 2, MaxInt)) = 0);
end;

function TCmdObj.ExtractParamValue(const Param: string; const Key: string; var Value: string): Boolean;
begin
  Result := False;
  if FirstCharIsSwitch(Param) and (AnsiCompareText(Key, Copy(Param, 2, Length(Key))) = 0) then begin
    Value := Copy(Param, Length(Key) + 2, MaxInt);
    Result := True;
  end;
end;

initialization
  
finalization
  FreeAndNil(CmdLineVar);
end.
