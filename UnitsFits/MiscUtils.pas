{*****************************************************************************}
{                                                                             }
{ MiscUtils                                                                   }
{ (c) 2018 Maksym Pyatnytskyy                                                 }
{                                                                             }
{ This program is distributed                                                 }
{ WITHOUT ANY WARRANTY; without even the implied warranty of                  }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                        }
{                                                                             }
{*****************************************************************************}

{$MODE DELPHI}
{$INCLUDE FITSUtils.inc}

unit MiscUtils;

interface

uses
  Windows;

procedure PrintWarning(const S: string);
procedure PrintError(const S: string);

implementation

procedure PrintColorText(const S: string; TextAttr: Word);
var
  Handle: THandle;
  ConsoleScreenBufferInfo: TConsoleScreenBufferInfo;
begin
  Handle := GetStdHandle(STD_ERROR_HANDLE);
  if (Handle <> INVALID_HANDLE_VALUE) and (Handle <> 0) then begin
    getConsoleScreenBufferInfo(Handle, ConsoleScreenBufferInfo);
    SetConsoleTextAttribute(Handle, TextAttr);
  end;
  Write(StdErr, S);
  if (Handle <> INVALID_HANDLE_VALUE) and (Handle <> 0) then begin
    SetConsoleTextAttribute(Handle, ConsoleScreenBufferInfo.wAttributes);
  end;
end;

procedure PrintWarning(const S: string);
begin
  PrintColorText(S, FOREGROUND_RED + FOREGROUND_GREEN + FOREGROUND_INTENSITY);
end;

procedure PrintError(const S: string);
begin
  PrintColorText(S, FOREGROUND_RED + FOREGROUND_INTENSITY + BACKGROUND_RED + BACKGROUND_GREEN + BACKGROUND_BLUE);
end;

end.
