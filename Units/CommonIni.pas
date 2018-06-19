{*****************************************************************************}
{                                                                             }
{ CommonIni                                                                   }
{ (c) 2017 Maksym Pyatnytskyy                                                 }
{                                                                             }
{ This program is distributed                                                 }
{ WITHOUT ANY WARRANTY; without even the implied warranty of                  }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                        }
{                                                                             }
{*****************************************************************************}

{$MODE DELPHI}

unit CommonIni;

interface

uses
  SysUtils, IniFiles;

var
  Ini: TMemIniFile;

implementation

var
  Pause: Boolean;
  IniFileName: string;
  
initialization
  IniFileName := ExtractFilePath(ParamStr(0)) + 'FITSUTILS.INI';
  Ini := TMemIniFile.Create(IniFileName);
  Pause := Ini.ReadBool('SETTINGS', 'PAUSE', False);
finalization
  if Pause then begin
    WriteLn;
    Write('Press ENTER to exit: ');
    ReadLn;
  end;
  FreeAndNil(Ini);  
end.
