{*****************************************************************************}
{                                                                             }
{ FITSUtilsHelp                                                               }
{ (c) 2017 Maksym Pyatnytskyy                                                 }
{                                                                             }
{ This program is distributed                                                 }
{ WITHOUT ANY WARRANTY; without even the implied warranty of                  }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                        }
{                                                                             }
{*****************************************************************************}

{$MODE DELPHI}

unit FITSUtilsHelp;

interface

uses
  Classes, SysUtils;

const
  FITSUtilsHelpFile = 'FITSUTILS.TXT';

procedure PrintHelp;

implementation

procedure PrintHelp;
var
  HelpFile: string;
  HelpTag: string;
  Txt: TextFile;
  Found: Boolean;
  S: string;
begin
  HelpFile := ExtractFilePath(ParamStr(0)) + FITSUtilsHelpFile;
  if FileExists(HelpFile) then begin
    Found := False;
    HelpTag := '@' + ChangeFileExt(ExtractFileName(ParamStr(0)), '');
    try
      AssignFile(Txt, HelpFile);
      Reset(Txt);
      try
        While not EOF(Txt) do begin
          ReadLn(Txt, S);
          if Found then begin
            if (S <> '') and (S[1] = '@') then Break;
            WriteLn(S);
          end
          else
          if AnsiSameText(TrimRight(S), HelpTag) then
            Found := True;
        end;
        if not Found then
          WriteLn('(No help found for the application)');
      finally
        CloseFile(Txt);
      end;
    except
      WriteLn('**** Error reading help file ****');
    end;
  end
  else
    WriteLn('(Help file ', ExtractFileName(HelpFile), ' not found)');
end;

end.

