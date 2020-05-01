{*****************************************************************************}
{                                                                             }
{ HelloFits                                                                   }
{ (c) 2017 Maksym Pyatnytskyy                                                 }
{                                                                             }
{ This program is distributed                                                 }
{ WITHOUT ANY WARRANTY; without even the implied warranty of                  }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                        }
{                                                                             }
{*****************************************************************************}

{$APPTYPE CONSOLE}
{$MODE DELPHI}

program HelloFits;

uses
  SysUtils, Version;

var
  I: Integer;
  
const  
  Programs: array [0..14] of string = 
  (
    'apdat',
    'cfa2rgb',
    'fflip',
    'fihed',
    'find_hot',
    'fitscfa',
    'fitsrgb',
    'fitsstat',
    'iconvraw',
    'idfix',
    'igagen',
    'idobs',
    'ipdat',
    'iren',
    'makestack'
  );


var 
  ProgramPath: string;
  
begin
  WriteLn;
  if (ParamCount = 1) and AnsiSameStr(ParamStr(1), '/v') then begin
    ProgramPath := ExtractFilePath(ParamStr(0));
    WriteLn(ProgramPath);
    for I := Low(Programs) to High(Programs) do
      WriteLn(Programs[I] + '.exe', ^I,  GetVersionString2(ProgramPath + Programs[I] + '.exe'));
  end
  else begin
    WriteLn('Hi!');
    WriteLn('This is IRIS and FITS command-line utilities.');
    WriteLn;
    WriteLn('To see how to use them, go to command prompt and run');
    WriteLn('the following commands without parameters:');
    WriteLn;
    for I := Low(Programs) to High(Programs) do
      WriteLn(Programs[I]);
    WriteLn;
    WriteLn('Your sincerely,');
    WriteLn('  Max [mpyat2@gmail.com]');
    WriteLn;
  end;  
  Write('Press ENTER to exit... ');
  ReadLn;
end.