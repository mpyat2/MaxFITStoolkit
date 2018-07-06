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

var
  I: Integer;
  Programs: array [0..11] of string = 
  (
    'fihed',
    'fflip',
    'fitscfa',
    'fitsrgb',
    'cfa2rgb',
    'idobs',
    'iren',
    'ipdat',
    'apdat',
    'fitsstat',
    'makestack',
    'iconvraw'
  );


begin
  WriteLn;
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
  Write('Press ENTER to exit... ');
  ReadLn;
end.