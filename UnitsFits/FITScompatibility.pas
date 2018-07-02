{*****************************************************************************}
{                                                                             }
{ FITScompatibility                                                           }
{ (c) 2017-2018 Maksym Pyatnytskyy                                            }
{                                                                             }
{ This program is distributed                                                 }
{ WITHOUT ANY WARRANTY; without even the implied warranty of                  }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                        }
{                                                                             }
{*****************************************************************************}

{$MODE DELPHI}
{$INCLUDE FITSUtils.inc}

// This unit must be included before any other FITS-related units
unit FITScompatibility;

interface

{$IFDEF ROUND_C}
function Round(R: Extended): Int64; inline;
{$ENDIF}

implementation

{$IFDEF ROUND_C}
function Round(R: Extended): Int64; inline;
begin
  if R > 0.0 then Result := Trunc(R + 0.5) else Result := Trunc(R - 0.5);
end;
{$ENDIF}

end.
