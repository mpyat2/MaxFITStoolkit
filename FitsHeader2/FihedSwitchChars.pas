{*****************************************************************************}
{                                                                             }
{ FihedSwitchChars                                                            }
{ (c) 2017-2018 Maksym Pyatnytskyy                                            }
{                                                                             }
{ This program is distributed                                                 }
{ WITHOUT ANY WARRANTY; without even the implied warranty of                  }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                        }
{                                                                             }
{*****************************************************************************}

{$MODE DELPHI}
{$INCLUDE FITSUtils.inc}

unit FihedSwitchChars;

interface

const
  defFihedfSwitchChar = '/';

implementation

uses
  CmdObj;

initialization
  CmdLine.Switches := [defFihedfSwitchChar];
end.
