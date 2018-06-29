{$MODE DELPHI}

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
