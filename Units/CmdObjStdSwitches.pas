{$DENYPACKAGEUNIT} // Should be included in EXE only.

unit CmdObjStdSwitches;

interface

implementation

uses
  CmdObj;

initialization
  CmdLine.Switches := ['/', '-'];
end.
