{*****************************************************************************}
{                                                                             }
{ dcdft                                                                   }
{ (c) 2024 Maksym Pyatnytskyy                                                 }
{                                                                             }
{ This program is distributed                                                 }
{ WITHOUT ANY WARRANTY; without even the implied warranty of                  }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                        }
{                                                                             }
{*****************************************************************************}

{$APPTYPE CONSOLE}
{$MODE DELPHI}
{$INCLUDE FITSUtils.inc}

program dcdft;

uses
  SysUtils, typ, sle, omv,
  CmdObj, CmdObjStdSwitches, Version, FITScompatibility, MiscUtils,
  dataio, dcdft_unit;

{$R *.res}

function Time_S(): Double;
begin
  Result := Now() * 24 * 60 * 60;
end;

procedure PrintHelp;
begin
  WriteLn('Usage:');
  WriteLn(ExtractFileName(ParamStr(0)) + ' /L=<lowfreq> /H=<hifreq> /N=<n_intervals> [Input file|~] [Output file] [/T]');
end;

var
  Times: TFloatArray;
  Magnitudes: TFloatArray;
  frequencies, periods, amp, power: TFloatArray;
  InputFileName: string;
  OutFileName: string;
  t0: Double;
  DisplayTime: Boolean;
  lofreq, hifreq: ArbFloat;
  n_freq: ArbInt;
  ParamN: Integer;
  CmdParam: string;
  CmdParamValue: string;
  Code: Integer;

begin
  if CmdObj.CmdLine.ParamCount = 0 then begin
    PrintHelp;
    Halt(1);
  end;

  if (CmdObj.CmdLine.IsCmdOption('V') or CmdObj.CmdLine.IsCmdOption('version')) then begin
    WriteLn('DCDFT by Ferraz-Mello');
    WriteLn('Converted from Grant Foster''s realization');
    WriteLn(GetVersionString(AnsiUpperCase(ParamStr(0))), ' ', {$I %FPCTARGETOS%}, ' ', {$I %DATE%}, ' ', {$I %TIME%});
    Halt(1);
  end;

  if (CmdObj.CmdLine.IsCmdOption('?') or CmdObj.CmdLine.IsCmdOption('H') or CmdObj.CmdLine.IsCmdOption('help')) then begin
    PrintHelp;
    Halt(1);
  end;

  if CmdObj.CmdLine.FileCount > 2 then begin
    PrintError('**** Invalid parameters: no, 1 or 2 file names must be specified.'^M^J);
    WriteLn;
    Halt(1);
  end;

  InputFileName := CmdObj.CmdLine.ParamFile(1);
  if InputFileName = '~' then
     InputFileName := '';
  OutFileName := CmdObj.CmdLine.ParamFile(2);

  DisplayTime := False;
  lofreq := 0.0;
  hifreq := 0.0;
  n_freq := 0;
  for ParamN := 1 to CmdObj.CmdLine.ParamCount do begin
    CmdParam := CmdObj.CmdLine.ParamStr(ParamN);
    if CmdObj.CmdLine.FirstCharIsSwitch(CmdParam) then begin
      if Length(CmdParam) = 1 then begin
        PrintError('**** Invalid command-line parameter: ' + CmdParam);
        Halt(1);
      end;
      if CmdObj.CmdLine.ParamIsKey(CmdParam, 'V') or CmdObj.CmdLine.ParamIsKey(CmdParam, 'version') then begin
        // nothing: already processed.
      end
      else
      if CmdObj.CmdLine.ParamIsKey(CmdParam, 'T') then begin
        DisplayTime := True;
      end
      else
      if CmdObj.CmdLine.ExtractParamValue(CmdParam, 'L=', CmdParamValue) then begin
        Val(CmdParamValue, lofreq, Code);
        if Code <> 0 then begin
          PrintError('**** Invalid command-line parameter: ' + CmdParam);
          Halt(1);
        end;
      end
      else
      if CmdObj.CmdLine.ExtractParamValue(CmdParam, 'H=', CmdParamValue) then begin
        Val(CmdParamValue, hifreq, Code);
        if Code <> 0 then begin
          PrintError('**** Invalid command-line parameter: ' + CmdParam);
          Halt(1);
        end;
      end
      else
      if CmdObj.CmdLine.ExtractParamValue(CmdParam, 'N=', CmdParamValue) then begin
        Val(CmdParamValue, n_freq, Code);
        if Code <> 0 then begin
          PrintError('**** Invalid command-line parameter: ' + CmdParam);
          Halt(1);
        end;
      end
      else begin
        PrintError('**** Invalid command-line parameter: ' + CmdParam);
        Halt(1);
      end;
    end
    else begin
      //
    end;
  end;

  if lofreq <= 0 then begin
    PrintError('**** /L= parameter must specify a positive non-zero value (low frequency limit)');
    Halt(1);
  end;

  if hifreq <= 0 then begin
    PrintError('**** /H= parameter must specify a positive non-zero value (high frequency limit)');
    Halt(1);
  end;

  if n_freq <= 0 then begin
    PrintError('**** /N= parameter must specify a positive non-zero integer (number of frequency intervals)');
    Halt(1);
  end;

  if lofreq >= hifreq then begin
    PrintError('**** High-frequency limit must be greater than Low-frequency limit');
    Halt(1);
  end;

  ReadTable(InputFileName, Times, Magnitudes);

  t0 := Time_S();
  dcdft_proc(Times, Magnitudes, lofreq, hifreq, n_freq, frequencies, periods, amp, power);
  if DisplayTime then
     WriteLn(Format('## DCDFT Time: %fs', [Time_S() - t0]));

  WriteTable(OutFileName, frequencies, periods, amp, power);

  //Write('Press ENTER: ');
  //ReadLn;
end.

