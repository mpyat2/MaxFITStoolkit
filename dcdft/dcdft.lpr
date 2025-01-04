{*****************************************************************************}
{                                                                             }
{ dcdft                                                                       }
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

const
  MAX_THREADS = 128;

function Time_S(): Double;
begin
  Result := Now() * 24 * 60 * 60;
end;

procedure PrintHelp;
begin
  WriteLn('Usage:');
  WriteLn(ExtractFileName(ParamStr(0)) + ' /L=<lowfreq> /H=<hifreq> /N=<n_intervals>|/S=<step> [Input file|~] [Output file] [/T] [/MCV]');
end;

var
  Times: TFloatArray;
  Magnitudes: TFloatArray;
  frequencies, periods, amp, power: TFloatArray;
  InputFileName: string;
  OutFileName: string;
  t0: Double;
  DisplayTime: Boolean;
  lofreq, hifreq, freq_step: ArbFloat;
  n_freq: ArbInt;
  ParamN: Integer;
  mcv_mode: Boolean;
  CmdLineNumberOfThreads: Integer;
  CmdParam: string;
  CmdParamValue: string;
  Code: Integer;
  Pause: Boolean;

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

  Pause := False;
  DisplayTime := False;
  lofreq := 0.0;
  hifreq := 0.0;
  freq_step := -1.0;
  n_freq := -1;
  mcv_mode := False;
  CmdLineNumberOfThreads := 0;
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
      if CmdObj.CmdLine.ParamIsKey(CmdParam, 'MCV') then begin
        mcv_mode := True;
      end
      else
      if CmdObj.CmdLine.ParamIsKey(CmdParam, 'PAUSE') then begin
        Pause := True;
      end
      else
      if CmdObj.CmdLine.ExtractParamValue(CmdParam, 'T=', CmdParamValue) then begin
        Val(CmdParamValue, CmdLineNumberOfThreads, Code);
        if (Code <> 0) or (CmdLineNumberOfThreads <= 0) or (CmdLineNumberOfThreads > MAX_THREADS) then begin
          PrintError('**** Number of threads must be in the range [1..' + IntToStr(MAX_THREADS) + ']');
          Halt(1);
        end;
      end
      else
      if CmdObj.CmdLine.ExtractParamValue(CmdParam, 'L=', CmdParamValue) then begin
        Val(CmdParamValue, lofreq, Code);
        if (Code <> 0) or (lofreq <= 0.0) then begin
          PrintError('**** Invalid command-line parameter: ' + CmdParam);
          Halt(1);
        end;
      end
      else
      if CmdObj.CmdLine.ExtractParamValue(CmdParam, 'H=', CmdParamValue) then begin
        Val(CmdParamValue, hifreq, Code);
        if (Code <> 0) or (hifreq <= 0.0) then begin
          PrintError('**** Invalid command-line parameter: ' + CmdParam);
          Halt(1);
        end;
      end
      else
      if CmdObj.CmdLine.ExtractParamValue(CmdParam, 'S=', CmdParamValue) then begin
        Val(CmdParamValue, freq_step, Code);
        if (Code <> 0) or (freq_step <= 0.0) then begin
          PrintError('**** Invalid command-line parameter: ' + CmdParam);
          Halt(1);
        end;
      end
      else
      if CmdObj.CmdLine.ExtractParamValue(CmdParam, 'N=', CmdParamValue) then begin
        Val(CmdParamValue, n_freq, Code);
        if (Code <> 0) or (n_freq <= 0) then begin
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

  if (freq_step <= 0.0) and (n_freq <= 0) then begin
    PrintError('**** /N= or /S= must be specified');
    Halt(1);
  end;

  if (freq_step > 0.0) and (n_freq > 0) then begin
    PrintError('**** /N= and /S= cannot be set simultaneously');
    Halt(1);
  end;

  if lofreq >= hifreq then begin
    PrintError('**** High-frequency limit must be greater than Low-frequency limit');
    Halt(1);
  end;

  ReadTable(InputFileName, Times, Magnitudes);

  t0 := Time_S();
  dcdft_proc(Times, Magnitudes, lofreq, hifreq, freq_step, n_freq, mcv_mode, CmdLineNumberOfThreads, frequencies, periods, amp, power);
  if DisplayTime then
     WriteLn(Format('## DCDFT Time: %fs', [Time_S() - t0]));

  WriteTable(OutFileName, mcv_mode, frequencies, periods, amp, power);

  if Pause then begin
    Write('Press ENTER: ');
    ReadLn;
  end;
end.

