// DCDFT code in this unit is derived from Grant Foster's Rprogs.r
// https://www.aavso.org/software-directory

unit dcdft_unit;

{$mode ObjFPC}{$H+}
//{$R+}

interface

uses
  Windows, Classes, SysUtils, math, typ, sle;

type
  TFloatArray = array of ArbFloat;
  TFloat3 = array[0..2] of ArbFloat;
  TFloat3Array = array of TFloat3;

procedure dcdft_proc(
          const t, mag: TFloatArray;
          lowfreq, hifreq: ArbFloat;
          freq_step: ArbFloat;
          n_freq: ArbInt;
          mcv_mode: Boolean;
          out frequencies, periods, amp, power: TFloatArray);

implementation

procedure CalcError(const S: string);
begin
  raise Exception.Create(S);
end;

// http://wiki.freepascal.org/Example_of_multi-threaded_application:_array_of_threads
function GetLogicalCpuCount: integer;
// returns a good default for the number of threads on this system
// Windows only!
//returns total number of processors available to system including logical hyperthreaded processors
var
  i: Integer;
  ProcessAffinityMask, SystemAffinityMask: DWORD_PTR;
  Mask: DWORD;
  SystemInfo: SYSTEM_INFO;
begin
  if GetProcessAffinityMask(GetCurrentProcess, ProcessAffinityMask, SystemAffinityMask) then begin
    Result := 0;
    for i := 0 to 31 do begin
      Mask := DWord(1) shl i;
      if (ProcessAffinityMask and Mask) <> 0 then
        inc(Result);
    end;
  end
  else begin
    //can't get the affinity mask so we just report the total number of processors
    GetSystemInfo(SystemInfo);
    Result := SystemInfo.dwNumberOfProcessors;
  end;
end;

var
  GlobalTerminateAllThreads: Boolean = False;

type
  TCalcThread = class(TThread)
  private
    Ft, Fmag: TFloatArray;
    FThreadNo: Integer;
    Flowfreq: ArbFloat;
    Ffreq_step: ArbFloat;
    Fn_freq: ArbInt;
    Fmcv_mode: Boolean;
    Fpartial_frequencies, Fpartial_periods, Fpartial_amp, Fpartial_power: TFloatArray;
    FExecuteCompleted: Boolean;
  private
    function GetFreq(I: ArbInt): ArbFloat;
    function GetPeriod(I: ArbInt): ArbFloat;
    function GetAmp(I: ArbInt): ArbFloat;
    function GetPower(I: ArbInt): ArbFloat;
  public
    property AThreadNo: Integer read FThreadNo;
    property ExecuteCompleted: Boolean read FExecuteCompleted;
    property An_freq: ArbInt read Fn_freq;
    property Freq[I: ArbInt]: ArbFloat read GetFreq;
    property Period[I: ArbInt]: ArbFloat read GetPeriod;
    property Amp[I: ArbInt]: ArbFloat read GetAmp;
    property Power[I: ArbInt]: ArbFloat read GetPower;
  private
    procedure dcdft_proc_1;
  protected
    procedure Execute; override;
  public
    constructor Create(ThreadNo: Integer;
                       const t, mag: TFloatArray;
                       lowfreq: ArbFloat;
                       freq_step: ArbFloat;
                       n_freq: ArbInt;
                       mcv_mode: Boolean);
  end;

function TCalcThread.GetFreq(I: ArbInt): ArbFloat;
begin
  Result := Fpartial_frequencies[I];
end;

function TCalcThread.GetPeriod(I: ArbInt): ArbFloat;
begin
  Result := Fpartial_periods[I];
end;

function TCalcThread.GetAmp(I: ArbInt): ArbFloat;
begin
  Result := Fpartial_amp[I];
end;

function TCalcThread.GetPower(I: ArbInt): ArbFloat;
begin
  Result := Fpartial_power[I];
end;

constructor TCalcThread.Create(ThreadNo: Integer;
                               const t, mag: TFloatArray;
                               lowfreq: ArbFloat;
                               freq_step: ArbFloat;
                               n_freq: ArbInt;
                               mcv_mode: Boolean);
begin
  inherited Create(True);
  FreeOnTerminate := False;  // after inherited Create!
  FThreadNo := ThreadNo;
  Ft := t;
  Fmag := mag;
  Flowfreq := lowfreq;
  Ffreq_step := freq_step;
  Fn_freq := n_freq;
  Fmcv_mode := mcv_mode;
  SetLength(Fpartial_frequencies, Fn_freq);
  SetLength(Fpartial_periods, Fn_freq);
  SetLength(Fpartial_amp, Fn_freq);
  SetLength(Fpartial_power, Fn_freq);
  FillChar(Fpartial_frequencies[0], Fn_freq * SizeOf(ArbFloat), 0);
  FillChar(Fpartial_periods[0], Fn_freq * SizeOf(ArbFloat), 0);
  FillChar(Fpartial_amp[0], Fn_freq * SizeOf(ArbFloat), 0);
  FillChar(Fpartial_power[0], Fn_freq * SizeOf(ArbFloat), 0);
  FExecuteCompleted := False;
end;

procedure TCalcThread.Execute;
begin
  try
    dcdft_proc_1;
  except
    on E: Exception do begin
      GlobalTerminateAllThreads := True;
      raise;
    end;
  end;
end;

// Translated from R (https://www.aavso.org/sites/default/files/software/Rcodes.zip)
// Sligtly modified to get the result identical to the Peranso DCDFT (v. 3.0.4.4)
procedure TCalcThread.dcdft_proc_1;
var
  ndata: ArbInt;
  nu: ArbFloat;
  angle: ArbFloat;
  term: ArbInt;
  amp_squared, pwr: ArbFloat;
  I, II: ArbInt;
  a: TFloat3Array;
  x: TFloat3;
  fittedValues: TFloatArray;
begin
  ndata := Length(Ft);

  SetLength(a, ndata);
  SetLength(fittedValues, ndata);

  for II := 0 to ndata - 1 do begin
    a[II][0] := 1;
  end;

  nu := Flowfreq;
  for I := 0 to Fn_freq - 1 do begin
    if Terminated or GlobalTerminateAllThreads then begin
      Exit;
    end;

    Fpartial_frequencies[I] := nu;
    Fpartial_periods[I] := 1 / nu;
    // Calculate cos and sin (c1 and s1) of the time-sequence for the trial frequency
    for II := 0 to ndata - 1 do begin
      angle := 2 * Pi * nu * Ft[II];
      a[II][1] := Cos(angle);
      a[II][2] := Sin(angle);
    end;
    // Fit to the signal (mag)
    slegls(a[0, 0], ndata, 3, 3, Fmag[0], x[0], term);
    if term <> 1 then
      raise Exception.Create('"slegls" error: ' + IntToStr(term));
    amp_squared := x[1] * x[1] + x[2] * x[2];
    Fpartial_amp[I] := Sqrt(amp_squared);
    // Get the power of the trial frequency
    for II := 0 to ndata - 1 do begin
      fittedvalues[II] := x[0] + x[1] * a[II][1] + x[2] * a[II][2];
    end;
    pwr := PopnVariance(fittedvalues); // population variance instead of the sample variance as in the R source
    Fpartial_power[I] := pwr;
    nu := nu + Ffreq_step;
  end;
  FExecuteCompleted := True;
end;

procedure dcdft_proc(
          const t, mag: TFloatArray;
          lowfreq, hifreq: ArbFloat;
          freq_step: ArbFloat;
          n_freq: ArbInt;
          mcv_mode: Boolean;
          out frequencies, periods, amp, power: TFloatArray);
var
  ndata: ArbInt;
  mag_var: ArbFloat;
  startfreq: ArbFloat;
  NumberOfThreads, StepsPerThread, Remainder, StepsToDo: integer;
  I, II, Idx: Integer;
  Threads: array of TCalcThread;
begin
  if freq_step <= 0.0 then
    freq_step := (hifreq - lowfreq) / n_freq
  else
    n_freq := Floor((hifreq - lowfreq) / freq_step);

  SetLength(frequencies, n_freq + 1);
  SetLength(periods, n_freq + 1);
  SetLength(amp, n_freq + 1);
  SetLength(power, n_freq + 1);

  NumberOfThreads := GetLogicalCpuCount();
  StepsPerThread := (n_freq + 1) div NumberOfThreads;
  Remainder := (n_freq + 1) - StepsPerThread * NumberOfThreads;

  GlobalTerminateAllThreads := False;
  SetLength(Threads, NumberOfThreads);
  for I := 0 to Length(Threads) - 1 do Threads[I] := nil; // not nesessary, already initialized

  try
    for I := 0 to NumberOfThreads - 1 do begin
      startfreq := lowfreq + StepsPerThread * I * freq_step;
      StepsToDo := StepsPerThread;
      if I = NumberOfThreads - 1 then
        StepsToDo := StepsToDo + Remainder;
      Threads[I] := TCalcThread.Create(I, t, mag, startfreq, freq_step, StepsToDo, mcv_mode);
      // check for exception while creation (see FPC docs)
      if Assigned(Threads[I].FatalException) then begin
        if Assigned(Threads[I].FatalException) then begin
          if Threads[I].FatalException is Exception then
            CalcError(Exception(Threads[I].FatalException).Message)
          else
            CalcError('Unknown Exception');
        end;
      end;
    end;

    for I := 0 to NumberOfThreads - 1 do
      Threads[I].Start;

    for I := 0 to NumberOfThreads - 1 do
      Threads[I].WaitFor;

    for I := 0 to NumberOfThreads - 1 do begin
      if Assigned(Threads[I].FatalException) then begin
        if Threads[I].FatalException is Exception then
          CalcError(Exception(Threads[I].FatalException).Message)
        else
          CalcError('Unknown Exception');
      end;
      if not Threads[I].ExecuteCompleted then
         CalcError('Unknown Error: not all threads are completed.');
    end;

    for I := 0 to NumberOfThreads - 1 do begin
      StepsToDo := Threads[I].An_freq;
      for II := 0 to StepsToDo - 1 do begin
         Idx := StepsPerThread * I + II;
         frequencies[Idx] := Threads[I].Freq[II];
         periods[Idx] := Threads[I].Period[II];
         amp[Idx] := Threads[I].Amp[II];
         power[Idx] := Threads[I].Power[II];
      end;
    end;

    ndata := Length(t);
    mag_var := PopnVariance(mag);

    if mcv_mode then begin
      for I := 0 to n_freq do begin
        power[I] := power[I] / mag_var;
      end;
    end
    else begin
      for I := 0 to n_freq do begin
        power[I] := power[I] * (ndata - 1) / mag_var / 2.0; // (ndata - 1) instead of n data as in the R source
      end;
    end;

  finally
    for I := Length(Threads) - 1 downto 0 do begin
       FreeAndNil(Threads[I]);
    end;
  end;
end;

end.

