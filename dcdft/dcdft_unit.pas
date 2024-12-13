// DCDFT code in this unit is derived from Grant Foster's Rprogs.r
// https://www.aavso.org/software-directory

unit dcdft_unit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, math, typ, sle;

type
  TFloatArray = array of ArbFloat;
  TFloat3 = array[0..2] of ArbFloat;
  TFloat3Array = array of TFloat3;

procedure dcdft_proc(
          const t, mag: TFloatArray;
          lowfreq, hifreq: ArbFloat;
          n_freq: ArbInt;
          out frequencies, periods, amp, power: TFloatArray);

implementation

//procedure AppendValue(var A: TFloatArray; V: ArbFloat);
//begin
//  SetLength(A, Length(A) + 1);
//  A[Length(A) - 1] := V;
//end;

// Translated from R (https://www.aavso.org/sites/default/files/software/Rcodes.zip)
// Sligtly modified to get the result identical to the Peranso DCDFT (v. 3.0.4.4)
procedure dcdft_proc(
          const t, mag: TFloatArray;
          lowfreq, hifreq: ArbFloat;
          n_freq: ArbInt;
          out frequencies, periods, amp, power: TFloatArray);
var
  ndata: ArbInt;
  mag_var: ArbFloat;
  freq_step: ArbFloat;
  nu: ArbFloat;
  angle: ArbFloat;
  term: ArbInt;
  amp_squared, pwr: ArbFloat;
  I, II: ArbInt;
  a: TFloat3Array;
  x: TFloat3;
  fittedValues: TFloatArray;
begin
  ndata := Length(t);
  mag_var := PopnVariance(mag);
  freq_step := (hifreq - lowfreq) / n_freq;

  SetLength(frequencies, n_freq + 1);
  SetLength(periods, n_freq + 1);
  SetLength(amp, n_freq + 1);
  SetLength(power, n_freq + 1);

  SetLength(a, ndata);
  SetLength(fittedValues, ndata);

  for II := 0 to ndata - 1 do begin
    a[II][0] := 1;
  end;

  nu := lowfreq;
  // n_freq + 1 points!
  for I := 0 to n_freq do begin
    frequencies[I] := nu;
    periods[I] := 1 / nu;
    // Calculate cos and sin (c1 and s1) of the time-sequence for the trial frequency
    for II := 0 to ndata - 1 do begin
      angle := 2 * Pi * nu * t[II];
      a[II][1] := Cos(angle);
      a[II][2] := Sin(angle);
    end;
    // Fit to the signal (mag)
    slegls(a[0, 0], ndata, 3, 3, mag[0], x[0], term);
    if term <> 1 then
      raise Exception.Create('"slegls" error: ' + IntToStr(term));
    amp_squared := x[1] * x[1] + x[2] * x[2];
    amp[I] := Sqrt(amp_squared);
    // Get the power of the trial frequency
    for II := 0 to ndata - 1 do begin
      fittedvalues[II] := x[0] + x[1] * a[II][1] + x[2] * a[II][2];
    end;
    pwr := PopnVariance(fittedvalues); // population variance instead of the sample variance as in the R source
    power[I] := pwr;
    nu := nu + freq_step;
  end;
  for I := 0 to n_freq do begin
    power[I] := power[I] * (ndata - 1) / mag_var / 2.0; // (ndata - 1) instead of n data as in the R source
  end;
end;

end.

