{*****************************************************************************}
{                                                                             }
{ FITSstatUtils                                                               }
{ (c) 2017-2018 Maksym Pyatnytskyy                                            }
{                                                                             }
{ This program is distributed                                                 }
{ WITHOUT ANY WARRANTY; without even the implied warranty of                  }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                        }
{                                                                             }
{*****************************************************************************}

{$MODE DELPHI}
{$INCLUDE FITSUtils.inc}

unit FITSstatUtils;

interface

uses
  FITScompatibility;

type
  TStatHelper<T> = class
  private
    //class procedure SortArray(var data: array of T);
    class function WirthFind(var a: array of T; k: SizeInt; n: SizeInt): T;
  public
    //class function SortAndMedian(var data: array of T): Extended;
    class function WirthMedian(var data: array of T): Extended; // 'data' changed but not sorted completely!
    class procedure MinMax(var data: array of T; out Min, Max: Extended);
    class function Sum(var data: array of T): Extended;
    class function Mean(var data: array of T): Extended;
    class procedure MeanAndStdev(var data: array of T; out m, s: Extended);
  end;

implementation

(*
////////////////////////////////////////////////////////////////////////////////
// Some procedures are based on
// http://wiki.freepascal.org/Functions_for_descriptive_statistics
// modified.

class procedure TStatHelper<T>.SortArray(var data: array of T);
{ Based on Shell Sort - avoiding recursion allows for sorting of very
  large arrays, too }
var
  arrayLength, i, j, k: SizeInt;
  h: T;
begin
  arrayLength := high(data);
  k := arrayLength div 2;
  while k > 0 do
  begin
    for i := 0 to arrayLength - k do
    begin
      j := i;
      while (j >= 0) and (data[j] > data[j + k]) do
      begin
        h := data[j];
        data[j] := data[j + k];
        data[j + k] := h;
        if j > k then
          dec(j, k)
        else
          j := 0;
      end;
    end;
    k := k div 2
  end;
end;


// modifies data!
class function TStatHelper<T>.SortAndMedian(var data: array of T): Extended;
var
  centralElement: SizeInt;
begin
  result := 0;
  if length(data) = 0 then exit;
  SortArray(data);
  centralElement := length(data) div 2;
  if odd(length(data)) then
    result := data[centralElement]
  else begin
    result := data[centralElement - 1];
    result := (result + data[centralElement]) / 2; // to prevent integer overflow
  end;
end;
*)

// from: N.Wirth. "Algorithms and Data Structures. Oberon version": "2.3.4 Finding the Median"
// Translated from Oberon (with 1 fix!)
// See also http://ndevilla.free.fr/median/median/src/wirth.c
class function TStatHelper<T>.WirthFind(var a: array of T; k: SizeInt; n: SizeInt): T;
var
 L, R, i, j: SizeInt;
 w, x: T;
begin
  L := 0; R := n - 1;
  while L < R do begin
    x := a[k]; i := L; j := R;
    repeat
      while a[i] < x do Inc(i);
      while x < a[j] do Dec(j);
      if i <= j then begin
        w := a[i]; a[i] := a[j]; a[j] := w;
        Inc(i); Dec(j);
      end;
    until i > j;
    if j < k then L := i;
    if k < i then R := j;
  end;
  Result := a[k];
end;

// 'data' changed but not sorted completely!
class function TStatHelper<T>.WirthMedian(var data: array of T): Extended;
var
  k, n: SizeInt;
begin
  Result := 0;
  n := Length(data);
  if n < 1 then Exit;
  if n = 1 then begin
    Result := data[0];
    Exit;
  end;
  if n = 2 then begin
    Result := data[0];
    Result := (Result + data[1]) / 2; // to prevent integer overflow
    Exit;
  end;
  k := n div 2;
  Result := WirthFind(data, k, n);
  if Odd(n) then
    Exit;
  Result := (Result + WirthFind(data, k - 1, n)) / 2;
end;

class procedure TStatHelper<T>.MinMax(var data: array of T; out Min, Max: Extended);
var
  i: SizeInt;
  elem: T;
begin
  Min := 0;
  Max := 0;
  if length(data) = 0 then exit;
  Min := data[0];
  Max := Min;
  for i := 1 to high(data) do begin
    elem := data[i];
    if elem < Min then Min := elem;
    if elem > Max then Max := elem;
  end;
end;

class function TStatHelper<T>.Sum(var data: array of T): Extended;
var
  i: SizeInt;
begin
  result := 0;
  for i := 0 to high(data) do
    result := result + data[I];
end;

class function TStatHelper<T>.Mean(var data: array of T): Extended;
begin
  result := Sum(data) / length(data);
end;

class procedure TStatHelper<T>.MeanAndStdev(var data: array of T; out m, s: Extended);
var
  i: SizeInt;
begin
  m := Mean(data);
  s := 0;
  for i := 0 to high(data) do
    s := s + (data[i] - m) * (data[i] - m);
  s := sqrt(s / length(data));
end;

end.
