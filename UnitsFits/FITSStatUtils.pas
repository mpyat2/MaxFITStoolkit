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
    class procedure SortArray(var data: array of T);
    class function SortAndMedian(var data: array of T): Extended;
    class function Sum(var data: array of T): Extended;
    class function Mean(var data: array of T): Extended;
    class procedure MeanAndStdev(var data: array of T; out m, s: Extended);
  end;

implementation

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
    result := (data[centralElement - 1] + data[centralElement]) / 2;
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
