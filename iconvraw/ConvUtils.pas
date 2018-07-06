{*****************************************************************************}
{                                                                             }
{ ConvUtils                                                                    }
{ (c) 2017 Maksym Pyatnytskyy                                                 }
{                                                                             }
{ This program is distributed                                                 }
{ WITHOUT ANY WARRANTY; without even the implied warranty of                  }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                        }
{                                                                             }
{*****************************************************************************}

{$MODE DELPHI}
{$INCLUDE FITSUtils.inc}

unit ConvUtils;

interface

uses
  SysUtils, DateUtils;

type
  TUnixTimeStr = array[0..25] of Char;

function GetMonth(D: TDateTime): string;
function GetDayOfWeek(D: TDateTime): string;
function MMMtoMonth(const MMM: string): Word;
function DateTimeFromUnixTimeStringSafe(const TimeStr: TUnixTimeStr): TDateTime;

implementation

const
  Months: array[1..12] of string = ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
  Days:   array[1..7]  of string = ('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat');

function GetMonth(D: TDateTime): string;
var
  N: Integer;
begin
  N := MonthOf(D);
  if (N > 0) and (N <= 12) then
    Result := Months[N]
  else
    raise EConvertError.Create('Cannot get month abbreviation');
end;

function GetDayOfWeek(D: TDateTime): string;
var
  N: Integer;
begin
  N := DayOfWeek(D);
  if (N > 0) and (N <= 7) then
    Result := Days[N]
  else
    raise EConvertError.Create('Cannot get day abbreviation');
end;

function MMMtoMonth(const MMM: string): Word;
begin
  if MMM = 'Jan' then Result :=  1 else
  if MMM = 'Feb' then Result :=  2 else
  if MMM = 'Mar' then Result :=  3 else
  if MMM = 'Apr' then Result :=  4 else
  if MMM = 'May' then Result :=  5 else
  if MMM = 'Jun' then Result :=  6 else
  if MMM = 'Jul' then Result :=  7 else
  if MMM = 'Aug' then Result :=  8 else
  if MMM = 'Sep' then Result :=  9 else
  if MMM = 'Oct' then Result := 10 else
  if MMM = 'Nov' then Result := 11 else
  if MMM = 'Dec' then Result := 12 else
  raise EConvertError.Create('Cannot convert Month string to a number');
end;

function DateTimeFromUnixTimeStringSafe(const TimeStr: TUnixTimeStr): TDateTime;
var
  year, month, day, hour, min, sec: Word;
begin
  Result := 0;
  if StrLen(TimeStr) = 25 then begin
    try
       //Www Mmm dd hh:mm:ss yyyy\n
       year := StrToInt(Copy(TimeStr, 21, 4));
       month := MMMtoMonth(Copy(TimeStr, 5, 3));
       day := StrToInt(Copy(TimeStr, 9, 2));
       hour := StrToInt(Copy(TimeStr, 12, 2));
       min := StrToInt(Copy(TimeStr, 15, 2));
       sec := StrToInt(Copy(TimeStr, 18, 2));
       Result := EncodeDateTime(year, month, day, hour, min, sec, 0);
    except
      on E: EConvertError do Result := 0;
    end;
  end;
end;

end.
