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
  SysUtils, DateUtils, FITSUtils;

const
  defBZeroShift: array[Boolean] of LongInt = (0, 32768);
  defFITSbpp: array[Boolean] of Integer = (16, -32);

type
  TUnixTimeStr = array[0..25] of Char;

function GetMonth(D: TDateTime): string;
function GetDayOfWeek(D: TDateTime): string;
function MMMtoMonth(const MMM: string): Word;
function DateTimeFromUnixTimeStringSafe(const TimeStr: TUnixTimeStr): TDateTime;

// Returns FITS image padded to block size!
function ConvertRawBytes(RawBits: PChar;
                         RawWidth: Word;
                         RawFrameWidth, RawFrameHeight, RawFrameLeft, RawFrameTop: Word;
                         PixelRealNumber: Boolean;
                         BzeroShift: Boolean;
                         DoFlip: Boolean;
                         out MinValue, MaxValue: Word;
                         out AverageValue: Double;
                         out ImageSize: PtrUInt): PChar;

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

// Returns FITS image padded to block size!
function ConvertRawBytes(RawBits: PChar;
                         RawWidth: Word;
                         RawFrameWidth, RawFrameHeight, RawFrameLeft, RawFrameTop: Word;
                         PixelRealNumber: Boolean;
                         BzeroShift: Boolean;
                         DoFlip: Boolean;
                         out MinValue, MaxValue: Word;
                         out AverageValue: Double;
                         out ImageSize: PtrUInt): PChar;
var
  ImageSizeInBlocks: PtrUInt;
  BytePerPix: Byte;
  X, Y, Y2: Word;
  scanline: PChar;
  Pixel: Word;
  PixelBytes: array[0..1] of Char absolute Pixel;
  PixelAsSignedInt: SmallInt;
  PixelAsSignedIntBytes: array[0..1] of Char absolute PixelAsSignedInt;
  PixelR: Single;
  PixelRBytes: array[0..3] of Char absolute PixelR;
  FirstPixel: Boolean;
  PixelCount: LongWord;
  SumValue: Double;
  N: PtrUInt;
begin
  Result := nil;
  MinValue := 0;
  MaxValue := 0;
  AverageValue := 0;
  if PixelRealNumber then
    BytePerPix := 4
  else
    BytePerPix := 2;
  ImageSize := RawFrameWidth * RawFrameHeight * BytePerPix;                   // not padded
  if ImageSize = 0 then Exit;

  ImageSizeInBlocks := (ImageSize - 1) div (RecordsInBlock * SizeOf(FITSRecordType)) + 1;
  ImageSize := ImageSizeInBlocks * (RecordsInBlock * SizeOf(FITSRecordType)); // padded to block size
  GetMem(Result, ImageSize);
  try
    FillChar(Result[0], ImageSize, 0);
    N := 0;
    SumValue := 0;
    PixelCount := 0;
    for Y := RawFrameTop + RawFrameHeight - 1 downto RawFrameTop do begin
      if DoFlip then
        Y2 := (RawFrameTop + RawFrameHeight - 1) - Y + RawFrameTop
      else
        Y2 := Y;
      scanline := @RawBits[Y2 * Rawwidth * 2];
      for X := RawFrameLeft to RawFrameLeft + RawFrameWidth - 1 do begin
        Pixel := PWord(@scanline[X * 2])^;
        if FirstPixel then begin
          MinValue := Pixel;
          MaxValue := Pixel;
          FirstPixel := False;
        end
        else begin
          if Pixel < MinValue then MinValue := Pixel;
          if Pixel > MaxValue then MaxValue := Pixel;
        end;
        SumValue := SumValue + Pixel;
        Inc(PixelCount);

        if not PixelRealNumber then begin
          if not BzeroShift then begin
            Result[N    ] := PixelBytes[1];
            Result[N + 1] := PixelBytes[0];
          end
          else begin
            PixelAsSignedInt := Pixel - defBZeroShift[BzeroShift];
            Result[N    ] := PixelAsSignedIntBytes[1];
            Result[N + 1] := PixelAsSignedIntBytes[0];
          end;
        end
        else begin
          PixelR := Pixel;
          PixelR := PixelR - defBZeroShift[BzeroShift];
          Result[N    ] := PixelRBytes[3];
          Result[N + 1] := PixelRBytes[2];
          Result[N + 2] := PixelRBytes[1];
          Result[N + 3] := PixelRBytes[0];
        end;

        Inc(N, BytePerPix);
      end;
    end;

    AverageValue := SumValue / PixelCount;
  except
    FreeMem(Result);
    Result := nil;
    raise;
  end;
end;

end.
