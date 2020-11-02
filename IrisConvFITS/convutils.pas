{*****************************************************************************}
{                                                                             }
{ This program is distributed                                                 }
{ WITHOUT ANY WARRANTY; without even the implied warranty of                  }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                        }
{                                                                             }
{*****************************************************************************}

unit ConvUtils;

//{$R+}{$Q+} // can be commented out in release mode. Should be BEFORE {$INCLUDE FITSUtils.inc}!

{$MODE DELPHI}
{$INCLUDE FITSUtils.inc}

interface

uses
  Classes, SysUtils, FITSUtils, Math;

type
  TOutFITSbitpix = (bitpixDefault, bitpixU8, bitpixI16, bitpixI32, bitpixF32, bitpixF64);

type
  T3Integer = array[0..2] of Integer;
  T3IntegerArray = array of T3Integer;

procedure FileError(const S: string);

function ConstructOutputName(const FileName: string; const GenericName: string; const OutputDir: string; const OutputExt: string; Number: Integer): string;

procedure GetFITSpixelArray(const FileName: string;
                            out PixelArray: TExtendedArray;
                            out Header: TFITSRecordArray;
                            out BitPix: Integer;
                            out PixelNumber: Integer;
                            out Width: Integer);

procedure PadHeader(var Header: TFITSRecordArray);

function GetDestBitPix(DefaultBitPix: Integer; OutFITSbitpix: TOutFITSbitpix): Integer;

procedure SetFITSpixel(FITSdata: PChar; N, BitPix: Integer; Value: Double; var ErrorCount: Integer);

procedure WriteFITS(const OutFileName: string; const Header: TFITSRecordArray; Image: PChar; ImageMemSize: PtrUInt);

implementation

procedure FileError(const S: string);
begin
  raise Exception.Create(S);
end;

function ConstructOutputName(const FileName: string; const GenericName: string; const OutputDir: string; const OutputExt: string; Number: Integer): string;
var
  LocalOutExt: string;
begin
  LocalOutExt := OutputExt;
  if LocalOutExt = '' then
    LocalOutExt := ExtractFileExt(FileName);
  if GenericName = '' then
    Result := ChangeFileExt(ExtractFileName(FileName), LocalOutExt)
  else
    Result := GenericName + IntToStr(Number) + LocalOutExt;
  Result := OutputDir + Result;
end;

procedure GetFITSpixelArray(const FileName: string;
                            out PixelArray: TExtendedArray;
                            out Header: TFITSRecordArray;
                            out BitPix: Integer;
                            out PixelNumber: Integer;
                            out Width: Integer);
var
  FITSfile: FITSRecordFile;
  Image: PChar;
  Height, Layers: Integer;
  Bscale, Bzero: Double;
begin
  AssignFile(FITSfile, FileName);
  Reset(FITSfile);
  try
    if not IsFits(FITSfile) then
      FileError('Not a valid FITS file: ' + AnsiQuotedStr(FileName, '"'));
    // Read data
    PixelArray := nil;
    Image := GetFITSimage(FITSFile, Width, Height, Layers, BitPix, Bscale, Bzero);
    try
      if Layers <> 1 then
        FileError('2D FITS is expected');
{$IFNDEF range_check}{$R+}{$ENDIF}
{$IFNDEF overflow_check}{$Q+}{$ENDIF}
      PixelNumber := Height * Width; // one layer
{$IFNDEF range_check}{$R-}{$ENDIF}
{$IFNDEF overflow_check}{$Q-}{$ENDIF}
      SetLength(PixelArray, PixelNumber);
      CopyFITSValues(Image, PixelArray, PixelNumber, BitPix, BScale, BZero);
    finally
      FreeMem(Image);
      Image := nil;
    end;
    GetHeader(FITSfile, Header);
  finally
    CloseFile(FITSfile);
  end;
end;

procedure PadHeader(var Header: TFITSRecordArray);
var
  I, N, N2: Integer;
begin
  // Padding to FITS block size
  N := Length(Header);
  N2 := N mod RecordsInBlock;
  if N2 > 0 then begin
    for I := 1 to RecordsInBlock - N2 do begin
      SetLength(Header, N + 1);
      FillChar(Header[N], SizeOf(FITSRecordType), ' ');
      Inc(N);
    end;
  end;
end;

function GetDestBitPix(DefaultBitPix: Integer; OutFITSbitpix: TOutFITSbitpix): Integer;
begin
  Result := DefaultBitPix;
  case OutFITSbitpix of
    bitpixU8:  Result := 8;
    bitpixI16: Result := 16;
    bitpixI32: Result := 32;
    bitpixF32: Result := -32;
    bitpixF64: Result := -64;
    else ; // default value.
  end;
end;

procedure SetFITSpixel(FITSdata: PChar; N, BitPix: Integer; Value: Double; var ErrorCount: Integer);
var
  FITSValue: TFITSValue;
  BytePix: Integer;
  Addr: Integer;
  I: Integer;
begin
  case BitPix of
      8: begin
           if Value < Low(Byte) then begin
             FITSValue.B := Low(Byte);
             Inc(ErrorCount);
           end
           else
           if Value > High(Byte) then begin
             FITSValue.B := High(Byte);
             Inc(ErrorCount);
           end
           else
             FITSValue.B := Round(Value);
         end;
     16: begin
           if Value < Low(SmallInt) then begin
             FITSValue.I := Low(SmallInt);
             Inc(ErrorCount);
           end
           else
           if Value > High(SmallInt) then begin
             FITSValue.I := High(SmallInt);
             Inc(ErrorCount);
           end
           else
             FITSValue.I := Round(Value);
         end;
     32: begin
           if Value < Low(LongInt) then begin
             FITSValue.L := Low(LongInt);
             Inc(ErrorCount);
           end
           else
           if Value > High(LongInt) then begin
             FITSValue.L := High(LongInt);
             Inc(ErrorCount);
           end
           else
             FITSValue.L := Round(Value);
         end;
    -32: begin
           if Value < -MaxSingle then begin
             FITSValue.S := -MaxSingle;
             Inc(ErrorCount);
           end
           else
           if Value > MaxSingle then begin
             FITSValue.S := MaxSingle;
             Inc(ErrorCount);
           end
           else
             FITSValue.S := Value;
         end;
    -64: FITSValue.D := Value;
    else raise Exception.Create('Internal error: Unsupported BITPIX');
  end;
  BytePix := Abs(BitPix) div 8;
  Addr := N * BytePix;
  for I := 0 to BytePix - 1 do
    FITSdata[Addr + I] := Char(FITSValue.A[BytePix - 1 - I]);
end;

procedure WriteFITS(const OutFileName: string; const Header: TFITSRecordArray; Image: PChar; ImageMemSize: PtrUInt);
var
  FITSfile: FITSRecordFile;
begin
  AssignFile(FITSfile, OutFileName);
  Rewrite(FITSfile);
  try
    BlockWrite(FITSfile, Header[0], Length(Header));
    BlockWrite(FITSfile, Image^, ImageMemSize div FITSRecordLen);
  finally
    CloseFile(FITSfile);
  end;
end;

end.

