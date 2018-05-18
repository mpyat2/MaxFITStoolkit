{$APPTYPE CONSOLE}

program testsample;

uses
  SysUtils, Classes, FitsUtils;

procedure FileError(const S: string);
begin
  raise Exception.Create(S);
end;

type
  TDoubleArray = array of Double;

const
  AreaSize = 100;

var
  FITSFileName: string;
  FITSFile: FITSRecordFile;
  Image: PChar;
  Width, Height, BitPix: Integer;
  Bscale, Bzero: Double;
  A: TFITSValue;
  X, Y, Addr, N: Integer;
  BytePix: Integer;
  data: TDoubleArray;
  //medianV, meanV, stdevV, minV, maxV: Double;
begin
  try
    FITSFileName := Trim(ParamStr(1));
    if FITSFileName = '' then FileError('FITS file name expected');

    Assign(FITSFile, FITSFileName);
    Reset(FITSFile);
    try
      if not IsFits(FITSFile) then
        FileError('Not a valid FITS file: ' + AnsiQuotedStr(FITSFileName, '"'));
      Image := GetFITSimage2D(FITSFile, Width, Height, BitPix, Bscale, Bzero);
    finally
      CloseFile(FITSFile);
    end;
    if Width * Height = 0 then
      FileError('One of dimensions is zero. File ' + AnsiQuotedStr(FITSfileName, '"')); // should never occured...
    try
      BytePix := Abs(BitPix) div 8;
      SetLength(data, Height * Width);
      for Y := 0 to Height -1 do begin
        for X := 0 to Width - 1 do begin
          Addr := (Y * Width + X) * BytePix;
          Move(Image[Addr], A, BytePix);
          RevertBytes(A, BitPix);
          N := Y * Width + X;
          case BitPix of
              8: data[N] := A.B;
             16: data[N] := A.I;
             32: data[N] := A.L;
            -32: data[N] := A.S;
            -64: data[N] := A.D;
            else FileError('Invalid BitPix = ' + IntToStr(BitPix));
          end;
          data[N] := Bscale * data[N] + Bzero;
        end;
      end;
      for Y := 0 to AreaSize - 1 do begin
        for X := 0 to AreaSize - 1 do begin
          if X > 0 then Write(^I);
          Addr := (Y * Width + X);
          Write(FloatToStr(data[Addr]));
        end;
        WriteLn;
      end;

    finally
      FreeMem(Image);
      Image := nil;
    end;
  except
    on E: Exception do begin
      WriteLn;
      WriteLn('**** Error:');
      WriteLn(E.Message);
      Halt(1);
    end;
  end;
end.

