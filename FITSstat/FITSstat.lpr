{*****************************************************************************}
{                                                                             }
{ FITSstat                                                                    }
{ (c) 2017 Maksym Pyatnytskyy                                                 }
{                                                                             }
{ This program is distributed                                                 }
{ WITHOUT ANY WARRANTY; without even the implied warranty of                  }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                        }
{                                                                             }
{*****************************************************************************}

{$APPTYPE CONSOLE}
{$MODE DELPHI}
{$INCLUDE FITSUtils.inc}

program FITSstat;

uses
  SysUtils, CmdObj{, CmdObjStdSwitches}, Version, FITScompatibility,
  FITSstatUtils, FITSUtils, FitsUtilsHelp, CommonIni;

{$R *.res}

procedure PrintVersion;
begin
  WriteLn('FITSstat  Maksym Pyatnytskyy  2017');
  WriteLn(GetVersionString(AnsiUpperCase(ParamStr(0))){$IFDEF WIN64}, ' WIN64'{$ENDIF}, ' ', {$I %DATE%}, ' ', {$I %TIME%});
  WriteLn;
end;

procedure FileError(const S: string);
begin
  raise Exception.Create(S);
end;

procedure PrintV(Name: string; V: Extended);
begin
  Write(Name);
  if Frac(V) = 0 then
    WriteLn(V:0:0)
  else
    WriteLn(V:0:7);
end;

function ApplyBScaleBzero(V: Extended; Bscale, Bzero: Double): Extended;
begin
  Result := BScale * V + BZero;
end;

procedure ProcessInput(const FITSFileName: string);
var
  FITSFile: FITSRecordFile;
  Image: PChar;
  Width, Height, Layers, BitPix, LayerN: Integer;
  Bscale, Bzero: Double;
  A: TFITSValue;
  X, Y: Integer;
  PixelNumber, Addr0, Addr, N: SizeInt;
  BytePix: Integer;
  DataB: TByteArray;
  DataI: TSmallIntArray;
  DataL: TLongIntArray;
  DataS: TSingleArray;
  DataD: TDoubleArray;
  MedianV, MeanV, StdevV, MinV, MaxV: Extended;
begin
  MedianV := 0;
  MeanV := 0;
  StdevV := 0;
  MinV := 0;
  MaxV := 0;
  try
    Assign(FITSFile, FITSFileName);
    Reset(FITSFile);
    try
      if not IsFits(FITSFile) then
        FileError('Not a valid FITS file: ' + AnsiQuotedStr(FITSFileName, '"'));
      Image := GetFITSimage(FITSFile, Width, Height, Layers, BitPix, Bscale, Bzero);
    finally
      CloseFile(FITSFile);
    end;
    try
{$IFNDEF range_check}{$R+}{$ENDIF}
{$IFNDEF overflow_check}{$Q+}{$ENDIF}
      PixelNumber := Height * Width; // one layer
{$IFNDEF range_check}{$R-}{$ENDIF}
{$IFNDEF overflow_check}{$Q-}{$ENDIF}

      BytePix := Abs(BitPix) div 8;
      case BitPix of
               8: SetLength(DataB, PixelNumber);
              16: SetLength(DataI, PixelNumber);
              32: SetLength(DataL, PixelNumber);
             -32: SetLength(DataS, PixelNumber);
             -64: SetLength(DataD, PixelNumber);
        else
          FileError('Unsupported BITPIX');
      end;

      WriteLn('File             : ', ExtractFileName(FITSFileName));
      PrintV ('BitPix           : ', BitPix);
      PrintV ('Layers           : ', Layers);
      PrintV ('Width            : ', Width);
      PrintV ('Height           : ', Height);
      PrintV ('Layer Pixel Count: ', PixelNumber);

      for LayerN := 0 to Layers - 1 do begin
        Addr0 := PixelNumber * BytePix * LayerN;

        for Y := 0 to Height - 1 do begin
          for X := 0 to Width - 1 do begin
            Addr := Addr0 + (Y * Width + X) * BytePix;
            Move(Image[Addr], A, BytePix);
            RevertBytes(A, BitPix);
            N := Y * Width + X;
            case BitPix of
                8: DataB[N] := A.B;
               16: DataI[N] := A.I;
               32: DataL[N] := A.L;
              -32: DataS[N] := A.S;
              -64: DataD[N] := A.D;
            end;
            // Do not apply Bscale/Bzero here (faster!)
          end;
        end;
        case BitPix of
            8: begin
                 MedianV := TStatHelper<Byte>.SortAndMedian(DataB);
                 MinV := DataB[0];
                 MaxV := DataB[High(DataB)];
                 TStatHelper<Byte>.MeanAndStdev(DataB, MeanV, StdevV);
               end;
           16: begin
                 MedianV := TStatHelper<SmallInt>.SortAndMedian(DataI);
                 MinV := DataI[0];
                 MaxV := DataI[High(DataI)];
                 TStatHelper<SmallInt>.MeanAndStdev(DataI, MeanV, StdevV);
               end;
           32: begin
                 MedianV := TStatHelper<LongInt>.SortAndMedian(DataL);
                 MinV := DataL[0];
                 MaxV := DataL[High(DataL)];
                 TStatHelper<LongInt>.MeanAndStdev(DataL, MeanV, StdevV);
               end;
          -32: begin
                 MedianV := TStatHelper<Single>.SortAndMedian(DataS);
                 MinV := DataS[0];
                 MaxV := DataS[High(DataS)];
                 TStatHelper<Single>.MeanAndStdev(DataS, MeanV, StdevV);
               end;
          -64: begin
                 MedianV := TStatHelper<Double>.SortAndMedian(DataD);
                 MinV := DataD[0];
                 MaxV := DataD[High(DataD)];
                 TStatHelper<Double>.MeanAndStdev(DataD, MeanV, StdevV);
               end;
        end;
        MedianV := ApplyBScaleBzero(MedianV, BScale, BZero);
        MinV    := ApplyBScaleBzero(MinV,    BScale, BZero);
        MaxV    := ApplyBScaleBzero(MaxV,    BScale, BZero);
        MeanV   := ApplyBScaleBzero(MeanV,   BScale, BZero);
        StdevV  := ApplyBScaleBzero(StdevV,  BScale, 0);

        WriteLn;
        PrintV ('Layer            : ', LayerN + 1);
        PrintV ('Min              : ', MinV);
        PrintV ('Max              : ', MaxV);
        PrintV ('Median           : ', MedianV);
        PrintV ('Mean             : ', MeanV);
        PrintV ('StDev            : ', StdevV);

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
end;

var
  PrintVer: Boolean;
  InputFileName: string;
  S: string;
  ParamN: Integer;

begin
  FileMode := fmOpenRead;

  PrintVer := (CmdObj.CmdLine.IsCmdOption('V') or CmdObj.CmdLine.IsCmdOption('version'));
  if PrintVer then PrintVersion;

  if (CmdObj.CmdLine.IsCmdOption('?') or CmdObj.CmdLine.IsCmdOption('H') or CmdObj.CmdLine.IsCmdOption('help')) then begin
    PrintHelp;
    Halt(1);
  end;

  if (CmdObj.CmdLine.FileCount <> 1) then begin
    if not PrintVer then begin
      WriteLn('**** File Name is expected');
      WriteLn;
      PrintHelp;
    end;
    Halt(1);
  end;

  for ParamN := 1 to CmdObj.CmdLine.ParamCount do begin
    S := CmdObj.CmdLine.ParamStr(ParamN);
    if CmdObj.CmdLine.FirstCharIsSwitch(S) then begin
      if Length(S) = 1 then begin
        WriteLn('**** Invalid command-line parameter: ' + S);
        Halt(1);
      end;
      if CmdObj.CmdLine.ParamIsKey(S, 'V') or CmdObj.CmdLine.ParamIsKey(S, 'version') then begin
        // nothing: already processed.
      end
      else begin
        WriteLn('**** Invalid command-line parameter: ' + S);
        Halt(1);
      end;
    end;
  end;

  InputFileName := ExpandFileName(CmdObj.CmdLine.ParamFile(1));

  ProcessInput(InputFileName);

end.

