{*****************************************************************************}
{                                                                             }
{ This program is distributed                                                 }
{ WITHOUT ANY WARRANTY; without even the implied warranty of                  }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                        }
{                                                                             }
{*****************************************************************************}

{$MODE DELPHI}
{$INCLUDE FITSUtils.inc}

{.DEFINE DEBUG_OUTPUT}

unit CalcThread;

interface

uses 
  Windows, SysUtils, Classes, FITScompatibility, FitsStatUtils, FitsUtils, Math;

type
  TStackMode = ( smAdd, smAvg, smMed );
  PDoubleArray = ^TDoubleArray;
  PExtendedArray = ^TExtendedArray;
  TProgressProc = function (ThreadNo, Counter, FStartIndex, FNumberOfItems: Integer): Boolean of object;

type
  TPrimitiveThread = class(TThread)
  private
    FThreadNo: Integer;
    FStartIndex: Integer;
    FNumberOfItems: Integer;
    FExecuteCompleted: Boolean;
    FProgressProc: TProgressProc;
    procedure Progress(Counter, StartIndex, NumberOfPixels: Integer);
  public
    property ThreadNo: Integer read FThreadNo;
    property StartIndex: Integer read FStartIndex;
    property NumberOfItems: Integer read FNumberOfItems;
    property ExecuteCompleted: Boolean read FExecuteCompleted;
  end;

type
  TCalcThread = class(TPrimitiveThread)
    private
      FStackMode: TStackMode;
      FStackList: TStringList;
      FImages: TPCharArray;
      FFileToSubractDataPtr: PExtendedArray;
      FNormalizationFactors: TExtendedArray;
      FDestPixelArrayPtr: PDoubleArray;
      FUseFast16bitProcs: Boolean;
      FStackedResultMin: Extended;
      FStackedResultMax: Extended;
    protected
      procedure Execute; override;
    public
      constructor Create(ThreadNo: Integer;
                         StartIndex, NumberOfPixels: Integer;
                         StackMode: TStackMode;
                         const StackList: TStringList;
                         const Images: TPCharArray;
                         FileToSubtractDataPtr: PExtendedArray;
                         const NormalizationFactors: TExtendedArray;
                         DestPixelArrayPtr: PDoubleArray;
                         UseFast16bitProcs: Boolean;
                         ProgressProc: TProgressProc);
      property StackedResultMin: Extended read FStackedResultMin;
      property StackedResultMax: Extended read FStackedResultMax;
    end;

type
  TNormThread = class(TPrimitiveThread)
  private
    FNumberOfPixelsInImage: Integer;
    FStackList: TStringList;
    FImages: TPCharArray;
    FFileToSubractDataPtr: PExtendedArray;
    FDestNormalizationFactorsPtr: PExtendedArray;
    FNormalizeMVal: Extended;
    FNormalizeMedian: Boolean;
    FUseFast16bitProcs: Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(ThreadNo: Integer;
                       StartIndex, NumberOfImages, NumberOfPixelsInImage: Integer;
                       const StackList: TStringList;
                       const Images: TPCharArray;
                       FileToSubtractDataPtr: PExtendedArray;
                       DestNormalizationFactorsPtr: PExtendedArray;
                       NormalizeMVal: Extended;
                       NormalizeMedian: Boolean;
                       UseFast16bitProcs: Boolean;
                       ProgressProc: TProgressProc);
    property NumberOfPixelsInImage: Integer read FNumberOfPixelsInImage;
  end;

type
  TFITSFileInfo = class(TObject)
  public
    DateObs: TDateTime;
    BitPix: Integer;
    Naxis: TIntArray;
    BScale: Double;
    BZero: Double;
    StartOfImage: Integer;
    ImageMemSize: PtrUInt; // Padded to FITSRecordLen!
    ObjectName: string;
    Telescope: string;
    Instrument: string;
    Exposure: Double;
    constructor Create;
  end;

function GetLogicalCpuCount: integer;

procedure SetFITSpixel(FITSdata: PChar; N, BitPix: Integer; Value: Double; var ErrorCount: Integer);

var
  GlobalTerminateAllThreads: Boolean = False;

implementation

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

////////////////////////////////////////////////////////////////////////////////

// Special quick procedure for IRIS FITS (no scale, BITPIX=16)
function GetFITSpixel16bitNoScale(FITSdata: PChar; N: Integer): SmallInt; inline;
var
  ResultBytes: array[0..1] of Byte absolute Result;
begin
  ResultBytes[0] :=  Byte(FITSdata[N * 2 + 1]);
  ResultBytes[1] :=  Byte(FITSdata[N * 2]);
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

procedure CopyFitsValues16bit(Image: PChar; var PixelArray16bit: TSmallIntArray; Pixels: Integer);
var
  I: Integer;
begin
  for I := 0 to Pixels - 1 do
    PixelArray16bit[I] := GetFITSpixel16bitNoScale(Image, I);
end;

{ TPrimitiveThread }

procedure TPrimitiveThread.Progress(Counter, StartIndex, NumberOfPixels: Integer);
begin
{$IFDEF DEBUG_OUTPUT}
  WriteLn('##ThreadNo=', ^I, FThreadNo, ^I, 'Counter=', ^I, Counter, ^I, 'StartIndex=', ^I, StartIndex, ^I, 'NumberOfPixels=', ^I, NumberOfPixels);
{$ELSE}
  if Assigned(FProgressProc) then begin
    if not FProgressProc(FThreadNo, Counter, StartIndex, NumberOfPixels) then Self.Terminate;
  end;
{$ENDIF}
end;

{ TCalcThread }

constructor TCalcThread.Create(ThreadNo: Integer;
                               StartIndex, NumberOfPixels: Integer;
                               StackMode: TStackMode;
                               const StackList: TStringList;
                               const Images: TPCharArray;
                               FileToSubtractDataPtr: PExtendedArray;
                               const NormalizationFactors: TExtendedArray;
                               DestPixelArrayPtr: PDoubleArray;
                               UseFast16bitProcs: Boolean;
                               ProgressProc: TProgressProc);
begin
  inherited Create(True);
  FreeOnTerminate := False;  // after inherited Create!
  FThreadNo := ThreadNo;
  FStartIndex := StartIndex;
  FNumberOfItems := NumberOfPixels;
  FStackMode := StackMode;
  FStackList := StackList;
  FImages := Images;
  FFileToSubractDataPtr := FileToSubtractDataPtr;
  FNormalizationFactors := NormalizationFactors;
  FDestPixelArrayPtr := DestPixelArrayPtr;
  FUseFast16bitProcs := UseFast16bitProcs;
  FProgressProc := ProgressProc;
  FStackedResultMax := 0; // not nesessary
  FStackedResultMin := 0; // not nesessary
  FExecuteCompleted := False;
end;

procedure TCalcThread.Execute;
var
  StackPixels: TExtendedArray;
  StackPixels16bit: TSmallIntArray;
  StackedResult: Extended;
  I, II, Counter: Integer;
begin
{$IFDEF DEBUG_OUTPUT}
  WriteLn;
  WriteLn('ThreadNo: ', FThreadNo);
  WriteLn('StartIndex, NumberOfPixels: ', FStartIndex, ', ', FNumberOfPixels);
{$ENDIF}
  try
    Counter := 0;
    if FNumberOfItems <= 0 then Exit; // FExecuteComplete is not set!
    StackPixels := nil;
    StackPixels16bit := nil;
    if FUseFast16bitProcs then begin
      SetLength(StackPixels16bit, FStackList.Count);
      if (FNormalizationFactors <> nil) or (FFileToSubractDataPtr <> nil) then
        SetLength(StackPixels, FStackList.Count);
    end
    else
      SetLength(StackPixels, FStackList.Count);
    for II := FStartIndex to FStartIndex + FNumberOfItems - 1 do begin
      if Terminated or GlobalTerminateAllThreads then begin
{$IFDEF DEBUG_OUTPUT}
        if GlobalTerminateAllThreads then begin WriteLn; WriteLn('ThreadNo: ', FThreadNo, ' GlobalTerminateAllThreads is TRUE'); end;
{$ENDIF}
        Exit;
      end;

      if FUseFast16bitProcs then begin
        for I := 0 to FStackList.Count - 1 do
          StackPixels16bit[I] := GetFITSpixel16bitNoScale(FImages[I], II);
        if (FNormalizationFactors <> nil) or (FFileToSubractDataPtr <> nil) then begin
          for I := 0 to FStackList.Count - 1 do
            StackPixels[I] := StackPixels16bit[I];
          if FFileToSubractDataPtr <> nil then begin
            for I := 0 to FStackList.Count - 1 do
              StackPixels[I] := StackPixels[I] - FFileToSubractDataPtr^[II];
          end;
          if FNormalizationFactors <> nil then begin
            for I := 0 to FStackList.Count - 1 do
              StackPixels[I] := StackPixels[I] * FNormalizationFactors[I];
          end;
          case FStackMode of
            smAdd: StackedResult := TStatHelper<Extended>.Sum(StackPixels);
            smAvg: StackedResult := TStatHelper<Extended>.Mean(StackPixels);
            smMed: StackedResult := TStatHelper<Extended>.WirthMedian(StackPixels); // array is reordered!
            else raise Exception.Create('Internal error: invalid Stack Mode');
          end;
        end
        else begin
          case FStackMode of
            smAdd: StackedResult := TStatHelper<SmallInt>.Sum(StackPixels16bit);
            smAvg: StackedResult := TStatHelper<SmallInt>.Mean(StackPixels16bit);
            smMed: StackedResult := TStatHelper<SmallInt>.WirthMedian(StackPixels16bit); // array is reordered!
            else raise Exception.Create('Internal error: invalid Stack Mode');
          end;
        end;
      end
      else begin
        for I := 0 to FStackList.Count - 1 do
          StackPixels[I] := GetFITSpixelAsExtended(FImages[I], II, TFITSFileInfo(FStackList.Objects[I]).BitPix, TFITSFileInfo(FStackList.Objects[I]).BScale, TFITSFileInfo(FStackList.Objects[I]).BZero);
        if FNormalizationFactors <> nil then
          for I := 0 to FStackList.Count - 1 do
            StackPixels[I] := StackPixels[I] * FNormalizationFactors[I];
        case FStackMode of
          smAdd: StackedResult := TStatHelper<Extended>.Sum(StackPixels);
          smAvg: StackedResult := TStatHelper<Extended>.Mean(StackPixels);
          smMed: StackedResult := TStatHelper<Extended>.WirthMedian(StackPixels); // array is reordered!
          else raise Exception.Create('Internal error: invalid Stack Mode');
        end;
      end;
      if Counter = 0 then begin
        FStackedResultMax := StackedResult;
        FStackedResultMin := StackedResult;
      end
      else begin
       if StackedResult > FStackedResultMax then FStackedResultMax := StackedResult;
       if StackedResult < FStackedResultMin then FStackedResultMin := StackedResult;
      end;

{$IFNDEF range_check}{$R+}{$ENDIF}
{$IFNDEF overflow_check}{$Q+}{$ENDIF}
      FDestPixelArrayPtr^[II] := StackedResult;
{$IFNDEF range_check}{$R-}{$ENDIF}
{$IFNDEF overflow_check}{$Q-}{$ENDIF}

      Inc(Counter);
      if ((Counter + 1) mod (FNumberOfItems div 128) = 0) then
        Progress(Counter, FStartIndex, FNumberOfItems);
    end;
    Progress(Counter, FStartIndex, FNumberOfItems);
    FExecuteCompleted := True;
{$IFDEF DEBUG_OUTPUT}
    WriteLn;
    WriteLn('ThreadNo: ', FThreadNo, ' is about to finish.');
{$ENDIF}
  except
    on E: Exception do begin
{$IFDEF DEBUG_OUTPUT}
      WriteLn;
      WriteLn('ThreadNo: ', FThreadNo, ' exception. Setting GlobalTerminateAllThreads to TRUE.');
{$ENDIF}
      GlobalTerminateAllThreads := True;
      raise;
    end;
  end;
end;


constructor TNormThread.Create(ThreadNo: Integer;
                               StartIndex, NumberOfImages, NumberOfPixelsInImage: Integer;
                               const StackList: TStringList;
                               const Images: TPCharArray;
                               FileToSubtractDataPtr: PExtendedArray;
                               DestNormalizationFactorsPtr: PExtendedArray;
                               NormalizeMVal: Extended;
                               NormalizeMedian: Boolean;
                               UseFast16bitProcs: Boolean;
                               ProgressProc: TProgressProc);
begin
  inherited Create(True);
  FreeOnTerminate := False;  // after inherited Create!
  FThreadNo := ThreadNo;
  FStartIndex := StartIndex;
  FNumberOfItems := NumberOfImages;
  FNumberOfPixelsInImage := NumberOfPixelsInImage;
  FStackList := StackList;
  FImages := Images;
  FFileToSubractDataPtr := FileToSubtractDataPtr;
  FDestNormalizationFactorsPtr := DestNormalizationFactorsPtr;
  FNormalizeMVal := NormalizeMVal;
  FNormalizeMedian := NormalizeMedian;
  FUseFast16bitProcs := UseFast16bitProcs;
  FProgressProc := ProgressProc;
  FExecuteCompleted := False;
end;

procedure TNormThread.Execute;
var
  FileInfo: TFITSFileInfo;
  TempPixelArray16bit: TSmallIntArray;    // for normalization only
  TempPixelArrayExtended: TExtendedArray; // for normalization only
  LocationValue: Extended;                  // for normalization only
  I, II, Counter: Integer;
begin
  try
    Counter := 0;
    if FNumberOfItems <= 0 then Exit; // FExecuteComplete is not set!
    // Temp array, will be permutated.
    if FUseFast16bitProcs and (FFileToSubractDataPtr = nil) then
      SetLength(TempPixelArray16bit, FNumberOfPixelsInImage)
    else
      SetLength(TempPixelArrayExtended, FNumberOfPixelsInImage);
    for I := FStartIndex to FStartIndex + FNumberOfItems - 1 do begin
      if Terminated or GlobalTerminateAllThreads then begin
        Exit;
      end;
      FileInfo := TFITSFileInfo(FStackList.Objects[I]);
      if FUseFast16bitProcs and (FFileToSubractDataPtr = nil) then begin
        CopyFitsValues16bit(FImages[I], TempPixelArray16bit, FNumberOfPixelsInImage);
        if FNormalizeMedian then
          LocationValue := TStatHelper<SmallInt>.WirthMedian(TempPixelArray16bit)
        else
          LocationValue := TStatHelper<SmallInt>.Mean(TempPixelArray16bit);
      end
      else begin
        CopyFitsValues(FImages[I], TempPixelArrayExtended, FNumberOfPixelsInImage, FileInfo.BitPix, FileInfo.BScale, FileInfo.BZero);
        if FFileToSubractDataPtr <> nil then begin
          for II := 0 to FNumberOfPixelsInImage - 1 do
            TempPixelArrayExtended[II] := TempPixelArrayExtended[II] - FFileToSubractDataPtr^[II];
        end;
        if FNormalizeMedian then
          LocationValue := TStatHelper<Extended>.WirthMedian(TempPixelArrayExtended)
        else
          LocationValue := TStatHelper<Extended>.Mean(TempPixelArrayExtended);
      end;
      if LocationValue <= 0 then begin
        if FNormalizeMedian then
          raise Exception.Create('When normalization is active, each file must have median value > 0')
        else
          raise Exception.Create('When normalization is active, each file must have mean value > 0')
      end;
      FDestNormalizationFactorsPtr^[I] := FNormalizeMVal / LocationValue;
      Inc(Counter);
      Progress(Counter, FStartIndex, FNumberOfItems);
    end;
    Progress(Counter, FStartIndex, FNumberOfItems);
    FExecuteCompleted := True;
  except
    on E: Exception do begin
      GlobalTerminateAllThreads := True;
      raise;
    end;
  end;
end;

{ TFITSFileInfo }

constructor TFITSFileInfo.Create;
begin
  inherited Create;
  DateObs := 0;
  BitPix := 0;
  Naxis := nil;
  BScale := 0;
  BZero := 0;
  StartOfImage := 0;
  ImageMemSize := 0;
  ObjectName := '';;
  Telescope := '';
  Instrument := '';
  Exposure := 0;
end;

end.
