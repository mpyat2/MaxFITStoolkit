{*****************************************************************************}
{                                                                             }
{ This program is distributed                                                 }
{ WITHOUT ANY WARRANTY; without even the implied warranty of                  }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                        }
{                                                                             }
{*****************************************************************************}

{$MODE DELPHI}
{$INCLUDE FITSUtils.inc}

{.$DEFINE DEBUG_OUTPUT}

unit CalcThread;

interface

uses 
  Windows, SysUtils, Classes, FITScompatibility, FitsStatUtils, FitsUtils;

type
  TStackMode = ( smAdd, smAvg, smMed );
  PDoubleArray = ^TDoubleArray;
  TProgressProc = function (ThreadNo, Counter, FStartIndex, FNumberOfPixels: Integer): Boolean of object;

type
  TCalcThread = class(TThread)
    private
      FThreadNo: Integer;
      FStartIndex: Integer;
      FNumberOfPixels: Integer;
      FStackMode: TStackMode;
      FStackList: TStringList;
      FImages: TPCharArray;
      FDestPixelArrayPtr: PDoubleArray;
      FUseFast16bitProcs: Boolean;
      FStackedResultMin: Extended;
      FStackedResultMax: Extended;
      FExecuteCompleted: Boolean;
      FProgressProc: TProgressProc;
      procedure Progress(Counter, StartIndex, NumberOfPixels: Integer);
    protected
      procedure Execute; override;
    public
      constructor Create(ThreadNo: Integer;
                         StartIndex, NumberOfPixels: Integer;
                         StackMode: TStackMode;
                         const StackList: TStringList;
                         const Images: TPCharArray;
                         DestPixelArrayPtr: PDoubleArray;
                         UseFast16bitProcs: Boolean;
                         ProgressProc: TProgressProc);
      property ThreadNo: Integer read FThreadNo;
      property StackedResultMin: Extended read FStackedResultMin;
      property StackedResultMax: Extended read FStackedResultMax;
      property StartIndex: Integer read FStartIndex;
      property NumberOfPixels: Integer read FNumberOfPixels;
      property ExecuteCompleted: Boolean read FExecuteCompleted;
    end;

type
  TFITSFileInfo = class(TObject)
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
  end;

function GetLogicalCpuCount: integer;
procedure SetFITSpixel(FITSdata: PChar; N, BitPix: Integer; Value: Double);

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

// Universal procesure for all FITS
function GetFITSpixelAsExtended(FITSdata: PChar; N, BitPix: Integer; BScale: Double; BZero: Double): Extended;
var
  FITSValue: TFITSValue;
  BytePix: Integer;
  Addr: Integer;
  I: Integer;
begin
  BytePix := Abs(BitPix) div 8;
  Addr := N * BytePix;
  for I := 0 to BytePix - 1 do
    FITSValue.A[BytePix - 1 - I] := Byte(FITSdata[Addr + I]);
  case BitPix of
      8: Result := FITSValue.B;
     16: Result := FITSValue.I;
     32: Result := FITSValue.L;
    -32: Result := FITSValue.S;
    -64: Result := FITSValue.D;
    else raise Exception.Create('Internal error: Unsupported BITPIX');
  end;
  Result := BScale * Result + BZero;
end;

procedure SetFITSpixel(FITSdata: PChar; N, BitPix: Integer; Value: Double);
var
  FITSValue: TFITSValue;
  BytePix: Integer;
  Addr: Integer;
  I: Integer;
begin
  case BitPix of
      8: FITSValue.B := Round(Value);
     16: FITSValue.I := Round(Value);
     32: FITSValue.L := Round(Value);
    -32: FITSValue.S := Value;
    -64: FITSValue.D := Value;
    else raise Exception.Create('Internal error: Unsupported BITPIX');
  end;
  BytePix := Abs(BitPix) div 8;
  Addr := N * BytePix;
  for I := 0 to BytePix - 1 do
    FITSdata[Addr + I] := Char(FITSValue.A[BytePix - 1 - I]);
end;

{ TCalcThread }

constructor TCalcThread.Create(ThreadNo: Integer;
                               StartIndex, NumberOfPixels: Integer;
                               StackMode: TStackMode;
                               const StackList: TStringList;
                               const Images: TPCharArray;
                               DestPixelArrayPtr: PDoubleArray;
                               UseFast16bitProcs: Boolean;
                               ProgressProc: TProgressProc);
begin
  inherited Create(True);
  FreeOnTerminate := False;  // after inherited Create!
  FThreadNo := ThreadNo;
  FStartIndex := StartIndex;
  FNumberOfPixels := NumberOfPixels;
  FStackMode := StackMode;
  FStackList := StackList;
  FImages := Images;
  FDestPixelArrayPtr := DestPixelArrayPtr;
  FUseFast16bitProcs := UseFast16bitProcs;
  FProgressProc := ProgressProc;
  FStackedResultMax := 0; // not nesessary
  FStackedResultMin := 0; // not nesessary
  FExecuteCompleted := False;
end;

procedure TCalcThread.Progress(Counter, StartIndex, NumberOfPixels: Integer);
begin
  if Assigned(FProgressProc) then begin
    if not FProgressProc(FThreadNo, Counter, StartIndex, NumberOfPixels) then Self.Terminate;
  end;
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
    if FNumberOfPixels <= 0 then Exit; // FExecuteComplete is not set!
    StackPixels := nil;
    StackPixels16bit := nil;
    if FUseFast16bitProcs then
      SetLength(StackPixels16bit, FStackList.Count)
    else
      SetLength(StackPixels, FStackList.Count);
    for II := FStartIndex to FStartIndex + FNumberOfPixels - 1 do begin
      if Terminated or GlobalTerminateAllThreads then begin
{$IFDEF DEBUG_OUTPUT}
        if GlobalTerminateAllThreads then begin WriteLn; WriteLn('ThreadNo: ', FThreadNo, ' GlobalTerminateAllThreads is TRUE'); end;
{$ENDIF}
        Exit;
      end;

      if FUseFast16bitProcs then begin
        for I := 0 to FStackList.Count - 1 do
          StackPixels16bit[I] := GetFITSpixel16bitNoScale(FImages[I], II);
        case FStackMode of
          smAdd: StackedResult := TStatHelper<SmallInt>.Sum(StackPixels16bit);
          smAvg: StackedResult := TStatHelper<SmallInt>.Mean(StackPixels16bit);
          smMed: StackedResult := TStatHelper<SmallInt>.WirthMedian(StackPixels16bit); // array is reordered!
          else raise Exception.Create('Internal error: invalid Stack Mode');
        end;
      end
      else begin
        for I := 0 to FStackList.Count - 1 do
          StackPixels[I] := GetFITSpixelAsExtended(FImages[I], II, TFITSFileInfo(FStackList.Objects[I]).BitPix, TFITSFileInfo(FStackList.Objects[I]).BScale, TFITSFileInfo(FStackList.Objects[I]).BZero);
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
      if ((Counter + 1) mod (FNumberOfPixels div 8192) = 0) then
        Progress(Counter, FStartIndex, FNumberOfPixels);
    end;
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

end.
