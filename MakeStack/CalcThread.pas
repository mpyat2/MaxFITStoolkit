{.$DEFINE DEBUG_OUTPUT}

unit CalcThread;

interface

uses Windows, SysUtils, Classes, FitsUtils;

type
  TStackMode = ( smAdd, smAvg, smMed );

type
  TExtendedArray = array of Extended;

type
  TDoubleArray = array of Double;
  PDoubleArray = ^TDoubleArray;

type
  TPCharArray = array of PChar;

type
  TProgressProc = function (ThreadNo, Counter, FStartIndex, FNumberOfPixels: Integer): Boolean of Object;

type

  { TCalcThread }

  TCalcThread = class(TThread)
    private
      FThreadNo: Integer;
      FStartIndex: Integer;
      FNumberOfPixels: Integer;
      FStackMode: TStackMode;
      FStackList: TStringList;
      FImages: TPCharArray;
      FDestPixelArrayPtr: PDoubleArray;
      FStackedResultMin: Extended;
      FStackedResultMax: Extended;
      FExecuteProcessed: Boolean;
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
                         ProgressProc: TProgressProc);
      property ThreadNo: Integer read FThreadNo;
      property StackedResultMin: Extended read FStackedResultMin;
      property StackedResultMax: Extended read FStackedResultMax;
      property StartIndex: Integer read FStartIndex;
      property NumberOfPixels: Integer read FNumberOfPixels;
      property ExecuteProcessed: Boolean read FExecuteProcessed;
    end;

type
  TFITSFileInfo = class(TObject)
    DateObs: TDateTime;
    BitPix: Integer;
    Naxis: TIntArray;
    BScale: Double;
    BZero: Double;
    StartOfImage: Integer;
    ImageMemSize: Integer; // Padded!
    ObjectName: string;
    Telescope: string;
    Instrument: string;
    Exposure: Double;
    destructor Destroy; override;
  end;

function GetLogicalCpuCount: integer;
procedure SetFITSpixel(FITSdata: PChar; N, BitPix: Integer; Value: Double);

var
  GlobalTerminateAllThreads: Boolean = False;

implementation

// http://wiki.freepascal.org/Example_of_multi-threaded_application:_array_of_threads
function GetLogicalCpuCount: integer;
// returns a good default for the number of threads on this system
{$IF defined(windows)}
//returns total number of processors available to system including logical hyperthreaded processors
var
  i: Integer;
  ProcessAffinityMask, SystemAffinityMask: DWORD_PTR;
  Mask: DWORD;
  SystemInfo: SYSTEM_INFO;
begin
  if GetProcessAffinityMask(GetCurrentProcess, ProcessAffinityMask, SystemAffinityMask)
  then begin
    Result := 0;
    for i := 0 to 31 do begin
      Mask := DWord(1) shl i;
      if (ProcessAffinityMask and Mask)<>0 then
        inc(Result);
    end;
  end else begin
    //can't get the affinity mask so we just report the total number of processors
    GetSystemInfo(SystemInfo);
    Result := SystemInfo.dwNumberOfProcessors;
  end;
end;
{$ELSE}
begin
   FileError('not implemented yet.');
end;
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////
// http://wiki.freepascal.org/Functions_for_descriptive_statistics

procedure SortExtendedArray(var data: TExtendedArray);
{ Based on Shell Sort - avoiding recursion allows for sorting of very
  large arrays, too }
var
  arrayLength, i, j, k: longint;
  h: extended;
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
function median(var data: TExtendedArray): extended;
var
  centralElement: integer;
begin
  SortExtendedArray(data);
  centralElement := length(data) div 2;
  if odd(length(data)) then
    result := data[centralElement]
  else
    result := (data[centralElement - 1] + data[centralElement]) / 2;
end;

////////////////////////////////////////////////////////////////////////////////

function sum(const data: TExtendedArray): extended;
var
  I: Integer;
begin
  Result := 0;
  if Length(data) = 0 then Exit;
  for I := 0 to Length(data) - 1 do
    Result := Result + data[I];
end;

function average(const data: TExtendedArray): extended;
begin
  Result := sum(data) / Length(data);
end;

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
  FProgressProc := ProgressProc;
  FStackedResultMax := 0; // not nesessary
  FStackedResultMin := 0; // not nesessary
  FExecuteProcessed := False;
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
    if FNumberOfPixels <= 0 then Exit;
    SetLength(StackPixels, FStackList.Count);
    for II := FStartIndex to FStartIndex + FNumberOfPixels - 1 do begin
      if Terminated or GlobalTerminateAllThreads then begin
{$IFDEF DEBUG_OUTPUT}
        if GlobalTerminateAllThreads then WriteLn('ThreadNo: ', FThreadNo, ' GlobalTerminateAllThreads is TRUE');
{$ENDIF}
        Exit;
      end;

      for I := 0 to FStackList.Count - 1 do
        StackPixels[I] := GetFITSpixelAsExtended(FImages[I], II, TFITSFileInfo(FStackList.Objects[I]).BitPix, TFITSFileInfo(FStackList.Objects[I]).BScale, TFITSFileInfo(FStackList.Objects[I]).BZero);
      case FStackMode of
        smAdd: StackedResult := sum(StackPixels);
        smAvg: StackedResult := average(StackPixels);
        smMed: StackedResult := median(StackPixels);
        else raise Exception.Create('Internal error: invalid Stack Mode');
      end;
      if Counter = 0 then begin
        FStackedResultMax := StackedResult;
        FStackedResultMin := StackedResult;
       end
       else begin
        if StackedResult > FStackedResultMax then FStackedResultMax := StackedResult;
        if StackedResult < FStackedResultMin then FStackedResultMin := StackedResult;
      end;
      FDestPixelArrayPtr^[II] := StackedResult;
      Inc(Counter);
      if ((Counter + 1) mod (FNumberOfPixels div 1024) = 0) then
        Progress(Counter, FStartIndex, FNumberOfPixels);
    end;
    FExecuteProcessed := True;
{$IFDEF DEBUG_OUTPUT}
    WriteLn('ThreadNo: ', FThreadNo, ' finished');
{$ENDIF}
  except
    on E: Exception do begin
{$IFDEF DEBUG_OUTPUT}
          WriteLn('ThreadNo: ', FThreadNo, ' exception. Setting GlobalTerminateAllThreads.');
{$ENDIF}
      GlobalTerminateAllThreads := True;
      raise;
    end;
  end;
end;

{ TFITSFileInfo }

destructor TFITSFileInfo.Destroy;
begin
  inherited Destroy;
end;

end.
