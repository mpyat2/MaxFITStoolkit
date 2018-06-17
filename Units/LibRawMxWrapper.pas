{$IFDEF FPC} {$MODE DELPHI} {$ENDIF}

unit LibRawMxWrapper;

interface

uses
  Windows, SysUtils;

const LibRawWrapper = {$IFDEF WIN64}'LibRawMxWrapper_s_crt_64.dll'{$ELSE}'LibRawMxWrapper_s_crt.dll'{$ENDIF};

var
  RawProcessorCreate: function : Pointer; stdcall;
  RawProcessorFree: procedure (RawProcessor: Pointer); stdcall;
  RawProcessorStrError: function (Reserved: Pointer; P: Integer): PChar; stdcall;
  RawProcessorVersion: function : PChar; stdcall;
  RawProcessorOpenFile: function (RawProcessor: Pointer; FileName: PChar): Integer; stdcall;
  RawProcessorSizes: procedure (
    RawProcessor: Pointer;
    var RawWidth, RawHeight: Word;
    var Width, Height: Word;
    var LeftMargin, TopMargin: Word;
    var Iwidth, Iheight: Word;
    var RawPitch: LongWord;
    var PixelAspect: Double;
    var Flip: Integer); stdcall;
  RawProcessorAdjustSizesInfoOnly: function (RawProcessor: Pointer): Integer; stdcall;
  RawProcessorUnpack: function (RawProcessor: Pointer): Integer; stdcall;
  RawProcessorCheck: function (RawProcessor: Pointer): Integer; stdcall;
  RawProcessorRawImage: function (RawProcessor: Pointer): PWord; stdcall;
  RawProcessorMake: function (RawProcessor: Pointer): PChar; stdcall;
  RawProcessorModel: function (RawProcessor: Pointer): PChar; stdcall;
  RawProcessorSoftware: function (RawProcessor: Pointer): PChar; stdcall;
  RawProcessorTimestamp: function (RawProcessor: Pointer): Int64; stdcall;
  RawProcessorTime: procedure (RawProcessor: Pointer; TimeStr: PChar; TimeStrLen: Integer); stdcall;
  RawProcessorShutter: function (RawProcessor: Pointer): Single; stdcall;
  RawProcessorISOspeed: function (RawProcessor: Pointer): Single; stdcall;
  RawProcessorBayerPattern: procedure (RawProcessor: Pointer; BayerPatternStr: PChar; BayerPatternStrLen: Integer); stdcall;

procedure InitLibrary(const DLLname: string);

implementation

function InitFunctionPointer(H: THandle; const ProcName: string; const DLLname: string): Pointer;
begin
  Result := GetProcAddress(H, PChar(ProcName));
  if (Result = nil) then
    raise Exception.Create('Cannot find procedure ' + ProcName + ' in ' + DLLname);
end;

procedure InitLibrary(const DLLname: string);
var
  H: THandle;
begin
  H := SafeLoadLibrary(DLLname);
  if H = 0 then RaiseLastOSError;
{$IFDEF WIN64}
  RawProcessorCreate := InitFunctionPointer(H, 'RawProcessorCreate', DLLname);
  RawProcessorFree := InitFunctionPointer(H, 'RawProcessorFree', DLLname);
  RawProcessorStrError := InitFunctionPointer(H, 'RawProcessorStrError', DLLname);
  RawProcessorVersion := InitFunctionPointer(H, 'RawProcessorVersion', DLLname);
  RawProcessorOpenFile := InitFunctionPointer(H, 'RawProcessorOpenFile', DLLname);
  RawProcessorSizes := InitFunctionPointer(H, 'RawProcessorSizes', DLLname);
  RawProcessorAdjustSizesInfoOnly := InitFunctionPointer(H, 'RawProcessorAdjustSizesInfoOnly', DLLname);
  RawProcessorUnpack := InitFunctionPointer(H, 'RawProcessorUnpack', DLLname);
  RawProcessorCheck := InitFunctionPointer(H, 'RawProcessorCheck', DLLname);
  RawProcessorRawImage := InitFunctionPointer(H, 'RawProcessorRawImage', DLLname);
  RawProcessorMake := InitFunctionPointer(H, 'RawProcessorMake', DLLname);
  RawProcessorModel := InitFunctionPointer(H, 'RawProcessorModel', DLLname);
  RawProcessorSoftware := InitFunctionPointer(H, 'RawProcessorSoftware', DLLname);
  RawProcessorTimestamp := InitFunctionPointer(H, 'RawProcessorTimestamp', DLLname);
  RawProcessorTime := InitFunctionPointer(H, 'RawProcessorTime', DLLname);
  RawProcessorShutter := InitFunctionPointer(H, 'RawProcessorShutter', DLLname);
  RawProcessorISOspeed := InitFunctionPointer(H, 'RawProcessorISOspeed', DLLname);
  RawProcessorBayerPattern := InitFunctionPointer(H, 'RawProcessorBayerPattern', DLLname);
{$ELSE}
  RawProcessorCreate := InitFunctionPointer(H, '_RawProcessorCreate@0', DLLname);
  RawProcessorFree := InitFunctionPointer(H, '_RawProcessorFree@4', DLLname);
  RawProcessorStrError := InitFunctionPointer(H, '_RawProcessorStrError@8', DLLname);
  RawProcessorVersion := InitFunctionPointer(H, '_RawProcessorVersion@0', DLLname);
  RawProcessorOpenFile := InitFunctionPointer(H, '_RawProcessorOpenFile@8', DLLname);
  RawProcessorSizes := InitFunctionPointer(H, '_RawProcessorSizes@48', DLLname);
  RawProcessorAdjustSizesInfoOnly := InitFunctionPointer(H, '_RawProcessorAdjustSizesInfoOnly@4', DLLname);
  RawProcessorUnpack := InitFunctionPointer(H, '_RawProcessorUnpack@4', DLLname);
  RawProcessorCheck := InitFunctionPointer(H, '_RawProcessorCheck@4', DLLname);
  RawProcessorRawImage := InitFunctionPointer(H, '_RawProcessorRawImage@4', DLLname);
  RawProcessorMake := InitFunctionPointer(H, '_RawProcessorMake@4', DLLname);
  RawProcessorModel := InitFunctionPointer(H, '_RawProcessorModel@4', DLLname);
  RawProcessorSoftware := InitFunctionPointer(H, '_RawProcessorSoftware@4', DLLname);
  RawProcessorTimestamp := InitFunctionPointer(H, '_RawProcessorTimestamp@4', DLLname);
  RawProcessorTime := InitFunctionPointer(H, '_RawProcessorTime@12', DLLname);
  RawProcessorShutter := InitFunctionPointer(H, '_RawProcessorShutter@4', DLLname);
  RawProcessorISOspeed := InitFunctionPointer(H, '_RawProcessorISOspeed@4', DLLname);
  RawProcessorBayerPattern := InitFunctionPointer(H, '_RawProcessorBayerPattern@12', DLLname);
{$ENDIF}
end;

initialization
  RawProcessorCreate := nil;
  RawProcessorFree := nil;
  RawProcessorStrError := nil;
  RawProcessorVersion := nil;
  RawProcessorOpenFile := nil;
  RawProcessorSizes := nil;
  RawProcessorAdjustSizesInfoOnly := nil;
  RawProcessorUnpack := nil;
  RawProcessorCheck := nil;
  RawProcessorRawImage := nil;
  RawProcessorMake := nil;
  RawProcessorModel := nil;
  RawProcessorSoftware := nil;
  RawProcessorTimestamp := nil;
  RawProcessorTime := nil;
  RawProcessorShutter := nil;
  RawProcessorISOspeed := nil;
  RawProcessorBayerPattern := nil;
end.
