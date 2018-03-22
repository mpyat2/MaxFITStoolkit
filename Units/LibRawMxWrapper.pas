{$IFDEF FPC} {$MODE DELPHI} {$ENDIF}

unit LibRawMxWrapper;

interface

uses
  Windows, SysUtils;

const LibRawWrapper = 'LibRawMxWrapper_s_crt.dll';

(*
function RawProcessorCreate: Pointer; stdcall; external LibRawWrapper name '_RawProcessorCreate@0';
procedure RawProcessorFree(RawProcessor: Pointer); stdcall; external LibRawWrapper name '_RawProcessorFree@4';
function RawProcessorStrError(RawProcessor: Pointer; P: Integer): PChar; stdcall; external LibRawWrapper name '_RawProcessorStrError@8';
function RawProcessorVersion: PChar; stdcall; external LibRawWrapper name '_RawProcessorVersion@0';
function RawProcessorOpenFile(RawProcessor: Pointer; FileName: PChar): Integer; stdcall; external LibRawWrapper name '_RawProcessorOpenFile@8';
procedure RawProcessorSizes(
  RawProcessor: Pointer;
  var RawWidth, RawHeight: Word;
  var Width, Height: Word;
  var LeftMargin, TopMargin: Word;
  var Iwidth, Iheight: Word;
  var RawPitch: LongWord;
  var PixelAspect: Double;
  var Flip: Integer); stdcall; external LibRawWrapper name '_RawProcessorSizes@48';
function RawProcessorAdjustSizesInfoOnly(RawProcessor: Pointer): Integer; stdcall; external LibRawWrapper name '_RawProcessorAdjustSizesInfoOnly@4';
function RawProcessorUnpack(RawProcessor: Pointer): Integer; stdcall; external LibRawWrapper name '_RawProcessorUnpack@4';
function RawProcessorCheck(RawProcessor: Pointer): Integer; stdcall; external LibRawWrapper name '_RawProcessorCheck@4';
function RawProcessorRawImage(RawProcessor: Pointer): PWord; stdcall; external LibRawWrapper name '_RawProcessorRawImage@4';
function RawProcessorMake(RawProcessor: Pointer): PChar; stdcall; external LibRawWrapper name '_RawProcessorMake@4';
function RawProcessorModel(RawProcessor: Pointer): PChar; stdcall; external LibRawWrapper name '_RawProcessorModel@4';
function RawProcessorSoftware(RawProcessor: Pointer): PChar; stdcall; external LibRawWrapper name '_RawProcessorSoftware@4';
function RawProcessorTimestamp(RawProcessor: Pointer): Int64; stdcall; external LibRawWrapper name '_RawProcessorTimestamp@4';
procedure RawProcessorTime(RawProcessor: Pointer; TimeStr: PChar; TimeStrLen: Integer); stdcall; external LibRawWrapper name '_RawProcessorTime@12';
function RawProcessorShutter(RawProcessor: Pointer): Single; stdcall; external LibRawWrapper name '_RawProcessorShutter@4';
function RawProcessorISOspeed(RawProcessor: Pointer): Single; stdcall; external LibRawWrapper name '_RawProcessorISOspeed@4';
procedure RawProcessorBayerPattern(RawProcessor: Pointer; BayerPatternStr: PChar; BayerPatternStrLen: Integer); stdcall; external LibRawWrapper name '_RawProcessorBayerPattern@12';
*)

var
  RawProcessorCreate: function : Pointer; stdcall;
  RawProcessorFree: procedure (RawProcessor: Pointer); stdcall;
  RawProcessorStrError: function (RawProcessor: Pointer; P: Integer): PChar; stdcall;
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
