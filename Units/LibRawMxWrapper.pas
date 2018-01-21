unit LibRawMxWrapper;

interface

function RawProcessorCreate: Pointer; stdcall; external 'LibRawMxWrapper.dll' name '_RawProcessorCreate@0';
procedure RawProcessorFree(RawProcessor: Pointer); stdcall; external 'LibRawMxWrapper.dll' name '_RawProcessorFree@4';
function RawProcessorStrError(RawProcessor: Pointer; P: Integer): PChar; stdcall; external 'LibRawMxWrapper.dll' name '_RawProcessorStrError@8';
function RawProcessorVersion: PChar; stdcall; external 'LibRawMxWrapper.dll' name '_RawProcessorVersion@0';
function RawProcessorOpenFile(RawProcessor: Pointer; FileName: PChar): Integer; stdcall; external 'LibRawMxWrapper.dll' name '_RawProcessorOpenFile@8';
procedure RawProcessorSizes(
  RawProcessor: Pointer;
  var Width: Word;
  var Height: Word;
  var RawWidth: Word;
  var RawHeight: Word;
  var TopMargin: Word;
  var LeftMargin: Word); stdcall; external 'LibRawMxWrapper.dll' name '_RawProcessorSizes@28';
function RawProcessorUnpack(RawProcessor: Pointer): Integer; stdcall; external 'LibRawMxWrapper.dll' name '_RawProcessorUnpack@4';
function RawProcessorCheck(RawProcessor: Pointer): Integer; stdcall; external 'LibRawMxWrapper.dll' name '_RawProcessorCheck@4';
function RawProcessorRawImage(RawProcessor: Pointer): PWord; stdcall; external 'LibRawMxWrapper.dll' name '_RawProcessorRawImage@4';
function RawProcessorMake(RawProcessor: Pointer): PChar; stdcall; external 'LibRawMxWrapper.dll' name '_RawProcessorMake@4';
function RawProcessorModel(RawProcessor: Pointer): PChar; stdcall; external 'LibRawMxWrapper.dll' name '_RawProcessorModel@4';
function RawProcessorTimestamp(RawProcessor: Pointer): Int64; stdcall; external 'LibRawMxWrapper.dll' name '_RawProcessorTimestamp@4';
procedure RawProcessorTime(RawProcessor: Pointer; TimeStr: PChar; TimeStrLen: Integer); stdcall; external 'LibRawMxWrapper.dll' name '_RawProcessorTime@12';
function RawProcessorShutter(RawProcessor: Pointer): Double; stdcall; external 'LibRawMxWrapper.dll' name '_RawProcessorShutter@4';
function RawProcessorISOspeed(RawProcessor: Pointer): Double; stdcall; external 'LibRawMxWrapper.dll' name '_RawProcessorISOspeed@4';


implementation
end.
