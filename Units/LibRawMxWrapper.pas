unit LibRawMxWrapper;

interface

const LibRawWrapper = 'LibRawMxWrapper.dll';

function RawProcessorCreate: Pointer; stdcall; external LibRawWrapper name '_RawProcessorCreate@0';
procedure RawProcessorFree(RawProcessor: Pointer); stdcall; external LibRawWrapper name '_RawProcessorFree@4';
function RawProcessorStrError(RawProcessor: Pointer; P: Integer): PChar; stdcall; external LibRawWrapper name '_RawProcessorStrError@8';
function RawProcessorVersion: PChar; stdcall; external LibRawWrapper name '_RawProcessorVersion@0';
function RawProcessorOpenFile(RawProcessor: Pointer; FileName: PChar): Integer; stdcall; external LibRawWrapper name '_RawProcessorOpenFile@8';
procedure RawProcessorSizes(
  RawProcessor: Pointer;
  var Width, Height: Word;
  var RawWidth, RawHeight: Word;
  var TopMargin, LeftMargin: Word;
  var Iwidth, Iheight: Word;
  var RawPitch: LongWord;
  var PixelAspect: Double;
  var Flip: Integer); stdcall; external LibRawWrapper name '_RawProcessorSizes@48';
function RawProcessorUnpack(RawProcessor: Pointer): Integer; stdcall; external LibRawWrapper name '_RawProcessorUnpack@4';
function RawProcessorCheck(RawProcessor: Pointer): Integer; stdcall; external LibRawWrapper name '_RawProcessorCheck@4';
function RawProcessorRawImage(RawProcessor: Pointer): PWord; stdcall; external LibRawWrapper name '_RawProcessorRawImage@4';
function RawProcessorMake(RawProcessor: Pointer): PChar; stdcall; external LibRawWrapper name '_RawProcessorMake@4';
function RawProcessorModel(RawProcessor: Pointer): PChar; stdcall; external LibRawWrapper name '_RawProcessorModel@4';
function RawProcessorTimestamp(RawProcessor: Pointer): Int64; stdcall; external LibRawWrapper name '_RawProcessorTimestamp@4';
procedure RawProcessorTime(RawProcessor: Pointer; TimeStr: PChar; TimeStrLen: Integer); stdcall; external LibRawWrapper name '_RawProcessorTime@12';
function RawProcessorShutter(RawProcessor: Pointer): Single; stdcall; external LibRawWrapper name '_RawProcessorShutter@4';
function RawProcessorISOspeed(RawProcessor: Pointer): Single; stdcall; external LibRawWrapper name '_RawProcessorISOspeed@4';


implementation
end.
