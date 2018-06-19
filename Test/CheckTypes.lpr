{$MODE DELPHI}
program CheckTypes;

uses SysUtils;

begin
  Writeln('SizeOf(Integer):', SizeOf(Integer));
  Writeln('Low(Integer)   :',    Low(Integer));
  Writeln('High(Integer)  :',   High(Integer));
  WriteLn;
  Writeln('SizeOf(LongInt):', SizeOf(LongInt));
  Writeln('Low(LongInt)   :',    Low(LongInt));
  Writeln('High(LongInt)  :',   High(LongInt));
  WriteLn;
  Writeln('SizeOf(Int64)  :', SizeOf(Int64));
  Writeln('Low(Int64)     :',    Low(Int64));
  Writeln('High(Int64)    :',   High(Int64));
  WriteLn;
  Writeln('SizeOf(Pointer):', SizeOf(Pointer));
  WriteLn;
  Writeln('SizeOf(PtrUInt):', SizeOf(PtrUInt));
  Writeln('Low(PtrUInt)   :',    Low(PtrUInt));
  Writeln('High(PtrUInt)  :',   High(PtrUInt));
  WriteLn;
  ReadLn;
end.
