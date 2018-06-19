{$MODE DELPHI}
program CheckTypes;

uses SysUtils;

var
  A, B, R, I: Integer;

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
  Writeln('SizeOf(Char)   :', SizeOf(Char));
  Writeln('Low(Char)      :',Ord( Low(Char)));
  Writeln('High(Char)     :',Ord(High(Char)));
  WriteLn;
  Writeln('SizeOf(SizeInt):', SizeOf(SizeInt));
  Writeln('Low(SizeInt)   :',    Low(SizeInt));
  Writeln('High(SizeInt)  :',   High(SizeInt));
  WriteLn;
  Writeln('SizeOf(PtrUInt):', SizeOf(PtrUInt));
  Writeln('Low(PtrUInt)   :',    Low(PtrUInt));
  Writeln('High(PtrUInt)  :',   High(PtrUInt));
  WriteLn;
  Writeln('SizeOf(String) :', SizeOf(String));
  WriteLn;
  Writeln('SizeOf(PChar)  :', SizeOf(PChar));
  WriteLn;
  Writeln('SizeOf(Pointer):', SizeOf(Pointer));
  WriteLn;
{$Q+}{$R+}
  WriteLn;
  A := MaxInt - 100;
  for I := 0 to 10 do begin
    B := Random(10);
    WriteLn('A=', A, ' B=', B);
    R := A*B;
    WriteLn('R=A*B:', R);
  end;
  WriteLn;
  Write('Press ENTER: ');
  ReadLn;
end.
