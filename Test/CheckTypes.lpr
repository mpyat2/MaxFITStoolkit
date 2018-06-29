{$MODE DELPHI}
program CheckTypes;

uses SysUtils, StrUtils, FitsUtils;

var
  A, B, C, R, I: Integer;
  A64: Int64;
  F: Double;
  S: string;
  ErrorPos: Integer;
  E: Extended;

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
(*
{$Q+}{$R+}
  E := 1e+4930;
  WriteLn('E=', E);
  F := E;
  WriteLn('F=', F);
  WRITELN('All Is OK...');

  A64 := MaxInt+1;
  Str(A64, S);
  WriteLn('A64 as string =', S);
  Val(S, A, ErrorPos);
  WriteLn('ErrorPos = ', ErrorPos);
  WriteLn('A = ', A);

  S := '1e+4932';
  Val(S, F, ErrorPos);
  WriteLn('ErrorPos = ', ErrorPos);
  WriteLn('F = ', F);
*)
(*
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
*)
(*
  while True do begin
    Write('Enter string: ');
    ReadLn(S);
    WriteLn('AnsiDequotedStr: ', '[', AnsiDequotedStr(S, ''''), ']');
    WriteLn('StripQuotes    : ', '[', StripQuotes(S), ']');
    //WriteLn('FITSQuotedValue: ', '[', FITSQuotedValue(S), ']');
    //WriteLn('Length FITSQuotedValue: ', Length(FITSQuotedValue(S)));
    WriteLn;
  end;
*)
(*
  for I := 0 to ParamCount do
    WriteLn(ParamStr(I));
*)

  A := MaxInt div 2;
  B := MaxInt div 2;
  C := 2;

{$Q+}{$R+}
  WriteLn('A*B*C : ', Int64(A)*B*C);
  WriteLn('MaxInt: ', MaxInt);
  if Int64(A)*B*C > MaxInt then
    WriteLn('Too large');

  Write('Press ENTER: ');
  ReadLn;
end.
