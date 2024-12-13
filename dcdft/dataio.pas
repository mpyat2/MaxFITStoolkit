unit dataio;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, typ;

type
  TFloatArray = array of ArbFloat;

procedure ReadTable(const FileName: string; out X, Y: TFloatArray);

procedure WriteTable(const FileName: string; mcv_mode: boolean; const frequencies, periods, amp, power: TFloatArray);

//procedure WriteCSV

implementation

procedure ReadTable(const FileName: string; out X, Y: TFloatArray);
var
  InputFile: TextFile;
  LL: TStringList;
  S: String;
  N: Integer;
  Code: Integer;
  XVal, YVal: ArbFloat;
  SavedFileMode: Byte;
begin
  X := nil;
  Y := nil;
  N := 0;
  SavedFileMode := FileMode;
  FileMode := fmOpenRead + fmShareDenyNone;
  try
    AssignFile(InputFile, FileName);
    Reset(InputFile);
    try
      LL := TStringList.Create;
      try
        LL.Delimiter := ',';
        while not EOF(InputFile) do begin
          ReadLn(InputFile, S);
          S := Trim(S);
          if (S = '') or (S[1] = '#') then
            Continue;
          LL.DelimitedText := S;
          if (LL.Count <= 1) then
            Continue;
          Val(LL.Strings[0], XVal, Code);
          if Code <> 0 then
            Continue;
          Val(LL.Strings[1], YVal, Code);
          if Code <> 0 then
            Continue;
          Inc(N);
          SetLength(X, N);
          SetLength(Y, N);
          X[Length(X) - 1] := XVal;
          Y[Length(Y) - 1] := YVal;
        end;
      finally
        FreeAndNil(LL);
      end;
    finally
      CloseFile(InputFile);
    end;
  finally
    FileMode := SavedFileMode;
  end;
end;

procedure WriteTable(const FileName: string; mcv_mode: boolean; const frequencies, periods, amp, power: TFloatArray);
var
  OutFile: TextFile;
  I: Integer;
begin
  Assign(OutFile, FileName);
  Rewrite(OutFile);
  Write(OutFile, '# freq'^I'per'^I'amp'^I'pow');
  if mcv_mode then
    Write(OutFile, '(MCV)');
  WriteLn(OutFile);
  for I := 0 to Length(frequencies) - 1 do begin
    WriteLn(OutFile, frequencies[I], ^I, periods[I], ^I, amp[I], ^I, power[I]);
  end;
  CloseFile(OutFile);
end;

end.

