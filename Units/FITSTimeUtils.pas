unit FITSTimeUtils;

interface 

uses Windows, SysUtils {$IFNDEF FPC}, DateUtils{$ENDIF};

function TimeObs(const TimeStr: string; out T: TDateTime): Boolean;
function DateObs(const DateStr: string; out T: TDateTime): Boolean;
function IrisDateObs(const DateStr: string; out T: TDateTime): Boolean;
function MakeDateObsFromIrisDate(out DateTimeObs: TDateTime; const DateObsStr: string; UtStartStr: string = ''; Exposure: Double = 0.0): Boolean; overload;
function MakeDateObsFromIrisDate(out DateTimeObsStr: string; const DateObsStr: string; UtStartStr: string = ''; Exposure: Double = 0.0): Boolean; overload;
function DateObsToDateTime(const DateObsStr: string; out DateTimeObs: TDateTime): Boolean;

implementation

const
  Digits = ['0','1','2','3','4','5','6','7','8','9'];

function TimeObs(const TimeStr: string; out T: TDateTime): Boolean;
// Assuming that time must be like this: 'H[H]:M[M]:[S[S][.<millisec>]]'
var
  I, II: Integer;
  N: Integer;
  HH, MM, SS, MS: Integer;
  MsPart: string;
  S: string;
  P: Integer;
begin
  Result := False;
  T := 0;
  HH := 0;
  MM := 0;
  SS := 0;
  MS := 0;
  S := '';
  N := 0;
  I := 1;
  while I <= Length(TimeStr) + 1 do begin
    if (I > Length(TimeStr)) or (TimeStr[I] = ':') then begin
    Inc(N);
    if N in [1, 2] then begin
      if (Length(S) < 1) or (Length(S) > 2) then Exit;
      if not (S[1] in Digits) then Exit;
      if (Length(S) > 1) and not (S[2] in Digits) then Exit;
        if N = 1 then HH := StrToInt(S) else MM := StrToInt(S);
      end
      else 
      if N = 3 then begin
        // Seconds can have millisecond part
        if Length(S) < 1 then Exit;
        for II := 1 to Length(S) do
          if not ((S[II] in Digits) or (S[II] = '.')) then Exit;
        P := Pos('.', S);
        if not (P in [0, 2, 3]) then Exit;
        MsPart := '';
        if (P > 0) then begin
          MsPart := Copy(S, P+1, MaxInt);
          if Length(MsPart) > 3 then Exit;
          S := Copy(S, 1, P-1);
        end;
        if (Length(S) < 1) or (Length(S) > 2) then Exit;
        SS := StrToInt(S);
        if MsPart <> '' then MS := StrToInt(MsPart) else MS := 0;
      end
      else
        Exit;
    S := '';
  end
    else
      S := S + TimeStr[I];
    Inc(I);
  end;
  if N < 2 then Exit;
  if (HH < 0) or (HH > 23) then Exit;
  if (MM < 0) or (MM > 59) then Exit;
  if (SS < 0) or (SS > 59) then Exit;
  if (MS < 0) or (MS > 999) then Exit;
  T := EncodeTime(HH, MM, SS, MS);
  Result := True;
end;

function DateObs(const DateStr: string; out T: TDateTime): Boolean;
// This procedure allows: 'YYYY-[M]M-[D]D'
// Year must be always 4-digit.
var
  I, II: Integer;
  N: Integer;
  DD, MM, YYYY: Integer;
  S: string;
begin
  Result := False;
  T := 0;
  DD := 0;
  MM := 0;
  YYYY := 0;
  S := '';
  N := 0;
  I := 1;
  while I <= Length(DateStr) + 1 do begin
    if (I > Length(DateStr)) or (DateStr[I] = '-') then begin
    Inc(N);
    if N < 4 then begin
      if (N = 1) and (Length(S) <> 4) then Exit;
      if (N > 1) and ((Length(S) < 1) or (Length(S) > 2)) then Exit;
      for II := 1 to Length(S) do 
      if not (S[II] in Digits) then Exit;
      if N = 3 then DD := StrToInt(S)
      else
      if N = 2 then MM := StrToInt(S)
      else
        YYYY := StrToInt(S);
    end
    else
      Exit;
    S := '';
  end
    else
      S := S + DateStr[I];
    Inc(I);
  end;
  if N <> 3 then Exit;
  Result := TryEncodeDate(YYYY, MM, DD, T);
end;

function IrisDateObs(const DateStr: string; out T: TDateTime): Boolean;
// IRIS date format: 'DD/MM/YYYY'
// This procedure allows: '[D]D/[M]M/YYYY'
// Year must be always 4-digit.
var
  I, II: Integer;
  N: Integer;
  DD, MM, YYYY: Integer;
  S: string;
begin
  Result := False;
  T := 0;
  DD := 0;
  MM := 0;
  YYYY := 0;
  S := '';
  N := 0;
  I := 1;
  while I <= Length(DateStr) + 1 do begin
    if (I > Length(DateStr)) or (DateStr[I] = '/') then begin
    Inc(N);
    if N < 4 then begin
      if (N = 3) and (Length(S) <> 4) then Exit;
      if (N < 3) and ((Length(S) < 1) or (Length(S) > 2)) then Exit;
      for II := 1 to Length(S) do 
      if not (S[II] in Digits) then Exit;
      if N = 1 then DD := StrToInt(S)
      else
      if N = 2 then MM := StrToInt(S)
      else
        YYYY := StrToInt(S);
    end
    else
      Exit;
    S := '';
  end
    else
      S := S + DateStr[I];
    Inc(I);
  end;
  if N <> 3 then Exit;
  Result := TryEncodeDate(YYYY, MM, DD, T);
end;

function MakeDateObsFromIrisDate(out DateTimeObs: TDateTime; const DateObsStr: string; UtStartStr: string = ''; Exposure: Double = 0.0): Boolean; overload;
var
  D, T: TDateTime;
begin
  Result := False;
  if Pos('/', DateObsStr) <> 0 then begin 
    if not IrisDateObs(DateObsStr, D) then Exit;
  end
  else begin
    if not DateObs(DateObsStr, D) then Exit;
  end;  
  T := 0;
  if UtStartStr <> '' then begin
    if not TimeObs(UtStartStr, T) then Exit;
  end;
  DateTimeObs := D + T;
  if (UtStartStr <> '') and (Exposure > 0) then
  DateTimeObs := DateTimeObs + Exposure / (24.0*60.0*60.0) / 2.0;
  Result := True;
end;

function MakeDateObsFromIrisDate(out DateTimeObsStr: string; const DateObsStr: string; UtStartStr: string = ''; Exposure: Double = 0.0): Boolean; overload;
var
  DateTimeObs: TDateTime;
begin
  DateTimeObsStr := '';
  Result := MakeDateObsFromIrisDate(DateTimeObs, DateObsStr, UtStartStr, Exposure);
  if Result then
    DateTimeObsStr := FormatDateTime('YYYY-MM-DD"T"hh:nn:ss', DateTimeObs);
end;

function DateObsToDateTime(const DateObsStr: string; out DateTimeObs: TDateTime): Boolean;
// '2017-11-16[T20:22:21.345]'
var
  S: string;
  P: Integer;
  T, D: TDateTime;
begin
  Result := False;
  DateTimeObs := 0;
  S := DateObsStr;
  if Length(S) < 2 then Exit;
  if S[1] = '''' then Delete(S, 1, 1);
  if S[Length(S)] = '''' then Delete(S, Length(S), 1);
  P := Pos('T', S);
  if P = 0 then begin
    Result := DateObs(S, DateTimeObs);
  end
  else begin
    if not DateObs(Copy(S, 1, P - 1), D) then Exit;
    if not TimeObs(Copy(S, P + 1, MaxInt), T) then Exit;
    DateTimeObs := D + T;
    Result := True;
  end;  
end;

end.