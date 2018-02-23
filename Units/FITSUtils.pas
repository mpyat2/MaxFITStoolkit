{*****************************************************************************}
{                                                                             }
{ FITSUtils                                                                   }
{ (c) 2017 Maksym Pyatnytskyy                                                 }
{                                                                             }
{*****************************************************************************}

{$R+}

unit FITSUtils;

// https://fits.gsfc.nasa.gov/fits_primer.html
// https://heasarc.gsfc.nasa.gov/docs/fcg/standard_dict.html

interface

uses
  Windows, SysUtils, Classes, FITSTimeUtils;

const recordEND     = 'END                                                                             ';    
const KeywordEND    = 'END';
const KeywordSIMPLE = 'SIMPLE';
const KeywordComment= 'COMMENT';
const KeywordHistory= 'HISTORY';
const KeywordHierarch='HIERARCH';
const KeywordContinue='CONTINUE';
const FITSRecordLen = Length(recordEND);
const RecordsInBlock = 36;
const FITSKeywordLen = 8;
const MinStringConstLen = 8;
const FITSNumericAlign = 30;

type
  FITSRecordType = array[1..FITSRecordLen] of Char;
  FITSRecordFile = File of FITSRecordType;
  TFITSRecordArray = array of FITSRecordType;
  TStringArray = array of string;
  TIntArray = array of Integer;

type
  TFITSValue = record
    case Integer of
      0: (A: array[0..7] of Byte);
      1: (B: Byte);
      2: (I: SmallInt);
      3: (L: LongInt);
      4: (H: Int64);     // Currently unsupported FITS value, used to store result.
      5: (S: Single);
      6: (D: Double);
      7: (E: Extended);  // Currently unsupported FITS value, used to store result.
  end;

type
  EFITSerror = class(Exception);

function PadCh(const S: string; L: Integer; Ch: Char): string;
function LeftPadCh(const S: string; L: Integer; Ch: Char): string;
function FITSQuotedValue(const S: string): string;
function StripQuotes(const S: string): string;

function IsFITS(var FITSfile: FITSRecordFile): Boolean;
function GetKeywordValue(var FITSfile: FITSRecordFile; const Keyword: string; out Value: string; RemoveComment: Boolean; TrimVal: Boolean): Integer;
function SetKeywordValue(var FITSfile: FITSRecordFile; const Keyword: string; const Value: string; AlignNumeric: Boolean; const Comment: string; CanResize: Boolean): Boolean;
function AddCommentLikeKeyword(var FITSfile: FITSRecordFile; const Keyword: string; const Value: string; CanResize: Boolean): Boolean;
procedure GetHeader(var FITSfile: FITSRecordFile; const Header: TStrings);
function GetEndPosition(var FITSfile: FITSRecordFile): Integer;
procedure RevertBytes(var FITSvalue: TFITSValue; BitPix: Integer);
function MakeFITSHeader(BitPix: Integer;
                        const Axes: TIntArray;
                        Bzero, Bscale: Double;
                        DateObs: TDateTime; DateObsComment: string;
                        Date: TDateTime;    DateComment: string;
                        Exposure: Double;   ExposureComment: string;
                        const Instrument: string;
                        const Comments: TStringArray): TFITSRecordArray;
procedure GetBitPixAndNaxis(var FITSfile: FITSRecordfile; out BitPix: Integer; out NaxisN: TIntArray);
// physical_value = BZERO + BSCALE * array_value (https://heasarc.gsfc.nasa.gov/docs/fcg/standard_dict.html)
procedure GetBscaleBzero(var FITSfile: FITSRecordFile; out Bscale, Bzero: Double);
function GetDateObs(var FITSfile: FITSRecordFile): TDateTime;
function GetExposureTime(var FITSfile: FITSRecordFile): Double;
function GetFITSimage2D(var FITSfile: FITSRecordFile; out Width, Height, BitPix: Integer; out Bscale, Bzero: Double): PChar;

implementation

procedure FileError(const S: string);
begin
  raise EFITSerror.Create(S);
end;

function FITSRecordTypeFileName(var FITSfile: FITSRecordFile): string;
begin
  Result := TFileRec(FITSfile).name;
end;

function PadCh(const S: string; L: Integer; Ch: Char): string;
begin
  Result := S;
  while Length(Result) < L do Result := Result + Ch;
end;

function LeftPadCh(const S: string; L: Integer; Ch: Char): string;
begin
  Result := S;
  while Length(Result) < L do Result := Ch + Result;
end;

function GetQuotedLen(const S: string; MaxLen: Integer): Integer;
var
  I, L: Integer;
begin
  Result := 0;
  L := Length(S);
  if L > MaxLen then L := MaxLen;
  for I := 1 to L do begin
    Inc(Result);
    if S[I] = '''' then Inc(Result);
  end;
  Inc(Result, 2);
end;

function FITSQuotedValue(const S: string): string;
var
  TempS: string;
  L: Integer;
begin
  TempS := PadCh(S, MinStringConstLen, ' ');
  L := Length(TempS);
  while GetQuotedLen(TempS, L) > SizeOf(FITSRecordType) - 10 do
    Dec(L);
  Result := QuotedStr(Copy(TempS, 1, L));
end;

function StripQuotes(const S: string): string;
var
  I: Integer;
begin
  Result := S;
  if (Result = '') or (Result[1] <> '''') then Exit;
  Delete(Result, 1, 1);
  I := 1;
  while I <= Length(Result) do begin
    if Result[I] = '''' then begin
      if I = Length(Result) then begin
        Delete(Result, Length(Result), 1);
        Exit;
      end
      else begin
        if Result[I + 1] <> '''' then begin
          Delete(Result, I + 1, MaxInt);
          Exit;
        end
        else begin
          Delete(Result, I, 1);
        end;
      end;
    end;
    Inc(I);
  end;
end;

function IsFITS(var FITSfile: FITSRecordFile): Boolean;
var
  Buf: FITSRecordType;
  Value: string;
  EndPosition: Integer;
begin
  Result := False;
  if FileSize(FITSfile) < 2 then Exit;
  // at least 2 records (however for correct FITS should be more...)
  BlockRead(FITSfile, Buf, 1);
  if Copy(Buf, 1, Length('SIMPLE  = ')) <> 'SIMPLE  = ' then Exit;
  Value := Trim(Copy(Buf, Length('SIMPLE  = ') + 1, MaxInt));
  if Copy(Value, 1, 1) <> 'T' then Exit;
  Value := Trim(Copy(Value, 2, MaxInt));
  if (Value <> '') and (Value[1] <> '/') then Exit;
  EndPosition := GetEndPosition(FITSfile);
  if EndPosition < 1 then Exit;
  Result := True;
end;

function GetEndPosition(var FITSfile: FITSRecordFile): Integer;
// Position is zero-based
var
  Buf: FITSRecordType;
  NRecord: Integer;
begin
  Result := -1;
  NRecord := -1;
  Seek(FITSfile, 0);
  while not EOF(FITSfile) do begin
    BlockRead(FITSfile, Buf, 1);
    Inc(NRecord);
    if Buf = recordEND then begin
      Result := NRecord;
      Exit;
    end;
  end;
end;

function GetHeaderRecordPosition(var FITSfile: FITSRecordFile; const Keyword: string): Integer;
// For keywords, which may occupy several nines, returns first occurence.
// Position is zero-based
var
  Buf: FITSRecordType;
  NRecord: Integer;
  S: string;
begin
  Result := -1;
  NRecord := -1;
  if Length(Keyword) > FITSKeywordLen then Exit;
  S := AnsiUpperCase(PadCh(Keyword, FITSKeywordLen, ' '));
  Seek(FITSfile, 0);
  while not EOF(FITSfile) do begin
    BlockRead(FITSfile, Buf, 1);
    Inc(NRecord);
    if Copy(Buf, 1, FITSKeywordLen) = S then begin
      Result := NRecord;
      Exit;
    end;
    if Buf = recordEND then
      Exit;
  end;
end;

function GetKeywordValue(var FITSfile: FITSRecordFile; const Keyword: string; out Value: string; RemoveComment: Boolean; TrimVal: Boolean): Integer;
// Only for keywords with '='
var
  Buf: FITSRecordType;
  P: Integer;
  N: Integer;
  I: Integer;
  Q: Boolean;
begin
  Value := '';
  Result := -1;
  Seek(FITSfile, 0);
  N := GetHeaderRecordPosition(FITSfile, Keyword);
  if N < 0 then Exit;
  Seek(FITSfile, N);
  BlockRead(FITSfile, Buf, 1);
  P := Pos('=', Buf);
  if P <> FITSKeywordLen + 1 then Exit;
  if Buf[P + 1] <> ' ' then Exit;
  Result := N;
  Value := Copy(Buf, P + 2, FITSRecordLen);
  if RemoveComment then begin
    Q := False;
    P := 0;
    for I := 1 to Length(Value) do begin
      if Value[I] <> ' ' then begin
        P := I;
        Break;
      end;
    end;
    if (P > 0) then begin
      if Value[P] = '''' then begin
        Q := True;
        Inc(P);
      end;
      while P <= Length(Value) do begin
        if (Value[P] = '/') and (not Q) then begin
          Value := Copy(Value, 1, P - 1);
          Break;
        end;
        if (Value[P] = '''') and Q then begin
          if (P < Length(Value)) and (Value[P + 1] = '''') then Inc(P) else Q := False;
        end;
        Inc(P);
      end;
    end;
    if TrimVal then Value := Trim(Value);
  end;  
end;

procedure InsertHeaderBlock(var InF: FITSRecordFile);
var
  HeaderBlockCount: Integer;
  FileImage: array of FITSRecordType;
  ENDPosition: Integer;
  N: Integer;
begin
  ENDPosition := GetEndPosition(InF);
  if ENDPosition < 0 then
    FileError('Cannot find End of Header in file ' + AnsiQuotedStr(FITSRecordTypeFileName(InF), '"'));
  N := FileSize(InF);
  SetLength(FileImage, N + RecordsInBlock);
  FillChar(FileImage[0], (N + RecordsInBlock) * FITSRecordLen, 0);
  HeaderBlockCount := (ENDPosition div RecordsInBlock) + 1;
  FillChar(FileImage[0], (HeaderBlockCount + 1) * RecordsInBlock * FITSRecordLen, ' ');
  Seek(InF, 0);
  BlockRead(InF, FileImage[0], HeaderBlockCount * RecordsInBlock);
  if (N - HeaderBlockCount * RecordsInBlock > 0) then
    BlockRead(InF, FileImage[(HeaderBlockCount + 1) * RecordsInBlock], N - HeaderBlockCount * RecordsInBlock);
  FillChar(FileImage[ENDPosition], FITSRecordLen, ' ');
  FileImage[HeaderBlockCount * RecordsInBlock] := recordEND;
  Seek(InF, 0);
  Truncate(InF);
  BlockWrite(InF, FileImage[0], N + RecordsInBlock);
end;

function SetKeywordValue(var FITSfile: FITSRecordFile; const Keyword: string; const Value: string; AlignNumeric: Boolean; const Comment: string; CanResize: Boolean): Boolean;
// Only for keywords with '='
var
  Buf: FITSRecordType;
  P: Integer;
  N: Integer;
  I: Integer;
  PC: Integer;
  AValue: string;
  AComment: string;
  AKeyword: string;
  ErrorPos: Integer;
  TempF: Double;
  TempN: LongInt;
  IsNumeric: Boolean;
begin
  Result := False;
  AKeyword := PadCh(AnsiUpperCase(Keyword), FITSKeywordLen, ' ');
  if Length(AKeyword) > FITSKeywordLen then FileError('Keyword ' + AKeyword + ' too long');
  AValue := Value;
  IsNumeric := False;
  
  if (Trim(AValue) <> '') and (AValue <> 'T') and (AValue <> 'F') then begin
    Val(Trim(AValue), TempF, ErrorPos);
    IsNumeric := ErrorPos = 0;
  end  
  else
    TempF := 0;  
  if IsNumeric and AlignNumeric then begin
    Val(Trim(AValue), TempN, ErrorPos);
    if (ErrorPos = 0) then begin
      Str(TempN : FITSNumericAlign - FITSKeywordLen - 2, AValue);
    end
    else begin
      Str(TempF : FITSNumericAlign - FITSKeywordLen - 2, AValue);
    end;  
  end;
  if not IsNumeric and (AValue <> 'T') and (AValue <> 'F') and (AValue <> '') then begin
    AValue := StripQuotes(AValue);
    AValue := FITSQuotedValue(AValue);
  end else begin
    if (AValue = 'T') or (AValue = 'F') then
      AValue := LeftPadCh(AValue, FITSNumericAlign - FITSKeywordLen - 2, ' ');
  end;
  if Comment <> '' then begin
    PC := 1;
    AComment := ' / ' + Comment;
  end  
    else begin
    PC := 0;
    AComment := '';
  end;
  Seek(FITSfile, 0);  
  N := GetHeaderRecordPosition(FITSfile, AKeyword);
  if N < 0 then begin
    // Do not create entry if value is empty.
    if AValue = '' then begin
      Result := True;
      Exit;
    end;  
    AKeyword := AKeyword + '= ';
    // Creating new entry.
    N := GetEndPosition(FITSfile);
    if N < 0 then FileError('Cannot find End of Header in file ' + AnsiQuotedStr(FITSRecordTypeFileName(FITSfile), '"'));
    if (N mod RecordsInBlock) < (RecordsInBlock - 1) then begin
      Seek(FITSfile, N);
      FillChar(Buf, SizeOf(Buf), ' ');
      BlockWrite(FITSfile, Buf, 1);
      Buf := recordEND;
      BlockWrite(FITSfile, Buf, 1);
    end
    else begin
      if not CanResize then
        FileError('Cannot create new entry: another header block is needed (CanResize flag is not set). File ' + AnsiQuotedStr(FITSRecordTypeFileName(FITSfile), '"'));
      InsertHeaderBlock(FITSfile);
    end;  
    Seek(FITSfile, N);
    FillChar(Buf, SizeOf(Buf), ' ');
    for I := 1 to Length(AKeyword) do Buf[I] := AKeyword[I];
    BlockWrite(FITSfile, Buf, 1);
  end;
  Seek(FITSfile, N);
  BlockRead(FITSfile, Buf, 1);
  P := Pos('=', Buf);
  if P <> FITSKeywordLen + 1 then Exit;
  if Buf[P + 1] <> ' ' then Exit;
  for I := P + 2 to SizeOf(Buf) do begin
    if I - (P + 2) + 1 <= Length(AValue) then begin
      Buf[I] := AValue[I - (P + 2) + 1];
    end
    else begin
      if (PC > 0) and (PC <= Length(AComment)) then begin
        Buf[I] := AComment[PC];
        Inc(PC);
      end
      else
        Buf[I] := ' ';
    end;
  end;
  Seek(FITSfile, N);
  BlockWrite(FITSfile, Buf, 1);
  Result := True;
end;

function AddCommentLikeKeyword(var FITSfile: FITSRecordFile; const Keyword: string; const Value: string; CanResize: Boolean): Boolean;
// Only for keywords with '='
var
  Buf: FITSRecordType;
  AKeyword: string;
  S: string;
  N: Integer;
  I: Integer;
begin
  Result := False;
  AKeyword := PadCh(AnsiUpperCase(Keyword), FITSKeywordLen, ' ');
  if Length(AKeyword) > FITSKeywordLen then FileError('Keyword ' + AKeyword + ' too long');
  Seek(FITSfile, 0);
  N := GetEndPosition(FITSfile);
  if N < 0 then FileError('Cannot find End of Header in file ' + AnsiQuotedStr(FITSRecordTypeFileName(FITSfile), '"'));
  if (N mod RecordsInBlock) < (RecordsInBlock - 1) then begin
    Seek(FITSfile, N);
    FillChar(Buf, SizeOf(Buf), ' ');
    BlockWrite(FITSfile, Buf, 1);
    Buf := recordEND;
    BlockWrite(FITSfile, Buf, 1);
  end
  else begin
    if not CanResize then
      FileError('Cannot create new entry: another header block is needed (CanResize flag is not set). File ' + AnsiQuotedStr(FITSRecordTypeFileName(FITSfile), '"'));
    InsertHeaderBlock(FITSfile);
  end;  
  Seek(FITSfile, N);
  FillChar(Buf, SizeOf(Buf), ' ');
  S := AKeyword + Value;  
  for I := 1 to Length(S) do begin
    if I > SizeOf(Buf) then Break;
    Buf[I] := S[I];
  end;  
  BlockWrite(FITSfile, Buf, 1);
  Result := True;
end;

procedure GetHeader(var FITSfile: FITSRecordFile; const Header: TStrings);
var
  Buf: FITSRecordType;
begin
  Header.Clear;
  if GetEndPosition(FITSfile) < 0 then
    FileError('Cannot find End of Header in file ' + AnsiQuotedStr(FITSRecordTypeFileName(FITSfile), '"'));
  Seek(FITSfile, 0);
  while not EOF(FITSfile) do begin
    BlockRead(FITSfile, Buf, 1);
    Header.Add(Buf);
    if Buf = recordEND then
      Exit;
  end;
end;

procedure RevertBytes(var FITSvalue: TFITSValue; BitPix: Integer);
var
  TempFITSvalue: TFITSvalue;
  I, N: Integer;
begin
  N := Abs(BitPix) div 8;
  for I := 0 to N - 1 do
    TempFITSvalue.A[N - 1 - I] := FITSvalue.A[I];
  FITSvalue := TempFITSvalue;
end;

procedure StrToFITSRecord(const S: string; out FITSRecord: FITSRecordType);
var
  L: Integer;
begin
  FillChar(FITSRecord, SizeOf(FITSRecord), ' ');
  L := Length(S);
  if L > SizeOf(FITSRecord) then L := SizeOf(FITSRecord);
  Move(S[1], FITSRecord, L);
end;
  
function MakeFITSHeader(BitPix: Integer;
                        const Axes: TIntArray;
                        Bzero, Bscale: Double;
                        DateObs: TDateTime; DateObsComment: string;
                        Date: TDateTime;    DateComment: string;
                        Exposure: Double;   ExposureComment: string;
                        const Instrument: string;
                        const Comments: TStringArray): TFITSRecordArray;
var
  I, N: Integer;
  TempS: string;
begin
  N := 0;

  SetLength(Result, N + 1);
  StrToFITSRecord('SIMPLE  =                    T', Result[N]);
  Inc(N);

  SetLength(Result, N + 1);
  Str(BitPix:FITSNumericAlign - FITSKeywordLen - 2, TempS);
  StrToFITSRecord('BITPIX  = ' + TempS, Result[N]);
  Inc(N);

  SetLength(Result, N + 1);
  Str(Length(Axes):FITSNumericAlign - FITSKeywordLen - 2, TempS);
  StrToFITSRecord('NAXIS   = ' + TempS, Result[N]);
  Inc(N);

  for I := 0 to Length(Axes) - 1 do begin
    SetLength(Result, N + 1);
    Str(Axes[I]:FITSNumericAlign - FITSKeywordLen - 2, TempS);
    StrToFITSRecord(PadCh('NAXIS' + IntToStr(I + 1), 8, ' ') + '= ' + TempS, Result[N]);
    Inc(N);
  end;

  if Bzero <> 0 then begin
    SetLength(Result, N + 1);
    Str(Bzero:FITSNumericAlign - FITSKeywordLen - 2, TempS);
    StrToFITSRecord('BZERO   = ' + TempS, Result[N]);
    Inc(N);
  end;

  if Bscale <> 1 then begin
    SetLength(Result, N + 1);
    Str(Bscale:FITSNumericAlign - FITSKeywordLen - 2, TempS);
    StrToFITSRecord('BSCALE  = ' + TempS, Result[N]);
    Inc(N);
  end;

  if DateObs <> 0 then begin
    SetLength(Result, N + 1);
    TempS := 'DATE-OBS= ' + '''' + FormatDateTime('YYYY-MM-DD"T"hh:nn:ss', DateObs) + '''';
    if DateObsComment <> '' then
      TempS := TempS + ' / ' + DateObsComment;
    StrToFITSRecord(TempS, Result[N]);
    Inc(N);
  end
  else begin
    SetLength(Result, N + 1);
    StrToFITSRecord('COMMENT **** DATE-OBS was not specified!', Result[N]);
    Inc(N);
  end;

  if Instrument <> '' then begin
    SetLength(Result, N + 1);
    TempS := FITSQuotedValue(Instrument);
    StrToFITSRecord('INSTRUME= ' + TempS, Result[N]);
    Inc(N);
  end;

  if Exposure > 0 then begin
    SetLength(Result, N + 1);
    Str(Exposure:20, TempS);
    TempS := 'EXPTIME = ' + TempS;
    if ExposureComment <> '' then
      TempS := TempS + ' / ' + ExposureComment;
    StrToFITSRecord(TempS, Result[N]);
    Inc(N);
  end;

  if Date <> 0 then begin
    SetLength(Result, N + 1);
    TempS := 'DATE    = ' + '''' + FormatDateTime('YYYY-MM-DD"T"hh:nn:ss.zzz', Date) + '''';
    if DateComment <> '' then
      TempS := TempS + ' / ' + DateComment;
    StrToFITSRecord(TempS, Result[N]);
    Inc(N);
  end;

  for I := 0 to Length(Comments) - 1 do begin
    SetLength(Result, N + 1);
    StrToFITSRecord('COMMENT ' + Comments[I], Result[N]);
    Inc(N);
  end;

  SetLength(Result, N + 1);
  Result[N] := recordEND;
  Inc(N);

  N := N mod RecordsInBlock;
  if N > 0 then begin
    for I := 1 to RecordsInBlock - N do begin
      SetLength(Result, N + 1);
      FillChar(Result[N], SizeOf(FITSRecordType), ' ');
      Inc(N);
    end;
  end;
end;

procedure GetBitPixAndNaxis(var FITSfile: FITSRecordfile; out BitPix: Integer; out NaxisN: TIntArray);
var
  Value: string;
  Naxis: Integer;
  N: Integer;
  I: Integer;
  ErrorPos: Integer;
  FITSfileName: string;
begin
  BitPix := 0;
  NaxisN := nil;
  FITSfileName := FITSRecordTypeFileName(FITSfile);
  if GetKeywordValue(FITSfile, 'BITPIX', Value, True, True) < 0 then
    FileError('Cannot get value of BITPIX. File ' + AnsiQuotedStr(FITSfileName, '"'));
  Val(Value, BitPix, ErrorPos);
  if (ErrorPos <> 0) or (BitPix = 0) or (Abs(BitPix) mod 8 <> 0) then
    FileError('BITPIX has invalid value. File ' + AnsiQuotedStr(FITSfileName, '"'));
  if GetKeywordValue(FITSfile, 'NAXIS', Value, True, True) < 0 then
    FileError('Cannot get value of NAXIS. File ' + AnsiQuotedStr(FITSfileName, '"'));
  Val(Value, Naxis, ErrorPos);
  if (ErrorPos <> 0) or (Naxis < 0) or (Naxis > 999) then
    FileError('NAXIS has invalid value. File ' + AnsiQuotedStr(FITSfileName, '"'));
  if Naxis = 0 then Exit;
  SetLength(NaxisN, Naxis);
  for I := 0 to Naxis - 1 do begin
    if GetKeywordValue(FITSfile, 'NAXIS' + IntToStr(I + 1), Value, True, True) < 0 then
      FileError('Cannot get value of NAXIS' + IntToStr(I + 1) + '. File ' + AnsiQuotedStr(FITSfileName, '"'));
    Val(Value, N, ErrorPos);
    if (ErrorPos <> 0) or (N < 0) then
      FileError('NAXIS' + IntToStr(I + 1) + ' has invalid value. File ' + AnsiQuotedStr(FITSfileName, '"'));
    NaxisN[I] := N;
  end;
end;

procedure GetBscaleBzero(var FITSfile: FITSRecordFile; out Bscale, Bzero: Double);
var
  S: string;
  ErrorPos: Integer;
begin
  Bscale := 1;
  Bzero := 0;
  if (GetKeywordValue(FITSfile, 'BSCALE', S, True, True) >= 0) and (S <> '') then begin
    Val(S, Bscale, ErrorPos);
    if ErrorPos <> 0 then
      FileError('Invalid BSCALE value in file ' + AnsiQuotedStr(FITSRecordTypeFileName(FITSfile), '"'));
  end;
  if (GetKeywordValue(FITSfile, 'BZERO', S, True, True) >= 0) and (S <> '') then begin
    Val(S, Bzero, ErrorPos);
    if ErrorPos <> 0 then
      FileError('Invalid BZERO value in file ' + AnsiQuotedStr(FITSRecordTypeFileName(FITSfile), '"'));
  end;
end;

function GetDateObs(var FITSfile: FITSRecordFile): TDateTime;
var
  DateObsStr: string;
  TimeObsStr: string;
  TimeObsKeyUsed: Boolean;
  UtStartKeyUsed: Boolean;
  P: Integer;
begin
  Result := 0;
  DateObsStr := '';
  TimeObsStr := '';
  TimeObsKeyUsed := False;
  UtStartKeyUsed := False;
  if (GetKeywordValue(FITSFile, 'DATE-OBS', DateObsStr, True, True) < 0) or (DateObsStr = '') then
    FileError('DATE-OBS keyword is not found or is empty. File: ' + AnsiQuotedStr(FITSRecordTypeFileName(FITSfile), '"'));
  DateObsStr := StripQuotes(DateObsStr);
  P := Pos('T', DateObsStr);
  if P <> 0 then begin
    TimeObsStr := Copy(DateObsStr, P + 1, MaxInt);
    DateObsStr := Copy(DateObsStr, 1, P - 1);
  end;
  if TimeObsStr = '' then begin
    // There is no time part in DATE-OBS.
    // Try to get time from TIME-OBS
    TimeObsKeyUsed := (GetKeywordValue(FITSFile, 'TIME-OBS', TimeObsStr, True, True) >= 0) and (TimeObsStr <> '');
    if not TimeObsKeyUsed then begin
      // There is no TIME-OBS...
      // Try to get time from UT-START (IRIS-specific)
      UtStartKeyUsed := (GetKeywordValue(FITSFile, 'UT-START', TimeObsStr, True, True) >= 0) and (TimeObsStr <> '');
      if not UtStartKeyUsed then
        FileError('DATE-OBS value does not contain time part and there is no TIME-OBS nor UT-START keywords. File: ' + AnsiQuotedStr(FITSRecordTypeFileName(FITSfile), '"'));
    end;
    TimeObsStr := StripQuotes(TimeObsStr);
  end;

  if not MakeDateObsFromStrings(Result, DateObsStr, TimeObsStr) then
    FileError('Cannot determine date/time of observation. Date part = ' + DateObsStr + '; Time part = ' + TimeObsStr + '. File: ' + AnsiQuotedStr(FITSRecordTypeFileName(FITSfile), '"'));
end;

function GetExposureTime(var FITSfile: FITSRecordFile): Double;
var
  ExpTimeStr: string;
  ErrorPos: Integer;
begin
  Result := 0;
  if (GetKeywordValue(FITSFile, 'EXPTIME', ExpTimeStr, True, True) < 0) or (ExpTimeStr = '') then
    GetKeywordValue(FITSFile, 'EXPOSURE', ExpTimeStr, True, True);
  if (ExpTimeStr <> '') then begin
    Val(ExpTimeStr, Result, ErrorPos);
    if (ErrorPos <> 0) or (Result < 0) then
      FileError('EXPTIME/EXPOSURE keyword does exist however it contains invalid value: ' + ExpTimeStr + '. File: ' + AnsiQuotedStr(FITSRecordTypeFileName(FITSfile), '"'));
  end;
end;

function GetFITSimage2D(var FITSfile: FITSRecordFile; out Width, Height, BitPix: Integer; out Bscale, Bzero: Double): PChar;
var
  I, N, NblocksInHeader, StartOfImage, BytePix, NImagePoints, NrecordsToRead, ImageMemSize: Integer;
  NaxisN: TIntArray;
begin
  N := GetEndPosition(FITSfile);
  if N < 0 then
    FileError('Cannot find End of Header in file ' + AnsiQuotedStr(FITSRecordTypeFileName(FITSfile), '"'));
  GetBscaleBzero(FITSfile, Bscale, Bzero);
  NblocksInHeader := N div RecordsInBlock + 1;
  StartOfImage := NblocksInHeader * RecordsInBlock;
  GetBitPixAndNaxis(FITSfile, BitPix, NaxisN);
  if Length(NAxisN) <> 2 then
    FileError('2D FITS expected; NAXIS = ' + IntToStr(Length(NAxisN)) + ' in file ' + AnsiQuotedStr(FITSRecordTypeFileName(FITSfile), '"'));
  Width := NAxisN[0];
  Height := NAxisN[1];
  BytePix := Abs(BitPix) div 8;
  NImagePoints := 1;
  for I := 0 to Length(NaxisN) - 1 do
    NImagePoints := NImagePoints * NaxisN[I];
  NrecordsToRead := (NImagePoints * BytePix - 1) div FITSRecordLen + 1;
  ImageMemSize := NrecordsToRead * FITSRecordLen;
  GetMem(Result, ImageMemSize);
  try
    FillChar(Result^, ImageMemSize, 0);
    Seek(FITSfile, StartOfImage);
    BlockRead(FITSfile, Result^, NrecordsToRead);
  except
    FreeMem(Result);
    Result := nil;
    raise;
  end;
end;

end.
