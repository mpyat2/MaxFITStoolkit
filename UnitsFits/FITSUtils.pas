{*****************************************************************************}
{                                                                             }
{ FITSUtils                                                                   }
{ (c) 2017-2018 Maksym Pyatnytskyy                                            }
{                                                                             }
{ This program is distributed                                                 }
{ WITHOUT ANY WARRANTY; without even the implied warranty of                  }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                        }
{                                                                             }
{*****************************************************************************}

{$R+}{$Q+} // can be commented out in release mode. Should be BEFORE {$INCLUDE FITSUtils.inc}!

{$MODE DELPHI}
{$INCLUDE FITSUtils.inc}

unit FITSUtils;

// https://fits.gsfc.nasa.gov/fits_primer.html
// https://heasarc.gsfc.nasa.gov/docs/fcg/standard_dict.html

interface

uses
  SysUtils, Classes, StrUtils, Math, FITScompatibility, FITSTimeUtils;

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

function PadCh(const S: string; L: Integer; Ch: Char): string; inline;
function LeftPadCh(const S: string; L: Integer; Ch: Char): string; inline;
function FITSQuotedValue(const S: string): string;
function StripQuotes(const S: string): string;

// FPC 64 handles VAL errors incorrectly. Replacement routines:
function GetDouble(const S: string; out V: Double): Boolean; inline;
function GetInt(const S: string; out V: Integer): Boolean; inline;

function IsFITS(var FITSfile: FITSRecordFile): Boolean;
function GetKeywordValue(var FITSfile: FITSRecordFile; const Keyword: string; out Value: string; RemoveComment: Boolean; TrimVal: Boolean): Int64;
function SetKeywordValue(var FITSfile: FITSRecordFile; const Keyword: string; const Value: string; AlignNumeric: Boolean; const Comment: string; CanResize: Boolean): Boolean;
function AddCommentLikeKeyword(var FITSfile: FITSRecordFile; const Keyword: string; const Value: string; CanResize: Boolean): Boolean;
procedure GetHeader(var FITSfile: FITSRecordFile; const Header: TStrings);
function GetEndPosition(var FITSfile: FITSRecordFile): Int64;
procedure RevertBytes(var FITSvalue: TFITSValue; BitPix: Integer);
procedure StrToFITSRecord(const S: string; out FITSRecord: FITSRecordType);
function MakeFITSHeader(BitPix: Integer;
                        const Axes: TIntArray;
                        Bzero, Bscale: Double;
                        DateObs: TDateTime; DateObsComment: string;
                        Date: TDateTime;    DateComment: string;
                        Exposure: Double;   ExposureComment: string;
                        const ObjectName: string;
                        const Telescope: string;
                        const Instrument: string;
                        const Comments: TStringArray): TFITSRecordArray;
procedure GetBitPixAndNaxis(var FITSfile: FITSRecordfile; out BitPix: Integer; out NaxisN: TIntArray);
// physical_value = BZERO + BSCALE * array_value (https://heasarc.gsfc.nasa.gov/docs/fcg/standard_dict.html)
procedure GetBscaleBzero(var FITSfile: FITSRecordFile; out Bscale, Bzero: Double);
function GetDateObs(var FITSfile: FITSRecordFile): TDateTime;
function GetExposureTime(var FITSfile: FITSRecordFile): Double;
// Returns padded 2D or 3D image.
function GetFITSimage(var FITSfile: FITSRecordFile;
                      out Width, Height, Layers, BitPix: Integer;
                      out Bscale, Bzero: Double): PChar;

implementation

procedure FITSError(const S: string);
begin
  raise EFITSerror.Create(S);
end;

function FITSRecordTypeFileName(var FITSfile: FITSRecordFile): string; inline;
begin
  Result := TFileRec(FITSfile).name;
end;

function PadCh(const S: string; L: Integer; Ch: Char): string; inline;
begin
  Result := AddCharR(Ch, S, L);
end;

function LeftPadCh(const S: string; L: Integer; Ch: Char): string; inline;
begin
  Result := AddChar(Ch, S, L);
end;

function GetQuotedLen(const S: string; MaxLen: Integer): SizeInt;
var
  I, L: SizeInt;
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
  L: SizeInt;
begin
  TempS := PadCh(S, MinStringConstLen, ' ');
  L := Length(TempS);
  while GetQuotedLen(TempS, L) > SizeOf(FITSRecordType) - FITSKeywordLen - 2 do // length of keywors + position for equals sign + space after equals sign.
    Dec(L);
  Result := QuotedStr(Copy(TempS, 1, L));
end;

// Do not use AnsiDequotedStr: it remains quotes if length of a string <=2
function StripQuotes(const S: string): string;
var
  I: SizeInt;
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
          Delete(Result, I, MaxInt);
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

function GetDouble(const S: string; out V: Double): Boolean; inline;
var
  Code: Integer;
  tempV: Extended;
begin
  Result := False;
  V := 0;
  Val(S, tempV, Code);
  if (Code = 0) and (tempV < MaxDouble) and (tempV > -MaxDouble) then begin
    V := tempV;
    Result := True;
  end;
end;

function GetInt(const S: string; out V: Integer): Boolean; inline;
var
  Code: Integer;
  tempV: Int64;
begin
  Result := False;
  V := 0;
  Val(S, tempV, Code);
  if (Code = 0) and (tempV <= High(Integer)) and (tempV >= Low(Integer)) then begin
    V := tempV;
    Result := True;
  end;
end;

function IsFITS(var FITSfile: FITSRecordFile): Boolean;
var
  Buf: FITSRecordType;
  Value: string;
  EndPosition: Int64;
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

function GetEndPosition(var FITSfile: FITSRecordFile): Int64;
// Position is zero-based
var
  Buf: FITSRecordType;
  NRecord: Int64;
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

function GetHeaderRecordPosition(var FITSfile: FITSRecordFile; const Keyword: string): Int64;
// For keywords, which may occupy several nines, returns first occurence.
// Position is zero-based
var
  Buf: FITSRecordType;
  NRecord: Int64;
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

function GetKeywordValue(var FITSfile: FITSRecordFile; const Keyword: string; out Value: string; RemoveComment: Boolean; TrimVal: Boolean): Int64;
// Only for keywords with '='
var
  Buf: FITSRecordType;
  FilePosition: Int64;
  P: Integer;
  I: Integer;
  Q: Boolean;
begin
  Value := '';
  Result := -1;
  Seek(FITSfile, 0);
  FilePosition := GetHeaderRecordPosition(FITSfile, Keyword);
  if FilePosition < 0 then Exit;
  Seek(FITSfile, FilePosition);
  BlockRead(FITSfile, Buf, 1);
  P := Pos('=', Buf);
  if P <> FITSKeywordLen + 1 then Exit;
  if Buf[P + 1] <> ' ' then Exit;
  Result := FilePosition;
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

procedure InsertHeaderBlock(var FITSfile: FITSRecordFile);
var
  FileImage: array of FITSRecordType;
  HeaderBlockCount: Int64;
  ENDPosition: Int64;
  OldFileSize: Int64;
begin
  ENDPosition := GetEndPosition(FITSfile);
  if ENDPosition < 0 then
    FITSError('Cannot find End of Header in file ' + AnsiQuotedStr(FITSRecordTypeFileName(FITSfile), '"'));
  OldFileSize := FileSize(FITSfile);
  SetLength(FileImage, OldFileSize + RecordsInBlock);
  FillChar(FileImage[0], (OldFileSize + RecordsInBlock) * FITSRecordLen, 0);
  HeaderBlockCount := (ENDPosition div RecordsInBlock) + 1;
  FillChar(FileImage[0], (HeaderBlockCount + 1) * RecordsInBlock * FITSRecordLen, ' ');
  Seek(FITSfile, 0);
  BlockRead(FITSfile, FileImage[0], HeaderBlockCount * RecordsInBlock);
  if (OldFileSize - HeaderBlockCount * RecordsInBlock > 0) then
    BlockRead(FITSfile, FileImage[(HeaderBlockCount + 1) * RecordsInBlock], OldFileSize - HeaderBlockCount * RecordsInBlock);
  FillChar(FileImage[ENDPosition], FITSRecordLen, ' ');
  FileImage[HeaderBlockCount * RecordsInBlock] := recordEND;
  Seek(FITSfile, 0);
  Truncate(FITSfile);
  BlockWrite(FITSfile, FileImage[0], OldFileSize + RecordsInBlock);
end;

function SetKeywordValue(var FITSfile: FITSRecordFile; const Keyword: string; const Value: string; AlignNumeric: Boolean; const Comment: string; CanResize: Boolean): Boolean;
// Only for keywords with '='
var
  Buf: FITSRecordType;
  PosInFile: Int64;
  P: Integer;
  I: Integer;
  PC: Integer;
  AValue: string;
  AComment: string;
  AKeyword: string;
  TempF: Double;
  TempN: Integer;
  IsNumeric: Boolean;
begin
  Result := False;
  AKeyword := PadCh(AnsiUpperCase(Keyword), FITSKeywordLen, ' ');
  if Length(AKeyword) > FITSKeywordLen then FITSError('Keyword ' + AKeyword + ' too long');
  AValue := Value;
  IsNumeric := False;
  
  if (Trim(AValue) <> '') and (AValue <> 'T') and (AValue <> 'F') then
    IsNumeric := GetDouble(Trim(AValue), TempF)
  else
    TempF := 0;  
  if IsNumeric and AlignNumeric then begin
    if GetInt(Trim(AValue), TempN) then begin
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
  PosInFile := GetHeaderRecordPosition(FITSfile, AKeyword);
  if PosInFile < 0 then begin
    // Do not create entry if value is empty.
    if AValue = '' then begin
      Result := True;
      Exit;
    end;  
    AKeyword := AKeyword + '= ';
    // Creating new entry.
    PosInFile := GetEndPosition(FITSfile);
    if PosInFile < 0 then FITSError('Cannot find End of Header in file ' + AnsiQuotedStr(FITSRecordTypeFileName(FITSfile), '"'));
    if (PosInFile mod RecordsInBlock) < (RecordsInBlock - 1) then begin
      Seek(FITSfile, PosInFile);
      FillChar(Buf, SizeOf(Buf), ' ');
      BlockWrite(FITSfile, Buf, 1);
      Buf := recordEND;
      BlockWrite(FITSfile, Buf, 1);
    end
    else begin
      if not CanResize then
        FITSError('Cannot create new entry: another header block is needed (CanResize flag is not set). File ' + AnsiQuotedStr(FITSRecordTypeFileName(FITSfile), '"'));
      InsertHeaderBlock(FITSfile);
    end;  
    Seek(FITSfile, PosInFile);
    FillChar(Buf, SizeOf(Buf), ' ');
    for I := 1 to Length(AKeyword) do Buf[I] := AKeyword[I];
    BlockWrite(FITSfile, Buf, 1);
  end;
  Seek(FITSfile, PosInFile);
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
  Seek(FITSfile, PosInFile);
  BlockWrite(FITSfile, Buf, 1);
  Result := True;
end;

function AddCommentLikeKeyword(var FITSfile: FITSRecordFile; const Keyword: string; const Value: string; CanResize: Boolean): Boolean;
// Only for keywords with '='
var
  Buf: FITSRecordType;
  PosInFile: Int64;
  AKeyword: string;
  S: string;
  I: Integer;
begin
  Result := False;
  AKeyword := PadCh(AnsiUpperCase(Keyword), FITSKeywordLen, ' ');
  if Length(AKeyword) > FITSKeywordLen then FITSError('Keyword ' + AKeyword + ' too long');
  Seek(FITSfile, 0);
  PosInFile := GetEndPosition(FITSfile);
  if PosInFile < 0 then FITSError('Cannot find End of Header in file ' + AnsiQuotedStr(FITSRecordTypeFileName(FITSfile), '"'));
  if (PosInFile mod RecordsInBlock) < (RecordsInBlock - 1) then begin
    Seek(FITSfile, PosInFile);
    FillChar(Buf, SizeOf(Buf), ' ');
    BlockWrite(FITSfile, Buf, 1);
    Buf := recordEND;
    BlockWrite(FITSfile, Buf, 1);
  end
  else begin
    if not CanResize then
      FITSError('Cannot create new entry: another header block is needed (CanResize flag is not set). File ' + AnsiQuotedStr(FITSRecordTypeFileName(FITSfile), '"'));
    InsertHeaderBlock(FITSfile);
  end;  
  Seek(FITSfile, PosInFile);
  FillChar(Buf, SizeOf(Buf), ' ');
  S := Copy(AKeyword + Value, 1, SizeOf(Buf));
  for I := 1 to Length(S) do begin
    if I > SizeOf(Buf) then Break;
    Buf[I] := S[I];
  end;  
  BlockWrite(FITSfile, Buf, 1);
  Result := True;
end;

procedure GetHeader(var FITSfile: FITSRecordFile; const Header: TStrings);
// It is practically impossible that number of keywords in a header could be > MaxInt.
var
  Buf: FITSRecordType;
begin
  Header.Clear;
  if GetEndPosition(FITSfile) < 0 then
    FITSError('Cannot find End of Header in file ' + AnsiQuotedStr(FITSRecordTypeFileName(FITSfile), '"'));
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
  L: SizeInt;
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
                        const ObjectName: string;
                        const Telescope: string;
                        const Instrument: string;
                        const Comments: TStringArray): TFITSRecordArray;
var
  I, N, N2: Integer;
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

  if (Bzero <> 0) or (Bscale <> 1) then begin
    SetLength(Result, N + 1);
    Str(Bzero:FITSNumericAlign - FITSKeywordLen - 2, TempS);
    StrToFITSRecord('BZERO   = ' + TempS, Result[N]);
    Inc(N);
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

  if ObjectName <> '' then begin
    SetLength(Result, N + 1);
    TempS := FITSQuotedValue(ObjectName);
    StrToFITSRecord('OBJECT  = ' + TempS, Result[N]);
    Inc(N);
  end;

  if Telescope <> '' then begin
    SetLength(Result, N + 1);
    TempS := FITSQuotedValue(Telescope);
    StrToFITSRecord('TELESCOP= ' + TempS, Result[N]);
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

  // Padding to FITS block size
  N2 := N mod RecordsInBlock;
  if N2 > 0 then begin
    for I := 1 to RecordsInBlock - N2 do begin
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
  ValidNumber: Boolean;
  FITSfileName: string;
begin
  BitPix := 0;
  NaxisN := nil;
  FITSfileName := FITSRecordTypeFileName(FITSfile);

  if GetKeywordValue(FITSfile, 'BITPIX', Value, True, True) < 0 then
    FITSError('Cannot get value of BITPIX. File ' + AnsiQuotedStr(FITSfileName, '"'));
  ValidNumber := GetInt(Value, BitPix);
  if (not ValidNumber) or (BitPix = 0) or (Abs(BitPix) mod 8 <> 0) then
    FITSError('BITPIX has invalid value. File ' + AnsiQuotedStr(FITSfileName, '"'));

  if GetKeywordValue(FITSfile, 'NAXIS', Value, True, True) < 0 then
    FITSError('Cannot get value of NAXIS. File ' + AnsiQuotedStr(FITSfileName, '"'));
  ValidNumber := GetInt(Value, Naxis);
  if (not ValidNumber) or (Naxis < 1) or (Naxis > 999) then
    FITSError('NAXIS has invalid or unsupported value. File ' + AnsiQuotedStr(FITSfileName, '"'));

  SetLength(NaxisN, Naxis);
  for I := 0 to Naxis - 1 do begin
    if GetKeywordValue(FITSfile, 'NAXIS' + IntToStr(I + 1), Value, True, True) < 0 then
      FITSError('Cannot get value of NAXIS' + IntToStr(I + 1) + '. File ' + AnsiQuotedStr(FITSfileName, '"'));
    ValidNumber := GetInt(Value, N);
    if (not ValidNumber) or (N < 1) then
      FITSError('NAXIS' + IntToStr(I + 1) + ' has invalid or unsupported value. File ' + AnsiQuotedStr(FITSfileName, '"'));
    NaxisN[I] := N;
  end;
end;

procedure GetBscaleBzero(var FITSfile: FITSRecordFile; out Bscale, Bzero: Double);
var
  S: string;
  ValidNumber: Boolean;
begin
  Bscale := 1;
  Bzero := 0;
  if (GetKeywordValue(FITSfile, 'BSCALE', S, True, True) >= 0) and (S <> '') then begin
    ValidNumber := GetDouble(S, Bscale);
    if not ValidNumber then
      FITSError('Invalid BSCALE value in file ' + AnsiQuotedStr(FITSRecordTypeFileName(FITSfile), '"'));
  end;
  if (GetKeywordValue(FITSfile, 'BZERO', S, True, True) >= 0) and (S <> '') then begin
    ValidNumber := GetDouble(S, Bzero);
    if not ValidNumber then
      FITSError('Invalid BZERO value in file ' + AnsiQuotedStr(FITSRecordTypeFileName(FITSfile), '"'));
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
    FITSError('DATE-OBS keyword is not found or is empty. File: ' + AnsiQuotedStr(FITSRecordTypeFileName(FITSfile), '"'));
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
        FITSError('DATE-OBS value does not contain time part and there is no TIME-OBS nor UT-START keywords. File: ' + AnsiQuotedStr(FITSRecordTypeFileName(FITSfile), '"'));
    end;
    TimeObsStr := StripQuotes(TimeObsStr);
  end;

  if not MakeDateObsFromStrings(Result, DateObsStr, TimeObsStr) then
    FITSError('Cannot determine date/time of observation. Date part = ' + DateObsStr + '; Time part = ' + TimeObsStr + '. File: ' + AnsiQuotedStr(FITSRecordTypeFileName(FITSfile), '"'));
end;

function GetExposureTime(var FITSfile: FITSRecordFile): Double;
// Returns 0 if exposure is not defined
var
  ExpTimeStr: string;
  FilePos1, FilePos2: Int64;
  Exp1, Exp2: Double;
  ValidNumber: Boolean;
begin
  Result := 0;

  FilePos1 := GetKeywordValue(FITSFile, 'EXPTIME', ExpTimeStr, True, True);
  if (FilePos1 >= 0) and (ExpTimeStr <> '') then begin
    ValidNumber := GetDouble(ExpTimeStr, Exp1);
    if (not ValidNumber) or (Exp1 < 0) then
      FITSError('EXPTIME contains invalid value: ' + ExpTimeStr + '. File: ' + AnsiQuotedStr(FITSRecordTypeFileName(FITSfile), '"'));
  end;

  FilePos2 := GetKeywordValue(FITSFile, 'EXPOSURE', ExpTimeStr, True, True);
  if (FilePos2 >= 0) and (ExpTimeStr <> '') then begin
    ValidNumber := GetDouble(ExpTimeStr, Exp2);
    if (not ValidNumber) or (Exp2 < 0) then
      FITSError('EXPOSURE contains invalid value: ' + ExpTimeStr + '. File: ' + AnsiQuotedStr(FITSRecordTypeFileName(FITSfile), '"'));
  end;

  if (FilePos1 >= 0) and (FilePos2 >= 0) and (Exp1 <> Exp2) then
     FITSError('EXPTIME and EXPOSURE have inconsistent values. File: ' + AnsiQuotedStr(FITSRecordTypeFileName(FITSfile), '"'));

  if FilePos1 >= 0 then
     Result := Exp1
  else
  if FilePos2 >= 0 then
     Result := Exp2;
end;

// Returns padded 2D or 3D image.
function GetFITSimage(var FITSfile: FITSRecordFile;
                      out Width, Height, Layers, BitPix: Integer;
                      out Bscale, Bzero: Double): PChar;
var
  EndPosition: Int64;
  NblocksInHeader: Int64;
  StartOfImage: Int64;
  NImagePoints: Int64;
  NrecordsToRead: Int64;
  ImageMemSize: PtrUInt;
  BytePix: Integer;
  I: Integer;
  NaxisN: TIntArray;
begin
  EndPosition := GetEndPosition(FITSfile);
  if EndPosition < 0 then
    FITSError('Cannot find End of Header in file ' + AnsiQuotedStr(FITSRecordTypeFileName(FITSfile), '"'));
  NblocksInHeader := EndPosition div RecordsInBlock + 1;
  StartOfImage := NblocksInHeader * RecordsInBlock;
  GetBscaleBzero(FITSfile, Bscale, Bzero);
  GetBitPixAndNaxis(FITSfile, BitPix, NaxisN);
  if (Length(NAxisN) <> 2) and (Length(NAxisN) <> 3) then
    FITSError('2D or 3D FITS expected; NAXIS = ' + IntToStr(Length(NAxisN)) + ' in file ' + AnsiQuotedStr(FITSRecordTypeFileName(FITSfile), '"'));
  Width := NAxisN[0];
  Height := NAxisN[1];
  if Length(NAxisN) = 3 then Layers := NAxisN[2] else Layers := 1;
  BytePix := Abs(BitPix) div 8;

  NImagePoints := 1;
{$IFNDEF range_check}{$R+}{$ENDIF}
{$IFNDEF overflow_check}{$Q+}{$ENDIF}
  for I := 0 to Length(NaxisN) - 1 do
    NImagePoints := NImagePoints * NaxisN[I];
  NrecordsToRead := (NImagePoints * BytePix - 1) div FITSRecordLen + 1;
  ImageMemSize := NrecordsToRead * FITSRecordLen;
{$IFNDEF range_check}{$R-}{$ENDIF}
{$IFNDEF overflow_check}{$Q-}{$ENDIF}

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
