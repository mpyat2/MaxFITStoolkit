{$R+}

unit FITSUtils;

// https://fits.gsfc.nasa.gov/fits_primer.html

interface

uses
  Windows, SysUtils, CmdObj, CmdObjStdSwitches, Classes;

const recordEND     = 'END                                                                             ';    
const KeywordEND    = 'END';
const KeywordSimple = 'SIMPLE';
const KeywordComment= 'COMMENT';
const KeywordHistory= 'HISTORY';
const KeywordHierarch='HIERARCH';
const FITSRecordLen = Length(recordEND);
const RecordsInBlock = 36;
const FITSKeywordLen = 8;
const MinStringConstLen = 8;
const FITSNumericAlign = 30;

type
  FITSRecordType = array[1..FITSRecordLen] of Char;
  FITSRecordFile = File of FITSRecordType;
  
function PadCh(const S: string; L: Integer; Ch: Char): string;
function LeftPadCh(const S: string; L: Integer; Ch: Char): string;
function StripQuotes(const S: string): string;

function IsFITS(const FITSfileName: string): Boolean;
function GetKeywordValue({$IFDEF FPC} var {$ELSE} const {$ENDIF} FITSfile: FITSRecordFile; const Keyword: string; out Value: string; RemoveComment: Boolean; TrimVal: Boolean): Integer;
function SetKeywordValue({$IFDEF FPC} var {$ELSE} const {$ENDIF} FITSfile: FITSRecordFile; const FITSfileName: string; const Keyword: string; const Value: string; AlignNumeric: Boolean; const Comment: string; CanResize: Boolean): Boolean;
function AddCommentLikeKeyword({$IFDEF FPC} var {$ELSE} const {$ENDIF} FITSfile: FITSRecordFile; const FITSfileName: string; const Keyword: string; const Value: string; CanResize: Boolean): Boolean;
procedure GetHeader(const FITSfileName: string; const Header: TStrings);
function GetEndPosition({$IFDEF FPC} var {$ELSE} const {$ENDIF} FITSfile: FITSRecordFile): Integer;

implementation

procedure FileError(S: string);
begin
  raise Exception.Create(S);
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
        if S[I + 1] <> '''' then begin
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

function GetEndPosition({$IFDEF FPC} var {$ELSE} const {$ENDIF} FITSfile: FITSRecordFile): Integer;
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
    if string(Buf) = recordEND then begin
      Result := NRecord;
      Exit;
    end;
  end;
end;

function IsFITS(const FITSfileName: string): Boolean;
var
  FITSfile: FITSRecordFile;
  Value: string;
  EndPosition: Integer;
begin
  Result := False;
  AssignFile(FITSfile, FITSfileName);
  Reset(FITSfile);
  try
    if FileSize(FITSfile) < 2 then Exit;
    // at least 2 records!
    if GetKeywordValue(FITSfile, KeywordSimple, Value, True, True) <> 0 then Exit;
    if (Value <> 'T') and (Value <> 'F') then Exit;
    EndPosition := GetEndPosition(FITSfile);
    if EndPosition < 1 then Exit;
  finally
    CloseFile(FITSfile);
  end;
  Result := True;
end;

function GetHeaderRecordPosition({$IFDEF FPC} var {$ELSE} const {$ENDIF} FITSfile: FITSRecordFile; const Keyword: string): Integer;
// For keywords, which may occupy several nines, returns first occurence.
// Position is zero-based
var
  Buf: FITSRecordType;
  NRecord: Integer;
  S: string;
begin
  Result := -1;
  NRecord := -1;
  S := AnsiUpperCase(PadCh(Keyword, FITSKeywordLen, ' '));
  Seek(FITSfile, 0);
  while not EOF(FITSfile) do begin
    BlockRead(FITSfile, Buf, 1);
    Inc(NRecord);
    if Copy(Buf, 1, FITSKeywordLen) = S then begin
      Result := NRecord;
      Exit;
    end;
    if string(Buf) = recordEND then Exit;
  end;
end;

function GetKeywordValue({$IFDEF FPC} var {$ELSE} const {$ENDIF} FITSfile: FITSRecordFile; const Keyword: string; out Value: string; RemoveComment: Boolean; TrimVal: Boolean): Integer;
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

procedure InsertHeaderBlock({$IFDEF FPC} var {$ELSE} const {$ENDIF} InF: FITSRecordFile; const FITSfileName: string);
var
  OutF: FITSRecordFile;
  OutFname: string;
  Buf2: array[1..RecordsInBlock] of FITSRecordType;
  ENDPosition: Integer;
  HeaderBlockCount: Integer;
  I, N: Integer;
begin
  OutFname := FITSfileName + '.$$$';  
  ENDPosition := GetEndPosition(InF); 
  if ENDPosition < 0 then begin
    FileError('Cannot find End of Header in file ' + AnsiQuotedStr(FITSfileName, '"'));
  end;
  HeaderBlockCount := (ENDPosition div RecordsInBlock) + 1;  
  N := FileSize(InF);
  // Increase space, using temporary file.
  Seek(InF, 0);
  AssignFile(OutF, OutFname);
  Rewrite(OutF);
  try
    for I := 0 to HeaderBlockCount - 1 do begin
      BlockRead(InF, Buf2, RecordsInBlock);
      if (I = HeaderBlockCount - 1) then FillChar(Buf2[ENDPosition mod RecordsInBlock + 1], SizeOf(FITSRecordType), ' ');
      BlockWrite(OutF, Buf2, RecordsInBlock);
    end;
    FillChar(Buf2, SizeOf(Buf2), ' ');
    Buf2[1] := recordEND;
    BlockWrite(OutF, Buf2, RecordsInBlock);
    for I := 0 to N div RecordsInBlock - HeaderBlockCount - 1 do begin
      BlockRead(InF, Buf2, RecordsInBlock);
      BlockWrite(OutF, Buf2, RecordsInBlock);
    end;
    N := FileSize(OutF);  
    Seek(OutF, 0);
    Seek(InF, 0);
    Truncate(InF);
    for I := 0 to N div RecordsInBlock - 1 do begin
      BlockRead(OutF, Buf2, RecordsInBlock);
      BlockWrite(InF, Buf2, RecordsInBlock);
    end;
  finally
    CloseFile(OutF);
   end;
  DeleteFile(OutFname);
end;

function SetKeywordValue({$IFDEF FPC} var {$ELSE} const {$ENDIF} FITSfile: FITSRecordFile; const FITSfileName: string; const Keyword: string; const Value: string; AlignNumeric: Boolean; const Comment: string; CanResize: Boolean): Boolean;
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
    AValue := QuotedStr(PadCh(AValue, MinStringConstLen, ' '));
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
    if N < 0 then FileError('Cannot find End of Header in file ' + AnsiQuotedStr(FITSfileName, '"'));
    if (N mod RecordsInBlock) < (RecordsInBlock - 1) then begin
      Seek(FITSfile, N);
      FillChar(Buf, SizeOf(Buf), ' ');
      BlockWrite(FITSfile, Buf, 1);
      Buf := recordEND;
      BlockWrite(FITSfile, Buf, 1);
    end
    else begin
      if not CanResize then
        FileError('Cannot create new entry: another header block is needed (CanResize flag is not set). File ' + AnsiQuotedStr(FITSfileName, '"'));
      InsertHeaderBlock(FITSfile, FITSfileName);
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

function AddCommentLikeKeyword({$IFDEF FPC} var {$ELSE} const {$ENDIF} FITSfile: FITSRecordFile; const FITSfileName: string; const Keyword: string; const Value: string; CanResize: Boolean): Boolean;
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
  if N < 0 then FileError('Cannot find End of Header in file ' + AnsiQuotedStr(FITSfileName, '"'));
  if (N mod RecordsInBlock) < (RecordsInBlock - 1) then begin
    Seek(FITSfile, N);
    FillChar(Buf, SizeOf(Buf), ' ');
    BlockWrite(FITSfile, Buf, 1);
    Buf := recordEND;
    BlockWrite(FITSfile, Buf, 1);
  end
  else begin
    if not CanResize then
      FileError('Cannot create new entry: another header block is needed (CanResize flag is not set). File ' + AnsiQuotedStr(FITSfileName, '"'));
    InsertHeaderBlock(FITSfile, FITSfileName);
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

procedure GetHeader(const FITSfileName: string; const Header: TStrings);
var
  FITSfile: FITSRecordFile;
  Buf: FITSRecordType;
begin
  Header.Clear;
  AssignFile(FITSfile, FITSfileName);
  Reset(FITSfile);
  try
    if GetEndPosition(FITSfile) < 0 then FileError('Cannot find End of Header in file ' + AnsiQuotedStr(FITSfileName, '"'));
    Seek(FITSfile, 0);
    while not EOF(FITSfile) do begin
      BlockRead(FITSfile, Buf, 1);
      Header.Add(Buf);
      if string(Buf) = recordEND then Exit;
    end;
  finally
    CloseFile(FITSfile);
  end;  
end;
  
end.
