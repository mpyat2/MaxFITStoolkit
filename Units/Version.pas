{*****************************************************************************}
{                                                                             }
{ CmdObj                                                                      }
{ (c) 2000 Maksym Pyatnytskyy                                                 }
{                                                                             }
{*****************************************************************************}

{$IFDEF FPC} {$MODE DELPHI} {$ENDIF}

unit Version;

interface

uses Windows, SysUtils;

procedure GetVersionValues(const ModuleName: String; out V1, V2, V3, V4: Word);
function GetVersionString(const ModuleName: String): String;
function GetVersionString2(const ModuleName: String): String;

(*
// The next functions are valid for Delphi Win32 executables.
// The functions do not work for FPC-generatede executabels.
function GetBuildTime(const ModuleName: string): TDateTime;
function GetBuildTimeStr(const ModuleName: string): string;
*)

implementation

(*
resourcestring
  SerrNotExe = 'Not an EXE file';
  SerrNotPE = 'Not a PE file';
  SerrRSRCnotFound = 'Cannot find .rsrc';
*)

procedure GetVersionValues(const ModuleName: String; out V1, V2, V3, V4: Word);
var
  VerInfoSize:  DWORD;
  VerInfo:      Pointer;
  VerValueSize: DWORD;
  VerValue:     PVSFixedFileInfo;
  Dummy:        DWORD;
begin
  V1 := 0;
  V2 := 0;
  V3 := 0;
  V4 := 0;
  VerInfoSize := GetFileVersionInfoSize(PChar(ModuleName), Dummy);
  if VerInfoSize = 0 then Exit;
  GetMem(VerInfo, VerInfoSize);
  try
    if not GetFileVersionInfo(PChar(ModuleName), 0, VerInfoSize, VerInfo) then Exit;
    if not VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize) then Exit;
    with VerValue^ do begin
      V1 := dwFileVersionMS shr 16;
      V2 := dwFileVersionMS and $FFFF;
      V3 := dwFileVersionLS shr 16;
      V4 := dwFileVersionLS and $FFFF;
   end;
 finally
   FreeMem(VerInfo, VerInfoSize);
 end;
end;

function GetVersionString(const ModuleName: String): String;
var
  V1, V2, V3, V4: Word;
begin
  Result := '';
  GetVersionValues(ModuleName, V1, V2, V3, V4);
  Result := ExtractFileName(ModuleName) + ' Version ' + IntToStr(V1) + '.' + IntToStr(V2) + '.' + IntToStr(V3) + ' (Build ' + IntToStr(V4) + ')';
end;

function GetVersionString2(const ModuleName: String): String;
var
  V1, V2, V3, V4: Word;
begin
  Result := '';
  GetVersionValues(ModuleName, V1, V2, V3, V4);
  Result := IntToStr(V1) + '.' + IntToStr(V2) + '.' + IntToStr(V3) + '.' + IntToStr(V4);
end;

(*
function GetBuildTime(const ModuleName: string): TDateTime;

const
  INVALID_SET_FILE_POINTER = DWORD(-1);

type
  TImageDosHeader = packed record
    e_magic  : Word;
    e_ignore : packed array [0..28] of Word;
    e_lfanew : LongInt;
  end;

type
  TImageResourceDirectory = packed record
    Characteristics: DWORD;
    TimeDateStamp: DWORD;
    MajorVersion: Word;
    MinorVersion: Word;
    NumberOfNamedEntries: Word;
    NumberOfIdEntries: Word;
    //  IMAGE_RESOURCE_DIRECTORY_ENTRY DirectoryEntries[];
  end;
  PImageResourceDirectory = ^TImageResourceDirectory;

var
  hExeFile: HFile;
  ImageDosHeader: TImageDosHeader;
  Signature: Cardinal;
  ImageFileHeader: TImageFileHeader;
  ImageOptionalHeader: TImageOptionalHeader;
  ImageSectionHeader: TImageSectionHeader;
  ImageResourceDirectory: TImageResourceDirectory;
  Count: DWORD;
  I: Integer;
  SectionName: array[0..IMAGE_SIZEOF_SHORT_NAME] of Char; // Size = IMAGE_SIZEOF_SHORT_NAME + 1 !
  RSRCFound: Boolean;
begin
  hExeFile := CreateFile(PChar(ModuleName), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);
  if hExeFile = INVALID_HANDLE_VALUE then RaiseLastOSError;
  try
    if not ReadFile(hExeFile, ImageDosHeader, SizeOf(ImageDosHeader), Count, nil) then RaiseLastOSError;
    if ImageDosHeader.e_magic <> IMAGE_DOS_SIGNATURE then raise Exception.Create(SerrNotEXE);
    if SetFilePointer(hExeFile, ImageDosHeader.e_lfanew, nil, FILE_BEGIN) = INVALID_SET_FILE_POINTER then RaiseLastOSError;
    if not ReadFile(hExeFile, Signature, SizeOf(Signature), Count, nil) then RaiseLastOSError;
    if Signature <> IMAGE_NT_SIGNATURE then raise Exception.Create(SerrNotPE);
    if not ReadFile(hExeFile, ImageFileHeader, SizeOf(ImageFileHeader), Count, nil) then RaiseLastOSError;
    if not ReadFile(hExeFile, ImageOptionalHeader, SizeOf(ImageOptionalHeader), Count, nil) then RaiseLastOSError;
    RSRCFound := False;
    for I := 0 to ImageFileHeader.NumberOfSections - 1 do begin
      if not ReadFile(hExeFile, ImageSectionHeader, SizeOf(ImageSectionHeader), Count, nil) then RaiseLastOSError;
      Move(ImageSectionHeader.Name, SectionName, IMAGE_SIZEOF_SHORT_NAME);
      SectionName[IMAGE_SIZEOF_SHORT_NAME] := #0;
      if StrComp(SectionName, '.rsrc') = 0 then begin
        RSRCFound := True;
        Break;
      end;
    end;
    if not RSRCFound then raise Exception.Create(SerrRSRCnotFound);
    if SetFilePointer(hExeFile, ImageSectionHeader.PointerToRawData, nil, FILE_BEGIN) = INVALID_SET_FILE_POINTER then RaiseLastOSError;
    if not ReadFile(hExeFile, ImageResourceDirectory, SizeOf(ImageResourceDirectory), Count, nil) then RaiseLastOSError;
  finally
    CloseHandle(hExeFile);
  end;

  Result := FileDateToDateTime(ImageResourceDirectory.TimeDateStamp);
end;

function GetBuildTimeStr(const ModuleName: string): string;
begin
  try
    Result := DateTimeToStr(GetBuildTime(ModuleName));
  except
    Result := '';
  end;
end;
*)

end.
