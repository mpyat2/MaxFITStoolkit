{$APPTYPE CONSOLE}

program RawInfo;

uses
  Windows, SysUtils, CmdObj, FreeImage;

procedure PrintHelp;
begin
  WriteLn('Print RAW Info  Maksym Pyatnytskyy  2017');
  WriteLn('Version 2017.12.12.01');
  WriteLn('Usage:');
  WriteLn(ExtractFileName(ParamStr(0)), ' input_filename [/T]');
end;

procedure FileError(const S: string);
begin
  raise Exception.Create(S);
end;

type
  TRational = array[0..1] of LONG;
  PRational = ^TRational;
  TTagArray = array of PFITAG;

const
  RAW_UNPROCESSED = 8; // missing in FreeImage.pas

procedure FreeImage_GetAllMetadataTags(model: FREE_IMAGE_MDMODEL;
                                       dib: PFIBITMAP;
                                       out tags: TTagArray);

var
  hMD: PFIMETADATA;
  lpTag: PFITAG;
begin
  tags := nil;
  hMD := FreeImage_FindFirstMetadata(model, dib, lpTag);
  if hMD <> nil then begin
    repeat
      SetLength(tags, Length(tags) + 1);
      tags[Length(tags) - 1] := lpTag;
    until not FreeImage_FindNextMetadata(hMD, lpTag);
    FreeImage_FindCloseMetadata(hMD);
  end;
end;

procedure PrintTags(model: FREE_IMAGE_MDMODEL; const tags: TTagArray);
var
  I: Integer;
begin
  for I := 0 to Length(tags) - 1 do
    WriteLn(FreeImage_GetTagKey(tags[I]), ' = ', FreeImage_TagToString(model, tags[I]));
end;

var
  FileName: string;
  fif: FREE_IMAGE_FORMAT;
  dib: PFIBITMAP;
  tags: TTagArray;
  SaveTIFF: Boolean;

begin
  if (CmdObj.CmdLine.FileCount <> 1) then begin
    PrintHelp;
    Halt(1);
  end;
  SaveTIFF := CmdObj.CmdLine.IsCmdOption('T');
  try
    FileName := CmdObj.CmdLine.ParamFile(1);
    fif := FreeImage_GetFileType(PChar(FileName), 0);
    if (fif <> FIF_RAW) or not FreeImage_FIFSupportsReading(fif) then
      FileError('Unsupported file.');
    if SaveTIFF then
      dib := FreeImage_Load(fif, PChar(FileName), RAW_DEFAULT)
    else
      dib := FreeImage_Load(fif, PChar(FileName), RAW_UNPROCESSED); // Output a FIT_UINT16 raw Bayer image
    if (dib = nil) then
      FileError('Error loading bitmap.');
    try
      WriteLn('[MAIN]');
      FreeImage_GetAllMetadataTags(FIMD_EXIF_MAIN, dib, tags);
      PrintTags(FIMD_EXIF_MAIN, tags);
      WriteLn;
      WriteLn('[EXIF]');
      FreeImage_GetAllMetadataTags(FIMD_EXIF_EXIF, dib, tags);
      PrintTags(FIMD_EXIF_EXIF, tags);
      WriteLn;
      WriteLn('[GPS]');
      FreeImage_GetAllMetadataTags(FIMD_EXIF_GPS, dib, tags);
      PrintTags(FIMD_EXIF_GPS, tags);
      WriteLn;
      WriteLn('[MAKERNOTE]');
      FreeImage_GetAllMetadataTags(FIMD_EXIF_MAKERNOTE, dib, tags);
      PrintTags(FIMD_EXIF_MAKERNOTE, tags);
      WriteLn;
      WriteLn('[INTEROP]');
      FreeImage_GetAllMetadataTags(FIMD_EXIF_INTEROP, dib, tags);
      PrintTags(FIMD_EXIF_INTEROP, tags);
      WriteLn;
      WriteLn('[COMMENTS]');
      FreeImage_GetAllMetadataTags(FIMD_COMMENTS, dib, tags);
      PrintTags(FIMD_COMMENTS, tags);
      WriteLn;
      WriteLn('[OTHER]');
      WriteLn('dib.width = ', FreeImage_GetWidth(dib));
      WriteLn('dib.height = ', FreeImage_GetHeight(dib));

      if SaveTIFF then begin
        if not FreeImage_Save(FIF_TIFF, dib, PChar(FileName + '.TIF'), TIFF_NONE) then
          FileError('Cannon save TIFF');
      end;
    finally
      FreeImage_Unload(dib);
    end;
  except
    on E: Exception do begin
      WriteLn;
      WriteLn('**** Error:');
      WriteLn(E.Message);
      Halt(1);
    end;
  end;
end.

