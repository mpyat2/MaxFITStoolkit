{*****************************************************************************}
{                                                                             }
{ find_hot                                                                    }
{ (c) 2018 Maksym Pyatnytskyy                                                 }
{                                                                             }
{ This program is distributed                                                 }
{ WITHOUT ANY WARRANTY; without even the implied warranty of                  }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                        }
{                                                                             }
{*****************************************************************************}

{$APPTYPE CONSOLE}
{$MODE DELPHI}
{$INCLUDE FITSUtils.inc}

program find_hot;

uses
  SysUtils, CmdObj{, CmdObjStdSwitches}, Version, FITScompatibility,
  FITSUtils, StringListNaturalSort, FitsUtilsHelp, CommonIni;

{$R *.res} // include version info!

{$INCLUDE PrintVersion.inc}

procedure FileError(const S: string);
begin
  raise Exception.Create(S);
end;

procedure ProcessInput(const FITSFileName: string; Level: Double; MaxNumber: Integer; const OutFileName: string);
var
  FITSFile: FITSRecordFile;
  ListFile: TextFile;
  BadPixelNumber: Integer;
  Image: PChar;
  Width, Height, Layers, BitPix: Integer;
  PixelNumber: Integer;
  Bscale, Bzero: Double;
  PixelArray: TExtendedArray;
  X, Y: Integer;
  V: Extended;
  I: Integer;
begin
  BadPixelNumber := 0;
  try
    Assign(FITSFile, FITSFileName);
    Reset(FITSFile);
    try
      if not IsFits(FITSFile) then
        FileError('Not a valid FITS file: ' + AnsiQuotedStr(FITSFileName, '"'));
      Image := GetFITSimage(FITSFile, Width, Height, Layers, BitPix, Bscale, Bzero);
      try
{$IFNDEF range_check}{$R+}{$ENDIF}
{$IFNDEF overflow_check}{$Q+}{$ENDIF}
        PixelNumber := Height * Width; // one layer
{$IFNDEF range_check}{$R-}{$ENDIF}
{$IFNDEF overflow_check}{$Q-}{$ENDIF}
        SetLength(PixelArray, PixelNumber);
        CopyFITSValues(Image, PixelArray, PixelNumber, BitPix, BScale, BZero);
      finally
        FreeMem(Image);
        Image := nil;
      end;
    finally
      CloseFile(FITSFile);
    end;
    if Layers <> 1 then
      FileError('2D FITS is expected');

    Assign(ListFile, OutFileName);
    Rewrite(ListFile);
    try
      for I := 0 to PixelNumber - 1 do begin
        V := PixelArray[I];
        if V > Level then begin
          X := I mod Width;
          Y := I div Width;
          WriteLn(ListFile, 'P', ' ', X + 1, ' ', Y + 1);
          Inc(BadPixelNumber);
          if BadPixelNumber >= MaxNumber then begin
            WriteLn('Number of pixels found exceeds limit of ', MaxNumber, ' pixels. Use /M= option to increase the limit or /L to increase threshold.');
            Exit;
          end;
        end;
      end;
      WriteLn('Hot pixel count: ', BadPixelNumber);
    finally
      CloseFile(ListFile);
    end;
  except
    on E: Exception do begin
      WriteLn;
      WriteLn('**** Error:');
      WriteLn(E.Message);
      Halt(1);
    end;
  end;
end;

var
  InputFileName: string;
  PrintVer: Boolean;
  S, S2: string;
  ParamN: Integer;
  Level: Double;
  LevelSet: Boolean;
  MaxNumber: Integer;
  OutFileName: string;

begin
  FileMode := fmOpenReadWrite;

  PrintVer := (CmdObj.CmdLine.IsCmdOption('V') or CmdObj.CmdLine.IsCmdOption('version'));
  if PrintVer then PrintVersion('Find Hot Pixels');

  if (CmdObj.CmdLine.IsCmdOption('?') or CmdObj.CmdLine.IsCmdOption('H') or CmdObj.CmdLine.IsCmdOption('help')) then begin
    PrintHelp;
    Halt(1);
  end;

  if (CmdObj.CmdLine.FileCount <> 1) then begin
    if not PrintVer then begin
      WriteLn('**** File name is expected');
      WriteLn;
      PrintHelp;
    end;
    Halt(1);
  end;

  Level := 0;
  LevelSet := False;
  MaxNumber := 10000;
  OutFileName := '';

  // Other options

  for ParamN := 1 to CmdObj.CmdLine.ParamCount do begin
    S := CmdObj.CmdLine.ParamStr(ParamN);
    if CmdObj.CmdLine.FirstCharIsSwitch(S) then begin
      if Length(S) = 1 then begin
        WriteLn('**** Invalid command-line parameter: ' + S);
        Halt(1);
      end;
      if CmdObj.CmdLine.ParamIsKey(S, 'V') or CmdObj.CmdLine.ParamIsKey(S, 'version') then begin
        // nothing: already processed.
      end
      else
      if CmdObj.CmdLine.ExtractParamValue(S, 'L=', S2) then begin
        if S2 <> '' then begin
          if not GetDouble(S2, Level) then begin
            WriteLn('**** Invalid Level value');
            Halt(1);
          end;
          LevelSet := True;
        end;
      end
      else
      if CmdObj.CmdLine.ExtractParamValue(S, 'M=', S2) then begin
        if S2 <> '' then begin
          if not GetInt(S2, MaxNumber) then begin
            WriteLn('**** MaxNumber must be an integer value > 0');
            Halt(1);
          end;
        end;
      end
      else
      if CmdObj.CmdLine.ExtractParamValue(S, 'N=', S2) then begin
        if S2 <> '' then begin
          OutFileName := ExpandFileName(S2);
          if ExtractFileExt(OutFileName) = '' then
            OutFileName := ChangeFileExt(OutFileName, '.lst');
        end;
      end
      else begin
        WriteLn('**** Invalid command-line parameter: ' + S);
        Halt(1);
      end;
    end;
  end;

  if not LevelSet then begin
    WriteLn('**** Hot pixel threshold must be specified by /L= parameter');
    Halt(1);
  end;

  InputFileName := ExpandFileName(CmdObj.CmdLine.ParamFile(1));
  if ExtractFileExt(InputFileName) = '' then InputFileName := ChangeFileExt(InputFileName, '.fit');
  ProcessInput(InputFileName, Level, MaxNumber, OutFileName);

end.


