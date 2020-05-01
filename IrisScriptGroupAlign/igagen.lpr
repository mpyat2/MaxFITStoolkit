{*****************************************************************************}
{                                                                             }
{ Iris Group Align Script Generator                                           }
{ (c) 2020 Maksym Pyatnytskyy                                                 }
{                                                                             }
{ This program is distributed                                                 }
{ WITHOUT ANY WARRANTY; without even the implied warranty of                  }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                        }
{                                                                             }
{*****************************************************************************}

{$APPTYPE CONSOLE}
{$MODE DELPHI}
{$INCLUDE FITSUtils.inc}

program igagen;

uses
  SysUtils, CmdObj{, CmdObjStdSwitches}, Version, EnumFiles, MiscUtils, 
  FITSUtils, FitsUtilsHelp, CommonIni;

{$R *.res} // include version info!

{$INCLUDE PrintVersion.inc}

procedure GeneralError(const S: string);
begin
  raise Exception.Create(S);
end;

function GenNameValid(const S: string): Boolean;
begin
  // \/:*?
  Result :=  (S <> '') and
     (Pos('\', S) = 0) and
     (Pos('/', S) = 0) and
     (Pos(':', S) = 0) and
     (Pos('*', S) = 0) and
     (Pos('?', S) = 0) and
     (Pos('<', S) = 0) and
     (Pos('>', S) = 0);
end;

procedure CreateScript(const OutputFileName: string;
                       const InputGenName: string;
                       const OutputGenName: string;
                       GroupSize: Integer;
                       TotalCount: Integer);
var
  GroupCount: Integer;
  OutFile: TextFile;
  S: string;
  I, II, N: Integer;
begin
    AssignFile(OutFile, OutputFileName);
    Rewrite(OutFile);
    try
      WriteLn(OutFile, '= IRIS SCRIPT =');
      WriteLn(OutFile);
      WriteLn(OutFile);
      WriteLn(OutFile, 'SETREGISTER 2');
      WriteLn(OutFile);

      GroupCount := TotalCount div GroupSize;
      N := 1;
      for I := 0 to GroupCount - 1 do begin
        S := InputGenName + IntToStr(N);
        WriteLn(OutFile, 'LOAD ' + S);
        WriteLn(OutFile, 'SAVE ' + OutputGenName + IntToStr(N));
        Inc(N);
        for II := 2 to GroupSize do begin
          WriteLn(OutFile, 'COREGISTER ' + S + ' ' + InputGenName + IntToStr(N));
          WriteLn(OutFile, 'SAVE ' + OutputGenName + IntToStr(N));
          Inc(N);
        end;
        WriteLn(OutFile);
      end;

    finally
      CloseFile(OutFile);
    end;
end;

var
  PrintVer: Boolean;
  S, S2: string;
  ParamN: Integer;

var
  OutputFileName: string = '';
  InputGenName: string = '';
  OutputGenName: string = '';
  GroupSize: Integer = 0;
  TotalCount: Integer = 0;

begin
  FileMode := fmOpenReadWrite;

  PrintVer := (CmdObj.CmdLine.IsCmdOption('V') or CmdObj.CmdLine.IsCmdOption('version'));
  if PrintVer then PrintVersion('Group align IRIS script generator');

  if (CmdObj.CmdLine.IsCmdOption('?') or CmdObj.CmdLine.IsCmdOption('H') or CmdObj.CmdLine.IsCmdOption('help')) then begin
    PrintHelp;
    Halt(1);
  end;

  if (CmdObj.CmdLine.FileCount <> 1) then begin
    if not PrintVer then begin
      PrintWarning('**** Single output file name must be specified'^M^J);
      WriteLn;
      PrintHelp;
    end;
    Halt(1);
  end;

  // Other options

  for ParamN := 1 to CmdObj.CmdLine.ParamCount do begin
    S := CmdObj.CmdLine.ParamStr(ParamN);
    if CmdObj.CmdLine.FirstCharIsSwitch(S) then begin
      if Length(S) = 1 then begin
        PrintError('**** Invalid command-line parameter: ' + S + ^M^J);
        Halt(1);
      end;
      if CmdObj.CmdLine.ParamIsKey(S, 'V') or CmdObj.CmdLine.ParamIsKey(S, 'version') then begin
        // nothing: already processed.
      end
      else
      if CmdObj.CmdLine.ExtractParamValue(S, 'I=', InputGenName) then begin
         if not GenNameValid(InputGenName) then begin
           PrintError('**** [/I=] Input generic name must be not be empty and must not contain \/:*?<>'^M^J);
           Halt(1);
         end;
      end
      else
      if CmdObj.CmdLine.ExtractParamValue(S, 'O=', OutputGenName) then begin
         if not GenNameValid(OutputGenName) then begin
           PrintError('**** [/O=] Output generic name must be not be empty and must not contain \/:*?<>'^M^J);
           Halt(1);
         end;
      end
      else
      if CmdObj.CmdLine.ExtractParamValue(S, 'N=', S2) then begin
         if (not GetInt(S2, GroupSize)) or (GroupSize < 1) then begin
           PrintError('**** [/N=] Group size must be integer number > 0'^M^J);
           Halt(1);
         end;
      end
      else
      if CmdObj.CmdLine.ExtractParamValue(S, 'NN=', S2) then begin
         if (not GetInt(S2, TotalCount)) or (TotalCount < 1) then begin
           PrintError('**** [/NN=] Total number of images be integer number > 0'^M^J);
           Halt(1);
         end;
      end
      else begin
        PrintError('**** Invalid command-line parameter: ' + S + ^M^J);
        Halt(1);
      end;
    end
    else begin
      if S <> '' then begin
        OutputFileName := ExpandFileName(S);
        if ExtractFileExt(OutputFileName) = '' then OutputFileName := ChangeFileExt(OutputFileName, '.pgm');
      end;
    end;
  end;

  if InputGenName = '' then begin
    PrintError('**** [/I=] Input generic name must be specified'^M^J);
    Halt(1);
  end;

  if OutputGenName = '' then begin
    PrintError('**** [/O=] Output generic name must be specified'^M^J);
    Halt(1);
  end;

  if GroupSize = 0 then begin
    PrintError('**** [/N=] Group Size must be specified'^M^J);
    Halt(1);
  end;

  if TotalCount = 0 then begin
    PrintError('**** [/NN=] Total number of images must be specified'^M^J);
    Halt(1);
  end;

  if GroupSize > TotalCount then begin
    PrintError('**** Group size must be less or equal than total number of images'^M^J);
    Halt(1);
  end;

  try
    CreateScript(OutputFileName, InputGenName, OutputGenName, GroupSize, TotalCount);
    WriteLn('Script created.');
  except
    on E: Exception do begin
      PrintError(^M^J'**** Error:'^M^J + E.Message + ^M^J);
      Halt(1);
    end;
  end;

end.

