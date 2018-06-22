{*****************************************************************************}
{                                                                             }
{ StringListNaturalSort                                                       }
{ (c) 2017 Maksym Pyatnytskyy                                                 }
{                                                                             }
{ This program is distributed                                                 }
{ WITHOUT ANY WARRANTY; without even the implied warranty of                  }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                        }
{                                                                             }
{*****************************************************************************}

{$MODE DELPHI}

unit StringListNaturalSort;

interface

uses
  Windows, SysUtils, Classes;

function StrCmpLogicalW(P1, P2: PWideChar): Integer;  stdcall; external 'Shlwapi.dll';
  
type
  TStringListNaturalSort = class(TStringList)
  public
    procedure NaturalSort;
 end;

implementation

{ TStringListNaturalSort }

function ListSortFunc(List: TStringList; Index1, Index2: Integer): Integer;
var
  WS1, WS2: WideString;
begin
  WS1 := List.Strings[Index1];
  WS2 := List.Strings[Index2];
  Result:= StrCmpLogicalW(PWideChar(WS1), PWideChar(WS2));
end;

procedure TStringListNaturalSort.NaturalSort;
begin
  CustomSort(@ListSortFunc);
end;

end.
