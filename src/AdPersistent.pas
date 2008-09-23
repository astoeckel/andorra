{
* This program is licensed under the Common Public License (CPL) Version 1.0
* You should have recieved a copy of the license with this file.
* If not, see http://www.opensource.org/licenses/cpl1.0.txt for more informations.
* 
* Inspite of the incompatibility between the Common Public License (CPL) and the GNU General Public License (GPL) you're allowed to use this program * under the GPL. 
* You also should have recieved a copy of this license with this file. 
* If not, see http://www.gnu.org/licenses/gpl.txt for more informations.
*
* Project: Andorra 2D
* Author:  Andreas Stoeckel
* File: AdPersistent.pas
* Comment: Contains a replacement for TPersistent from Classes
}

{Contains a replacement for TPersistent from Classes}
unit AdPersistent;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  AdContainers;

type

{$M+}
  {A replacement for TPersistent from classes.}
  TAdPersistent = class
  end;
{$M-}
  {A replacement for TPersistentClass.}
  TAdPersistentClass = class of TAdPersistent;


{Registeres a persistent class within the Andorra class registration map. @seealso(AdGetClass)}
procedure AdRegisterClass(AClass:TAdPersistentClass);
{Returns a registered class by passing the classname. Returns nil, if the class isn't found. @seealso(AdRegisterClass)}
function AdGetClass(AName:String):TAdPersistentClass;

implementation

type
  TAdClassMap = class(TAdMap)
    destructor Destroy;override;
  end;


  TAdClassKey = class(TAdMapKey)
    public
      Value:String;
      function Hash:integer;override;
      function Equal(AItem:TAdMapKey):boolean;override;
  end;

var
  ClassMap:TAdClassMap;

procedure AdRegisterClass(AClass:TAdPersistentClass);
var
  Key:TAdClassKey;
begin
  Key := TAdClassKey.Create;
  Key.Value := AClass.ClassName;
  ClassMap.Insert(Key, TObject(AClass));
end;

function AdGetClass(AName:String):TAdPersistentClass;
var
  Key:TAdClassKey;
  p:TObject;
begin
  Key := TAdClassKey.Create;
  Key.Value := AName;
  p := ClassMap.GetValue(Key);
  if p <> nil then
  begin
    result := TAdPersistentClass(p);
  end
  else
  begin
    result := nil;
  end;
  Key.Free;
end;

{ TAdClassKey }

function TAdClassKey.Equal(AItem: TAdMapKey): boolean;
begin
  result := Value = TAdClassKey(AItem).Value;
end;

function TAdClassKey.Hash: integer;
var
  i:integer;
begin
  result := 0;
  for i := 1 to Length(Value) do
    result := result + ord(Value[i]);
end;

{ TAdClassMap }

destructor TAdClassMap.Destroy;
var
  i,j:integer;
  p:PAdLinkedList;
  c:PAdMapPair;
begin
  p := Data;
  for i := 0 to Capacity - 1 do
  begin
    p^.StartIteration;
    while not p^.ReachedEnd do
    begin
      c := PAdMapPair(p^.GetCurrent);
      c^.Key.Free;
      Dispose(c);
      FCount := FCount - 1;
    end;
    for j := 0 to p^.Count - 1 do
    begin
      p^.Delete(0);
    end;
    inc(p);
  end;

  inherited Destroy;
end;

initialization
  ClassMap := TAdClassMap.Create(32);

finalization
  ClassMap.Free;

end.
