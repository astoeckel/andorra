unit AdPersistent;

interface

uses
  AdContainers;

type

{$M+}
  TAdPersistent = class
  end;
{$M-}
  TAdPersistentClass = class of TAdPersistent;


procedure AdRegisterClass(AClass:TAdPersistentClass);
function AdGetClass(AName:ShortString):TAdPersistentClass;

implementation

type
  TAdClassMap = class(TAdMap)
    destructor Destroy;override;
  end;


  TAdClassKey = class(TAdMapKey)
    public
      Value:ShortString;
      ClassType:TAdPersistentClass;
      function Hash:Cardinal;override;
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

function AdGetClass(AName:ShortString):TAdPersistentClass;
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

function TAdClassKey.Hash: Cardinal;
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
