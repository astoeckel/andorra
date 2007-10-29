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
* File: AdContainers.pas
* Comment: Some container classes
* Creation Date: 10-12-07
}

unit AdContainers;

interface

uses
  SysUtils;

type
  PAdLinkedListItem = ^TAdLinkedListItem;
  TAdLinkedListItem = record
    next:PAdLinkedListItem;
    data:Pointer;
  end;

  EAdListError = class(Exception);

  PAdLinkedList = ^TAdLinkedList;
  TAdLinkedList = class
    private
      FStart,FLast:PAdLinkedListItem;
      FCount:Integer;

      FIterItem:PAdLinkedListItem;

      function GetItem(AIndex:integer):Pointer;
      procedure SetItem(AIndex:integer;AValue:Pointer);
    protected

    public
      Tag:integer;
      property Count:Integer read FCount;
      property Items[Index:integer]:Pointer read GetItem write SetItem; default;

      constructor Create;
      destructor Destroy;override;

      function Add(AItem:Pointer):Integer;
      procedure Insert(AIndex:integer;AItem:Pointer);
      function Remove(AItem:Pointer):boolean;
      function Delete(AIndex:integer):boolean;
      function IndexOf(AItem:Pointer):integer;
      function First:Pointer;
      function Last:Pointer;

      procedure StartIteration;
      function ReachedEnd:boolean;
      function GetCurrent:Pointer;
  end;

  TAdMapKey = class
    public
      function Hash:Cardinal;virtual;abstract;
      function Equal(AItem:TAdMapKey):boolean;virtual;abstract;
  end;

  PAdMapPair = ^TAdMapPair;
  TAdMapPair = record
    Key:TAdMapKey;
    Value:TObject;
  end;

  TAdMap = class
    private
      FCapacity:Cardinal;
      FData:Pointer;
      FCount:Cardinal;
      FMemSize:Cardinal;
      procedure Rehash(ACapacity:Cardinal);
      procedure FreeMemory;
    protected
    public
      constructor Create(ACapacity:Cardinal=128);
      destructor Destroy;override;

      function Insert(AKey:TAdMapKey;AValue:TObject):Boolean;
      function GetValue(AKey:TAdMapKey):TObject;
      function Remove(AKey:TAdMapKey):Boolean;

      property Count:Cardinal read FCount;
      property Capacity:Cardinal read FCapacity;
  end;

const
  UpperRehashBound = 1;
  LowerRehashBound = 0.25;
  RehashFactor = 2;


implementation

{ TAdLinkedList }

constructor TAdLinkedList.Create;
begin
  inherited Create;
  
  //Initialize Varibales
  FCount := 0;
  FStart := nil;
  FLast := nil;
end;

destructor TAdLinkedList.Destroy;
begin
  //Free all elements
  while Count > 0 do
  begin
    Delete(0);
  end;
  
  inherited Destroy;
end;

function TAdLinkedList.Add(AItem: Pointer): Integer;
var
  PItem:PAdLinkedListItem;
begin
  //Create a new element
  New(PItem);
  PItem^.data := AItem;
  
  //The new element hasn't got a next element
  PItem^.next := nil;

  //The next element of the last element is the new item
  if FLast <> nil then
  begin
    FLast^.next := PItem;
  end;

  //The new item now is the last element
  FLast := PItem;

  if FCount = 0 then
  begin
    //Because there is no other element, this new element is the first element
	  FStart := PItem;
  end;

  //Return the index of the new element
  result := FCount;
  FCount := FCount + 1;
end;

function TAdLinkedList.Delete(AIndex: Integer): boolean;
var
  i:integer;
  PItem:PAdLinkedListItem;
  PNext,PPrev:PAdLinkedListItem;
begin
  result := false;
  if (AIndex >= FCount) or (AIndex < 0) then exit;

  PItem := FStart;
  PPrev := nil;
  for i := 0 to AIndex-1 do
  begin
    PPrev := PItem;
    PItem := PItem^.next;
  end;

  if PItem <> nil then
  begin
    PNext := PItem^.next;

    Dispose(PItem);
    result := true;

    if AIndex = 0 then
    begin
      FStart := PNext;
    end
    else
    begin
      PPrev^.next := PNext;
    end;
    
    if AIndex = FCount - 1 then
    begin
      FLast := PPrev;
    end;

    FCount := FCount - 1;
  end;
end;

function TAdLinkedList.IndexOf(AItem: Pointer): integer;
var
  i:integer;
  PItem:PAdLinkedListItem;
begin
  PItem := FStart;
  i := 0;
  while (PItem <> nil) and (PItem^.data <> AItem) do
  begin
    PItem := PItem^.next;
    i := i + 1;
  end;

  if PItem = nil then
  begin
    result := -1;
  end
  else
  begin
    result := i;
  end;    
end;

procedure TAdLinkedList.Insert(AIndex: integer; AItem: Pointer);
var
  i:integer;
  PItem,PAdd,PPrev:PAdLinkedListItem;
begin

  if AIndex < 0 then
  begin
    AIndex := 0;
  end;
  
  if AIndex >= Count then
  begin
    Add(AItem);
    exit;
  end;

  PItem := FStart;
  PPrev := nil;
  for i := 0 to AIndex-1 do
  begin
    PPrev := PItem;
    PItem := PItem^.next;
  end;

  New(PAdd);
  PAdd^.data := AItem;
  PAdd^.next := PItem;

  if PPrev = nil then
  begin
    FStart := PAdd;
  end
  else
  begin
    PPrev^.next := PAdd;
  end;

  FCount := FCount + 1;
end;

function TAdLinkedList.Remove(AItem: Pointer): boolean;
begin
  result := Delete(IndexOf(AItem));
end;

function TAdLinkedList.GetItem(AIndex: integer): Pointer;
var
  i:integer;
  PItem:PAdLinkedListItem;
begin
  if AIndex >= Count then
    raise EAdListError.Create('List index exceeds maximum ('+IntToStr(AIndex)+')');
  if AIndex < 0 then
    raise EAdListError.Create('List index drops below minimum ('+IntToStr(AIndex)+')');

  PItem := FStart;
  
  for i := 0 to AIndex-1 do
  begin
    PItem := PItem^.next;
  end;

  result := PItem^.data;
end;

procedure TAdLinkedList.SetItem(AIndex: integer; AValue: Pointer);
var
  i:integer;
  PItem:PAdLinkedListItem;
begin
  if AIndex >= Count then
    raise EAdListError.Create('List index exceeds maximum ('+IntToStr(AIndex)+')');
  if AIndex < 0 then
    raise EAdListError.Create('List index drops below minimum ('+IntToStr(AIndex)+')');

  PItem := FStart;

  for i := 0 to AIndex-1 do
  begin
    PItem := PItem^.next;
  end;

  PItem^.data := AValue;
end;

procedure TAdLinkedList.StartIteration;
begin
  FIterItem := FStart;
end;

function TAdLinkedList.GetCurrent: Pointer;
begin
  result := FIterItem^.data;
  FIterItem := FIterItem^.next;
end;

function TAdLinkedList.ReachedEnd: boolean;
begin
  result := FIterItem = nil;
end;

function TAdLinkedList.First: Pointer;
begin
  result := nil;
  if FStart <> nil then
    result := FStart^.data;
end;

function TAdLinkedList.Last: Pointer;
begin
  result := nil;
  if FLast <> nil then
    result := FLast^.data;
end;

{ TAdMap }

constructor TAdMap.Create(ACapacity: Cardinal);
begin
  inherited Create;

  FData := nil;
  Rehash(ACapacity);
  FCount := 0;
end;

destructor TAdMap.Destroy;
begin
  FreeMemory;
  inherited;
end;

procedure TAdMap.FreeMemory;
var
  i:integer;
  PList:PAdLinkedList;
begin
  if FData <> nil then
  begin
    PList := FData;
    for i := 0 to FCapacity-1 do
    begin
      PList^.Free;
      inc(PList);
    end;
    FreeMem(FData,FMemSize);
    FData := nil;
  end;
end;

function TAdMap.GetValue(AKey: TAdMapKey): TObject;
var
  PItem:PAdMapPair;
  Pos:integer;
  PList:PAdLinkedList;
begin
  result := nil;

  //Search List Element
  Pos := (AKey.Hash mod FCapacity);
  PList := FData;
  inc(PList,Pos);

  with PList^ do
  begin
    StartIteration;
    while not ReachedEnd do
    begin
      PItem := PAdMapPair(GetCurrent);
      if PItem^.Key.Equal(AKey) then
      begin
        result := PItem^.Value;
        exit;
      end;
    end;
  end;
end;

function TAdMap.Insert(AKey: TAdMapKey; AValue: TObject): Boolean;
var
  PItem:PAdMapPair;
  Pos:integer;
  PList:PAdLinkedList;  
begin
  result := false;

  //Search List Element
  Pos := (AKey.Hash mod FCapacity);
  PList := FData;
  inc(PList,Pos);

  with PList^ do
  begin
    //Check wether key already exists - replace value if necessary
    StartIteration;
    while not ReachedEnd do
    begin
      PItem := PAdMapPair(GetCurrent);
      if PItem^.Key.Equal(AKey) then
      begin
        PItem^.Value := AValue;
        exit;
      end;
    end;

    //Key not found, insert item
    New(PItem);
    PItem^.Key := AKey;
    PItem^.Value := AValue;
    Add(PItem);
    result := true;
  end;

  if result then
  begin
    FCount := FCount + 1;
    if FCount >= FCapacity * UpperRehashBound then
    begin
      Rehash(round(FCapacity * RehashFactor));
    end;
  end;
end;

procedure TAdMap.Rehash(ACapacity: Cardinal);
var
  PTmp:Pointer;
  PList1,PList2:PAdLinkedList;
  Pos:integer;
  i:integer;
  PCurItem:PAdMapPair;
begin
  //Reserve new memory
  FMemSize := SizeOf(PAdLinkedList)*ACapacity;
  GetMem(PTmp,FMemSize);

  //Create lists
  PList2 := PTmp;
  for i := 0 to ACapacity - 1 do
  begin
    PList2^ := TAdLinkedList.Create;
    PList2^.Tag := i;
    inc(PList2);
  end;

  //Copy elements into new array
  PList1 := FData;
  for i := 0 to FCapacity - 1 do
  begin
    with PList1^ do
    begin
      StartIteration;
      while not ReachedEnd do
      begin
        PCurItem := GetCurrent;

        //Insert element
        PList2 := PTmp;
        Pos := (PCurItem^.Key.Hash mod ACapacity);
        Inc(PList2,Pos);

        PList2^.Add(PCurItem);
      end;
    end;
    Inc(PList1);
  end;

  //Free Memory and lists
  FreeMemory;

  //Set new values
  FData := PTmp;
  FCapacity := ACapacity;
end;

function TAdMap.Remove(AKey: TAdMapKey): Boolean;
var
  PItem:PAdMapPair;
  Pos:integer;
  PList:PAdLinkedList;  
begin
  result := false;

  //Search List Element
  Pos := (AKey.Hash mod FCapacity);
  PList := FData;
  inc(PList,Pos);
  
  with PList^ do
  begin
    StartIteration;
    while not ReachedEnd do
    begin
      PItem := PAdMapPair(GetCurrent);
      if PItem^.Key.Equal(AKey) then
      begin
        Remove(PItem);
        Dispose(PItem);
        result := true;
        FCount := FCount - 1;

        if self.FCount < FCapacity * LowerRehashBound  then
        begin
          Rehash(round(FCapacity / RehashFactor));
        end;

        exit;
      end;
    end;
  end;
end;

end.
