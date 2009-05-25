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

{This unit contains some container classes for the every day use in andorra 2d.}
unit AdContainers;

interface
   
type
  {Pointer on TAdLinkedListItem}
  PAdLinkedListItem = ^TAdLinkedListItem;
  
  {A list item used in TAdLinked list.}
  TAdLinkedListItem = record
    next:PAdLinkedListItem;{<Pointer to the next element}
    data:Pointer;{<Pointer to the stored object.}
  end;

  {Pointer on a TAdLinkedList.}
  PAdLinkedList = ^TAdLinkedList;
  {A linked list class.}
  TAdLinkedList = class
    private
      FStart,FLast:PAdLinkedListItem;
      FCount:Integer;

      FIterItem:PAdLinkedListItem;

      function GetItem(AIndex:integer):Pointer;
      procedure SetItem(AIndex:integer;AValue:Pointer);
    protected

    public
      {Don't now what this is for...}
      Tag:integer;
      {Returns the count of elements in the list}
      property Count:Integer read FCount;
      {Direct access the the items. Don't use in  time-critical cases.}
      property Items[Index:integer]:Pointer read GetItem write SetItem; default;

      {Creates an instance of TAdLinkedList.}
      constructor Create;
      {Destroys the instance of TAdLinkedList}
      destructor Destroy;override;

      {Adds an item to the list and returns its position.}
      function Add(AItem:Pointer):Integer;
      {Inserts a item on a specific place in the list.}
      procedure Insert(AIndex:integer;AItem:Pointer);
      {Removes an item from the list.}
      function Remove(AItem:Pointer):boolean;
      {Delets an item with a specific index from the list.}
      function Delete(AIndex:integer):boolean;
      {Returns the index of a specific item.}
      function IndexOf(AItem:Pointer):integer;
      {Pointer to the first item.} 
      function First:Pointer;
      {Pointer to the last item.}
      function Last:Pointer;

      {Deletes all entries in the linked list.}
      procedure Clear;

      {Resets the iterator.}
      procedure StartIteration;
      {Returns true, if the iterator reached the end of the list.}
      function ReachedEnd:boolean;
      {Returns the current item of the iterator and steps to the next item.}
      function GetCurrent:Pointer;
  end;

  {An abstract class for the use in TAdMap. Represents a key in the hashmap.}
  TAdMapKey = class
    public
      {Returns the hash of the object.}
      function Hash:integer;virtual;abstract;
      {Returns, whether this key is equal to another one.}
      function Equal(AItem:TAdMapKey):boolean;virtual;abstract;
  end;

  {Pointer on TAdMapPair.}
  PAdMapPair = ^TAdMapPair;
  
  {Represents a key and a value stored in the hash map.}
  TAdMapPair = record
    Key:TAdMapKey;{The key of the value}
    Value:TObject;{The value}
  end;

  {A simple bucket hash map class.}
  TAdMap = class
    private
      FCapacity: integer;
      FData: Pointer;
      FMemSize: Cardinal;
      procedure Rehash(ACapacity: integer);
    protected      
      FCount: integer;
      procedure FreeMemory(AFreeItems: boolean = false);
      property Data:Pointer read FData write FData;
    public
      {Creates a new instance of TAdMap. ACapacity specifies the size of the data array the elements are stored in.}
      constructor Create(ACapacity: integer=128);
      {Destroys the instance of TAdMap}
      destructor Destroy;override;

      {Inserts a key connected to a value in the hash map.}
      function Insert(AKey:TAdMapKey; AValue:TObject):Boolean;
      {Returns the stored object or nil, if the object is not found. AKey specifies the key you search with.}
      function GetValue(AKey:TAdMapKey):TObject;
      {Removes the object connected to the key from the hash map}
      function Remove(AKey:TAdMapKey):Boolean;

      {Returns the count of elements in the list}
      property Count: integer read FCount;
      {Returns the capacity of the data array.}
      property Capacity: integer read FCapacity;
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
  Clear;
  
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
    result := -1
  else
    result := i;
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
  if (AIndex >= Count) or (AIndex < 0) then
  begin
    result := nil;
  end
  else
  begin
    PItem := FStart;

    for i := 0 to AIndex-1 do
    begin
      PItem := PItem^.next;
    end;

    result := PItem^.data;
  end;
end;

procedure TAdLinkedList.SetItem(AIndex: integer; AValue: Pointer);
var
  i:integer;
  PItem:PAdLinkedListItem;
begin
  if (AIndex < Count) and (AIndex > 0) then
  begin
    PItem := FStart;

    for i := 0 to AIndex-1 do
    begin
      PItem := PItem^.next;
    end;

    PItem^.data := AValue;
  end;
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

procedure TAdLinkedList.Clear;
var
  p1, p2: PAdLinkedListItem;
begin
  //Get the first element
  p1 := FStart;

  //Iterate trough the list
  while p1 <> nil do
  begin
    //Save the element next to the current element
    p2 := p1^.next;
    //Free the current element
    Dispose(p1);
    //Go on iterating through the list using the next element
    p1 := p2;
  end;

  //Set everything to the initial settings
  FStart := nil;
  FLast := nil;
  FIterItem := nil;
  FCount := 0;
end;

{ TAdMap }

constructor TAdMap.Create(ACapacity: integer);
begin
  inherited Create;

  FData := nil;
  Rehash(ACapacity);
  FCount := 0;
end;

destructor TAdMap.Destroy;
begin
  FreeMemory(true);
  inherited;
end;

procedure TAdMap.FreeMemory(AFreeItems: boolean);
var
  i: integer;
  PList: PAdLinkedList;
  PItem: PAdMapPair;
begin
  if FData <> nil then
  begin
    PList := FData;
    for i := 0 to FCapacity-1 do
    begin
      if AFreeItems then
      begin
        PList^.StartIteration;
        while not PList^.ReachedEnd do
        begin
          PItem := PList^.GetCurrent;
          Dispose(PItem);
        end;
      end;

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
  Pos := (Abs(AKey.Hash) mod FCapacity);
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
  Pos := (Abs(AKey.Hash) mod FCapacity);
  PList := FData;
  inc(PList,Pos);

  //Check wether key already exists - replace value if necessary
  PList^.StartIteration;
  while not PList^.ReachedEnd do
  begin
    PItem := PAdMapPair(PList^.GetCurrent);
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
  PList^.Add(PItem);    

  FCount := FCount + 1;
  if FCount >= FCapacity * UpperRehashBound then
    Rehash(round(FCapacity * RehashFactor));
end;

procedure TAdMap.Rehash(ACapacity: integer);
var
  PTmp:Pointer;
  PList1,PList2:PAdLinkedList;
  Pos:integer;
  i:integer;
  PCurItem:PAdMapPair;
begin
  //Reserve new memory
  FMemSize := SizeOf(PAdLinkedList)*ACapacity;
  GetMem(PTmp, FMemSize);

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
    PList1^.StartIteration;
    while not PList1^.ReachedEnd do
    begin
      PCurItem := PList1^.GetCurrent;

      //Insert element
      PList2 := PTmp;
      Pos := (Abs(PCurItem^.Key.Hash) mod ACapacity);
      Inc(PList2, Pos);

      PList2^.Add(PCurItem);
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
  Pos := (Abs(AKey.Hash) mod FCapacity);
  PList := FData;
  inc(PList,Pos);

  PList^.StartIteration;
  while not PList^.ReachedEnd do
  begin
    PItem := PAdMapPair(PList^.GetCurrent);
    if PItem^.Key.Equal(AKey) then
    begin
      PList^.Remove(PItem);      
      Dispose(PItem);
      result := true;
      FCount := FCount - 1;

      if FCount < FCapacity * LowerRehashBound  then
        Rehash(round(FCapacity / RehashFactor));
        
      exit;
    end;
  end;
end;

end.
