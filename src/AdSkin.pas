{
* This program is licensed under the Common Public License (CPL) Version 1.0
* You should have recieved a copy of the license with this file.
* If not, see http://www.opensource.org/licenses/cpl1.0.txt for more informations.
* 
* Inspite of the incompatibility between the Common Public License (CPL) and the GNU General Public License (GPL) you're allowed to use this program * under the GPL. 
* You also should have recieved a copy of this license with this file. 
* If not, see http://www.gnu.org/licenses/gpl.txt for more informations.

* Project: Andorra 2D
* Author:  Andreas Stoeckel
* File: AdSkin.pas
* Comment: Contains classes for skin management of gui components
}

{Contains classes for the skin management of the gui components}
unit AdSkin;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  SysUtils, Classes, AdSimpleXML, AdXML, AdList, AdDraws, AdTypes;

type
  //Defines the type of drawing elements. Currently only Stretch is supported.
  TAdSkinDrawType = (
    dtWrap,//<The image is repeated like a wallpaper
    dtStretch//<The images is streched to fit
  );
  
  //Anchors are used to resize the skin elements properly. The values have the same meaning as in the VCL
  TAdAnchor = (aaLeft,aaTop,aaBottom,aaRight);
  //A set of TAdAnchor
  TAdAnchors = set of TAdAnchor;

  //A skin element represents one single region which may contain an image.
  TAdSkinElem = class
    public
      //The drawtype of the element
      Typ:TAdSkinDrawType;
      //The anchors of the element
      Anchors:TAdAnchors;
      //The coordinates of the element
      X1,X2,Y1,Y2:integer;
      //The image coordinates of the element
      ImgSrcX1,ImgSrcX2,ImgSrcY1,ImgSrcY2:integer;
      //Used in the Editor
      Selected:boolean;
      //Defines wether this element, is the element which will be the client region of the component
      ClientRect:boolean;
      //Array which contains the imageindex of this element for each state. 
      Images:array of integer;
      //Loads the element from a JvSimpleXMlComponent
      procedure LoadFromXML(aroot:TAdSimpleXMLElem);virtual;
      //Saves the element to a JvSimpleXMlComponent
      function SaveToXML(aroot:TAdSimpleXMLElems):TAdSimpleXMLElem;virtual;
      //Creates an instance of the element
      constructor Create;
      //Destroys the instance of the element
      destructor Destroy;override;            
  end;

  TAdSkinElemList = class(TAdList)
    private
      FOnAdd:TNotifyEvent;
      procedure SetItem(Index:integer;Value:TAdSkinElem);
      function GetItem(Index:integer):TAdSkinElem;
    protected
      procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    public
      property Items[Index:integer]:TAdSkinElem read GetItem write SetItem;default;
      property OnAdd:TNotifyEvent read FOnAdd write FOnAdd;
  end;

  TAdSkinItem = class
    private
      FStates:TStringList;
      FParent:TAdDraw;
      FSkinElems:TAdSkinElemList;
      FBaseWidth,FBaseHeight:integer;
      FName:string;
      FImages:TAdImageList;
      procedure SetBaseWidth(Value:integer);
      procedure SetBaseHeight(Value:integer);
    protected
      procedure ScaleElements(AWidth,AHeight:integer);
      procedure ChangeStates(Sender:TObject);
    public
      CreatedByList:boolean;

      procedure LoadFromXML(aroot:TAdSimpleXMLElem);virtual;
      function SaveToXML(aroot:TAdSimpleXMLElems):TAdSimpleXMLElem;virtual;

      procedure Draw(AState,AX,AY,AWidth,AHeight:integer;Alpha:byte=255);overload;
      procedure Draw(AState:integer;ARect:TAdRect;Alpha:byte=255);overload;

      constructor Create(AParent:TAdDraw);
      destructor Destroy;override;

      property States:TStringList read FStates;
      property Parent:TAdDraw read FParent;
      property Elements:TAdSkinElemList read FSkinElems;
      property BaseWidth:integer read FBaseWidth write SetBaseWidth;
      property BaseHeight:integer read FBaseHeight write SetBaseHeight;
      property Name:string read FName write FName;
      property Images:TAdImageList read FImages write FImages;
  end;

  TAdSkin = class(TAdList)
    private
      FParent:TAdDraw;
      procedure SetItem(Index:integer;Value:TAdSkinItem);
      function GetItem(Index:integer):TAdSkinItem;
      procedure SetNamedItem(Index:string;Value:TAdSkinItem);
      function GetNamedItem(Index:string):TAdSkinItem;
    protected
      procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    public
      constructor Create(AParent:TAdDraw);
      destructor Destroy;override;

      procedure SaveToFile(AFile:string);
      procedure LoadFromFile(AFile:string);

      procedure SaveToStream(AStream:TStream);
      procedure LoadFromStream(AStream:TStream);

      function SaveToXML(aroot:TAdSimpleXMLElems):TAdSimpleXMLElem;
      procedure LoadFromXML(aroot:TAdSimpleXMLElem);

      function Add(Item:TAdSkinItem):integer;overload;
      function Add(Name:string):integer;overload;

      function IndexOf(Name:string):integer;overload;
      property Items[Index:integer]:TAdSkinItem read GetItem write SetItem;default;
      property ItemNamed[Index:string]:TAdSkinItem read GetNamedItem write SetNamedItem;
      property Parent:TAdDraw read FParent;
  end;

implementation

{ TAdSkinItem }

procedure TAdSkinItem.ChangeStates(Sender: TObject);
var i,j,l:integer;
begin
  for i := 0 to Elements.Count-1 do
  begin
    l := Length(Elements[i].Images);
    SetLength(Elements[i].Images,States.Count);
    if l < States.Count then
    begin
      for j := l to States.Count-1 do
      begin
        Elements[i].Images[j] := -1;
      end;
    end;
  end;
end;

constructor TAdSkinItem.Create(AParent: TAdDraw);
begin
  inherited Create;
  FParent := AParent;  

  CreatedByList := false;

  FSkinElems := TAdSkinElemList.Create;
  FSkinElems.OnAdd := ChangeStates;

  FBaseWidth := 100;
  FBaseHeight := 100;

  FImages := TAdImageList.Create(FParent);

  FStates := TStringlist.Create;
  FStates.OnChange := ChangeStates;
  FStates.Add('standard');
end;

destructor TAdSkinItem.Destroy;
begin
  FSkinElems.Free;
  FImages.Free;
  FStates.Free;
  inherited Destroy;
end;

procedure TAdSkinItem.Draw(AState:integer; ARect: TAdRect; Alpha: byte);
begin
  Draw(AState, ARect.Left, ARect.Top, ARect.Right - ARect.Left, ARect.Bottom - ARect.Top, Alpha);
end;

procedure TAdSkinItem.Draw(AState, AX, AY, AWidth, AHeight: integer;Alpha: byte);
var i:integer;
begin
  if (FBaseWidth <> AWidth) or (FBaseHeight <> AHeight) then
  begin
    ScaleElements(AWidth,AHeight);
  end;
  for i := 0 to Elements.Count-1 do
  begin
    with Elements[i] do
    begin
      if (AState < Length(Images)) and (AState > -1) then
      begin
        if (Images[AState] < Self.Images.Count) and (Images[AState] > -1) then
        begin

          case Typ of
            dtStretch:
            begin
              Self.Images[Images[AState]].StretchBltAlpha(
                FParent,
                AdRect(ImgSrcX1,ImgSrcY1,ImgSrcX2,ImgSrcY2),
                AdRect(AX+X1,AY+Y1,AX+X2,AY+Y2),0.5,0.5,0,Alpha);
            end;
          end;

        end;
      end;
    end;
  end;
end;

procedure TAdSkinItem.LoadFromXML(aroot: TAdSimpleXMLElem);
var
  i:integer;
  trunk:TAdSimpleXMLElem;
  ms:TMemoryStream;
  elem:TAdSkinElem;
begin
  FName := aroot.Properties.Value('name','');
  FBaseWidth := aroot.Properties.IntValue('basewidth',100);
  FBaseHeight := aroot.Properties.IntValue('baseheight',100);

  FStates.Clear;
  trunk := aroot.Items.ItemNamed['states'];
  for i := 0 to trunk.Items.Count-1 do
  begin
    FStates.Add(trunk.Items[i].Name);
  end;  

  FSkinElems.Clear;
  trunk := aroot.Items.ItemNamed['elements'];
  for i := 0 to trunk.Items.Count-1 do
  begin
    elem := TAdSkinElem.Create;
    elem.LoadFromXML(trunk.Items[i]);
    FSkinElems.Add(elem);
  end;

  FImages.Clear;
  trunk := aroot.Items.ItemNamed['images'];
  for i := 0 to trunk.Items.Count-1 do
  begin
    ms := TMemoryStream.Create;
    ReadStream(ms,trunk.Items[i]);
    ms.Position := 0;
    try
      with Images.Add(trunk.Items[i].Name) do
      begin
        LoadFromStream(ms);
      end;
    finally
      ms.Free;
    end;
  end;
  Images.Restore;
end;

function TAdSkinItem.SaveToXML(aroot: TAdSimpleXMLElems): TAdSimpleXMLElem;
var
  i:integer;
  trunk:TAdSimpleXMLElem;
  ms:TMemoryStream;
begin
  result := aroot.Add('item');
  result.Properties.Add('name',FName);
  result.Properties.Add('basewidth',FBaseWidth);
  result.Properties.Add('baseheight',FBaseHeight);
  trunk := result.Items.Add('states');
  for i := 0 to States.Count - 1 do
  begin
    trunk.Items.Add(states[i]);
  end;
  trunk := result.Items.Add('elements');
  for i := 0 to Elements.Count - 1 do
  begin
    Elements[i].SaveToXML(trunk.Items);
  end;
  trunk := result.Items.Add('images');
  for i := 0 to Images.Count - 1 do
  begin
    ms := TMemoryStream.Create;
    Images[i].SaveToStream(ms);
    WriteStream(ms,trunk.Items.Add(Images.Items[i].Name));
    ms.Free;
  end;
end;

procedure TAdSkinItem.ScaleElements(AWidth, AHeight: integer);
var i:integer;
begin
  for i := 0 to FSkinElems.Count-1 do
  begin
    with FSkinElems[i] do
    begin
      if ((aaLeft in Anchors) and (aaRight in Anchors)) and
         not ((aaTop in Anchors) and (aaBottom in Anchors)) then
      begin
        X2 := X2 + (AWidth - FBaseWidth);
        if aaBottom in Anchors then
        begin
          Y1 := Y1 + (AHeight - FBaseHeight);
          Y2 := Y2 + (AHeight - FBaseHeight);
        end;
      end;
      if ((aaTop in Anchors) and (aaBottom in Anchors)) and
         not ((aaLeft in Anchors) and (aaRight in Anchors)) then
      begin
        Y2 := Y2 + (AHeight - FBaseHeight);
        if aaRight in Anchors then
        begin
          X1 := X1 + (AWidth - FBaseWidth);
          X2 := X2 + (AWidth - FBaseWidth);
        end;
      end;
      if Anchors = [aaRight,aaBottom]then
      begin
        X1 := X1 + (AWidth - FBaseWidth);
        Y1 := Y1 + (AHeight - FBaseHeight);
        X2 := X2 + (AWidth - FBaseWidth);
        Y2 := Y2 + (AHeight - FBaseHeight);
      end;
      if Anchors = [aaLeft,aaTop,aaRight,aaBottom]then
      begin
        X2 := X2 + (AWidth - FBaseWidth);
        Y2 := Y2 + (AHeight - FBaseHeight);
      end;
      if Anchors = [aaLeft,aaBottom]then
      begin
        Y1 := Y1 + (AHeight - FBaseHeight);
        Y2 := Y2 + (AHeight - FBaseHeight);
      end;
      if Anchors = [aaTop,aaRight]then
      begin
        X1 := X1 + (AWidth - FBaseWidth);
        X2 := X2 + (AWidth - FBaseWidth);
      end;
    end;
  end;
  FBaseWidth := AWidth;
  FBaseHeight := AHeight;
end;

procedure TAdSkinItem.SetBaseHeight(Value: integer);
begin
  ScaleElements(FBaseWidth,Value);
end;

procedure TAdSkinItem.SetBaseWidth(Value: integer);
begin
  ScaleElements(Value,FBaseHeight);
end;

{ TAdSkin }

function TAdSkin.Add(Item: TAdSkinItem): integer;
begin
  result := inherited Add(Item);
end;

function TAdSkin.Add(Name: string): integer;
var Item:TAdSkinItem;
begin
  Item := TAdSkinItem.Create(FParent);
  Item.CreatedByList := true;
  Item.Name := Name;
  result := inherited Add(Item);
end;

constructor TAdSkin.Create(AParent: TAdDraw);
begin
  inherited Create;
  FParent := AParent;
end;

destructor TAdSkin.Destroy;
begin  
  Clear;
  inherited Destroy;
end;

function TAdSkin.GetItem(Index: integer): TAdSkinItem;
begin
  result := inherited Items[Index];
end;

function TAdSkin.GetNamedItem(Index: string): TAdSkinItem;
var
  i:integer;
begin
  result := nil;
  i := IndexOf(Index);
  if i <> -1 then
  begin
    result := Items[i];
  end;
end;

function TAdSkin.IndexOf(Name: string): integer;
var
  i:integer;
begin
  result := -1;
  for i := 0 to Count - 1 do
  begin
    if lowercase(Items[i].Name) = lowercase(Name) then
    begin
      result := i;
      break;
    end;
  end;
end;

procedure TAdSkin.SetNamedItem(Index: string; Value: TAdSkinItem);
var
  i:integer;
begin
  i := IndexOf(Index);
  if i <> -1 then
  begin
    Items[i] := Value;
  end
  else
  begin
    raise EListError.Create('Element not found.');
  end;  
end;

procedure TAdSkin.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then
  begin
    with TAdSkinItem(Ptr) do
    begin
      if CreatedByList then Free;
    end;
  end;
end;

procedure TAdSkin.SetItem(Index: integer; Value: TAdSkinItem);
begin
  inherited Items[Index] := Value;
end;

procedure TAdSkin.SaveToFile(AFile: string);
var ms:TMemoryStream;
begin
  ms := TMemoryStream.Create;
  SaveToStream(ms);
  ms.SaveToFile(AFile);
  ms.Free;
end;

procedure TAdSkin.LoadFromFile(AFile: string);
var ms:TMemoryStream;
begin
  ms := TMemoryStream.Create;
  ms.LoadFromFile(AFile);
  ms.Position := 0;
  LoadFromStream(ms);
  ms.Free;
end;

procedure TAdSkin.LoadFromStream(AStream: TStream);
var XML:TAdSimpleXML;
begin
  XML := TAdSimpleXML.Create;
  XML.LoadFromStream(AStream);
  LoadFromXML(XML.Root);
  XML.Free;
end;

procedure TAdSkin.SaveToStream(AStream: TStream);
var
  XML:TAdSimpleXML;
begin
  XML := TAdSimpleXML.Create;
  SaveToXML(XML.Root.Items);
  XML.SaveToStream(AStream);
  XML.Free;
end;

procedure TAdSkin.LoadFromXML(aroot: TAdSimpleXMLElem);
var
  tmp:TAdSkinItem;
  i:integer;
begin
  Clear;
  for i := 0 to aroot.Items.Count - 1 do
  begin
    tmp := TAdSkinItem.Create(FParent);
    tmp.LoadFromXML(aroot.Items[i]);
    tmp.CreatedByList := true;
    Add(tmp);
  end;
end;

function TAdSkin.SaveToXML(aroot: TAdSimpleXMLElems): TAdSimpleXMLElem;
var i:integer;
begin
  result := aroot.Add('skin');
  for i := 0 to Count - 1 do
  begin
    Items[i].SaveToXML(result.Items);
  end;
end;

{ TAdSkinElemList }

function TAdSkinElemList.GetItem(Index: integer): TAdSkinElem;
begin
  result := inherited Items[Index];
end;

procedure TAdSkinElemList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnAdded then
  begin
    if Assigned(FOnAdd) then
    begin
      FOnAdd(self);
    end;
  end;
  if Action = lnDeleted then
  begin
    TAdSkinElem(Ptr).Free;
  end;
end;

procedure TAdSkinElemList.SetItem(Index: integer; Value: TAdSkinElem);
begin
  inherited Items[index] := Value;
end;

{ TAdSkinElem }

constructor TAdSkinElem.Create;
begin
  inherited Create;
  Anchors := [aaLeft,aaTop];
  Typ := dtStretch;
  ClientRect := false;
end;

destructor TAdSkinElem.Destroy;
begin
  inherited Destroy;
end;

procedure TAdSkinElem.LoadFromXML(aroot: TAdSimpleXMLElem);
var s:string;
    trunk:TAdSimpleXMLElem;
    i:integer;
begin
  x1 := aroot.Properties.IntValue('x1',0);
  y1 := aroot.Properties.IntValue('y1',0);
  x2 := aroot.Properties.IntValue('x2',100);
  y2 := aroot.Properties.IntValue('y2',100);
  imgsrcx1 := aroot.Properties.IntValue('imgsrcx1',0);
  imgsrcy1 := aroot.Properties.IntValue('imgsrcy1',0);
  imgsrcx2 := aroot.Properties.IntValue('imgsrcx2',0);
  imgsrcy2 := aroot.Properties.IntValue('imgsrcy2',0);
  clientrect := aroot.Properties.BoolValue('clientrect',false);

  s := lowercase(aroot.Properties.Value('typ','dtStretch'));
  if s = 'dtwrap' then typ := dtWrap;
  if s = 'dtstretch' then typ := dtStretch;

  s := lowercase(aroot.Properties.Value('anchors','aaLeft;aaRight'));
  Anchors := [];
  if Pos('aaleft',s) <> 0 then Anchors := Anchors + [aaLeft];
  if Pos('aaright',s) <> 0 then Anchors := Anchors + [aaRight];
  if Pos('aatop',s) <> 0 then Anchors := Anchors + [aaTop];
  if Pos('aabottom',s) <> 0 then Anchors := Anchors + [aaBottom];

  trunk := aroot.Items.ItemNamed['images'];
  SetLength(images,trunk.Items.Count);
  for i := 0 to trunk.Items.Count - 1 do
  begin
    Images[i] := trunk.Items[i].Properties.IntValue('index',-1);
  end;
end;

function TAdSkinElem.SaveToXML(aroot: TAdSimpleXMLElems): TAdSimpleXMLElem;
var s:string;
    i:integer;
    trunk:TAdSimpleXMLElem;
begin
  result := aroot.Add('element');
  result.Properties.Add('x1',x1);
  result.Properties.Add('y1',y1);
  result.Properties.Add('x2',x2);
  result.Properties.Add('y2',y2);
  result.Properties.Add('imgsrcx1',imgsrcx1);
  result.Properties.Add('imgsrcy1',imgsrcy1);
  result.Properties.Add('imgsrcx2',imgsrcx2);
  result.Properties.Add('imgsrcy2',imgsrcy2);
  result.Properties.Add('clientrect',clientrect);
  
  case typ of
    dtWrap: result.Properties.Add('typ','dtWrap');
    dtStretch: result.Properties.Add('typ','dtStretch');
  end;

  s := '';
  if aaLeft in Anchors then s := s + 'aaLeft;';
  if aaRight in Anchors then s := s + 'aaRight;';
  if aaTop in Anchors then s := s + 'aaTop;';
  if aaBottom in Anchors then s := s + 'aaBottom;';
  result.Properties.Add('anchors',s);

  trunk := result.Items.Add('images');
  for i := 0 to length(images)-1 do
  begin
    trunk.Items.Add('stateimg').Properties.Add('index',images[i]);
  end;    
end;

end.
