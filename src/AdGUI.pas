{
* This program is licensed under the Common Public License (CPL) Version 1.0
* You should have recieved a copy of the license with this file.
* If not, see http://www.opensource.org/licenses/cpl1.0.txt for more
* informations.
*
* Inspite of the incompatibility between the Common Public License (CPL) and
* the GNU General Public License (GPL) you're allowed to use this program
* under the GPL.
* You also should have recieved a copy of this license with this file.
* If not, see http://www.gnu.org/licenses/gpl.txt for more informations.
*
* Project: Andorra 2D
* Author:  Andreas Stoeckel
* File: AdGUI.pas
* Comment: Contains the main GUI Components
}

unit AdGUI;

interface

uses SysUtils, Classes, JvSimpleXML, AdDraws, AdSkin, AdClasses, {$I AdTypes.inc},
     AdXML;

const
  Opac = 255;

type
  TAdDownRgn = (drNone,drMiddle,drLeftTop,drLeftBottom,drRightTop,drRightBottom);

  TAdComponent = class;

  TAdComponents = class(TList)
  private
    procedure SetItem(Index:integer;AValue:TAdComponent);
    function GetItem(Index:integer):TAdComponent;
  public
    property Items[index:integer]:TAdComponent read GetItem write SetItem; default;
  end;

  TAdComponent = class(TPersistent)
    private
      FSkin:TAdSkin;
      FComponents:TAdComponents;
      FParent:TAdComponent;
      FAdDraw:TAdDraw;
      FDesignMode:boolean;
      FCursor:string;
      FX,FY:single;
      FWidth,FHeight:single;
      FAlpha:byte;
      FEnabled:boolean;
      FVisible:boolean;
      FName:string;
      FFocused:boolean;
      FKeyPreview:boolean;
      FCanGetFocus:boolean;
      FSubComponent:boolean;
      FSpacerTop,FSpacerLeft,FSpacerRight,FSpacerBottom:integer;
      FCurrentCursor:string;
      procedure SetSkin(Value:TAdSkin);
      procedure SetAdDraw(Value:TAdDraw);
      procedure SetDesignMode(Value:boolean);
      procedure SetName(Value:string);
      procedure SetFocus(Value:boolean);
    protected
      function GetBoundsRect:TRect;
      function GetClientRect:TRect;
      procedure DoDraw;virtual;
      procedure DoMove(TimeGap:double);virtual;

      procedure SetFocused;
      procedure LooseFocus(Sender:TAdComponent);
      function GetFocusedComponent:TAdComponent;
      
      procedure LoadSkinItem;virtual;
      procedure SetCurrentCursor(Value:string);virtual;

      property KeyPreview:boolean read FKeyPreview write FKeyPreview;
      property SpacerTop:integer read FSpacerTop write FSpacerTop;
      property SpacerBottom:integer read FSpacerBottom write FSpacerBottom;
      property SpacerLeft:integer read FSpacerLeft write FSpacerLeft;
      property SpacerRight:integer read FSpacerRight write FSpacerRight;
      property Focused:boolean read FFocused write SetFocus;
      property CanGetFocus:boolean read FCanGetFocus write FCanGetFocus;
      property SubComponent:boolean read FSubComponent write FSubComponent;
    public
      procedure Draw;
      procedure Move(TimeGap:double);

      procedure AddComponent(AComponent:TAdComponent);virtual;

      constructor Create(AParent:TAdComponent);
      destructor Destroy;override;

      property Skin:TAdSkin read FSkin write SetSkin;
      property Parent:TAdComponent read FParent write FParent;
      property Components:TAdComponents read FComponents;
      property AdDraw:TAdDraw read FAdDraw write SetAdDraw;
      property DesignMode:boolean read FDesignMode write SetDesignMode;
      property BoundsRect:TRect read GetBoundsRect;
      property ClientRect:TRect read GetClientRect;
      property CurrentCursor:string read FCurrentCursor write SetCurrentCursor;
    published
      property Name:string read FName write SetName;
      property Cursor:string read FCursor write FCursor;
      property X:single read FX write FX;
      property Y:single read FY write FY;
      property Width:single read FWidth write FWidth;
      property Height:single read FHeight write FHeight;
      property Alpha:byte read FAlpha write FAlpha;
      property Visible:boolean read FVisible write FVisible;
      property Enabled:boolean read FEnabled write FEnabled;
  end;

  TAdMouseLibrary = class;

  TAdMouseCursor = class
    private
      FParent:TAdDraw;
      FLib:TAdMouseLibrary;
      FImage:TPictureCollectionItem;
      FAnimPos:single;
      FAnimStart:integer;
      FAnimStop:integer;
      FAnimSpeed:integer;
      FName:string;
      FOwnImage:boolean;
      FHotSpotX:integer;
      FHotSpotY:integer;
    protected
      property AnimPos:single read FAnimPos write FAnimPos;
    public
      CreatedByList:boolean;
      procedure Move(TimeGap:double);
      procedure Draw(X,Y:integer);

      procedure LoadFromXML(aroot:TJvSimpleXMLElem);virtual;
      function SaveToXML(aroot:TJvSimpleXMLElems):TJvSimpleXMLElem;virtual;

      constructor Create(AParent:TAdDraw);
      destructor Destroy;override;

      property Name:string read FName write FName;
      property Parent:TAdDraw read FParent write FParent;
      property AnimSpeed:integer read FAnimSpeed write FAnimSpeed;
      property AnimStart:integer read FAnimStart write FAnimStart;
      property AnimStop:integer read FAnimStop write FAnimStop;
      property Image:TPictureCollectionItem read FImage;
      property MouseLibrary:TAdMouseLibrary read FLib write FLib;

      property HotSpotX:integer read FHotSpotX write FHotSpotX;
      property HotSpotY:integer read FHotSpotY write FHotSpotY; 
  end;

  TAdMouseLibrary = class(TList)
    private
      FImages:TPictureCollection;
      FX,FY:integer;
      FParent:TAdDraw;
      FCurrentCursor:TAdMouseCursor;
      FCurrentCursorString:string;
      procedure SetCurrentCursor(Value:string);
      function GetItem(Index:integer):TAdMouseCursor;
      procedure SetItem(Index:integer;Value:TAdMouseCursor);
    protected
      procedure Notify(Ptr: Pointer; Action: TListNotification);override;
    public
      constructor Create(AParent:TAdDraw);
      destructor Destroy;override;

      procedure Move(TimeGap:double);
      procedure Draw;

      procedure LoadFromFile(AFile:string);
      procedure SaveToFile(AFile:string);
      procedure LoadFromXML(aroot:TJvSimpleXMLElem);virtual;
      function SaveToXML(aroot:TJvSimpleXMLElems):TJvSimpleXMLElem;virtual;

      property Items[Index:integer]:TAdMouseCursor read GetItem write SetItem;
      property Images:TPictureCollection read FImages write FImages;
      property X:integer read FX write FX;
      property Y:integer read FY write FY;
      property Parent:TAdDraw read FParent write FParent;
      property CurrentCursor:string read FCurrentCursorString write SetCurrentCursor;
  end;

  TAdGUI = class(TAdComponent)
    private
      FMouse:TAdMouseLibrary;
    protected
      procedure SetCurrentCursor(Value:string);override;
    public
      constructor Create(AParent:TAdDraw);
      destructor Destroy;override;

      procedure Update(TimeGap:double);

      property Cursors:TAdMouseLibrary read FMouse;
  end;


type TAdComponentClass = class of TAdComponent;

procedure RegisterComponent(AClass:TClass);

var
  RegisteredComponents:TStringList;

implementation

procedure RegisterComponent(AClass:TClass);
begin
  RegisteredComponents.Add(AClass.ClassName);
  RegisterClass(TPersistentClass(AClass));
end;

function InRect(x,y:integer;rect:TRect):boolean;
begin
  result := (x >= rect.Left) and
            (y >= rect.Top) and
            (x <= rect.Right) and
            (y <= rect.Bottom);
end;

{ TAdComponents }

procedure TAdComponents.SetItem(Index:integer;avalue:TAdComponent);
begin
  inherited Items[index] := avalue;
end;

function TAdComponents.GetItem(Index:integer):TAdComponent;
begin
  result := inherited Items[index];
end;

{ TAdComponent }

constructor TAdComponent.Create(AParent: TAdComponent);
begin
  inherited Create;

  FParent := AParent;

  FEnabled := true;

  FComponents := TAdComponents.Create;

  if FParent <> nil then
  begin
    AdDraw := FParent.AdDraw;
    FParent.AddComponent(self);
    FEnabled := FParent.Enabled;
    FDesignMode := FParent.DesignMode;
    FSkin := FParent.Skin;
  end;

  FAlpha := 255;
  FFocused := false;
  FVisible := true;
  FCursor := 'default';
  FCanGetFocus := true;
end;

destructor TAdComponent.Destroy;
begin
  while Components.Count > 0 do
  begin
    Components[0].Free;
  end;
  Components.Free;

  if FParent <> nil then
  begin
    FParent.Components.Remove(Self);
  end;
  inherited Destroy;
end;

procedure TAdComponent.AddComponent(AComponent: TAdComponent);
begin
  Components.Add(AComponent);
end;

procedure TAdComponent.DoDraw;
begin
  //
end;

procedure TAdComponent.DoMove(TimeGap:double);
begin
  //
end;

procedure TAdComponent.Draw;
var
  i:integer;
begin
  if FVisible then
  begin
    DoDraw;
    for i := 0 to Components.Count - 1 do
    begin
      Components[i].Draw;
    end;
  end;
end;

procedure TAdComponent.Move(TimeGap:double);
var
  i: Integer;
begin
  DoMove(TimeGap);
  for i := 0 to Components.Count - 1 do
  begin
    Components[i].Move(TimeGap);
  end;
end;

function TAdComponent.GetBoundsRect: TRect;
var rect:TRect;
begin
  if FParent = nil then
  begin
    result := Bounds(round(FX),round(FY),round(FWidth),round(FHeight));
  end
  else
  begin
    rect := FParent.ClientRect;
    result := Bounds(rect.Left+round(FX),rect.Top+round(FY),round(FWidth),round(FHeight));
  end;
end;

function TAdComponent.GetClientRect: TRect;
var rect:TRect;
begin
  rect := GetBoundsRect;
  result.Left := rect.Left + FSpacerLeft;
  result.Top := rect.Top + FSpacerTop;
  result.Right := rect.Right + FSpacerRight;
  result.Bottom := rect.Bottom + FSpacerBottom;
end;

procedure TAdComponent.SetAdDraw(Value: TAdDraw);
var
  i: integer;
begin
  FAdDraw := Value;
  for i := 0 to Components.Count - 1 do
  begin
    Components[i].AdDraw := Value;
  end;
end;

procedure TAdComponent.SetCurrentCursor(Value: string);
begin
  FCurrentCursor := Value;
  if FParent <> nil then
  begin
    FParent.CurrentCursor := Value;
  end;
end;

procedure TAdComponent.SetDesignMode(Value: boolean);
var
  i: integer;
begin
  FDesignMode := Value;
  for i := 0 to Components.Count - 1 do
  begin
    Components[i].DesignMode := Value;
  end;
end;

procedure TAdComponent.SetFocus(Value: boolean);
var
  i: integer;
begin
  if Value = true then
  begin
    SetFocused;
  end
  else
  begin
    Focused := false;
  end;                 
end;

procedure TAdComponent.SetFocused;
var i:integer;
begin
  if (CanGetFocus or DesignMode) and Enabled then
  begin
    if Assigned(Parent) then
    begin
      Parent.LooseFocus(self);
    end;
    for i := 0 to Components.Count-1 do
    begin
      Components[i].LooseFocus(self);
    end;
    FFocused := true;
  end;
end;

function TAdComponent.GetFocusedComponent: TAdComponent;
var
  i:integer;
begin
  result := nil;
  if Focused then
  begin
    result := self;
  end
  else
  begin
    for i := 0 to Components.Count-1 do
    begin
      result := Components[i].GetFocusedComponent;
      if result <> nil then
      begin
        break;
      end;
    end;
  end;
end;

procedure TAdComponent.LooseFocus(Sender: TAdComponent);
var
  i:integer;
begin
  FFocused := false;
  if Assigned(Parent) then
  begin
    if Sender <> FParent then
    begin
      FParent.LooseFocus(self);
    end;
  end;
  for i := 0 to Components.count-1 do
  begin
    if Components[i] <> Sender then Components[i].LooseFocus(self);
  end;
end;

procedure TAdComponent.SetName(Value: string);
begin
  FName := Value;
end;

procedure TAdComponent.SetSkin(Value: TAdSkin);
var i:integer;
begin
  FSkin := Value;
  for i := 0 to Components.Count - 1 do
  begin
    Components[i].Skin := Value;
  end;
  LoadSkinItem;
end;

procedure TAdComponent.LoadSkinItem;
begin
  //
end;


{ TAdMouseCursor }

constructor TAdMouseCursor.Create(AParent: TAdDraw);
begin
  inherited Create;
  FParent := AParent;
  CreatedByList := false;
  FAnimPos := 0;
  FAnimStart := 0;
  FAnimStop := 0;
  FOwnImage := false;
  FHotSpotX := 0;
  FHotSpotY := 0;
end;

destructor TAdMouseCursor.Destroy;
begin
  if FOwnImage then
  begin
    FImage.Free;
  end;
  inherited;
end;

procedure TAdMouseCursor.Draw(X, Y: integer);
begin
  if FImage <> nil then
  begin
    FImage.Draw(FParent,X-FHotSpotX,Y-FHotSpotY,round(FAnimPos));
  end;
end;

procedure TAdMouseCursor.Move(TimeGap:double);
begin
  if FAnimStop <> FAnimStart then
  begin
    FAnimPos := FAnimPos + FAnimSpeed * TimeGap;
    if round(FAnimPos) > FAnimStop then
    begin
      FAnimPos := FAnimStart;
    end;
  end
  else
  begin
    FAnimPos := FAnimStart;
  end;
end;

procedure TAdMouseCursor.LoadFromXML(aroot: TJvSimpleXMLElem);
var i:integer;
    ms:TMemoryStream;
begin
  if FOwnImage then
  begin
    FImage.Free;
    FOwnImage := false;
  end;

  FName := aroot.Properties.Value('name','');
  if (aroot.Properties.Value('src','') <> '') and (FLib <> nil) then
  begin
    FImage := FLib.Images.Find(aroot.Properties.Value('src',''));
  end;

  for i := 0 to aroot.Items.Count-1 do
  begin
    if aroot.Items[i].Name = 'hotspot' then
    begin
      FHotSpotX := aroot.Items[i].Properties.IntValue('x',0);
      FHotSpotY := aroot.Items[i].Properties.IntValue('y',0);
    end;
    if aroot.Items[i].Name = 'anim' then
    begin
      FAnimStart := aroot.Items[i].Properties.IntValue('start',0);
      FAnimStop := aroot.Items[i].Properties.IntValue('stop',0);
      FAnimSpeed := aroot.Items[i].Properties.IntValue('speed',15);
    end;
    if aroot.Items[i].Name = 'image' then
    begin
      ms := TMemoryStream.Create;
      ReadStream(ms,aroot.Items[i]);
      FOwnImage := true;
      ms.Position := 0;
      FImage := TPictureCollectionItem.Create(FParent);
      FImage.LoadFromStream(ms);
      FImage.Restore;
      ms.Free;
    end;
  end;
end;

function TAdMouseCursor.SaveToXML(aroot: TJvSimpleXMLElems): TJvSimpleXMLElem;
var trunk:TJvSimpleXMLElem;
    ms:TMemoryStream;
begin
  result := aroot.Add('cursor');
  result.Properties.Add('name',FName);

  trunk := result.Items.Add('hotspot');
  trunk.Properties.Add('x',FHotSpotX);
  trunk.Properties.Add('y',FHotSpotY);

  if FImage <> nil then
  begin
    if FImage.Texture.Texture.Loaded then
    begin
      trunk := result.Items.Add('image');
      ms := TMemoryStream.Create;
      FImage.SaveToStream(ms);
      WriteStream(ms,trunk);
      ms.Free;
    end;

    trunk := result.Items.Add('anim');
    trunk.Properties.Add('start',FAnimStart);
    trunk.Properties.Add('stop',FAnimStop);
    trunk.Properties.Add('speed',FAnimSpeed);
  end;
end;

{ TAdMouseLibrary }

constructor TAdMouseLibrary.Create(AParent: TAdDraw);
begin
  inherited Create;
  FParent := AParent;
  FImages := TPictureCollection.Create(FParent);
  FCurrentCursorString := '';
  FCurrentCursor := nil;
end;

destructor TAdMouseLibrary.Destroy;
begin
  FImages.Free;
  inherited;
end;

procedure TAdMouseLibrary.Draw;
begin
  if FCurrentCursor <> nil then
  begin
    FCurrentCursor.Draw(FX,FY);
  end;
end;

procedure TAdMouseLibrary.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then
  begin
    if TAdMouseCursor(Ptr).CreatedByList then
    begin
      TAdMouseCursor(Ptr).Free;
    end;
  end;
end;

procedure TAdMouseLibrary.Move(TimeGap: double);
begin
  if FCurrentCursor <> nil then
  begin
    FCurrentCursor.Move(TimeGap);
  end;
end;

procedure TAdMouseLibrary.LoadFromFile(AFile: string);
var
  XML:TJvSimpleXML;
  str:string;
begin
  XML := TJvSimpleXML.Create(nil);
  XML.LoadFromFile(AFile);
  LoadFromXML(XML.Root);
  XML.Free;

  str := FCurrentCursorString;
  FCurrentCursorString := '';
  SetCurrentCursor(str);
end;

procedure TAdMouseLibrary.SaveToFile(AFile: string);
var
  XML:TJvSimpleXML;
begin
  XML := TJvSimpleXML.Create(nil);
  SaveToXML(XML.Root.Items);
  XML.SaveToFile(AFile);
  XML.Free;
end;

procedure TAdMouseLibrary.LoadFromXML(aroot: TJvSimpleXMLElem);
var
  tmp:TAdMouseCursor;
  i: Integer;
  ms:TMemoryStream;
begin
  FImages.Clear;
  Clear;

  for i := 0 to aroot.Items.Count - 1 do
  begin
    if aroot.Items[i].Name = 'cursor' then
    begin
      tmp := TAdMouseCursor.Create(FParent);
      tmp.CreatedByList := true;
      tmp.MouseLibrary := self;
      tmp.LoadFromXML(aroot.Items[i]);
      Add(tmp);
    end;
    if aroot.Items[i].Name = 'images' then
    begin
      ms := TMemoryStream.Create;
      ReadStream(ms,aroot.Items[i]);
      ms.Position := 0;
      FImages.LoadFromStream(ms);
      FImages.Restore;
      ms.Free;
    end;
  end;
end;

function TAdMouseLibrary.SaveToXML(aroot: TJvSimpleXMLElems): TJvSimpleXMLElem;
var i:integer;
begin
  result := aroot.Add('set');
  for i := 0 to Count - 1 do
  begin
    Items[i].SaveToXML(result.Items);
  end;
end;

procedure TAdMouseLibrary.SetCurrentCursor(Value: string);
var i:integer;
begin
  if Value <> FCurrentCursorString then
  begin
    for i := 0 to Count - 1 do
    begin
      if lowercase(Value) = lowercase(Items[i].Name) then
      begin
        FCurrentCursor := Items[i];
        break;
      end;
    end;
    FCurrentCursorString := Value;
  end;
end;

procedure TAdMouseLibrary.SetItem(Index: integer; Value: TAdMouseCursor);
begin
  inherited Items[Index] := Value;
end;

function TAdMouseLibrary.GetItem(Index: integer): TAdMouseCursor;
begin
  result := inherited Items[Index];
end;

{ TAdGUI }

constructor TAdGUI.Create(AParent: TAdDraw);
begin
  inherited Create(nil);
  AdDraw := AParent;
  
  Skin := TAdSkin.Create(AdDraw);
  FMouse := TAdMouseLibrary.Create(AdDraw);
  CurrentCursor := 'default'
end;

destructor TAdGUI.Destroy;
begin
  Skin.Free;
  FMouse.Free;
  inherited;
end;

procedure TAdGUI.SetCurrentCursor(Value: string);
begin
  inherited;
  Cursors.CurrentCursor := Value;
end;

procedure TAdGUI.Update(TimeGap:double);
begin
  Cursors.Move(TimeGap);
  Cursors.Draw;
  Move(TimeGap);
  Draw;
end;

initialization
  RegisteredComponents := TStringList.Create;
  RegisterComponent(TAdComponent);

finalization
  RegisteredComponents.Free;


end.

