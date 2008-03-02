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

{Contains the main GUI classes.}
unit AdGUI;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  SysUtils, Classes,
  AdEvents, AdSimpleXML, AdDraws, AdSkin, AdClasses, AdTypes,
  AdXML, AdList, AdCanvas, AdFont, AdFontList, AdPersistent;


type

  TAdDownRgn = (drNone,drMiddle,drLeftTop,drLeftBottom,drRightTop,drRightBottom);

  TAdComponent = class;

  TGUIColor = -$7FFFFFFF-1..$7FFFFFFF;

  TAdHint = class
    private
      FShowTime:single;
      FWaitTime:single;
      FX,FY:integer;
      FText:string;
      FShowedTime:single;
      FColor:longint;

      FAlpha:byte;
      FCurrentAlpha:single;
      FParent:TAdDraw;
      FVisible:boolean;
      FFadeTime:single;
      FFadeIn,FFadeOut:boolean;
      FTextColor:longint;
      FBorderColor:longint;
      procedure SetAlpha(Value:byte);
    public
      constructor Create(AParent:TAdDraw);
      destructor Destroy; override;
      procedure Show(MouseX,MouseY:integer; Text:string; Sender:TAdComponent);virtual;
      procedure Hide;virtual;
      procedure Draw;virtual;
      procedure Move(TimeGap:double);virtual;
      
      property ShowTime:single read FShowTime write FShowTime;
      property WaitTime:single read FWaitTime write FWaitTime;
      property Color:longint read FColor write FColor;
      property Alpha:byte read FAlpha write SetAlpha;
      property Parent:TAdDraw read FParent;
      property Visible:boolean read FVisible write FVisible;
      property FadeTime:single read FFadeTime write FFadeTime;
      property BorderColor:longint read FBordercolor write FBorderColor;
      property TextColor:longint read FTextColor write FTextColor;
  end;

  TAdComponents = class(TAdList)
    private
      procedure SetItem(Index:integer;AValue:TAdComponent);
      function GetItem(Index:integer):TAdComponent;
    public
      property Items[index:integer]:TAdComponent read GetItem write SetItem; default;
  end;

  TAdComponent = class(TAdPersistent)
    private
      FSkin:TAdSkin;
      FComponents:TAdComponents;
      FParent:TAdComponent;
      FAdDraw:TAdDraw;
      FDesignMode:boolean;
      FCursor:string;
      FX,FY:integer;
      FWidth,FHeight:integer;
      FAlpha:byte;
      FEnabled:boolean;
      FVisible:boolean;
      FName:string;
      FFocused:boolean;
      FKeyPreview:boolean;
      FMousePreview:boolean;
      FCanGetFocus:boolean;
      FSubComponent:boolean;
      FSpacerTop,FSpacerLeft,FSpacerRight,FSpacerBottom:integer;
      FCurrentCursor:string;

      FMouseOver:boolean;
      FMouseOverTime:single;
      FMouseDownIn:TAdDownRgn;
      FDraging:boolean;
      FOX,FOY:integer;

      FOnMouseDown:TAdMouseEvent;
      FOnMouseUp:TAdMouseEvent;
      FOnMouseMove:TAdMouseMoveEvent;
      FOnDblClick:TAdNotifyEvent;
      FOnClick:TAdNotifyEvent;
      FOnKeyPress:TAdKeyPressEvent;
      FOnKeyUp:TAdKeyEvent;
      FOnKeyDown:TAdKeyEvent;
      FOnMouseEnter:TAdNotifyEvent;
      FOnMouseLeave:TAdNotifyEvent;
      FOnMouseWheel:TAdMouseWheelEvent;

      FHint:string;
      FShowHint:boolean;
      FShowedHint:boolean;
      FHintWnd:TAdHint;

      FMouseX,FMouseY:integer;

      FFont:TAdFont;
      FFontColor:TGUIColor;
      FFontName:string;
      FFonts:TAdFontList;

      FGridX,FGridY:integer;
      FGrid:Boolean;

      FMinWidth:integer;
      FMinHeight:integer;
      FMaxWidth:integer;
      FMaxHeight:integer;

      FAcceptChildComponents:boolean;

      FLockEvents:boolean;

      procedure SetSkin(Value:TAdSkin);
      procedure SetAdDraw(Value:TAdDraw);
      procedure SetDesignMode(Value:boolean);
      procedure SetName(Value:string);
      procedure SetFocus(Value:boolean);
      procedure SetHintWnd(Value:TAdHint);
      function GetFont:TAdFont;
      procedure SetFont(Value:TAdFont);
      procedure SetFontName(Value:string);
      procedure SetFonts(Value:TAdFontList);
      procedure SetGridX(Value:integer);
      procedure SetGridY(Value:integer);
      procedure SetGrid(Value:boolean);
      procedure SetWidth(Value:integer);
      procedure SetHeight(Value:integer);
      procedure SetLockEvents(Value:boolean);
    protected
      function GetBoundsRect:TAdRect;
      function GetClientRect:TAdRect;
      procedure DoDraw;virtual;
      procedure DoMove(TimeGap:double);virtual;

      procedure LooseFocus(Sender:TAdComponent);virtual;
      function GetFocusedComponent:TAdComponent;
      
      procedure LoadSkinItem;virtual;
      procedure SetCurrentCursor(Value:string);virtual;

      function DoResize:boolean;virtual;
      function DoMouseMove(Shift: TAdShiftState; X, Y: Integer):boolean;virtual;
      function DoMouseDown(Button: TAdMouseButton; Shift: TAdShiftState; X, Y: Integer):boolean;virtual;
      function DoMouseUp(Button: TAdMouseButton; Shift: TAdShiftState; X, Y: Integer):boolean;virtual;
      function DoMouseEnter:boolean;virtual;
      function DoMouseLeave:boolean;virtual;
      function DoClick(X, Y:Integer):boolean;virtual;
      function DoDblClick(X, Y:Integer):boolean;virtual;
      function DoKeyPress(key:Char):boolean;virtual;
      function DoKeyUp(Key:Word; Shift:TAdShiftState):boolean;virtual;
      function DoKeyDown(Key:Word; Shift:TAdShiftState):boolean;virtual;
      function DoMouseWheel(Shift: TAdShiftState; WheelDelta: Integer; X, Y: Integer):boolean;virtual;

      procedure DesignSize(X,Y:integer);
      function GetDownRgn(AX,AY:integer):TAdDownRgn;      
      procedure CheckMouseEnter(oldvalue:boolean);

      procedure SetSpacer(ASkinItem:TAdSkinItem);

      procedure SetFontColor;

      property KeyPreview:boolean read FKeyPreview write FKeyPreview;
      property MousePreview:boolean read FMousePreview write FMousePreview;
      property SpacerTop:integer read FSpacerTop write FSpacerTop;
      property SpacerBottom:integer read FSpacerBottom write FSpacerBottom;
      property SpacerLeft:integer read FSpacerLeft write FSpacerLeft;
      property SpacerRight:integer read FSpacerRight write FSpacerRight;
      property Focused:boolean read FFocused write SetFocus;
      property CanGetFocus:boolean read FCanGetFocus write FCanGetFocus;

      property MouseOver:boolean read FMouseOver write FMouseOver;
      property MouseOverTime:single read FMouseOverTime;
      property OX:integer read FOX write FOX;
      property OY:integer read FOY write FOY;
      property MouseDownIn:TAdDownRgn read FMouseDownIn write FMouseDownIn;
      
      property ShowedHint:boolean read FShowedHint write FShowedHint;

      property MouseX:integer read FMouseX;
      property MouseY:integer read FMouseY;
      property Draging:boolean read FDraging write FDraging;

      property MinWidth:integer read FMinWidth write FMinWidth;
      property MaxWidth:integer read FMaxWidth write FMaxWidth;
      property MinHeight:integer read FMinHeight write FMinHeight;
      property MaxHeight:integer read FMaxHeight write FMaxHeight;

      property AcceptChildComponents:boolean read FAcceptChildComponents write FAcceptChildComponents;

      property LockEvents:boolean read FLockEvents write SetLockEvents;

    public
      procedure Draw;
      procedure Move(TimeGap:double);

      procedure AddComponent(AComponent:TAdComponent);virtual;
      procedure Clear;

      function DblClick(X,Y:integer) : boolean;
      function Click(X,Y:integer) : boolean;
      function MouseMove(Shift: TAdShiftState; X, Y: Integer) : boolean;
      function MouseDown(Button: TAdMouseButton; Shift: TAdShiftState; X, Y: Integer) : boolean;
      function MouseUp(Button: TAdMouseButton; Shift: TAdShiftState; X, Y: Integer) : boolean;
      function MouseWheel(Shift: TAdShiftState; WheelDelta: Integer; X, Y: Integer) : boolean;
      function KeyPress(Key: Char) : boolean;
      function KeyDown(Key: Word; Shift:TAdShiftState) : boolean;
      function KeyUp(Key: Word; Shift:TAdShiftState) : boolean;

      function ClientToScreen(p:TAdPoint):TAdPoint;
      function ScreenToClient(p:TAdPoint):TAdPoint;

      constructor Create(AParent:TAdComponent);virtual;
      destructor Destroy;override;

      procedure SaveToFile(AFile:string);
      procedure LoadFromFile(AFile:string);
      procedure SaveToStream(AStream:TStream);
      procedure LoadFromStream(AStream:TStream);
      procedure LoadFromXML(aroot:TAdSimpleXMLElem);virtual;
      function SaveToXML(aroot:TAdSimpleXMLElems):TAdSimpleXMLElem;virtual;

      procedure BringToFront;
      procedure SendToBack;

      function GetUniqueName(AName:string; AForceNumber:boolean=false):string;
      function NameExists(AName:string):boolean;
      function FindComponent(AName:string):TAdComponent;

      function OwnsComponent(AComponent:TAdComponent):boolean;

      property Skin:TAdSkin read FSkin write SetSkin;
      property Parent:TAdComponent read FParent write FParent;
      property Components:TAdComponents read FComponents;
      property AdDraw:TAdDraw read FAdDraw write SetAdDraw;
      property DesignMode:boolean read FDesignMode write SetDesignMode;
      property BoundsRect:TAdRect read GetBoundsRect;
      property ClientRect:TAdRect read GetClientRect;
      property CurrentCursor:string read FCurrentCursor write SetCurrentCursor;
      property HintWnd:TAdHint read FHintWnd write SetHintWnd;

      property GridX:integer read FGridY write SetGridY;
      property GridY:integer read FGridX write SetGridX;
      property Grid:boolean read FGrid write SetGrid;

      property FontColor:TGUIColor read FFontColor write FFontColor;
      property Font:TAdFont read GetFont write SetFont;
      property FontName:string read FFontName write SetFontName;
      property Fonts:TAdFontList read FFonts write SetFonts;

      property FocusedComponent:TAdComponent read GetFocusedComponent;
      procedure SetFocused;

      property SubComponent:boolean read FSubComponent write FSubComponent;
    published
      property Name:string read FName write SetName;
      property Cursor:string read FCursor write FCursor;
      property X:integer read FX write FX;
      property Y:integer read FY write FY;
      property Width:integer read FWidth write SetWidth;
      property Height:integer read FHeight write SetHeight;
      property Alpha:byte read FAlpha write FAlpha;
      property Visible:boolean read FVisible write FVisible;
      property Enabled:boolean read FEnabled write FEnabled;

      property Hint:string read FHint write FHint;
      property ShowHint:boolean read FShowHint write FShowHint;

      property OnClick:TAdNotifyEvent read FOnClick write FOnClick;
      property OnDblClick:TAdNotifyEvent read FOnDblClick write FOnDblClick;
      property OnMouseMove:TAdMouseMoveEvent read FOnMouseMove write FOnMouseMove;
      property OnMouseUp:TAdMouseEvent read FOnMouseUp write FOnMouseUp;
      property OnMouseDown:TAdMouseEvent read FOnMouseDown write FOnMouseDown;
      property OnMouseEnter:TAdNotifyEvent read FOnMouseEnter write FOnMouseEnter;
      property OnMouseLeave:TAdNotifyEvent read FOnMouseLeave write FOnMouseLeave;
      property OnMouseWheel:TAdMouseWheelEvent read FOnMouseWheel write FOnMouseWheel;
      property OnKeyPress:TAdKeyPressEvent read FOnKeyPress write FOnKeyPress;
      property OnKeyUp:TAdKeyEvent read FOnKeyUp write FOnKeyUp;
      property OnKeyDown:TAdKeyEvent read FOnKeyDown write FOnKeyDown;
  end;

  TAdMouseLibrary = class;

  TAdMouseCursor = class
    private
      FParent:TAdDraw;
      FLib:TAdMouseLibrary;
      FImage:TAdImage;
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

      procedure LoadFromXML(aroot:TAdSimpleXMLElem);virtual;
      function SaveToXML(aroot:TAdSimpleXMLElems):TAdSimpleXMLElem;virtual;

      constructor Create(AParent:TAdDraw);
      destructor Destroy;override;

      property Name:string read FName write FName;
      property Parent:TAdDraw read FParent write FParent;
      property AnimSpeed:integer read FAnimSpeed write FAnimSpeed;
      property AnimStart:integer read FAnimStart write FAnimStart;
      property AnimStop:integer read FAnimStop write FAnimStop;
      property Image:TAdImage read FImage;
      property MouseLibrary:TAdMouseLibrary read FLib write FLib;

      property HotSpotX:integer read FHotSpotX write FHotSpotX;
      property HotSpotY:integer read FHotSpotY write FHotSpotY; 
  end;

  TAdMouseLibrary = class(TAdList)
    private
      FImages:TAdImageList;
      FX,FY:integer;
      FParent:TAdDraw;
      FCurrentCursor:TAdMouseCursor;
      FCurrentCursorString:string;
      FVisible:boolean;
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
      procedure LoadFromXML(aroot:TAdSimpleXMLElem);virtual;
      function SaveToXML(aroot:TAdSimpleXMLElems):TAdSimpleXMLElem;virtual;

      property Items[Index:integer]:TAdMouseCursor read GetItem write SetItem;
      property Images:TAdImageList read FImages write FImages;
      property X:integer read FX write FX;
      property Y:integer read FY write FY;
      property Parent:TAdDraw read FParent write FParent;
      property CurrentCursor:string read FCurrentCursorString write SetCurrentCursor;
      property Visible:boolean read FVisible write FVisible;
  end;

  TAdGUI = class(TAdComponent)
    private
      FMouse:TAdMouseLibrary;
      FMouseX,FMouseY:integer;
      FOwnHintWnd:boolean;
      FOwnFonts:boolean;
      FSaveFonts:boolean;
      procedure SetHintWnd(Value:TAdHint);
    protected
      procedure SetCurrentCursor(Value:string);override;
      function DoMouseMove(Shift: TAdShiftState; X, Y: Integer):boolean;override;
    public
      constructor Create(AParent:TAdDraw);reintroduce;
      destructor Destroy;override;

      procedure LoadFromXML(aroot:TAdSimpleXMLElem);override;
      function SaveToXML(aroot:TAdSimpleXMLElems):TAdSimpleXMLElem;override;

      procedure Update(TimeGap:double);

      property Cursors:TAdMouseLibrary read FMouse;
      property MouseX:integer read FMouseX;
      property MouseY:integer read FMouseY;

      property HintWnd write SetHintWnd;
      property SaveFonts:boolean read FSaveFonts write FSaveFonts;
    published
      property Fonts;
  end;

type TAdComponentClass = class of TAdComponent;

procedure RegisterComponent(AClass:TClass;ACard:string);

var
  RegisteredComponents:TStringList;

implementation

procedure RegisterComponent(AClass:TClass;ACard:string);
begin
  RegisteredComponents.Add(AClass.ClassName+'='+ACard);
  AdRegisterClass(TAdPersistentClass(AClass));
end;

function InRect(x,y:integer;rect:TAdRect):boolean;
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
var
  tmp:TAdComponent;
begin
  inherited Create;

  FFontColor := 0;

  FParent := AParent;

  FEnabled := true;

  FComponents := TAdComponents.Create;

  FGrid := true;
  FGridX := 5;
  FGridY := 5;

  FMinWidth := 0;
  FMinHeight := 0;
  FMaxWidth := -1;
  FMaxHeight := -1;

  FAcceptChildComponents := true;

  if FParent <> nil then
  begin
    tmp := FParent;
    while not tmp.AcceptChildComponents do
    begin
      tmp := tmp.FParent;
    end;
    FParent := tmp;
    AdDraw := FParent.AdDraw;
    FParent.AddComponent(self);
    FEnabled := FParent.Enabled;
    FDesignMode := FParent.DesignMode;
    FHintWnd := FParent.HintWnd;
    Skin := FParent.Skin;

    FFontColor := FParent.FontColor;
    FFonts := FParent.Fonts;
    FFontName := '';

    FFont := TAdFont.Create(AdDraw.AdAppl);
    Font := FParent.Font;

    FGrid := FParent.Grid;
    FGridX := FParent.GridX;
    FGridY := FParent.GridY;
  end;

  FAlpha := 255;
  FFocused := false;
  FVisible := true;
  FCursor := 'default';
  FCanGetFocus := true;

  FName := GetUniqueName(copy(ClassName,2,length(ClassName)-1),true);
end;

destructor TAdComponent.Destroy;
begin
  while Components.Count > 0 do
  begin
    Components[0].Free;
  end;
  Components.Free;

  FFont.Free;

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

//Load/Save

procedure TAdComponent.LoadFromFile(AFile: string);
var XML:TAdSimpleXML;
begin
  XML := TAdSimpleXML.Create;
  XML.LoadFromFile(AFile);
  LoadFromXML(XML.Root);
  XML.Free;
end;

procedure TAdComponent.SaveToFile(AFile: string);
var XML:TAdSimpleXML;
begin
  XML := TAdSimpleXML.Create;
  SaveToXML(XML.Root.Items);
  XML.SaveToFile(AFile);
  XML.Free;
end;

procedure TAdComponent.LoadFromStream(AStream: TStream);
var XML:TAdSimpleXML;
begin
  XML := TAdSimpleXML.Create;
  XML.LoadFromStream(AStream);
  LoadFromXML(XML.Root);
  XML.Free;
end;

procedure TAdComponent.SaveToStream(AStream: TStream);
var XML:TAdSimpleXML;
begin
  XML := TAdSimpleXML.Create;
  SaveToXML(XML.Root.Items);
  XML.SaveToStream(AStream);
  XML.Free;
end;

procedure TAdComponent.LoadFromXML(aroot: TAdSimpleXMLElem);
var
  i:integer;
  cref:TAdPersistentClass;
begin
  Clear;
  
  FName := aroot.Properties.Value('name','');
  FAlpha := aroot.Properties.IntValue('alpha',255);
  FCursor := aroot.Properties.Value('cursor','default');
  FEnabled := aroot.Properties.BoolValue('enabled',true);
  FVisible := aroot.Properties.BoolValue('visble',true);
  FHint := aroot.Properties.Value('hint','');
  FShowHint := aroot.Properties.BoolValue('showhint',false);
  FWidth := aroot.Properties.IntValue('width',100);
  FHeight := aroot.Properties.IntValue('height',100);
  FX := aroot.Properties.IntValue('x',0);
  FY := aroot.Properties.IntValue('y',0);
  FontColor := aroot.Properties.IntValue('fontcolor',0);
  FontName := aroot.Properties.Value('fontname','');

  for i := 0 to aroot.Items.Count - 1 do
  begin
    cref := AdGetClass(aroot.Items[i].Name);
    if cref <> nil then
    begin
      with TAdComponent(TAdComponentClass(cref).Create(self)) do
      begin
        LoadFromXML(aroot.Items[i]);
      end;
    end;
  end;
end;

function TAdComponent.SaveToXML(aroot: TAdSimpleXMLElems): TAdSimpleXMLElem;
var
  i: Integer;
begin
  result := aroot.Add(ClassName);

  for i := 0 to Components.Count-1 do
  begin
    if not Components[i].SubComponent then Components[i].SaveToXML(result.Items);
  end;

  with result.Properties do
  begin
    Add('name',FName);
    Add('alpha',FAlpha);
    Add('cursor',FCursor);
    Add('enabled',FEnabled);
    Add('height',round(FHeight));
    Add('width',round(FWidth));
    Add('hint',FHint);
    Add('showhint',FShowHint);
    Add('visible',FVisible);
    Add('x',round(FX));
    Add('y',round(FY));
    Add('fontcolor',FFontColor);
    Add('fontname',FFontName);
  end;
end;

procedure TAdComponent.DoDraw;
var
  r:TAdRect;
  ax,ay:integer;
begin
  if FDesignMode then
  begin
    with AdDraw.Canvas do
    begin
      r := BoundsRect;
      if Focused then
      begin
        Pen.Color := ad_argb(128,64,64,255);
        Brush.Style := abClear;

        Rectangle(r);

        Brush.Color := ad_argb(64,64,64,255);
        Rectangle(AdBounds(r.Left,r.Top,4,4));
        Rectangle(AdBounds(r.Left,r.Bottom-4,4,4));
        Rectangle(AdBounds(r.Right-4,r.Top,4,4));
        Rectangle(AdBounds(r.Right-4,r.Bottom-4,4,4));
      end;
      if Grid then
      begin
        for ax := 0 to round(Width) div FGridX do
        begin
          for ay := 0 to round(Height) div FGridY do
          begin
            PlotPixel((r.Left div FGridX)*FGridX + ax*FGridX,
                      (r.Top  div FGridY)*FGridY + ay*FGridY,
                      ad_ARGB(64,128,128,128))
          end;
        end;
        Release;
      end;
      if Grid or Focused then Release;
    end;
  end;
end;

procedure TAdComponent.DoMove(TimeGap:double);
begin
  if MouseOver then
  begin
    FMouseOverTime := FMouseOverTime + TimeGap;
    if (FShowHint) and (FHintWnd <> nil) and (not FShowedHint) and
       (FMouseOverTime > FHintWnd.FWaitTime) then
    begin
      FHintWnd.Show(MouseX + 16,MouseY + 16,FHint,self);
      FShowedHint := true;
    end;
  end
  else
  begin
    FMouseOverTime := 0;
  end;
end;

procedure TAdComponent.Draw;
var
  i:integer;
begin
  if FVisible or FDesignMode then
  begin
    DoDraw;
    for i := 0 to Components.Count - 1 do
    begin
      Components[i].Draw;
    end;
  end;
end;

function TAdComponent.FindComponent(AName: string): TAdComponent;
var
  i:integer;
begin
  if LowerCase(Name) = LowerCase(AName) then
  begin
    result := self;
  end
  else
  begin
    result := nil;
    for i := 0 to Components.Count - 1 do
    begin
      result := Components[i].FindComponent(AName);
      if result <> nil then
      begin
        break;
      end;
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

function TAdComponent.GetBoundsRect: TAdRect;
var rect:TAdRect;
begin
  if FParent = nil then
  begin
    result := AdBounds(round(FX),round(FY),round(FWidth),round(FHeight));
  end
  else
  begin
    rect := FParent.ClientRect;
    result := AdBounds(rect.Left+round(FX),rect.Top+round(FY),round(FWidth),round(FHeight));
  end;
end;

function TAdComponent.GetClientRect: TAdRect;
var rect:TAdRect;
begin
  rect := GetBoundsRect;
  result.Left := rect.Left + FSpacerLeft;
  result.Top := rect.Top + FSpacerTop;
  result.Right := rect.Right - FSpacerRight;
  result.Bottom := rect.Bottom - FSpacerBottom;
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

function TAdComponent.GetFont: TAdFont;
begin
  result := FFont;
end;

procedure TAdComponent.SetFont(Value: TAdFont);
var
  i: Integer;
begin  
  if FFont <> nil then
  begin
    if Value <> nil then
    begin
      FFont.Assign(Value);
      i := Fonts.IndexOf(Value);
      if i >= 0 then
        FFontName := Fonts.Names[i];
    end
    else
    begin
      FFont.Assign(AdDraw.Canvas.Font);
    end;              
  end;
  for i := 0 to Components.Count - 1 do
  begin
    Components[i].Font := Value;
  end; 
end;

procedure TAdComponent.SetFontColor;
begin
  FFont.Color := ColorToAdColor(FFontColor);
end;

procedure TAdComponent.SetFontName(Value: string);
begin
  FFontName := Value;
  Font := Fonts.Font[value];
end;

procedure TAdComponent.SetFonts(Value: TAdFontList);
var
  i:integer;
begin
  FFonts := Value;
  for i := 0 to Components.Count - 1 do
  begin
    Components[i].Fonts := Value;
  end;
  if FontName <> '' then FontName := FFontName;
end;

procedure TAdComponent.SetGrid(Value: boolean);
var i:integer;
begin
  FGrid := Value;
  for i := 0 to Components.Count - 1 do
  begin
    Components[i].Grid := Value;
  end;
end;

procedure TAdComponent.SetGridX(Value: integer);
var i:integer;
begin
  FGridX := Value;
  for i := 0 to Components.Count - 1 do
  begin
    Components[i].GridX := Value;
  end;
end;

procedure TAdComponent.SetGridY(Value: integer);
var i:integer;
begin
  FGridY := Value;
  for i := 0 to Components.Count - 1 do
  begin
    Components[i].GridY := Value;
  end;
end;

procedure TAdComponent.SetHintWnd(Value: TAdHint);
var
  i: Integer;
begin
  FHintWnd := Value;
  for i := 0 to Components.Count - 1 do
  begin
    Components[i].HintWnd := FHintWnd;
  end;
end;

procedure TAdComponent.SetLockEvents(Value: boolean);
begin
  if Parent <> nil then
  begin
    Parent.LockEvents := Value;
    FLockEvents := Value;
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
  FName := GetUniqueName(Value, false);
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

procedure TAdComponent.SetSpacer(ASkinItem: TAdSkinItem);
var i:integer;
begin
  for i := 0 to ASkinItem.Elements.Count-1 do
  begin
    if ASkinItem.Elements.Items[i].ClientRect then
    begin
      SpacerLeft := ASkinItem.Elements[i].X1;
      SpacerTop := ASkinItem.Elements[i].Y1;
      SpacerRight := ASkinItem.BaseWidth - ASkinItem.Elements[i].X2;
      SpacerBottom := ASkinItem.BaseHeight - ASkinItem.Elements[i].Y2;

      FMinWidth := SpacerLeft + SpacerRight + 1;
      FMinHeight := SpacerTop + SpacerBottom + 1;

      break;
    end;
  end;
end;

procedure TAdComponent.LoadSkinItem;
begin
  //
end;

procedure TAdComponent.BringToFront;
begin
  if FParent <> nil then
  begin
    FParent.Components.Remove(self);
    FParent.AddComponent(self);
  end;
end;

procedure TAdComponent.SendToBack;
begin
  if FParent <> nil then
  begin
    FParent.Components.Remove(self);
    FParent.Components.Insert(0,self);
  end;
end;

function TAdComponent.GetUniqueName(AName: string; AForceNumber:boolean): string;
var i:integer;
begin
  if FParent <> nil then
  begin
    result := FParent.GetUniqueName(AName, AForceNumber);
  end
  else
  begin
    if NameExists(AName) or AForceNumber then
    begin
      i := 0;
      repeat
        i := i + 1;
      until NameExists(AName+inttostr(i)) = false;
      result := AName+inttostr(i);
    end
    else
    begin
      result := AName;
    end;
  end;
end;

function TAdComponent.NameExists(AName: string): boolean;
var
  i:integer;
begin
  result := FName = AName;
  if not Result then
  begin
    for i := 0 to Components.Count - 1 do
    begin
      result := Components[i].NameExists(AName);
      if result then
      begin
        break;
      end;
    end;
  end;
end;

function TAdComponent.OwnsComponent(AComponent: TAdComponent): boolean;
var
  i:integer;
begin
  result := AComponent = self;
  for i := 0 to Components.Count - 1 do
  begin
    result := Components[i].OwnsComponent(AComponent);
    if result then break;
  end;
end;

procedure TAdComponent.SetWidth(Value: integer);
begin
  FWidth := Value;
  if (FWidth > FMaxWidth) and (FMaxWidth > 0) then
    FWidth := FMaxWidth;
  if FWidth < FMinWidth then
    FWidth := FMinWidth;
  DoResize;
end;

procedure TAdComponent.SetHeight(Value: integer);
begin
  FHeight := Value;
  if (FHeight > FMaxHeight) and (FMaxWidth > 0) then
    FHeight := FMaxHeight;
  if FHeight < FMinHeight then
    FHeight := FMinHeight;
  DoResize;
end;

{Event-Handling}

procedure TAdComponent.Clear;
var
  i:integer;
begin
  for i := 0 to Components.Count - 1 do
  begin
    Components[i].Free;
  end;
  Components.Clear;
end;

function TAdComponent.Click(X, Y: integer):boolean;
var
  clicked:boolean;
  i:integer;
begin
  result := false;
  if (visible or (designmode and (not SubComponent))) and (InRect(x,y,boundsrect)) then
  begin
    clicked := true;
    for i := Components.Count-1 downto 0 do
    begin
      if InRect(x,y,Components[i].BoundsRect) then
      begin
        clicked := false;
        result := Components[i].Click(X,Y);
        break;
      end;
    end;
    if (clicked) and ((enabled) or (designmode and (not SubComponent))) then
    begin
      if not DesignMode then
      begin
        result := DoClick(X, Y);
      end
      else
      begin
        SetFocused;
      end;
    end;
  end;
  result := result and (not FLockEvents);
end;

function TAdComponent.ClientToScreen(p: TAdPoint): TAdPoint;
begin
  with result do
  begin
    x := p.X + ClientRect.Left;
    y := p.Y + ClientRect.Top;
  end;
end;

function TAdComponent.ScreenToClient(p: TAdPoint): TAdPoint;
begin
  with result do
  begin
    x := p.X - ClientRect.Left;
    y := p.Y - ClientRect.Top;
  end;
end;

function TAdComponent.DblClick(X, Y: integer):boolean;
var
  clicked:boolean;
  i:integer;
begin
  result := false;
  if (visible or (designmode and (not SubComponent))) and (InRect(x,y,boundsrect)) then
  begin
    clicked := true;
    for i := Components.Count-1 downto 0 do
    begin
      if InRect(x,y,Components[i].BoundsRect) then
      begin
        clicked := false;
        result := Components[i].DblClick(X,Y);
        break;
      end;
    end;
    if (clicked) and ((enabled) or (designmode and (not SubComponent))) then
    begin
      if not DesignMode then
      begin
        result := DoDblClick(X, Y);
      end
      else
      begin
        SetFocused;
      end;
    end;
  end;
  result := result or FLockEvents;
end;

function TAdComponent.MouseDown(Button: TAdMouseButton; Shift: TAdShiftState; X,
  Y: Integer):boolean;
var clicked:boolean;
    i:integer;
begin
  result := false;
  if (visible or (designmode and (not SubComponent))) and InRect(x,y,boundsrect) then
  begin
    clicked := true;
    for i := Components.Count-1 downto 0 do
    begin
      if (Components[i].Visible or DesignMode) and (InRect(x,y,Components[i].BoundsRect) or Components[i].MousePreview) then
      begin
        clicked := false;
        result := Components[i].MouseDown(Button,Shift,X,Y);
        break;
      end;
    end;
    if clicked and enabled then
    begin
      if not DesignMode then
      begin
        result := DoMouseDown(Button,Shift,X,Y);
      end
      else
      begin
        if not Subcomponent then
        begin
          MouseDownIn := GetDownRgn(X-BoundsRect.Left,Y-BoundsRect.Top);
          FDraging := true;
        end;
      end;
    end;
  end;
  result := result or FLockEvents;
end;

function TAdComponent.MouseMove(Shift: TAdShiftState; X, Y: Integer):boolean;
var clicked:boolean;
    overcomp:TAdComponent;
    i:integer;
    om:boolean;
begin
  result := false;
  om := FMouseOver;
  overcomp := nil;
  FMouseOver := false;
  DesignSize(X,Y);

  if (visible or (designmode and (not SubComponent))) and ((InRect(x,y,boundsrect)) or FMousePreview) then
  begin
    clicked := true;
    FMouseX := X;
    FMouseY := Y;
    for i := Components.Count-1 downto 0 do
    begin
      if (Components[i].Visible or DesignMode) and (InRect(x,y,Components[i].BoundsRect) or Components[i].MousePreview) then
      begin
        result := Components[i].MouseMove(Shift,X,Y);
        clicked := false;
        FMouseOver := false;
        overcomp := Components[i];
        if overcomp.FDraging or (not DesignMode) then
        begin
          break;
        end;
      end
      else
      begin
        if not Components[i].Subcomponent then
        begin
          Components[i].DesignSize(X,Y);
        end;
      end;
    end;

    if (FMousePreview) and enabled then
    begin
      DoMouseMove(Shift,X,Y);
    end;

    if (clicked) and enabled then
    begin
      if not DesignMode then
      begin
        FMouseOver := true;
        result := DoMouseMove(Shift,X,Y);
        for i := 0 to Components.Count-1 do
        begin
          if (Components[i] <> overcomp) then
          begin
            Components[i].FMouseOver := false;
            Components[i].CheckMouseEnter(true);
          end;
        end;
      end;
    end
    else
    begin
      if enabled and (not DesignMode) then
      begin
        for i := 0 to Components.Count-1 do
        begin
          if (Components[i] <> overcomp) then
          begin
            Components[i].MouseOver := false;
            Components[i].CheckMouseEnter(true);
          end;
        end;
      end;
    end;
  end;
  CheckMouseEnter(om);
  result := result or FLockEvents;
end;

function TAdComponent.MouseUp(Button: TAdMouseButton; Shift: TAdShiftState; X,
  Y: Integer):boolean;
var clicked:boolean;
    i:integer;
    overcomp:TAdComponent;
begin
  result := false;
  overcomp := nil;
  if (visible or (designmode and (not SubComponent))) and InRect(x,y,boundsrect) then
  begin
    clicked := true;
    for i := Components.Count-1 downto 0 do
    begin
      if (Components[i].Visible or DesignMode) and (InRect(x,y,Components[i].BoundsRect) or Components[i].MousePreview) then
      begin
        clicked := false;
        overcomp := Components[i];
        if Components[i].FDraging or (not DesignMode) then
        begin
          break;
        end;
      end
      else
      begin
        if (designmode and (not SubComponent)) then
        begin
          Components[i].MouseDownIn := drNone;
        end;
      end;
    end;
    if clicked then
    begin
      if (not DesignMode) and Enabled then
      begin
        result := DoMouseUp(Button, Shift, X, Y);
      end;
    end
    else
    begin
      if overcomp <> nil then
      begin
        result := overcomp.MouseUp(Button, Shift, X, Y);
        overcomp.FDraging := false;
      end;
    end;
  end;
  if (designmode and (not SubComponent)) then
  begin
    MouseDownIn := drNone;
    FDraging := false;
  end;
  result := result or FLockEvents;
end;

function TAdComponent.MouseWheel(Shift: TAdShiftState; WheelDelta: Integer;
  X, Y: integer):boolean;
var clicked:boolean;
    i:integer;
begin
  result := false;

  if (visible or (designmode and (not SubComponent))) and (InRect(X,Y,boundsrect)) then
  begin
    clicked := true;
    for i := Components.Count-1 downto 0 do
    begin
      if (Components[i].Visible or DesignMode) and InRect(X,Y,Components[i].BoundsRect) then
      begin
        clicked := false;
        result := Components[i].MouseWheel(Shift,WheelDelta,X,Y);
        break;
      end;
    end;
    if (clicked) and (enabled) then
    begin
      if not DesignMode then
      begin
        result := DoMouseWheel(Shift,WheelDelta,X,Y);
      end;
    end;
  end;
  result := result or FLockEvents;
end;

function TAdComponent.KeyDown(Key: Word; Shift: TAdShiftState):boolean;
var i:integer;
begin
  result := false;
  if (visible or (designmode and (not SubComponent))) and (Focused or FKeyPreview) and enabled then
  begin
    if not DesignMode then result := DoKeyDown(Key,Shift);
  end
  else
  begin
    for i := 0 to Components.Count-1 do
    begin
      result := Components[i].KeyDown(Key,Shift);
    end;
  end;
  result := result or FLockEvents;
end;

function TAdComponent.KeyPress(Key: Char):boolean;
var i:integer;
begin
  result := false;
  if (visible or (designmode and (not SubComponent))) and (Focused or FKeyPreview) and enabled then
  begin
    if not DesignMode then result := DoKeyPress(Key);
  end
  else
  begin
    for i := 0 to Components.Count-1 do
    begin
      result := Components[i].KeyPress(Key);
    end;
  end;
  result := result or FLockEvents;
end;

function TAdComponent.KeyUp(Key: Word; Shift: TAdShiftState):boolean;
var i:integer;
begin
  result := false;
  if (visible or (designmode and (not SubComponent))) and (Focused or FKeyPreview) and enabled then
  begin
    if not DesignMode then result := DoKeyUp(Key,Shift);
  end
  else
  begin
    for i := 0 to Components.Count-1 do
    begin
      result := Components[i].KeyUp(Key,Shift);
    end;
  end;
  result := result or FLockEvents;
end;

function TAdComponent.DoClick(X, Y: Integer):boolean;
begin
  result := false;
  if assigned(OnClick) then
    OnClick(self);
end;

function TAdComponent.DoDblClick(X, Y: Integer):boolean;
begin
  result := false;
  if assigned(OnDblClick) then
    OnDblClick(self);
end;

function TAdComponent.DoKeyDown(key: Word; Shift: TAdShiftState):boolean;
begin
  result := false;
  if assigned(OnKeyDown) then OnKeyDown(Self,Key,Shift);
end;

function TAdComponent.DoKeyPress(key: Char):boolean;
begin
  result := false;
  if assigned(OnKeyPress) then OnKeyPress(Self,Key);
end;

function TAdComponent.DoKeyUp(key: Word; Shift: TAdShiftState):boolean;
begin
  result := false;
  if assigned(OnKeyUp) then OnKeyUp(Self,Key,Shift);
end;

function TAdComponent.DoMouseDown(Button: TAdMouseButton; Shift: TAdShiftState;
  X, Y: Integer):boolean;
begin
  result := false;
  if assigned(OnMouseDown) then OnMouseDown(Self,Button,Shift,X,Y);
end;

function TAdComponent.DoMouseEnter:boolean;
begin
  result := false;
  if enabled and assigned(OnMouseEnter) then OnMouseEnter(self);
  CurrentCursor := Cursor;
end;

function TAdComponent.DoMouseLeave:boolean;
begin
  result := false;
  if enabled and assigned(OnMouseLeave) then OnMouseLeave(self);
  if (FShowedHint) and (FHintWnd <> nil) then
  begin
    FHintWnd.Hide;
    FShowedHint := false;
  end;
end;

function TAdComponent.DoMouseMove(Shift: TAdShiftState; X, Y: Integer):boolean;
begin
  result := false;
  if assigned(OnMouseMove) then
    OnMouseMove(Self, Shift, X, Y);
end;

function TAdComponent.DoMouseUp(Button: TAdMouseButton; Shift: TAdShiftState; X,
  Y: Integer):boolean;
begin
  result := false;
  if assigned(OnMouseUp) then
    OnMouseUp(Self, Button, Shift, X, Y);
end;

function TAdComponent.DoMouseWheel(Shift: TAdShiftState; WheelDelta: Integer;
  X, Y: Integer):boolean;
begin
  result := false;
  if assigned(OnMouseWheel) then
    OnMouseWheel(Self, Shift, Wheeldelta, X, Y);
end;

function TAdComponent.DoResize:boolean;
begin
  result := false;
end;

function TAdComponent.GetDownRgn(AX, AY: integer): TAdDownRgn;
var w,h:integer;
begin
  result := drNone;
  w := round(Width);
  h := round(Height);
  if InRect(AX,AY,AdRect(0,0,w,h)) then
  begin
    result := drMiddle;
    if InRect(AX,AY,AdRect(0,0,4,4)) then result := drLeftTop;
    if InRect(AX,AY,AdRect(w-4,0,w,4)) then result := drRightTop;
    if InRect(AX,AY,AdRect(0,h-4,4,h)) then result := drLeftBottom;
    if InRect(AX,AY,AdRect(w-4,h-4,w,h)) then result := drRightBottom;
  end;
end;

procedure TAdComponent.CheckMouseEnter(oldvalue: boolean);
begin
  if visible and not DesignMode then
  begin
    if oldvalue and (not FMouseOver) then
    begin
      DoMouseLeave;
    end;
    if (not oldvalue) and (FMouseOver) then
    begin
      DoMouseEnter;
    end;
  end;
end;

procedure TAdComponent.DesignSize(X,Y:integer);
var gx,gy:integer;
  procedure SnapLeftTop;
  begin
    if (round(FX) mod gx <> 0) or (round(FY) mod gy <> 0) then
    begin
      FX := round(FX) div gx * gx;
      FY := round(FY) div gy * gy;
    end;
  end;
  procedure SnapRightBottom;
  begin
    if (round(FWidth) mod gx <> 0) or (round(FHeight) mod gy <> 0) then
    begin
      Width := round(FWidth) div gx * gx;
      Height := round(FHeight) div gy * gy;
    end;
  end;
begin
  if DesignMode and (FMouseDownIn <> drNone) then
  begin
    if not FGrid then
    begin
      gx := 1;
      gy := 1;
    end
    else
    begin
      gx := FGridX;
      gy := FGridY;
    end;
    X := X div gx * gx;
    Y := Y div gy * gy;
    if FMouseDownIn = drMiddle then
    begin
      SnapLeftTop;
      FX := FX + X-FOX;
      FY := FY + Y-FOY;
    end;
    if FMouseDownIn = drLeftTop then
    begin
      SnapLeftTop;
      FX := FX + (X-FOX);
      FY := FY + (Y-FOY);
      Width := FWidth - (X-FOX);
      Height := FHeight - (Y-FOY);
    end;
    if FMouseDownIn = drLeftBottom then
    begin
      SnapRightBottom;
      FX := FX + (X-FOX);
      Width := FWidth - (X-FOX);
      Height := FHeight + (Y-FOY);
    end;
    if FMouseDownIn = drRightBottom then
    begin
      SnapRightBottom;
      Width := FWidth + (X-FOX);
      Height := FHeight + (Y-FOY);
    end;
    if FMouseDownIn = drRightTop then
    begin
      SnapRightBottom;
      FY := FY + (Y-FOY);
      Width := FWidth + (X-FOX);
      Height := FHeight - (Y-FOY);
    end;
  end;
  FOX := X;
  FOY := Y;
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

procedure TAdMouseCursor.LoadFromXML(aroot: TAdSimpleXMLElem);
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
      FImage := TAdImage.Create(FParent);
      FImage.LoadFromStream(ms);
      FImage.Restore;
      ms.Free;
    end;
  end;
end;

function TAdMouseCursor.SaveToXML(aroot: TAdSimpleXMLElems): TAdSimpleXMLElem;
var trunk:TAdSimpleXMLElem;
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
  FImages := TAdImageList.Create(FParent);
  FCurrentCursorString := '';
  FCurrentCursor := nil;
  FVisible := true;
end;

destructor TAdMouseLibrary.Destroy;
begin
  FImages.Free;
  inherited;
end;

procedure TAdMouseLibrary.Draw;
begin
  if (FCurrentCursor <> nil) and (FVisible) then
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
  XML:TAdSimpleXML;
  str:string;
begin
  XML := TAdSimpleXML.Create;
  XML.LoadFromFile(AFile);
  LoadFromXML(XML.Root);
  XML.Free;

  str := FCurrentCursorString;
  FCurrentCursorString := '';
  SetCurrentCursor(str);
end;

procedure TAdMouseLibrary.SaveToFile(AFile: string);
var
  XML:TAdSimpleXML;
begin
  XML := TAdSimpleXML.Create;
  SaveToXML(XML.Root.Items);
  XML.SaveToFile(AFile);
  XML.Free;
end;

procedure TAdMouseLibrary.LoadFromXML(aroot: TAdSimpleXMLElem);
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

function TAdMouseLibrary.SaveToXML(aroot: TAdSimpleXMLElems): TAdSimpleXMLElem;
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
  CurrentCursor := 'default';
  MousePreview := true;

  FOwnHintWnd := true;
  FHintWnd := TAdHint.Create(AdDraw);
  SaveFonts := true;

  FOwnFonts := true;
  FFonts := TAdFontList.Create(AdDraw.AdAppl);
end;

destructor TAdGUI.Destroy;
begin
  Skin.Free;
  FMouse.Free;
  if FOwnHintWnd then
  begin
    HintWnd := nil;
    FHintWnd.Free;
  end;
  if (FFonts <> nil) and FOwnFonts then
  begin
    FFonts.Free;
  end;
  inherited;
end;

function TAdGUI.DoMouseMove(Shift: TAdShiftState; X, Y: Integer):boolean;
begin
  inherited DoMouseMove(Shift,X,Y);
  FMouseX := X;
  FMouseY := Y;
  result := false;
end;

procedure TAdGUI.LoadFromXML(aroot: TAdSimpleXMLElem);
var
  ms:TMemoryStream;
begin
  inherited;

  if FOwnFonts then
  begin
    ms := TMemoryStream.Create;
    ReadStream(ms,aroot,'fontdata');
    if ms.Size > 0 then
    begin
      ms.Position := 0;
      FFonts.LoadFromStream(ms);
      Fonts := FFonts;
    end;
    ms.Free;
  end;
end;

function TAdGUI.SaveToXML(aroot: TAdSimpleXMLElems): TAdSimpleXMLElem;
var
  ms:TMemoryStream;
begin
  result := inherited SaveToXML(aroot);
  if (FFonts <> nil) and (FSaveFonts) then
  begin
    ms := TMemoryStream.Create;
    FFonts.SaveToStream(ms);
    WriteStream(ms,result,'fontdata');
    ms.Free;
  end;
end;

procedure TAdGUI.SetCurrentCursor(Value: string);
begin
  inherited;
  Cursors.CurrentCursor := Value;
end;

procedure TAdGUI.SetHintWnd(Value: TAdHint);
begin
  if FOwnHintWnd then
  begin
    FHintWnd.Free;
    FOwnHintWnd := false;
  end;
  FHintWnd := Value;

  inherited SetHintWnd(Value);
end;

procedure TAdGUI.Update(TimeGap:double);
begin
  if DesignMode then
  begin
    X := 0;
    Y := 0;
  end;
  Width := AdDraw.DisplayRect.Right;
  Height := AdDraw.DisplayRect.Bottom;

  Move(TimeGap);
  Draw;

  HintWnd.Move(TimeGap);
  HintWnd.Draw;

  AdDraw.Canvas.Release;

  Cursors.X := FMouseX;
  Cursors.Y := FMouseY;
  Cursors.Move(TimeGap);
  Cursors.Draw;
end;

{ TAdHint }

constructor TAdHint.Create(AParent: TAdDraw);
begin
  inherited Create;
  FParent := AParent;
  FColor := $00AAAA11;
  FTextColor := $00FFFFFF;
  FBorderColor := $00FFFFFF;
  FAlpha := 200;
  FShowTime := 3;
  FWaitTime := 0.2;
  FVisible := false;
  FX := 0;
  FY := 0;
  FText := '';
  FCurrentAlpha := 0;
  FAlpha := 220;
  FFadeTime := 0.2;
end;

destructor TAdHint.Destroy;
begin
  inherited Destroy;
end;

procedure TAdHint.Draw;
var Width,Height:integer;
    tmp2:TAndorraColor;
begin
  if Visible then
  begin
    with FParent.Canvas do
    begin
      Width := Font.TextWidth(FText);
      Height := Font.TextHeight(FText);
      Pen.Style := apSolid;
      Pen.Color := ad_ARGB(round(FCurrentAlpha),GetRValue(BorderColor),GetGValue(BorderColor),GetBValue(BorderColor));
      Brush.Style := abSolid;
      Brush.Color := ad_ARGB(round(FCurrentAlpha),GetRValue(Color),GetGValue(Color),GetBValue(Color));
      Rectangle(FX,FY,FX+Width+4,FY+Height+4);

      tmp2 := Font.Color;
      Font.Color := Ad_ARGB(round(FCurrentAlpha), 0, 0, 0);

      Textout(FX+2,FY+2,FText);
      Font.Color := tmp2;
    end;
  end;
end;

procedure TAdHint.Move(TimeGap: double);
begin
  if FVisible then
  begin
    FShowedTime := FShowedTime + TimeGap;
    if FShowedTime > FShowTime then
    begin
      Hide;
    end;

    if FFadeIn then
    begin
      if FCurrentAlpha < FAlpha then
      begin
        FCurrentAlpha := FCurrentAlpha + (FAlpha * TimeGap / FadeTime);
      end
      else
      begin
        FFadeIn := false;
      end;
    end;
    if FFadeOut then
    begin
      if FCurrentAlpha > 1 then
      begin
        FCurrentAlpha := FCurrentAlpha - (FAlpha * TimeGap / FadeTime);
        if FCurrentAlpha < 0 then FCurrentAlpha := 0;        
      end
      else
      begin
        FFadeOut := false;
        FVisible := false;
      end;
    end;
  end;
end;

procedure TAdHint.SetAlpha(Value: byte);
begin
  FAlpha := Value;
end;

procedure TAdHint.Show(MouseX, MouseY: integer; Text: string;
  Sender: TAdComponent);
begin
  FVisible := true;
  FX := MouseX;
  FY := MouseY;
  FText := Text;
  FShowedTime := 0;
  FFadeOut := false;
  FFadeIn := true;
  FCurrentAlpha := 0;
end;

procedure TAdHint.Hide;
begin
  FFadeOut := true;
  FFadeIn := false;
end;

function StringToGUIColor(const Ident: string; var Int: Longint): Boolean;
begin
  result := true;
  Int := StrToIntDef(Ident, 0);
end;

function GUIColorToString(Int: Longint; var Ident: string): Boolean;
begin
  result := true;
  Ident := '$'+IntToHex(Int, 6);
end;

initialization
  RegisteredComponents := TStringList.Create;
  RegisterComponent(TAdComponent,'');
  RegisterComponent(TAdGUI,'');

  RegisterIntegerConsts(TypeInfo(TGUIColor), StringToGUIColor, GUIColorToString);

finalization
  RegisteredComponents.Free;


end.

