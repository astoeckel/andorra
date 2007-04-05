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
     AdXML, Controls;

const
  Opac = 255;

type
  TAdDownRgn = (drNone,drMiddle,drLeftTop,drLeftBottom,drRightTop,drRightBottom);

  TAdComponent = class;

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
      destructor Destroy;
      procedure Show(MouseX,MouseY:integer; Text:string;Sender:TAdComponent);virtual;
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
      FMousePreview:boolean;
      FCanGetFocus:boolean;
      FSubComponent:boolean;
      FSpacerTop,FSpacerLeft,FSpacerRight,FSpacerBottom:integer;
      FCurrentCursor:string;

      FMouseOver:boolean;
      FMouseOverTime:single;
      FMouseDownIn:TAdDownRgn;
      FOX,FOY:integer;


      FOnMouseDown:TMouseEvent;
      FOnMouseUp:TMouseEvent;
      FOnMouseMove:TMouseMoveEvent;
      FOnDblClick:TNotifyEvent;
      FOnClick:TNotifyEvent;
      FOnKeyPress:TKeyPressEvent;
      FOnKeyUp:TKeyEvent;
      FOnKeyDown:TKeyEvent;
      FOnMouseEnter:TNotifyEvent;
      FOnMouseLeave:TNotifyEvent;
      FOnMouseWheel:TMouseWheelEvent;

      FHint:string;
      FShowHint:boolean;
      FShowedHint:boolean;
      FHintWnd:TAdHint;

      FMouseX,FMouseY:integer;

      procedure SetSkin(Value:TAdSkin);
      procedure SetAdDraw(Value:TAdDraw);
      procedure SetDesignMode(Value:boolean);
      procedure SetName(Value:string);
      procedure SetFocus(Value:boolean);
      procedure SetHintWnd(Value:TAdHint);
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

      procedure DoMouseMove(Shift: TShiftState; X, Y: Integer);virtual;
      procedure DoMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);virtual;
      procedure DoMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);virtual;
      procedure DoMouseEnter;virtual;
      procedure DoMouseLeave;virtual;
      procedure DoClick;virtual;
      procedure DoDblClick;virtual;
      procedure DoKeyPress(key:Char);virtual;
      procedure DoKeyUp(key:Word;Shift:TShiftState);virtual;
      procedure DoKeyDown(key:Word;Shift:TShiftState);virtual;
      procedure DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);virtual;

      procedure DesignSize(X,Y:integer);
      function GetDownRgn(AX,AY:integer):TAdDownRgn;      
      procedure CheckMouseEnter(oldvalue:boolean);
      
      property KeyPreview:boolean read FKeyPreview write FKeyPreview;
      property MousePreview:boolean read FMousePreview write FMousePreview;
      property SpacerTop:integer read FSpacerTop write FSpacerTop;
      property SpacerBottom:integer read FSpacerBottom write FSpacerBottom;
      property SpacerLeft:integer read FSpacerLeft write FSpacerLeft;
      property SpacerRight:integer read FSpacerRight write FSpacerRight;
      property Focused:boolean read FFocused write SetFocus;
      property CanGetFocus:boolean read FCanGetFocus write FCanGetFocus;
      property SubComponent:boolean read FSubComponent write FSubComponent;

      property MouseOver:boolean read FMouseOver write FMouseOver;
      property MouseOverTime:single read FMouseOverTime;
      property OX:integer read FOX write FOX;
      property OY:integer read FOY write FOY;
      property MouseDownIn:TAdDownRgn read FMouseDownIn write FMouseDownIn;
      
      property ShowedHint:boolean read FShowedHint write FShowedHint;

      property MouseX:integer read FMouseX;
      property MouseY:integer read FMouseY;
    public
      procedure Draw;
      procedure Move(TimeGap:double);

      procedure AddComponent(AComponent:TAdComponent);virtual;

      procedure DblClick(X,Y:integer);
      procedure Click(X,Y:integer);
      procedure MouseMove(Shift: TShiftState; X, Y: Integer);
      procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
      procedure KeyPress(Key: Char);
      procedure KeyDown(Key: Word;Shift:TShiftState);
      procedure KeyUp(Key: Word;Shift:TShiftState);

      function ClientToScreen(p:TPoint):TPoint;
      function ScreenToClient(p:TPoint):TPoint;

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
      property HintWnd:TAdHint read FHintWnd write SetHintWnd;
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

      property Hint:string read FHint write FHint;
      property ShowHint:boolean read FShowHint write FShowHint;

      property OnClick:TNotifyEvent read FOnClick write FOnClick;
      property OnDblClick:TNotifyEvent read FOnDblClick write FOnDblClick;
      property OnMouseMove:TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
      property OnMouseUp:TMouseEvent read FOnMouseUp write FOnMouseUp;
      property OnMouseDown:TMouseEvent read FOnMouseDown write FOnMouseDown;
      property OnMouseEnter:TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
      property OnMouseLeave:TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
      property OnMouseWheel:TMouseWheelEvent read FOnMouseWheel write FOnMouseWheel;
      property OnKeyPress:TKeyPressEvent read FOnKeyPress write FOnKeyPress;
      property OnKeyUp:TKeyEvent read FOnKeyUp write FOnKeyUp;
      property OnKeyDown:TKeyEvent read FOnKeyDown write FOnKeyDown;
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
      FMouseX,FMouseY:integer;
    protected
      procedure SetCurrentCursor(Value:string);override;

      procedure DoMouseMove(Shift: TShiftState; X, Y: Integer);override;
     public
      constructor Create(AParent:TAdDraw);
      destructor Destroy;override;

      procedure Update(TimeGap:double);

      property Cursors:TAdMouseLibrary read FMouse;
      property MouseX:integer read FMouseX;
      property MouseY:integer read FMouseY;

      property HintWnd;
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
    FHintWnd := FParent.HintWnd;
    Skin := FParent.Skin;
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

{Event-Handling}


procedure TAdComponent.Click(X, Y: integer);
var
  clicked:boolean;
  i:integer;
begin
  if (visible) and (InRect(x,y,boundsrect)) then
  begin
    clicked := true;
    for i := Components.Count-1 downto 0 do
    begin
      if InRect(x,y,Components[i].BoundsRect) then
      begin
        clicked := false;
        Components[i].Click(X,Y);
        break;
      end;
    end;
    if (clicked) and ((enabled) or (designmode)) then
    begin
      if not DesignMode then
      begin
        DoDblClick;
      end
      else
      begin
        SetFocused;
      end;
    end;
  end;
end;

function TAdComponent.ClientToScreen(p: TPoint): TPoint;
begin
  with result do
  begin
    x := p.X + ClientRect.Left;
    y := p.Y + ClientRect.Top;
  end;
end;

function TAdComponent.ScreenToClient(p: TPoint): TPoint;
begin
  with result do
  begin
    x := p.X - ClientRect.Left;
    y := p.Y - ClientRect.Top;
  end;
end;

procedure TAdComponent.DblClick(X, Y: integer);
var
  clicked:boolean;
  i:integer;
begin
  if (visible) and (InRect(x,y,boundsrect)) then
  begin
    clicked := true;
    for i := Components.Count-1 downto 0 do
    begin
      if InRect(x,y,Components[i].BoundsRect) then
      begin
        clicked := false;
        Components[i].DblClick(X,Y);
        break;
      end;
    end;
    if (clicked) and ((enabled) or (designmode)) then
    begin
      if not DesignMode then
      begin
        DoDblClick;
      end
      else
      begin
        SetFocused;
      end;
    end;
  end;
end;

procedure TAdComponent.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var clicked:boolean;
    i:integer;
begin
  if visible and InRect(x,y,boundsrect) then
  begin
    clicked := true;
    for i := Components.Count-1 downto 0 do
    begin
      if Components[i].Visible and InRect(x,y,Components[i].BoundsRect) then
      begin
        clicked := false;
        Components[i].MouseDown(Button,Shift,X,Y);
        break;
      end;
    end;
    if clicked and enabled then
    begin
      if not DesignMode then
      begin
        DoMouseDown(Button,Shift,X,Y);
      end
      else
      begin
        MouseDownIn := GetDownRgn(X-BoundsRect.Left,Y-BoundsRect.Top);
      end;
    end;
  end;
end;

procedure TAdComponent.MouseMove(Shift: TShiftState; X, Y: Integer);
var clicked:boolean;
    overcomp:TAdComponent;
    i:integer;
    om:boolean;
begin
  om := FMouseOver;
  overcomp := nil;
  FMouseOver := false;
  DesignSize(X,Y);

  if (visible) and ((InRect(x,y,boundsrect)) or FMousePreview) then
  begin
    clicked := true;
    FMouseX := X;
    FMouseY := Y;
    for i := Components.Count-1 downto 0 do
    begin
      if Components[i].Visible and InRect(x,y,Components[i].BoundsRect) then
      begin
        Components[i].MouseMove(Shift,X,Y);
        clicked := false;
        FMouseOver := false;
        overcomp := Components[i];
        break;
      end
      else
      begin
        Components[i].DesignSize(X,Y);
      end;
    end;

    if FMousePreview and enabled then
    begin
      DoMouseMove(Shift,X,Y);
    end;

    if (clicked) and enabled then
    begin
      if not DesignMode then
      begin
        FMouseOver := true;
        DoMouseMove(Shift,X,Y);
        for i := 0 to Components.Count-1 do
        begin
          if (Components[i] <> overcomp) then
          begin
            Components[i].FMouseOver := false;
            Components[i].CheckMouseEnter(true);
          end;
        end;
      end
      else
      begin
        if not Focused then
        begin
          AdDraw.Parent.Cursor := crDefault;
        end
        else
        begin
          if ClassType.ClassName <> 'TDXComponent' then
          begin
            case GetDownRgn(X-BoundsRect.Left,Y-BoundsRect.Top) of
              drNone:AdDraw.Parent.Cursor := crDefault;
              drMiddle:AdDraw.Parent.Cursor := crSizeAll;
              drLeftTop:AdDraw.Parent.Cursor := crSizeNWSE;
              drLeftBottom:AdDraw.Parent.Cursor := crSizeNESW;
              drRightBottom:AdDraw.Parent.Cursor := crSizeNWSE;
              drRightTop:AdDraw.Parent.Cursor := crSizeNESW;
            end;
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
end;

procedure TAdComponent.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var clicked:boolean;
    i:integer;
begin
  if visible and InRect(x,y,boundsrect) then
  begin
    clicked := true;
    for i := Components.Count-1 downto 0 do
    begin
      if Components[i].Visible and InRect(x,y,Components[i].BoundsRect) then
      begin
        clicked := false;
        Components[i].MouseUp(Button, Shift,X,Y);
        break;
      end
      else
      begin
        if DesignMode then
        begin
          Components[i].MouseDownIn := drNone;
        end;
      end;
    end;
    if clicked and enabled then
    begin
      if not DesignMode then DoMouseUp(Button, Shift,X,Y);
    end;
  end;
  if DesignMode then
  begin
    MouseDownIn := drNone;
  end;
end;

procedure TAdComponent.MouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint; var Handled: Boolean);
var clicked:boolean;
    i:integer;
begin
  if (visible) and (InRect(mousepos.x,mousepos.y,boundsrect)) then
  begin
    clicked := true;
    for i := Components.Count-1 downto 0 do
    begin
      if Components[i].Visible and InRect(mousepos.x,mousepos.y,Components[i].BoundsRect) then
      begin
        clicked := false;
        Components[i].MouseWheel(Shift,WheelDelta,MousePos,Handled);
        break;
      end;
    end;
    if (clicked) and (enabled) then
    begin
      if not DesignMode then DoMouseWheel(Shift,WheelDelta,MousePos,Handled);
    end;
  end;
end;

procedure TAdComponent.KeyDown(Key: Word; Shift: TShiftState);
var i:integer;
begin
  if visible and (Focused or FKeyPreview) and enabled then
  begin
    if not DesignMode then DoKeyDown(Key,Shift);
  end
  else
  begin
    for i := 0 to Components.Count-1 do
    begin
      Components[i].KeyDown(Key,Shift);
    end;
  end;
end;

procedure TAdComponent.KeyPress(Key: Char);
var i:integer;
begin
  if visible and (Focused or FKeyPreview) and enabled then
  begin
    if not DesignMode then DoKeyPress(Key);
  end
  else
  begin
    for i := 0 to Components.Count-1 do
    begin
      Components[i].KeyPress(Key);
    end;
  end;
end;

procedure TAdComponent.KeyUp(Key: Word; Shift: TShiftState);
var i:integer;
begin
  if visible and (Focused or FKeyPreview) and enabled then
  begin
    if not DesignMode then DoKeyUp(Key,Shift);
  end
  else
  begin
    for i := 0 to Components.Count-1 do
    begin
      Components[i].KeyUp(Key,Shift);
    end;
  end;
end;

procedure TAdComponent.DoClick;
begin
  if assigned(OnClick) then OnClick(self);
end;

procedure TAdComponent.DoDblClick;
begin
  if assigned(OnDblClick) then OnDblClick(self);
end;

procedure TAdComponent.DoKeyDown(key: Word; Shift: TShiftState);
begin
  if assigned(OnKeyDown) then OnKeyDown(Self,Key,Shift);
end;

procedure TAdComponent.DoKeyPress(key: Char);
begin
  if assigned(OnKeyPress) then OnKeyPress(Self,Key);
end;

procedure TAdComponent.DoKeyUp(key: Word; Shift: TShiftState);
begin
  if assigned(OnKeyUp) then OnKeyUp(Self,Key,Shift);
end;

procedure TAdComponent.DoMouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if assigned(OnMouseMove) then OnMouseMove(Self,Shift,X,Y);
end;

procedure TAdComponent.DoMouseEnter;
begin
  if enabled and assigned(OnMouseEnter) then OnMouseEnter(self);
  CurrentCursor := Cursor;
end;

procedure TAdComponent.DoMouseLeave;
begin
  if enabled and assigned(OnMouseLeave) then OnMouseLeave(self);
  if (FShowedHint) and (FHintWnd <> nil) then
  begin
    FHintWnd.Hide;
    FShowedHint := false;
  end;
end;

procedure TAdComponent.DoMouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if assigned(OnMouseMove) then OnMouseMove(Self,Shift,X,Y);
end;

procedure TAdComponent.DoMouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if assigned(OnMouseUp) then OnMouseUp(Self,Button,Shift,X,Y);
end;

procedure TAdComponent.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint; var Handled: Boolean);
begin
  if assigned(OnMouseWheel) then OnMouseWheel(self,shift,wheeldelta,mousepos,handled);
end;

function TAdComponent.GetDownRgn(AX, AY: integer): TAdDownRgn;
var w,h:integer;
begin
  result := drNone;
  w := round(Width);
  h := round(Height);
  if InRect(AX,AY,rect(0,0,w,h)) then
  begin
    result := drMiddle;
    if InRect(AX,AY,rect(0,0,4,4)) then result := drLeftTop;
    if InRect(AX,AY,rect(w-4,0,w,4)) then result := drRightTop;
    if InRect(AX,AY,rect(0,h-4,4,h)) then result := drLeftBottom;
    if InRect(AX,AY,rect(w-4,h-4,w,h)) then result := drRightBottom;
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
begin
  if DesignMode and (FMouseDownIn <> drNone) and
     (ClassType.ClassName <> 'TAdComponent') then
  begin
    if FMouseDownIn = drMiddle then
    begin
      FX := FX + (X-FOX);
      FY := FY + (Y-FOY);
    end;
    if FMouseDownIn = drLeftTop then
    begin
      FX := FX + (X-FOX);
      FY := FY + (Y-FOY);
      FWidth := FWidth - (X-FOX);
      FHeight := FHeight - (Y-FOY);
    end;
    if FMouseDownIn = drLeftBottom then
    begin
      FX := FX + (X-FOX);
      FWidth := FWidth - (X-FOX);
      FHeight := FHeight + (Y-FOY);
    end;
    if FMouseDownIn = drRightBottom then
    begin
      FWidth := FWidth + (X-FOX);
      FHeight := FHeight + (Y-FOY);
    end;
    if FMouseDownIn = drRightTop then
    begin
      FY := FY + (Y-FOY);
      FWidth := FWidth + (X-FOX);
      FHeight := FHeight - (Y-FOY);
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
  CurrentCursor := 'default';
  MousePreview := true;

  HintWnd := TAdHint.Create(AdDraw);
end;

destructor TAdGUI.Destroy;
begin
  Skin.Free;
  FMouse.Free;
  HintWnd.Free;
  HintWnd := nil;
  inherited;
end;

procedure TAdGUI.DoMouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FMouseX := X;
  FMouseY := Y;
end;

procedure TAdGUI.SetCurrentCursor(Value: string);
begin
  inherited;
  Cursors.CurrentCursor := Value;
end;

procedure TAdGUI.Update(TimeGap:double);
begin
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
    tmp1:byte;
    tmp2:longint;
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

      tmp1 := Font.Alpha;
      tmp2 := Font.Color;
      Font.Color := FTextColor;
      Font.Alpha := round(FCurrentAlpha);
//      Font.Restore;

      Textout(FX+2,FY+2,FText);
      Font.Alpha := tmp1;
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

initialization
  RegisteredComponents := TStringList.Create;
  RegisterComponent(TAdComponent);

finalization
  RegisteredComponents.Free;


end.

