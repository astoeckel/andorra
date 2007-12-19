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
* File: AdComponents.pas
* Comment: This unit contains the Andorra 2D gui classes
}

unit AdComponents;

interface

uses
  JvSimpleXML, AdGUI, AdXML, AdSkin, AdClasses, AdDraws, AdTypes, AdBitmap,
  Controls, Classes, Graphics, SysUtils, AdCanvas, AdFont;

type
  TAdAlignment = (alLeft,alCenter,alRight);
  TAdTextPos = (tpTop,tpCenter,tpBottom);
  TAdAlignmentEx = (axLeft, axRight, axTop, axBottom);
  TAdButtonState = (bsNormal, bsDown, bsHover, bsFocus, bsDisabled);

  TAdSkinBtn = class(TAdComponent)
    private
      FSkinName:string;
      FState:TAdButtonState;
      FStateNr:integer;
      FSkinItem:TAdSkinItem;
      procedure SetSkinName(AValue:string);
    protected
      procedure GetStateNr;
      procedure DoDraw;override;
      function DoMouseEnter:boolean;override;
      function DoMouseLeave:boolean;override;
      function DoMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer):boolean;override;
      function DoMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer):boolean;override;
      procedure LoadSkinItem;override;
    public
      constructor Create(AParent:TAdComponent);override;
      property SkinName:string read FSkinName write SetSkinName;
  end;

  TCloseEvent = procedure(Sender:TObject; var CanClose:boolean) of object;

  TAdForm = class(TAdComponent)
    private
      FCaption:string;
      FSkinItem:TAdSkinItem;
      FCenter:boolean;
      FCloseButton:TAdSkinBtn;
      FMovable:boolean;
      FMX,FMY:integer;
      FDown:boolean;
      FOnClose:TCloseEvent;
      procedure SetCenter(AValue:boolean);
      procedure CreateButtons;
    protected
      procedure LoadSkinItem;override;

      procedure LooseFocus(Sender:TAdComponent);override;

      procedure DoDraw;override;
      function DoResize:boolean;override;

      function DoMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer):boolean;override;
      function DoMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer):boolean;override;
      function DoMouseMove(Shift: TShiftState; X, Y: Integer):boolean;override;

      function GetShowCloseButton:boolean;
      procedure SetShowCloseButton(AValue:boolean);
      procedure CloseBtnClick(Sender:TObject);
    public
      constructor Create(AParent:TAdComponent);override;
      destructor Destroy;override;
      procedure LoadFromXML(aroot:TJvSimpleXMLElem);override;
      function SaveToXML(aroot:TJvSimpleXMLElems):TJvSimpleXMLElem;override;
      procedure Close;
      property CloseButton:TAdSkinBtn read FCloseButton;
    published
      property OnClose:TCloseEvent read FOnClose write FOnClose;
      property Caption:string read FCaption write FCaption;
      property Center:boolean read FCenter write SetCenter;
      property ShowCloseButton:boolean read GetShowCloseButton write SetShowCloseButton;
      property Movable:boolean read FMovable write FMovable;
      property FontName;
      property FontColor;
  end;

  TAdContainer = class(TAdComponent)
    protected
      procedure DoDraw;override;
  end;

  TAdLabel = class(TAdComponent)
    private
      FCaption:string;
      FAlignment:TAdAlignment;
      FTextPos:TAdTextPos;
      FWordWrap:boolean;
      FClipText:boolean;
    protected
      procedure DoDraw;override;
    public
      constructor Create(AParent:TAdComponent);override;
      procedure LoadFromXML(aroot:TJvSimpleXMLElem);override;
      function SaveToXML(aroot:TJvSimpleXMLElems):TJvSimpleXMLElem;override;
    published
      property Caption:string read FCaption write FCaption;
      property TextPos:TAdTextPos read FTextPos write FTextPos;
      property Alignment:TAdAlignment read FAlignment write FAlignment;
      property WordWrap:boolean read FWordWrap write FWordWrap;
      property ClipText:boolean read FClipText write FClipText;
      property FontName;
      property FontColor;
  end;

  TAdPanel = class(TAdLabel)
    private
      FSkinItem:TAdSkinItem;
    protected
      procedure LoadSkinItem;override;
      procedure DoDraw;override;
    public
      constructor Create(AParent:TAdComponent);override;
  end;

  TAdButton = class(TAdComponent)
    private
      FSkinItem:TAdSkinItem;
      FCaption:string;
      FState:TAdButtonState;
      FStateNr:integer;
    protected
      procedure GetStateNr;
      procedure LoadSkinItem;override;
      procedure DoDraw;override;
      function DoMouseEnter:boolean;override;
      function DoMouseLeave:boolean;override;
      function DoMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer):boolean;override;
      function DoMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer):boolean;override;
    public
      constructor Create(AParent:TAdComponent);override;
      destructor Destroy;override;
      procedure LoadFromXML(aroot:TJvSimpleXMLElem);override;
      function SaveToXML(aroot:TJvSimpleXMLElems):TJvSimpleXMLElem;override;
      property State:TAdButtonState read FState;
    published
      property Caption:string read FCaption write FCaption;
      property FontName;
      property FontColor;
    end;

  //Written by Michael Morstein alias Neutral General
  TAdCheckBox = class(TAdComponent)
    private
      FState: TAdButtonState;
      FCaption: String;
      FSkinItem: TAdSkinItem;
      FCheckedItem: TAdSkinItem;
      FChecked: boolean;
      FAlignment: TAdAlignmentEx;
      FGroupIndex: integer;
      procedure SetGroupIndex(AValue:integer);
      procedure SetChecked(AValue:boolean);
    protected
      procedure LoadSkinItem; override;
      procedure DoDraw; override;
      function DoMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:Integer):boolean;override;
      function DoMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y:Integer):boolean;override;
      function DoMouseEnter:boolean;override;
      function DoMouseLeave:boolean;override;
    public
      constructor Create(AParent:TAdComponent);override;
      procedure LoadFromXML(aroot:TJvSimpleXMLElem); override;
      function SaveToXML(aroot:TJvSimpleXMLElems): TJvSimpleXMLElem;override;
    published
      property Checked:Boolean read FChecked write SetChecked;
      property Caption:String read FCaption write FCaption;
      property Alignment:TAdAlignmentEx read FAlignment write FAlignment;
      property GroupIndex:integer read FGroupIndex write SetGroupIndex;
      property FontName;
      property FontColor;
  end;

  TAdResourceImage = class
    private
      FImage:TAdImage;
      FParent:TAdDraw;
      FTransparent:boolean;
      FTransparentColor:TColor;
      FCompressor:TAdGraphicCompressorClass;
      procedure SetTransparent(AValue:boolean);
      procedure SetTransparentColor(AValue:TColor);
      procedure SetCompressor(ACompressor:TAdGraphicCompressorClass);
      procedure UpdateTransparency;
      function GetLoaded:boolean;
    public
      constructor Create(AParent:TAdDraw);
      destructor Destroy;override;
      procedure LoadFromFile(AFile:string;ATransparent:boolean;ATransparentColor:LongInt);
      procedure LoadFromGraphic(AGraphic:TGraphic);
      procedure Draw(X,Y:integer);
      procedure LoadFromString(AString:string);
      function SaveToString:string;
      procedure LoadFromStream(AStream:TStream);
      procedure SaveToStream(AStream:TStream);
      property Picture:TAdImage read FImage;
      property Transparent:boolean read FTransparent write SetTransparent;
      property TransparentColor:TColor read FTransparentColor write SetTransparentColor;
      property Parent:TAdDraw read FParent;
      property Compressor:TAdGraphicCompressorClass read FCompressor write SetCompressor;
      property Loaded:boolean read GetLoaded;
  end;

  TAdBitmapButton = class(TAdComponent)
    private
      FImgHover:TAdResourceImage;
      FImgDown:TAdResourceImage;
      FImgCheckedHover:TAdResourceImage;
      FImgDisabled:TAdResourceImage;
      FImgNormal:TAdResourceImage;
      FState:TAdButtonState;
      FDown:boolean;
      FCheckButton:boolean;
      FAutoSize:boolean;
      FGroupIndex:integer;
      procedure SetDown(AValue:boolean);
      procedure CheckChecked;
      procedure SetGroupIndex(AValue:integer);
    protected
      procedure DoMove(TimeGap:double); override;
      procedure DoDraw; override;
      function DoMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:Integer):boolean; override;
      function DoMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y:Integer):boolean; override;
      function DoMouseEnter:boolean; override;
      function DoMouseLeave:boolean; override;
    public
      constructor Create(AParent:TAdComponent);override;
      destructor Destroy;override;
      procedure LoadFromXML(aroot:TJvSimpleXMLElem); override;
      function SaveToXML(aroot:TJvSimpleXMLElems): TJvSimpleXMLElem;override;
      property State:TAdButtonState read FState;
    published
      property ImgNormal:TAdResourceImage read FImgNormal;
      property ImgHover:TAdResourceImage read FImgHover;
      property ImgDown:TAdResourceImage read FImgDown;
      property ImgDisabled:TAdResourceImage read FImgDisabled;
      property ImgCheckedHover:TAdResourceImage read FImgCheckedHover;
      property Down:boolean read FDown write SetDown;
      property CheckButton:boolean read FCheckButton write FCheckButton;
      property AutoSize:boolean read FAutoSize write FAutoSize;
      property GroupIndex:integer read FGroupIndex write SetGroupIndex;
  end;

  TAdProgressBar = class(TAdComponent)
    private
      FSmooth:boolean;
      FMin:integer;
      FMax:integer;
      FPosition:integer;
      FShowPercentage:boolean;
      FSkinItem:TAdSkinItem;
      FSkinProgress:TAdSkinItem;
      FAlign:TAdAlignment;
      procedure SetSmooth(AValue:boolean);
      procedure SetMin(AValue:integer);
      procedure SetMax(AValue:integer);
      procedure SetPosition(AValue:integer);
    protected
      procedure LoadSkinItem; override;
      procedure DoDraw; override;
    public
      constructor Create(AParent:TAdComponent);override;
      destructor Destroy;override;
      procedure LoadFromXML(aroot:TJvSimpleXMLElem); override;
      function SaveToXML(aroot:TJvSimpleXMLElems): TJvSimpleXMLElem;override;
      function Percent:single;
    published
      property Min:integer read FMin write SetMin;
      property Max:integer read FMax write SetMax;
      property Position:integer read FPosition write SetPosition;
      property ShowPercentage:boolean read FShowPercentage write FShowPercentage;
      property Smooth:boolean read FSmooth write SetSmooth;
      property Align:TAdAlignment read FAlign write FAlign;
      property FontName;
      property FontColor;
  end;

  TAdGUIImage = class(TAdComponent)
    private
      FCenter:boolean;
      FStretch:boolean;
      FAutoSize:boolean;
      FProportional:boolean;
      FPicture:TAdResourceImage;
    protected
      procedure DoDraw; override;
      procedure DoMove(timegap:double);override;
      function DestinationRect:TAdRect;
    public
      constructor Create(AParent:TAdComponent);override;
      destructor Destroy;override;
      procedure LoadFromXML(aroot:TJvSimpleXMLElem); override;
      function SaveToXML(aroot:TJvSimpleXMLElems): TJvSimpleXMLElem;override;
    published
      property Center:boolean read FCenter write FCenter;
      property Stretch:boolean read FStretch write FStretch;
      property Proportional:boolean read FProportional write FProportional;
      property AutoSize:boolean read FAutoSize write FAutoSize;
      property Picture:TAdResourceImage read FPicture;
  end;

  TAdEdit = class(TAdComponent)
    private
      FText:string;
      FSkinItem:TAdSkinItem;
      FSelStart:integer;
      FSelStop:integer;
      FCursorVisible:boolean;
      FCursorTime:double;
      function GetSelCount:integer;
      procedure DelSelText;
      function MouseToSelPos(AX: integer): integer;
    protected
      procedure LoadSkinItem; override;
      procedure DoDraw;override;
      procedure DoMove(timegap:double);override;
      function DoKeyDown(Key:Word;shift:TShiftState):boolean;override;
      function DoKeyPress(Key:Char):boolean;override;
      function DoClick:boolean;override;
      function DoMouseDown(Button: TMouseButton; Shift: TShiftState; X,
        Y: integer):boolean;override;
      function DoMouseMove(Shift: TShiftState; X, Y: Integer):boolean;override;
      property CursorVisible:boolean read FCursorVisible write FCursorVisible;
      property CursorTime:double read FCursorTime write FCursorTime;
    public
      constructor Create(AParent:TAdComponent);override;
      property SelStart:integer read FSelStart write FSelStart;
      property SelStop:integer read FSelStop write FSelStop;
      property SelCount:integer read GetSelCount;
      procedure LoadFromXML(aroot:TJvSimpleXMLElem); override;
      function SaveToXML(aroot:TJvSimpleXMLElems): TJvSimpleXMLElem;override;
    published
      property Text:string read FText write FText;
      property FontName;
      property FontColor;
  end;

  const               
    SPACING = 5;

implementation

{ TAdPanel }

constructor TAdPanel.Create(AParent: TAdComponent);
begin
  inherited;
  AcceptChildComponents := true;
end;

procedure TAdPanel.DoDraw;
var
  rect:TAdRect;
begin
  if FSkinItem <> nil then
  begin
    rect := BoundsRect;
    FSkinItem.Draw(0,rect.Left,rect.Top,round(Width),round(Height),Alpha);
  end;

  inherited DoDraw;
end;

procedure TAdPanel.LoadSkinItem;
begin
  FSkinItem := Skin.ItemNamed['panel'];
  SetSpacer(FSkinItem);
end;

{ TAdButton }

constructor TAdButton.Create(AParent: TAdComponent);
begin
  inherited;
  FStateNr := 0;
  FState := bsNormal;
  AcceptChildComponents := false;
  MinWidth := 20;
  MinHeight := 20;
end;

destructor TAdButton.Destroy;
begin
  inherited;
end;

procedure TAdButton.DoDraw;
var
  rect:TAdRect;
begin
  if FSkinItem <> nil then
  begin
    SetFontColor;

    rect := BoundsRect;
    FSkinItem.Draw(FStateNr,rect.Left,rect.Top,round(Width),round(Height),Alpha);

    with TAdSimpleTypeSetter(Font.TypeSetter) do
    begin
      DrawMode := [dtMiddle, dtCenter, dtDoLineFeeds, dtCut];
    end;
    Font.TextOut(rect,FCaption);
  end;
  inherited DoDraw;
end;

function TAdButton.DoMouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer):boolean;
begin
  inherited DoMouseDown(Button,Shift,X,Y);
  result := true;
  FState := bsDown;
  GetStateNr;
end;

function TAdButton.DoMouseEnter:boolean;
begin
  inherited DoMouseEnter;
  FState := bsHover;
  GetStateNr;
  result := true;
end;

function TAdButton.DoMouseLeave:boolean;
begin
  inherited DoMouseLeave;
  if Focused then
  begin
    FState := bsFocus;
  end
  else
  begin
    FState := bsNormal;
  end;
  GetStateNr;
  result := true;
end;

function TAdButton.DoMouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer):boolean;
begin
  inherited DoMouseUp(Button,Shift,X,Y);
  result := true;
  FState := bsHover;
  SetFocused;
  GetStateNr;
end;

procedure TAdButton.LoadSkinItem;
begin
  FSkinItem := Skin.ItemNamed['button'];
  SetSpacer(FSkinItem);
  GetStateNr;
end;

procedure TAdButton.GetStateNr;
begin
  case FState of
    bsNormal: FStateNr := FSkinItem.States.IndexOf('standard');
    bsDown: FStateNr := FSkinItem.States.IndexOf('down');
    bsHover: FStateNr := FSkinItem.States.IndexOf('hover');
    bsFocus: FStateNr := FSkinItem.States.IndexOf('focus');
  else
    FStateNr := 0;
  end;
  
  if FStateNr < 0 then FStateNr := 0;  
end;

procedure TAdButton.LoadFromXML(aroot: TJvSimpleXMLElem);
begin
  inherited;
  with aroot.Properties do
  begin
    FCaption := Value('caption','');
  end;
end;

function TAdButton.SaveToXML(aroot: TJvSimpleXMLElems): TJvSimpleXMLElem;
begin
  result := inherited SaveToXML(aroot);
  with result.Properties do
  begin
    Add('caption',FCaption);
  end;
end;

{ TAdForm }

constructor TAdForm.Create(AParent: TAdComponent);
begin
  inherited;
  CreateButtons;

  FMovable := false;
  CanGetFocus := true;
end;

procedure TAdForm.Close;
var
  CanClose:boolean;
begin
  CanClose := true;
  if Assigned(OnClose) then
  begin
    OnClose(self,CanClose);
  end;
  if CanClose then
  begin
    Visible := false;
  end;
end;

procedure TAdForm.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TAdForm.CreateButtons;
begin
  FCloseButton := TAdSkinBtn.Create(self);
  FCloseButton.SkinName := 'formclosebtn';
  FCloseButton.SubComponent := true;
  FCloseButton.OnClick := CloseBtnClick;
end;

destructor TAdForm.Destroy;
begin
  FCloseButton.Free;
  inherited;
end;

procedure TAdForm.DoDraw;
var
  rect:TAdRect;
begin
  SetFontColor;

  rect := BoundsRect;
  FSkinItem.Draw(0,rect.Left,rect.Top,round(Width),round(Height),Alpha);
  Font.TextOut(rect.Left+SpacerLeft,rect.Top+(SpacerTop - Font.TextHeight(FCaption)) div 2,FCaption);
  inherited DoDraw;
end;

function TAdForm.DoMouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer):boolean;
begin
  inherited DoMouseDown(Button,Shift,X,Y);
  result := true;
  if not DesignMode then
  begin
    FMX := X;
    FMY := Y;
    FDown := true;

    SetFocused;
    BringToFront;

    MousePreview := true;
    LockEvents := true;
  end;
end;

function TAdForm.DoMouseMove(Shift: TShiftState; X, Y: Integer):boolean;
begin
  inherited DoMouseMove(Shift,X,Y);
  result := true;
  if (FMovable) and (ssLeft in Shift) and FDown then
  begin
    self.X := self.X + (X-FMX);
    self.Y := self.Y + (Y-FMY);
    FMX := X;
    FMY := Y;
  end;
end;

function TAdForm.DoMouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer):boolean;
begin
  inherited DoMouseUp(Button,Shift,X,Y);
  result := true;
  FDown := false;
  MousePreview := false;
  LockEvents := false;
end;

function TAdForm.DoResize:boolean;
begin
  inherited DoResize;
  result := true;
  FCloseButton.Y := (SpacerTop - FCloseButton.Height) div 2 - SpacerTop;
  FCloseButton.X := Width - SpacerRight - FCloseButton.Width - SpacerLeft;
  SetCenter(FCenter);
end;

procedure TAdForm.LoadFromXML(aroot: TJvSimpleXMLElem);
begin
  inherited;
  CreateButtons;
  DoResize;
  with aroot.Properties do
  begin
    FCaption := Value('caption','');
    Center := BoolValue('center',false);
    ShowCloseButton := BoolValue('showclosebutton',true);
    FMovable := BoolValue('moveable',false);
  end;
end;

function TAdForm.SaveToXML(aroot: TJvSimpleXMLElems): TJvSimpleXMLElem;
begin
  result := inherited SaveToXML(aroot);
  with result.Properties do
  begin
    Add('caption',FCaption);
    Add('center',FCenter);
    Add('showclosebutton',ShowCloseButton);
    Add('moveable',FMovable);
  end;
end;

procedure TAdForm.SetCenter(AValue: boolean);
begin
  FCenter := AValue;
  if (FCenter) and (Parent <> nil) then
  begin
    X := (Parent.Width - Width) div 2;
    Y := (Parent.Height - Height) div 2;
  end;
end;

procedure TAdForm.SetShowCloseButton(AValue: boolean);
begin
  FCloseButton.Visible := AValue;
end;

function TAdForm.GetShowCloseButton: boolean;
begin
  result := FCloseButton.Visible;
end;

procedure TAdForm.LoadSkinItem;
begin
  FSkinItem := Skin.ItemNamed['form'];
  SetSpacer(FSkinItem);
end;

procedure TAdForm.LooseFocus(Sender: TAdComponent);
begin
  inherited;
  if not DesignMode then
  begin
    FDown := false;
    if OwnsComponent(Sender) then BringToFront;
  end;
end;

{ TAdCheckBox }

constructor TAdCheckBox.Create(AParent: TAdComponent);
begin
  inherited;
  AcceptChildComponents := false;
  MinWidth := 20;
  MinHeight := 20;
end;

procedure TAdCheckBox.DoDraw;
var
  Rect: TAdRect;
begin
 if (FSkinItem <> nil) and (FCheckedItem <> nil) then
 begin
    Rect := BoundsRect;
    SetFontColor;
    case FAlignment of
      axLeft  : begin
                  FSkinItem.Draw(Integer(FState),Rect.Left,Rect.Top,FSkinItem.BaseWidth,FSkinItem.BaseHeight,Alpha);
                  if FChecked then
                    FCheckedItem.Draw(0,Rect.Left + Round(FSkinItem.BaseWidth / 2) - Round(FCheckedItem.BaseWidth / 2),
                                        Rect.Top + Round(FSkinItem.BaseHeight / 2) - Round(FCheckedItem.BaseHeight / 2),FCheckedItem.BaseWidth,
                                        FCheckedItem.BaseHeight,Alpha);
                  Font.TextOut(Rect.Left + FSkinItem.BaseWidth + SPACING,Rect.Top +
                               Round(FSkinItem.BaseHeight / 2) - (Font.TextHeight(FCaption) div 2),
                               FCaption);
                end;
      axRight : begin
                  FSkinItem.Draw(Integer(FState),Rect.Right - Round(FSkinItem.BaseWidth),Rect.Top,FSkinItem.BaseWidth,
                    FSkinItem.BaseHeight,Alpha);
                  if FChecked then
                    FCheckedItem.Draw(0,Rect.Right - Round(FSkinItem.BaseWidth / 2) - Round(FCheckedItem.BaseWidth / 2),
                                        Rect.Top + Round(FSkinItem.BaseHeight / 2) - Round(FCheckedItem.BaseHeight / 2),
                                      FCheckedItem.BaseWidth, FCheckedItem.BaseHeight,Alpha);
                  Font.TextOut(Rect.Left + Round(Width-FSkinItem.BaseWidth) - Font.TextWidth(FCaption) - SPACING,
                               Rect.Top + Round(FSkinItem.BaseHeight / 2) - (Font.TextHeight(FCaption) div 2) ,
                               FCaption);
                end;
      axTop   : begin
                  FSkinItem.Draw(Integer(FState),Rect.Left + Round(Width / 2) - Round(FSkinItem.BaseWidth / 2),
                                        Rect.Top,FSkinItem.BaseWidth, FSkinItem.BaseHeight,Alpha);
                  if FChecked then
                  FCheckedItem.Draw(0,Rect.Left + Round(Width / 2) - Round(FCheckedItem.BaseWidth / 2),
                                      Rect.Top + Round(FSkinItem.BaseHeight / 2) - Round(FCheckedItem.BaseHeight / 2),FCheckedItem.BaseWidth, FCheckedItem.BaseHeight, Alpha);
                  Font.TextOut(Rect.Left + Round(Width / 2) - Round(Font.TextWidth(FCaption) / 2),
                               Rect.Top + FSkinItem.BaseHeight + SPACING, FCaption);
                end;
      axBottom: begin
                  FSkinItem.Draw(Integer(FState),Rect.Left + Round(Width / 2) - Round(FSkinItem.BaseWidth / 2),
                                        Rect.Bottom - FSkinItem.BaseHeight,FSkinItem.BaseWidth, FSkinItem.BaseHeight,Alpha);
                  if FChecked then
                    FCheckedItem.Draw(0,Rect.Left + Round(Width / 2) - Round(FCheckedItem.BaseWidth / 2),
                                      Rect.Bottom - Round(FSkinItem.BaseHeight / 2) - Round(FCheckedItem.BaseHeight / 2),
                                      FCheckedItem.BaseWidth, FCheckedItem.BaseHeight,Alpha);
                  Font.TextOut(Rect.Left + Round(Width / 2) - Round(Font.TextWidth(FCaption) / 2),
                               Rect.Bottom - FSkinItem.BaseHeight - Font.TextHeight(FCaption) - SPACING,
                               FCaption);
                end;
    end;
  end;
  inherited DoDraw;
end;

function TAdCheckBox.DoMouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer):boolean;
begin
  inherited DoMouseDown(Button,Shift,X,Y);
  result := true;
  FState := bsDown;
end;

function TAdCheckBox.DoMouseEnter:boolean;
begin
  inherited DoMouseEnter;
  result := true;
  FState := bsHover;
end;

function TAdCheckBox.DoMouseLeave:boolean;
begin
  inherited DoMouseLeave;
  result := true;
  FState := bsNormal;
end;

function TAdCheckBox.DoMouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer):boolean;
begin
  inherited DoMouseUp(Button,Shift,X,Y);
  result := true;
  if GroupIndex = 0 then
  begin
    Checked := not Checked;
  end
  else
  begin
    Checked := true;
  end;
  FState := bsHover;
end;

procedure TAdCheckBox.LoadSkinItem;
begin
  FSkinItem := Skin.ItemNamed['checkbox'];
  SetSpacer(FSkinItem);
  if FGroupIndex = 0 then
  begin
    FCheckedItem := Skin.ItemNamed['checkboxhook'];
  end
  else
  begin
    FCheckedItem := Skin.ItemNamed['checkboxradio'];
  end;
  MinWidth := FSkinitem.BaseWidth;
  MinHeight := FSkinitem.BaseHeight;
end;

procedure TAdCheckBox.LoadFromXML(ARoot: TJvSimpleXMLElem);
begin
  inherited;
  with ARoot.Properties do
  begin
    FCaption := Value('caption','');
    FChecked := BoolValue('checked',false);
    FAlignment := TAdAlignmentEx(IntValue('align',Ord(axLeft)));
    GroupIndex := IntValue('groupindex',0);
  end;
end;

function TAdCheckBox.SaveToXML(aroot: TJvSimpleXMLElems): TJvSimpleXMLElem;
begin
  Result := inherited SaveToXML(aroot);
  with Result.Properties do
  begin
    Add('caption',FCaption);
    Add('checked',FChecked);
    Add('align',Ord(FAlignment));
    Add('groupindex',FGroupIndex);
  end;
end;

procedure TAdCheckBox.SetChecked(AValue: boolean);
var
  i:integer;
begin
  FChecked := AValue;
  if (Parent <> nil) and (GroupIndex <> 0) and (FChecked) then
  begin
    for i := 0 to Parent.Components.Count - 1 do
    begin
      if (Parent.Components[i].ClassType = ClassType) and (Parent.Components[i] <> self) then
      begin
        if TAdCheckBox(Parent.Components[i]).GroupIndex = GroupIndex then
        begin
          TAdCheckBox(Parent.Components[i]).Checked := false;
        end;        
      end;
    end;
  end;    
end;

procedure TAdCheckBox.SetGroupIndex(AValue: integer);
begin
  if FGroupIndex <> AValue then
  begin
    FGroupIndex := AValue;
    LoadSkinItem;
    if FChecked then
    begin
      SetChecked(true);
    end;
  end;
end;

{ TAdRecourceImage }

constructor TAdResourceImage.Create(AParent: TAdDraw);
begin
  inherited Create;
  FParent := AParent;
  FImage := TAdImage.Create(AParent);
end;

destructor TAdResourceImage.Destroy;
begin
  FImage.Free;
  inherited Destroy;
end;

procedure TAdResourceImage.Draw(X, Y: integer);
begin
  FImage.Draw(FParent,X,Y,0);
end;

function TAdResourceImage.GetLoaded: boolean;
begin
  result := FImage.Texture.Texture.Loaded;
end;

procedure TAdResourceImage.LoadFromFile(AFile: string; ATransparent: boolean;
  ATransparentColor: Integer);
begin
  FImage.Texture.LoadGraphicFromFile(AFile,ATransparent,ATransparentColor);
  FImage.Restore;
  FTransparent := false;
  FTransparentColor := clNone;
end;

procedure TAdResourceImage.LoadFromGraphic(AGraphic: TGraphic);
begin
  FImage.Texture.LoadFromGraphic(AGraphic);
  FTransparent := false;
  FTransparentColor := clNone;
end;

function TAdResourceImage.SaveToString: string;
var ms:TMemoryStream;
begin
  ms := TMemoryStream.Create;
  SaveToStream(ms);
  ms.Position := 0;
  result := WriteStreamToString(ms);
  ms.Free;
end;

procedure TAdResourceImage.LoadFromString(AString: string);
var
  ms:TMemoryStream;
begin
  if AString <> '' then
  begin
    ms := TMemoryStream.Create;
    ReadStreamFromString(ms,AString);
    ms.Position := 0;
    LoadFromStream(ms);
    ms.Free;
  end;
end;

procedure TAdResourceImage.SaveToStream(AStream: TStream);
begin
  AStream.Write(FTransparent,SizeOf(FTransparent));
  AStream.Write(FTransparentColor,SizeOf(FTransparentColor));
  FImage.SaveToStream(AStream);
end;

procedure TAdResourceImage.LoadFromStream(AStream: TStream);
begin
  AStream.Read(FTransparent,SizeOf(FTransparent));
  AStream.Read(FTransparentColor,SizeOf(FTransparentColor));
  try
    FImage.LoadFromStream(AStream);
  except
    FImage.Texture.Clear;
  end;
end;

procedure TAdResourceImage.SetCompressor(ACompressor: TAdGraphicCompressorClass);
begin
  if ACompressor <> FCompressor then
  begin
    FCompressor := ACompressor;
    FImage.Texture.Compressor := ACompressor;
  end;
end;

procedure TAdResourceImage.SetTransparent(AValue: boolean);
begin
  if AValue <> FTransparent then
  begin
    FTransparent := AValue;
    UpdateTransparency;
  end;
end;

procedure TAdResourceImage.SetTransparentColor(AValue: TColor);
begin
  if AValue <> FTransparentColor then
  begin
    FTransparentColor := AValue;
    UpdateTransparency;
  end;
end;

procedure TAdResourceImage.UpdateTransparency;
var
  bmp:TBitmap;
  adbmp:TAdBitmap;
begin
  if FImage.Texture.Texture.Loaded then
  begin
    adbmp := TAdBitmap.Create;
    adbmp.ReserveMemory(FImage.Texture.Texture.BaseWidth,FImage.Texture.Texture.BaseHeight);
    FImage.Texture.Texture.SaveToBitmap(adbmp);
    bmp := TBitmap.Create;
    adbmp.AssignTo(bmp);
    adbmp.Free;
    bmp.Transparent := FTransparent;
    bmp.TransparentMode := tmFixed;
    bmp.TransparentColor := FTransparentColor;
    adbmp := TAdBitmap.Create;
    adbmp.Assign(bmp);
    FImage.Texture.Texture.LoadFromBitmap(adbmp,FParent.GetTextureParams(32));
    adbmp.Free;
    bmp.Free;
  end;
end;

{ TAdBitmapButton }

constructor TAdBitmapButton.Create(AParent: TAdComponent);
begin
  inherited Create(AParent);
  FImgHover := TAdResourceImage.Create(AdDraw);
  FImgDown := TAdResourceImage.Create(AdDraw);
  FImgNormal := TAdResourceImage.Create(AdDraw);
  FImgDisabled := TAdResourceImage.Create(AdDraw);
  FImgCheckedHover := TAdResourceImage.Create(AdDraw);
  AcceptChildComponents := false;
  MinWidth := 20;
  MinHeight := 20;
end;

destructor TAdBitmapButton.Destroy;
begin
  FImgHover.Free;
  FImgDown.Free;
  FImgNormal.Free;
  FImgDisabled.Free;
  FImgCheckedHover.Free;
  inherited;
end;

procedure TAdBitmapButton.DoDraw;
begin
  if DesignMode then
  begin
    with AdDraw.Canvas do
    begin
      Brush.Style := abClear;
      Pen.Color := Ad_ARGB(200,128,128,128);
      Rectangle(BoundsRect);
    end;
  end;
  
  if (Enabled) or (not FImgDisabled.Loaded) then  
  begin
    if (State = bsNormal) and (FImgNormal.Loaded) then
    begin
      FImgNormal.Picture.DrawAlpha(AdDraw,Boundsrect,0,Alpha);
    end;
    if (State = bsDown) and (FImgDown.Loaded) then
    begin
      FImgDown.Picture.DrawAlpha(AdDraw,Boundsrect,0,Alpha);
    end;
    if (State = bsHover) and (FImgHover.Loaded) and (not down) then
    begin
      FImgHover.Picture.DrawAlpha(AdDraw,Boundsrect,0,Alpha);
    end;
    if (State = bsHover) and (FImgCheckedHover.Loaded) and (down) then
    begin
      FImgCheckedHover.Picture.DrawAlpha(AdDraw,Boundsrect,0,Alpha);
    end;
  end
  else
  begin
    FImgDisabled.Picture.DrawAlpha(AdDraw,Boundsrect,0,Alpha);
  end;

  inherited;
end;

function TAdBitmapButton.DoMouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer):boolean;
begin
  inherited DoMouseDown(Button,Shift,X,Y);
  result := true;
  if FImgDown.Loaded then
  begin
    FState := bsDown;
    if FCheckButton and ((FGroupIndex = 0) or (not FDown)) then
    begin
      FDown := not FDown;
      CheckChecked;
    end;
  end;
end;

function TAdBitmapButton.DoMouseEnter:boolean;
begin
  inherited DoMouseEnter;
  result := true;
  if FImgHover.Loaded and (((not FCheckButton) or (not FDown)) or FImgCheckedHover.Loaded) then
  begin
    FState := bsHover;
  end;
end;

function TAdBitmapButton.DoMouseLeave:boolean;
begin
  inherited DoMouseLeave;
  result := true;
  if FImgNormal.Loaded and ((not FCheckButton) or (not FDown)) then
  begin
    FState := bsNormal;
  end;
  if FImgCheckedHover.Loaded and FCheckButton and FDown then
  begin
    FState := bsDown;
  end;
end;

function TAdBitmapButton.DoMouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer):boolean;
begin
  inherited DoMouseUp(Button,Shift,X,Y);
  result := true;
  if FImgNormal.Loaded then
  begin
    if ((not FCheckButton) or (not FDown)) or
       (FCheckButton and FDown and FImgCheckedHover.Loaded) then
    begin
      FState := bsHover;
    end;
  end;
end;

procedure TAdBitmapButton.DoMove(timegap:double);
begin
  inherited;
  if (Designmode) and (FAutoSize) and (FImgNormal.Loaded) then
  begin
    Width := FImgNormal.Picture.Width;
    Height := FImgNormal.Picture.Height;
  end;
end;

procedure TAdBitmapButton.LoadFromXML(aroot: TJvSimpleXMLElem);
begin
  inherited;
  with aroot.Properties do
  begin
    FImgNormal.LoadFromString(Value('imgnormal',''));
    FImgDown.LoadFromString(Value('imgdown',''));
    FImgHover.LoadFromString(Value('imghover',''));
    FImgDisabled.LoadFromString(Value('imgdisabled',''));
    FImgCheckedHover.LoadFromString(Value('imgcheckedhover',''));
    FCheckButton := BoolValue('checkbutton',false);
    FAutoSize := BoolValue('autosize',false);
    FDown := BoolValue('down',false);
    FGroupIndex := IntValue('groupindex',FGroupIndex);
  end;
end;

function TAdBitmapButton.SaveToXML(aroot: TJvSimpleXMLElems): TJvSimpleXMLElem;
begin
  Result := inherited SaveToXML(aroot);
  with Result.Properties do
  begin
    Add('imgnormal',FImgNormal.SaveToString);
    Add('imgdown',FImgDown.SaveToString);
    Add('imghover',FImgHover.SaveToString);
    Add('imgdisabled',FImgDisabled.SaveToString);
    Add('imgcheckedhover',FImgCheckedHover.SaveToString);
    Add('checkbutton',FCheckButton);
    Add('autosize',FAutoSize);
    Add('down',FDown);
    Add('groupindex',FGroupIndex);
  end;
end;

procedure TAdBitmapButton.CheckChecked;
var
  i:integer;
begin
  if (Parent <> nil) and (GroupIndex <> 0) and (FDown) then
  begin
    for i := 0 to Parent.Components.Count - 1 do
    begin
      if (Parent.Components[i].ClassType = ClassType) and (Parent.Components[i] <> self) then
      begin
        if TAdBitmapButton(Parent.Components[i]).GroupIndex = GroupIndex then
        begin
          TAdBitmapButton(Parent.Components[i]).Down := false;
        end;
      end;
    end;
  end;
end;

procedure TAdBitmapButton.SetDown(AValue: boolean);
begin
  FCheckButton := true;
  FDown := AValue;
  if FDown then
  begin
    FState := bsDown;
  end
  else
  begin
    FState := bsNormal;
  end;
  CheckChecked;
end;

procedure TAdBitmapButton.SetGroupIndex(AValue: integer);
begin
  if FGroupIndex <> AValue then
  begin
    FGroupIndex := AValue;
    if FDown then
    begin
      SetDown(true);
    end;
  end;  
end;

{ TAdLabel }

constructor TAdLabel.Create(AParent: TAdComponent);
begin
  inherited;
  AcceptChildComponents := false;
  MinWidth := 20;
  MinHeight := 20;
  FClipText := true;
end;

procedure TAdLabel.DoDraw;
var
  opt:TAdFontDrawModes;
begin

  SetFontColor;
  if DesignMode then
  begin
    with AdDraw.Canvas do
    begin
      Brush.Style := abClear;
      Pen.Color := Ad_ARGB(200,128,128,128);
      Rectangle(BoundsRect);
    end;
  end;

  opt := [dtDoLineFeeds];
  case FAlignment of
    alLeft: opt := opt + [dtLeft];
    alCenter: opt := opt + [dtCenter];
    alRight: opt := opt + [dtRight];
  end;
  case FTextPos of
    tpTop: opt := opt + [dtTop];
    tpCenter: opt := opt + [dtMiddle];
    tpBottom: opt := opt + [dtBottom];
  end;
  if FWordWrap then
  begin
    opt := opt + [dtWordWrap];
  end;
  if FClipText then
  begin
    opt := opt + [dtCut];
  end;

  with TAdSimpleTypeSetter(Font.TypeSetter) do
  begin
    DrawMode := opt;
  end;

  Font.TextOut(ClientRect,FCaption);

  inherited;
end;

procedure TAdLabel.LoadFromXML(aroot: TJvSimpleXMLElem);
begin
  inherited;
  with aroot.Properties do
  begin
    FTextPos := TAdTextPos(IntValue('textpos',ord(tpCenter)));
    FAlignment := TAdAlignment(IntValue('alignment',ord(alCenter)));
    FCaption := Value('caption','');
    FWordWrap := BoolValue('wordwrap',false);
  end;
end;

function TAdLabel.SaveToXML(aroot: TJvSimpleXMLElems): TJvSimpleXMLElem;
begin
  result := inherited SaveToXML(aroot);
  with result.Properties do
  begin
    Add('textpos',ord(FTextPos));
    Add('alignment',ord(FAlignment));
    Add('caption',FCaption);
    Add('wordwrap',FWordWrap);
  end;
end;

{ TAdProgressBar }

constructor TAdProgressBar.Create(AParent: TAdComponent);
begin
  inherited;
  FMax := 100;
  FMin := 0;
  FPosition := 50;
  FSmooth := false;
  AcceptChildComponents := false;
end;

destructor TAdProgressBar.Destroy;
begin
  inherited;
end;

procedure TAdProgressBar.DoDraw;
var
  r:TAdRect;
  w,c:integer;
  i: Integer;
  start: Integer;
begin
  if (FSkinItem <> nil) and (FSkinProgress <> nil) then
  begin
    r := BoundsRect;
    FSkinItem.Draw(0,r.Left,r.Top,r.Right-r.Left,r.Bottom-r.Top);
    w := round((r.Right - r.Left - SpacerLeft) * Percent);
    start := r.Left + SpacerLeft div 2;
    if Smooth then
    begin
      case FAlign of
        alLeft: start := r.Left+SpacerLeft div 2;
        alCenter: start := r.Left + (r.Right - r.Left - w) div 2;
        alRight: start := r.Right-SpacerLeft div 2-w;
      end;
      if w > SpacerLeft then
      begin
        FSkinProgress.Draw(0,start,r.Top+SpacerTop div 2,
                             w,r.Bottom-r.Top-SpacerTop);
      end;
    end
    else
    begin
      if FPosition > FMin then
      begin
        c := (w - FSkinProgress.BaseWidth) div (FSkinProgress.BaseWidth + 2);
        case FAlign of
          alLeft: start := r.Left+SpacerLeft div 2;
          alCenter: start := r.Left + (r.Right - r.Left - c*(FSkinProgress.BaseWidth + 2)) div 2;
          alRight: start := r.Right-SpacerLeft div 2- c*(FSkinProgress.BaseWidth + 2);
        end;
        for i := 0 to c do
        begin
          FSkinProgress.Draw(0,start + i * (FSkinProgress.BaseWidth+2),
            r.Top + (r.Bottom - r.Top - FSkinProgress.BaseHeight) div 2,
            FSkinProgress.BaseWidth,FSkinProgress.BaseHeight);
        end;
      end;
    end;
    if FShowPercentage then
    begin
      SetFontColor;
      with Font do
      begin
        with TAdSimpleTypeSetter(TypeSetter) do
        begin
          DrawMode := [dtCenter,dtMiddle];        
        end;
        TextOut(ClientRect,FormatFloat('0',Percent * 100)+'%');
      end;
    end;
  end;    
  inherited DoDraw;
end;

procedure TAdProgressBar.LoadFromXML(aroot: TJvSimpleXMLElem);
begin
  inherited;
  with aroot.Properties do
  begin
    FMin := IntValue('min',0);
    FMax := IntValue('max',100);
    FPosition := IntValue('position',50);
    FShowPercentage := BoolValue('showpercentage',false);
    FAlign := TAdAlignment(IntValue('align',Ord(alLeft)));
    Smooth := BoolValue('smooth',false);
  end;
end;

function TAdProgressBar.SaveToXML(aroot: TJvSimpleXMLElems): TJvSimpleXMLElem;
begin
  result := inherited SaveToXML(aroot);
  with result.Properties do
  begin
    Add('min',FMin);
    Add('max',FMax);
    Add('position',FPosition);
    Add('showpercentage',FShowPercentage);
    Add('align',ord(FAlign));
    Add('smooth',FSmooth);
  end;
end;

procedure TAdProgressBar.LoadSkinItem;
begin
  FSkinItem := Skin.ItemNamed['progressbar'];
  SetSpacer(FSkinItem);
  if FSmooth then
  begin
    FSkinProgress := Skin.ItemNamed['progressbarbar'];
  end
  else
  begin
    FSkinProgress := Skin.ItemNamed['progressbarelem'];
  end;
end;

function TAdProgressBar.Percent: single;
begin
  result := (Position - FMin) / (FMax - FMin);
end;

procedure TAdProgressBar.SetMax(AValue: integer);
begin
  FMax := AValue;
  if FPosition > FMax then
  begin
    FPosition := FMax;
  end;
  if FMax < FMin then
  begin
    FMin := FMax - 1;
  end;
end;

procedure TAdProgressBar.SetMin(AValue: integer);
begin
  FMin := AValue;
  if FPosition < FMin then
  begin
    FPosition := FMin;
  end;
  if FMax < FMin then
  begin
    FMax := FMin + 1;
  end;
end;

procedure TAdProgressBar.SetPosition(AValue: integer);
begin
  FPosition := AValue;
  if FPosition < FMin then
  begin
    FPosition := FMin;
  end;
  if FPosition > FMax then
  begin
    FPosition := FMax;
  end;
end;

procedure TAdProgressBar.SetSmooth(AValue: boolean);
begin
  if AValue <> FSmooth then
  begin
    FSmooth := AValue;
    LoadSkinItem;
  end;
end;

{ TAdContainer }

procedure TAdContainer.DoDraw;
begin
  if DesignMode then
  begin
    with AdDraw.Canvas do
    begin
      Brush.Style := abClear;
      Pen.Color := Ad_ARGB(200,128,128,128);
      Rectangle(BoundsRect);
    end;
  end;
  inherited;
end;

{ TAdGUIImage }

constructor TAdGUIImage.Create(AParent: TAdComponent);
begin
  inherited Create(AParent);
  FPicture := TAdResourceImage.Create(AdDraw);
  AcceptChildComponents := false;
  MinWidth := 10;
  MinHeight := 10;
end;

destructor TAdGUIImage.Destroy;
begin
  FPicture.Free;
  inherited Destroy;
end;

//Adapted from the VCLs TImage.DestRect;
function TAdGUIImage.DestinationRect: TAdRect;
var
  w,h:integer;
  v: double;
begin
  w := FPicture.Picture.Width;
  h := FPicture.Picture.Height;

  if Stretch or (FProportional and ((w > Width) or (h > Height))) then
  begin
    if FProportional and (w > 0) and (h > 0) then
    begin
      v := w / h;
      if w > h then
      begin
        w := Width;
        h := round(Width / v);
        if h > Height then
        begin
          h := Height;
          w := round(Height * v);
        end;
      end
      else
      begin
        h := Height;
        w := round(Height * v);
        if w > Width then
        begin
          w := Width;
          h := round(Width / v);
        end;
      end;
    end
    else
    begin
      w := Width;
      h := Height;
    end;
  end;

  with result do
  begin
    Left := 0;
    Top := 0;
    Right := w;
    Bottom := h;
  end;

  if FCenter then
  begin
    AdOffsetRect(result, BoundsRect.Left + (Width - w) div 2,
                       BoundsRect.Top + (Height - h) div 2);
  end
  else
  begin
    AdOffsetRect(result, BoundsRect.Left, BoundsRect.Top);
  end;
end;

procedure TAdGUIImage.DoDraw;
begin
  if DesignMode then
  begin
    with AdDraw.Canvas do
    begin
      Brush.Style := abClear;
      Pen.Color := Ad_ARGB(200,128,128,128);
      Rectangle(BoundsRect);
    end;
  end;
  if FPicture.Loaded then
  begin
    FPicture.Picture.DrawAlpha(AdDraw,DestinationRect,0,Alpha);
  end;
  inherited;
end;

procedure TAdGUIImage.DoMove(timegap:double);
begin
  inherited;
  if (Designmode) and (FAutoSize) and (FPicture.Loaded) then
  begin
    Width := FPicture.Picture.Width;
    Height := FPicture.Picture.Height;
  end;
end;

procedure TAdGUIImage.LoadFromXML(aroot: TJvSimpleXMLElem);
begin
  inherited LoadFromXML(aroot);
  with aroot.Properties do
  begin
    FCenter := BoolValue('center',false);
    FProportional := BoolValue('proportional',false);
    FAutoSize := BoolValue('autosize',false);
    FStretch := BoolValue('stretch',false);
    FPicture.LoadFromString(Value('picture',''));
  end;
end;

function TAdGUIImage.SaveToXML(aroot: TJvSimpleXMLElems): TJvSimpleXMLElem;
begin
  result := inherited SaveToXML(aroot);
  with result.Properties do
  begin
    Add('center',FCenter);
    Add('stretch',FStretch);
    Add('proportional',FProportional);
    Add('autosize',FAutoSize);
    Add('picture',FPicture.SaveToString);
  end;
end;

{ TAdEdit }

function TAdEdit.DoClick:boolean;
begin
  inherited DoClick;
  result := true;
  SetFocused;
end;

procedure TAdEdit.DoDraw;
var
  r,cr:TAdRect;
  w,h,th:integer;
  SelStartX:integer;
  SelStopX:integer;
  XDif:integer;
begin
  r := BoundsRect;
  cr := ClientRect;
  FSkinItem.Draw(0,r.Left,r.Top,r.Right-r.Left,r.Bottom-r.Top);

  w := cr.Right-cr.Left;
  h := cr.Bottom-cr.Top;
  th := Font.TextHeight(FText+'W');
  SetFontColor;

  SelStartX := Font.TextWidth(copy(Text,1,SelStart));
  SelStopX  := Font.TextWidth(copy(Text,1,SelStop));

  XDif := 0;
  if SelStopX > w - 5 then
  begin
    XDif := SelStopX-w+Font.TextWidth('W')*2;
  end;

  with AdDraw.Canvas do
  begin
    if SelCount > 0 then
    begin
      Brush.Color := Ad_ARGB(128,128,128,128);
      Pen.Style := apNone;
      Rectangle(
        AdRect(
          cr.Left + SelStartX-XDif,
          cr.Top + (h-th) div 2,
          cr.Left + SelStopX-XDif,
          cr.Top + (h+th) div 2));
      Release;
    end;

    self.Font.TextOut(cr.Left-XDif,cr.Top+(h-th) div 2,FText);

    if (CursorVisible) and (Enabled) and (Focused) and (not DesignMode) then
    begin
      Pen.Color := ColorToAdColor(FontColor);
      MoveTo(cr.Left+SelStopX-XDif,cr.Top+(h-th) div 2);
      LineTo(cr.Left+SelStopX-XDif,cr.Top+(h+th) div 2);
      Release;
    end;
  end;

  inherited;
end;

constructor TAdEdit.Create(AParent: TAdComponent);
begin
  inherited;
  AcceptChildComponents := false;
end;

procedure TAdEdit.DelSelText;
var s:integer;
begin
  s := selStart;
  if SelStart > SelStop then s := SelStop;
  Delete(FText,S+1,SelCount);
  SelStart := s;
  SelStop := s;
end;

const
  VK_BACK = $08;
  VK_DELETE = $2E;
  VK_LEFT = 37;
  VK_HOME = 36;
  VK_END = 35;
  VK_RIGHT = 39;

function TAdEdit.DoKeyDown(Key:Word;shift:TShiftState):boolean;
begin
  result := true;
  CursorVisible := true;
  CursorTime := 0;
  if key = VK_BACK then
  begin
    if SelCount = 0 then
    begin
      Delete(FText,SelStart,1);
      SelStart := SelStart-1;
      SelStop := SelStart;
    end
    else
    begin
      DelSelText;
    end;
  end;
  if key = VK_DELETE then
  begin
    if SelCount = 0 then
    begin
      Delete(FText,SelStart+1,1);
    end
    else
    begin
      DelSelText;
    end;
  end;
  if key = VK_LEFT then
  begin
    if ssShift in Shift then
    begin
      SelStop := SelStop-1;
    end
    else
    begin
      if SelStop < SelStart then
      begin
        SelStart := SelStop;
      end
      else
      begin
        if SelStop > SelStart then
        begin
          SelStart := SelStop;
        end
        else
        begin
          SelStart := SelStart-1;
          SelStop := SelStart;
        end;
      end;
    end;
  end;
  if key = VK_HOME then
  begin
    if ssShift in Shift then
    begin
      SelStop := 0;
    end
    else
    begin
      SelStart := 0;
      SelStop := 0;
    end;
  end;
  if key = VK_END then
  begin
    if ssShift in Shift then
    begin
      SelStop := Length(Text);
    end
    else
    begin
      SelStart := Length(Text);
      SelStop := Length(Text);
    end;
  end;
  if key = VK_RIGHT then
  begin
    if ssShift in Shift then
    begin
      SelStop := SelStop+1;
    end
    else
    begin
      if SelStop < SelStart then
      begin
        SelStart := SelStop;
      end
      else
      begin
        if SelStop > SelStart then
        begin
          SelStart := SelStop;
        end
        else
        begin
          SelStart := SelStart+1;
          SelStop := SelStart;
        end;
      end;
    end;
  end;
  if SelStart < 0 then SelStart := 0;
  if SelStart > Length(Text) then SelStart := Length(Text);
  if SelStop < 0 then SelStop := 0;
  if SelStop > Length(Text) then SelStop := Length(Text);
end;

function TAdEdit.DoKeyPress(Key: Char):boolean;
begin
  inherited DoKeyPress(Key);
  result := true;
  if (ord(key) > 31) and ((ord(key) < 127) or (ord(key) > 159)) then
  begin
    if SelCount > 0 then
    begin
      DelSelText;
    end;
    if SelStart >= Length(FText)-1 then
    begin
      Text := Text + Key;
    end
    else
    begin
      Insert(Key,FText,SelStart+1);
    end;
    SelStart := SelStart+1;
    SelStop := SelStart;
  end;  
end;

procedure TAdEdit.DoMove(timegap: double);
begin
  inherited;
  CursorTime := CursorTime + timegap * 10;
  If CursorTime > 1 then
  begin
    CursorVisible := not CursorVisible;
    CursorTime := 0;
  end;
end;

function TAdEdit.MouseToSelPos(AX:integer):integer;
var
  i:integer;
  XDif:integer;
  SelStopX:integer;
  w:integer;
begin
 { Result := length(FText);

  w := (ClientRect.Right - ClientRect.Left);
  SelStopX  := Font.TextWidth(copy(Text,1,SelStop));

  XDif := 0;
  if SelStopX > w - 5 then
  begin
    XDif := SelStopX - w;
  end;

  AX := XDif + (AX - Clientrect.Left);

  for i := 0 to length(FText) do
  begin
    if Font.TextWidth(copy(FText,1,i)) >= AX then
    begin
      result := i;
      break;
    end;
  end;   }
end;

function TAdEdit.DoMouseMove(Shift: TShiftState; X, Y: Integer):boolean;
begin
  inherited DoMouseMove(Shift,X,Y);
  result := true;
  if ssLeft in Shift then
  begin
    SelStop := MouseToSelPos(X);
  end;
end;

function TAdEdit.DoMouseDown(Button:TMouseButton; Shift:TShiftState;X,Y:integer):boolean;
begin
  inherited DoMouseDown(Button,Shift,X,Y);
  result := true;
  if Button = mbLeft then
  begin
    SelStart := MouseToSelPos(X);
    SelStop := SelStart;
  end;
end;



function TAdEdit.GetSelCount: integer;
begin
  result := abs(SelStop-SelStart);
end;

procedure TAdEdit.LoadSkinItem;
begin
  FSkinItem := Skin.ItemNamed['edit'];
  SetSpacer(FSkinItem);
end;

function TAdEdit.SaveToXML(aroot: TJvSimpleXMLElems): TJvSimpleXMLElem;
begin
  result := inherited SaveToXML(aroot);
  result.Properties.Add('text',FText)
end;

procedure TAdEdit.LoadFromXML(aroot: TJvSimpleXMLElem);
begin
  inherited;
  FText := aroot.Properties.Value('text','');
end;

{ TAdSkinBtn }

constructor TAdSkinBtn.Create(AParent: TAdComponent);
begin
  inherited;
  FStateNr := -1;
  FState := bsNormal;
end;

procedure TAdSkinBtn.GetStateNr;
begin
  FStateNr := -1;
  if (FSkinItem <> nil) then
  begin
    case FState of
      bsNormal: FStateNr := FSkinItem.States.IndexOf('standard');
      bsDown: FStateNr := FSkinItem.States.IndexOf('down');
      bsHover: FStateNr := FSkinItem.States.IndexOf('hover');
    end;
  end;
end;

procedure TAdSkinBtn.LoadSkinItem;
begin
  if Skin <> nil then
  begin
    FSkinItem := Skin.ItemNamed[FSkinName];
    if FSkinItem <> nil then
    begin
      SetSpacer(FSkinItem);
      Width := FSkinItem.BaseWidth;
      Height := FSkinItem.BaseHeight;
      GetStateNr;
    end;
  end;
end;

procedure TAdSkinBtn.SetSkinName(AValue: string);
begin
  FSkinName := AValue;
  LoadSkinItem;
end;

procedure TAdSkinBtn.DoDraw;
var
  r:TAdRect;
begin
  inherited;
  if FStateNr <> -1 then
  begin
    r := BoundsRect;
    FSkinItem.Draw(FStateNr,r.Left,r.Top,Width,Height,Alpha);
  end;
end;

function TAdSkinBtn.DoMouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer):boolean;
begin
  inherited DoMouseDown(Button,Shift,X,Y);
  result := true;
  FState := bsDown;
  GetStateNr;
end;

function TAdSkinBtn.DoMouseEnter:boolean;
begin
  inherited DoMouseEnter;
  result := true;
  FState := bsHover;
  GetStateNr;
end;

function TAdSkinBtn.DoMouseLeave:boolean;
begin
  inherited DoMouseLeave;
  result := true;
  FState := bsNormal;
  GetStateNr;
end;

function TAdSkinBtn.DoMouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer):boolean;
begin
  inherited DoMouseUp(Button,Shift,X,Y);
  result := true;
  FState := bsHover;
  GetStateNr;
end;

initialization
  RegisterComponent(TAdProgressBar,'Standard');
  RegisterComponent(TAdButton,'Standard');
  RegisterComponent(TAdEdit,'Standard');
  RegisterComponent(TAdCheckbox,'Standard');
  RegisterComponent(TAdLabel,'Standard');
  RegisterComponent(TAdPanel,'Standard');
  RegisterComponent(TAdContainer,'Standard');
  RegisterComponent(TAdForm,'Standard');
  RegisterComponent(TAdBitmapButton, 'Additional');
  RegisterComponent(TAdGUIImage, 'Additional');

finalization

end.
