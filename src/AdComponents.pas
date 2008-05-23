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

{Contains components which can be used with the Andorra GUI.}
unit AdComponents;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  AdSimpleXML, AdGUI, AdXML, AdSkin, AdClasses, AdDraws, AdTypes, AdBitmap, AdList,
  Classes, Graphics, SysUtils, AdCanvas, AdFont, AdEvents;

type
  {Specifies the horizontal text alignment.}
  TAdAlignment = (
    alLeft,{<The text is aligned on the left}
    alCenter,{<The text is aligned in the center}
    alRight{The text is aligned on the right});
    
  {Specifies the vertical text alignment.}
  TAdTextPos = (
    tpTop,{<The text is aligned on the top.}
    tpCenter,{< The text is aligned in the center.}
    tpBottom{<The text is aligned on the bottom.});
    
  {@exclude}
  TAdAlignmentEx = (axLeft, axRight, axTop, axBottom);
  {Defines the state a button control is currently in.}
  TAdButtonState = (
    bsNormal,{<The button is in its standard state.}
    bsDown, {<The button is currently down.}
    bsHover, {<The button is hovered.}
    bsFocus, {< The button hase the focus}
    bsDisabled {< The button is currently disabled});

  {@exclude}
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
      function DoMouseDown(Button: TAdMouseButton; Shift: TAdShiftState;
        X, Y: Integer):boolean;override;
      function DoMouseUp(Button: TAdMouseButton; Shift: TAdShiftState;
        X, Y: Integer):boolean;override;
      procedure LoadSkinItem;override;
    public
      constructor Create(AParent:TAdComponent);override;
      property SkinName:string read FSkinName write SetSkinName;
  end;
  
  {A event which is called when closing a form.}
  TCloseEvent = procedure(Sender:TObject; var CanClose:boolean) of object;

  {A formular component.}
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

      function DoMouseDown(Button: TAdMouseButton; Shift: TAdShiftState;
        X, Y: Integer):boolean;override;
      function DoMouseUp(Button: TAdMouseButton; Shift: TAdShiftState;
        X, Y: Integer):boolean;override;
      function DoMouseMove(Shift: TAdShiftState; X, Y: Integer):boolean;override;

      function GetShowCloseButton:boolean;
      procedure SetShowCloseButton(AValue:boolean);
      procedure CloseBtnClick(Sender:TObject);
    public
      {Creates an instance of TAdForm.}
      constructor Create(AParent:TAdComponent);override;
      {Destroys the instance of TAdForm.}
      destructor Destroy;override;
      {Loads the form data from XML.}
      procedure LoadFromXML(aroot:TAdSimpleXMLElem);override;
      {Saves the from data to XML.}
      function SaveToXML(aroot:TAdSimpleXMLElems):TAdSimpleXMLElem;override;
      {Closes the form}
      procedure Close;
      {Link to the close button, which is displayed in the top-right edge.}
      property CloseButton:TAdSkinBtn read FCloseButton;
    published
      {Event, which is called when the form is closed.}
      property OnClose:TCloseEvent read FOnClose write FOnClose;
      {The caption of the formular.}
      property Caption:string read FCaption write FCaption;
      {Specifies, wether the form should be in the center of its parent element.}
      property Center:boolean read FCenter write SetCenter;
      {Specifies, wether the close button should be shown.}
      property ShowCloseButton:boolean read GetShowCloseButton write SetShowCloseButton;
      {Specifies, wether the form is moveable.}
      property Movable:boolean read FMovable write FMovable;
      {Specifies the name of the font.}
      property FontName;
      {Specifies the color of the font.}
      property FontColor;
  end;

  {A simple component which may contain other components.}
  TAdContainer = class(TAdComponent)
    protected
      procedure DoDraw;override;
  end;

  {A label component.}
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
      {Creates an instance of TAdLabel. }
      constructor Create(AParent:TAdComponent);override;
      {Loads the settings from XML.}
      procedure LoadFromXML(aroot:TAdSimpleXMLElem);override;
      {Saves the settings to XML}
      function SaveToXML(aroot:TAdSimpleXMLElems):TAdSimpleXMLElem;override;
    published
      {The caption of the label.}
      property Caption:string read FCaption write FCaption;
      {The horizontal position of the text.}
      property TextPos:TAdTextPos read FTextPos write FTextPos;
      {The vertical alignment of the text.}
      property Alignment:TAdAlignment read FAlignment write FAlignment;
      {Defines, whether the text should be wrapped.}
      property WordWrap:boolean read FWordWrap write FWordWrap;
      {Defines, whether the text should be clipped when it runs out of the bounds of the component.}
      property ClipText:boolean read FClipText write FClipText;
      {Defines the name of the font.}
      property FontName;
      {Degines the color of the font.}
      property FontColor;
  end;

  {A simple panel.}
  TAdPanel = class(TAdLabel)
    private
      FSkinItem:TAdSkinItem;
    protected
      procedure LoadSkinItem;override;
      procedure DoDraw;override;
    public
      {Creates an instance of TAdPanel.}
      constructor Create(AParent:TAdComponent);override;
  end;

  {A simple gui button.}
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
      function DoMouseDown(Button: TAdMouseButton; Shift: TAdShiftState;
        X, Y: Integer):boolean;override;
      function DoMouseUp(Button: TAdMouseButton; Shift: TAdShiftState;
        X, Y: Integer):boolean;override;
    public
      {Creates a new instance of TAdButton.}
      constructor Create(AParent:TAdComponent);override;
      {Destroys the instance of TAdButton.}
      destructor Destroy;override;
      {Loads the settings from XML.}
      procedure LoadFromXML(aroot:TAdSimpleXMLElem);override;
      {Saves the settings to XML.}
      function SaveToXML(aroot:TAdSimpleXMLElems):TAdSimpleXMLElem;override;
      {Returns the state the button is currently in.}
      property State:TAdButtonState read FState;
    published
      {Defines the caption of the button}
      property Caption:string read FCaption write FCaption;
      {Defines the font, which is used to draw the caption}      
      property FontName;
      {Defines the font color, which is used to draw the caption}
      property FontColor;
    end;

  {A simple CheckBox control. @author(Written by Michael Morstein alias Neutral General)}
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
      function DoMouseDown(Button: TAdMouseButton; Shift: TAdShiftState;
        X, Y:Integer):boolean;override;
      function DoMouseUp(Button: TAdMouseButton; Shift: TAdShiftState;
        X, Y:Integer):boolean;override;
      function DoMouseEnter:boolean;override;
      function DoMouseLeave:boolean;override;
    public      
      {Creates an instance of TAdCheckBoc}
      constructor Create(AParent:TAdComponent);override;
      {Loads the settings from XML}
      procedure LoadFromXML(aroot:TAdSimpleXMLElem); override;
      {Saves the settings to XML}
      function SaveToXML(aroot:TAdSimpleXMLElems): TAdSimpleXMLElem;override;
    published
      {Returns, wether the checkbox is checked or not. Set this property to check/uncheck the checkbox.}
      property Checked:Boolean read FChecked write SetChecked;
      {Defines the caption of the checkbox.}
      property Caption:String read FCaption write FCaption;
      {Sets the alignment of the checkbox.}
      property Alignment:TAdAlignmentEx read FAlignment write FAlignment;
      {If groupindex is unequal zero,  TAdCheckBox will behave like a radio button. It is grouped with all other checkboxes with the same group index on the same parent element.}
      property GroupIndex:integer read FGroupIndex write SetGroupIndex;
      {Defines the font, which is used to draw the caption}      
      property FontName;
      {Defines the font color, which is used to draw the caption}
      property FontColor;
  end;

  {A simple image, which may be stored in components.}
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
      {Creates an instance of TAdResourceImage.}
      constructor Create(AParent:TAdDraw);
      {Destroys the instance of TAdResourceImage.}
      destructor Destroy;override;
      {Loads the image from the specified file.}
      procedure LoadFromFile(AFile:string;ATransparent:boolean;ATransparentColor:LongInt);
      {Loads the image from the specified graphic.}
      procedure LoadFromGraphic(AGraphic:TGraphic);
      {Draws the image somewhere on the screen}
      procedure Draw(X,Y:integer);
      {Loads the picture data from a string}
      procedure LoadFromString(AString:string);
      {Saves the picture data to a string}
      function SaveToString:string;
      {Loads the picture data from a stream.}
      procedure LoadFromStream(AStream:TStream);
      {Saves the picture data to a stream.}
      procedure SaveToStream(AStream:TStream);
      {Link to the internal used TAdImage}
      property Picture:TAdImage read FImage;
      {Toggles whether the transparent color is used.}
      property Transparent:boolean read FTransparent write SetTransparent;
      {Set the transparent color here. Transparent will automaticly set to "true" when using it.}
      property TransparentColor:TColor read FTransparentColor write SetTransparentColor;
      {Link to the parent TAdDraw.}
      property Parent:TAdDraw read FParent;
      {The compressor which should be used to compress the picture.}
      property Compressor:TAdGraphicCompressorClass read FCompressor write SetCompressor;
      {Returns whether the picture is loaded.}
      property Loaded:boolean read GetLoaded;
  end;

  {A simple button, which uses bitmap resources to draw.}
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
      function DoMouseDown(Button: TAdMouseButton; Shift: TAdShiftState;
        X, Y:Integer):boolean; override;
      function DoMouseUp(Button: TAdMouseButton; Shift: TAdShiftState;
        X, Y:Integer):boolean; override;
      function DoMouseEnter:boolean; override;
      function DoMouseLeave:boolean; override;
    public
      {Creates an instance of TAdBitmapButton.}
      constructor Create(AParent:TAdComponent);override;
      {Destroys the instance of TAdBitmapButton.}
      destructor Destroy;override;
      {Loads the settings and pictures from XML}
      procedure LoadFromXML(aroot:TAdSimpleXMLElem); override;
      {Stores the settings and pictures in XML}
      function SaveToXML(aroot:TAdSimpleXMLElems): TAdSimpleXMLElem;override;
      {Returns the state of the button. @seealso(TAdButtonState)}
      property State:TAdButtonState read FState;
    published
      {The image, which is used to draw the button in its normal state.}
      property ImgNormal:TAdResourceImage read FImgNormal;
      {The image, which is used to draw the button in its hover state}
      property ImgHover:TAdResourceImage read FImgHover;
      {The image, which is used to draw the button when it is down.}
      property ImgDown:TAdResourceImage read FImgDown;
      {The image, which is used to draw the button when it is disabled.}
      property ImgDisabled:TAdResourceImage read FImgDisabled;
      {The image, which should be drawn when the button is down (checked) and hovered.}
      property ImgCheckedHover:TAdResourceImage read FImgCheckedHover;
      {Set wether the button should be down. Only possible, if groupindex is unequal zero or checkbutton is true}
      property Down:boolean read FDown write SetDown;
      {Set this to true, if the button should behave like a checkbox.}
      property CheckButton:boolean read FCheckButton write FCheckButton;
      {If true, the button is sized to the size of the "normal" image.}
      property AutoSize:boolean read FAutoSize write FAutoSize;
      {If groupindex is unequal zero,  TAdBitmapButton will behave like a radio button. It is grouped with all other TAdBitmapButtons with the same group index on the same parent element.}      
      property GroupIndex:integer read FGroupIndex write SetGroupIndex;
  end;

  {A simple progressbar component.}
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
      {Creates an instance of TAdProgressBar.}
      constructor Create(AParent:TAdComponent);override;
      {Destroys the instance of TAdProgressBar.}
      destructor Destroy;override;
      {Loads the settings from XML.}
      procedure LoadFromXML(aroot:TAdSimpleXMLElem); override;
      {Stors the settings in XML.}
      function SaveToXML(aroot:TAdSimpleXMLElems): TAdSimpleXMLElem;override;
      {Returns the percentage, with which the TAdProgressBar is filled.}
      function Percent:single;
    published
      {The minimum value of the progressbar.}
      property Min:integer read FMin write SetMin;
      {The maximum percentage of the progressbar.}
      property Max:integer read FMax write SetMax;
      {The position of the progressbar.}
      property Position:integer read FPosition write SetPosition;
      {If true, the percentage of the progress is displayed in the middle of the control.}
      property ShowPercentage:boolean read FShowPercentage write FShowPercentage;
      {If "Smooth" is true, the progress bar will be draw as one solid block. If false, there will be multiple blocks visible.}
      property Smooth:boolean read FSmooth write SetSmooth;
      {Defines, where the percentage text should stand. @seealso(TAdAlignment)}
      property Align:TAdAlignment read FAlign write FAlign;
      {The name of the font, which is used to draw the percentage.}
      property FontName;
      {The color of the font, which is used to draw the percentage.}
      property FontColor;
  end;

  {A simple image component.}
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
      {Creates an instance of TAdGUIImage.}
      constructor Create(AParent:TAdComponent);override;
      {Destroys the instance of TAdGUIImage.}
      destructor Destroy;override;
      {Loads the settings and picture data from XML.}
      procedure LoadFromXML(aroot:TAdSimpleXMLElem); override;
      {Saves the settings and picture data to XML.}
      function SaveToXML(aroot:TAdSimpleXMLElems): TAdSimpleXMLElem;override;
    published
      {Specifies, whether the image should be centered.}
      property Center:boolean read FCenter write FCenter;
      {Specifies, whether the image should be streched.}
      property Stretch:boolean read FStretch write FStretch;
      {Specifies, wheter the image should be streched proportional. If true the image will automaticly be minimized, if the component is smaller than the image.}
      property Proportional:boolean read FProportional write FProportional;
      {If true, the size of the component will automaticly be set to the size of the image}
      property AutoSize:boolean read FAutoSize write FAutoSize;
      {The picture resource.}
      property Picture:TAdResourceImage read FPicture;
  end;

  {A simple edit component.}
  TAdEdit = class(TAdComponent)
    private
      FText:string;
      FSkinItem:TAdSkinItem;
      FCursorTime:double;
      FCursorBlinkSpeed:double;
      FCursorVisible:boolean;
      FCursorPos:integer;
      FTextStart:integer;
      FSelStart:integer;
      function CalcRelCursorPos(ACurPos:integer):integer;
      function CalcRelPixelCursorPos(ACurPos:integer):integer;
      function IsNotSpecialCharacter(AChar: char): boolean;
      function CalcCursorPos(x:integer):integer;
      function GetSelCount:integer;
      procedure DeleteSelectedText;
      procedure CheckRange;
    protected
      procedure LoadSkinItem; override;
      procedure DoDraw;override;
      procedure DoMove(timegap:double);override;
      function DoKeyDown(Key:Word; Shift:TAdShiftState):boolean;override;
      function DoKeyPress(Key:Char):boolean;override;
      function DoClick(X, Y:integer):boolean;override;
      function DoMouseUp(Button: TAdMouseButton; Shift: TAdShiftState; X,
        Y: integer):boolean;override;
      function DoMouseDown(Button: TAdMouseButton; Shift: TAdShiftState; X,
        Y: integer):boolean;override;
      function DoMouseMove(Shift: TAdShiftState; X, Y: Integer):boolean;override;

      property CursorBlinkSpeed:double read FCursorBlinkSpeed write FCursorBlinkSpeed;
    public
      {Creates an instance of TAdEdit.}
      constructor Create(AParent:TAdComponent);override;
      {Loads the settings from XML.}
      procedure LoadFromXML(aroot:TAdSimpleXMLElem); override;
      {Stores the settings in XML.}
      function SaveToXML(aroot:TAdSimpleXMLElems): TAdSimpleXMLElem;override;

      {The start of the selection. If nothing is selected, "SelStart" will be equal to "CursorPos"}
      property SelStart:integer read FSelStart write FSelStart;
      {The position of the edit field.}
      property CursorPos:integer read FCursorPos;
      {The count of selected chars.}
      property SelCount:integer read GetSelCount;      
    published
      {The text, which is written in the edit field.}
      property Text:string read FText write FText;
      {The font the text should be written in.}
      property FontName;
      {The color the text should be written in.}
      property FontColor;
  end;

  TAdOrientation = (orHorizontal, orVertical);

  TAdTrackControl = class(TAdComponent)
    private
      FOX, FOY, FMX, FMY, FOPos: integer;
      FMax, FMin, FPosition: integer;
      FTrackerPos: integer;
      FSkinItem: TAdSkinItem;
      FTrackItem: TAdSkinItem;
      FSkinState: integer;
      FTrackerState: integer;
      FSmooth: boolean;
      FOrientation: TAdOrientation;

      FOnChange: TAdNotifyEvent;

      function TrackerPos:TAdRect;
      procedure CalcPosition;
      procedure SetPosition(AValue:integer);
      procedure SetMax(AValue:integer);
      procedure SetMin(AValue:integer);
      procedure SetOrientation(AValue:TAdOrientation);
    protected
      procedure DoDraw;override;

      function DoMouseEnter:boolean;override;
      function DoMouseLeave:boolean;override;
      function DoMouseDown(Button:TAdMouseButton; Shift:TAdShiftState; X, Y: integer):boolean;override;
      function DoMouseUp(Button:TAdMouseButton; Shift:TAdShiftState; X, Y: integer):boolean;override;
      function DoMouseMove(Shift: TAdShiftState; X, Y: Integer):boolean;override;
      function DoMouseWheel(Shift: TAdShiftState; WheelDelta: integer; X, Y: integer):boolean;override;
      function DoResize:boolean;override;
    public
      constructor Create(AParent:TAdComponent);override;

      procedure LoadFromXML(aroot:TAdSimpleXMLElem); override;
      function SaveToXML(aroot:TAdSimpleXMLElems): TAdSimpleXMLElem;override;
    published
      property Max: integer read FMax write SetMax;
      property Min: integer read FMin write SetMin;
      property Position: integer read FPosition write SetPosition;
      property Smooth: boolean read FSmooth write FSmooth;
      property Orientation: TAdOrientation read FOrientation write SetOrientation;

      property OnChange: TAdNotifyEvent read FOnChange write FOnChange;
  end;

  TAdTrackBar = class(TAdTrackControl)
    protected
      procedure LoadSkinItem; override;
  end;

  TAdScrollBar = class(TAdTrackControl)
    protected
      procedure LoadSkinItem; override;
  end;

  TAdListBox = class(TAdComponent)
    private
      FSkinItem : TAdSkinItem;
      FVertScrollBar: TAdScrollbar;
      FHorzScrollBar: TAdScrollbar;
      FStrings: TStrings;
      FItemIndex: integer;

      FDisplayList: TAdCanvasDisplayList;

      procedure CreateScrollBars;
      procedure StringsChange(Sender: TObject);
      procedure UpdateView;
    protected
      procedure LoadSkinItem; override;

      procedure DoDraw;override;
      function DoResize:boolean;override;
    public
      constructor Create(AParent: TAdComponent);override;
      destructor Destroy;override;
    published
      property FontName;
      property FontColor;
      property Items: TStrings read FStrings write FStrings;
      property ItemIndex: integer read FItemIndex write FItemIndex;
  end;

implementation

const               
  SPACING = 5;

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

function TAdButton.DoMouseDown(Button: TAdMouseButton; Shift: TAdShiftState; X,
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

function TAdButton.DoMouseUp(Button: TAdMouseButton; Shift: TAdShiftState; X,
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

procedure TAdButton.LoadFromXML(aroot: TAdSimpleXMLElem);
begin
  inherited;
  with aroot.Properties do
  begin
    FCaption := Value('caption','');
  end;
end;

function TAdButton.SaveToXML(aroot: TAdSimpleXMLElems): TAdSimpleXMLElem;
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

function TAdForm.DoMouseDown(Button: TAdMouseButton; Shift: TAdShiftState; X,
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

    if FMovable then
      Draging := true;
  end;
end;

function TAdForm.DoMouseMove(Shift: TAdShiftState; X, Y: Integer):boolean;
begin
  inherited DoMouseMove(Shift,X,Y);
  result := true;
  if (FMovable) and (asLeft in Shift) and FDown then
  begin
    self.X := self.X + (X-FMX);
    self.Y := self.Y + (Y-FMY);
    FMX := X;
    FMY := Y;
  end;
end;

function TAdForm.DoMouseUp(Button: TAdMouseButton; Shift: TAdShiftState; X,
  Y: Integer):boolean;
begin
  inherited DoMouseUp(Button,Shift,X,Y);
  result := true;
  FDown := false;
  Draging := false;
end;

function TAdForm.DoResize:boolean;
begin
  inherited DoResize;
  result := true;
  FCloseButton.Y := (SpacerTop - FCloseButton.Height) div 2 - SpacerTop;
  FCloseButton.X := Width - SpacerRight - FCloseButton.Width - SpacerLeft;
  SetCenter(FCenter);
end;

procedure TAdForm.LoadFromXML(aroot: TAdSimpleXMLElem);
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

function TAdForm.SaveToXML(aroot: TAdSimpleXMLElems): TAdSimpleXMLElem;
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

function TAdCheckBox.DoMouseDown(Button: TAdMouseButton; Shift: TAdShiftState;
  X, Y: Integer):boolean;
begin
  result := true;
  FState := bsDown;

  if GroupIndex = 0 then
  begin
    Checked := not Checked;
  end
  else
  begin
    Checked := true;
  end;


  inherited DoMouseDown(Button,Shift,X,Y);
end;

function TAdCheckBox.DoMouseEnter:boolean;
begin
  result := true;
  FState := bsHover;
  inherited DoMouseEnter;
end;

function TAdCheckBox.DoMouseLeave:boolean;
begin
  result := true;
  FState := bsNormal;
  inherited DoMouseLeave;
end;

function TAdCheckBox.DoMouseUp(Button: TAdMouseButton; Shift: TAdShiftState;
  X, Y: Integer):boolean;
begin
  result := true;
  FState := bsHover;
  inherited DoMouseUp(Button,Shift,X,Y);
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

procedure TAdCheckBox.LoadFromXML(ARoot: TAdSimpleXMLElem);
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

function TAdCheckBox.SaveToXML(aroot: TAdSimpleXMLElems): TAdSimpleXMLElem;
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
    FImage.Texture.Texture.LoadFromBitmap(adbmp, ad32Bit);
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

function TAdBitmapButton.DoMouseDown(Button: TAdMouseButton; Shift: TAdShiftState;
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

function TAdBitmapButton.DoMouseUp(Button: TAdMouseButton; Shift: TAdShiftState; X,
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

procedure TAdBitmapButton.LoadFromXML(aroot: TAdSimpleXMLElem);
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

function TAdBitmapButton.SaveToXML(aroot: TAdSimpleXMLElems): TAdSimpleXMLElem;
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

procedure TAdLabel.LoadFromXML(aroot: TAdSimpleXMLElem);
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

function TAdLabel.SaveToXML(aroot: TAdSimpleXMLElems): TAdSimpleXMLElem;
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

procedure TAdProgressBar.LoadFromXML(aroot: TAdSimpleXMLElem);
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

function TAdProgressBar.SaveToXML(aroot: TAdSimpleXMLElems): TAdSimpleXMLElem;
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

procedure TAdGUIImage.LoadFromXML(aroot: TAdSimpleXMLElem);
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

function TAdGUIImage.SaveToXML(aroot: TAdSimpleXMLElems): TAdSimpleXMLElem;
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

constructor TAdEdit.Create(AParent: TAdComponent);
begin
  inherited;
  AcceptChildComponents := false;

  FCursorBlinkSpeed := 0.5;
  FTextStart := 1;
  FCursorPos := 0;
end;

procedure TAdEdit.DeleteSelectedText;
var
  start:integer;
begin
  if GetSelCount > 0 then
  begin
    if FCursorPos > FSelStart then
      start := FSelStart
    else
      start := FCursorPos;

    Delete(FText, start + 1, GetSelCount);

    FCursorPos := start;
    FSelStart := FCursorPos;
  end;
end;

function TAdEdit.CalcCursorPos(x: integer): integer;
var
  i:integer;
  s:string;
begin
  x := x - GetClientRect.Left;
  s := '';
  result := Length(FText);
  for i := FTextStart to Length(FText) do
  begin
    if Font.TextWidth(s) >= x then
    begin
      result := i - 1;
      break;
    end;
    s := s + FText[i];
  end;
end;

function TAdEdit.CalcRelCursorPos(ACurPos:integer): integer;
begin
  result := ACurPos - FTextStart;
end;

function TAdEdit.CalcRelPixelCursorPos(ACurPos:integer): integer;
var
  s:string;
begin
  s := copy(FText, FTextStart, CalcRelCursorPos(ACurPos) + 1);
  result := Font.TextWidth(s);
end;

procedure TAdEdit.CheckRange;
var
  curxpos:integer;
  rect:TAdRect;
begin
  rect := GetClientRect;
  curxpos := CalcRelPixelCursorPos(FCursorPos);
  while curxpos > rect.Right - rect.Left do
  begin
    FTextStart := FTextStart + 1;
    curxpos := CalcRelPixelCursorPos(FCursorPos);
  end;
  
  if (curxpos < 10) and (FTextStart > 1) then
  begin
    FTextStart := FTextStart - 1;
  end;
end;

function TAdEdit.GetSelCount: integer;
begin
  result := abs(FSelStart - FCursorPos);
end;

procedure TAdEdit.DoDraw;
var
  rect:TAdRect;
  s:string;
  curxpos, curxpos2:integer;
  curheight:integer;
  y1pos, y2pos:integer;
begin
  if FSkinItem <> nil then
  begin
    rect := GetBoundsRect;
    FSkinItem.Draw(0, rect);

    rect := GetClientRect;

    SetFontColor;

    curxpos := rect.Left + CalcRelPixelCursorPos(FCursorPos);
    curheight := Font.TextHeight('W');
    y1pos := rect.Top + (rect.Bottom - rect.Top) div 2 - curheight div 2;
    y2pos := rect.Top + (rect.Bottom - rect.Top) div 2 + curheight div 2;

    if Focused and FCursorVisible and not Designmode then
    begin
      with AdDraw.Canvas do
      begin
        Pen.Color := ColorToAdColor(FontColor);
        Pen.Width := 1;
        MoveTo(curxpos, y1pos);
        LineTo(curxpos, y2pos);
        Release;
      end;
    end;

    if GetSelCount > 0 then
    begin
      curxpos2 := rect.Left + CalcRelPixelCursorPos(FSelStart);
      if curxpos2 > rect.Right then curxpos2 := rect.Right;
      
      with AdDraw.Canvas do
      begin
        Pen.Style := apNone;
        Brush.Color := Ad_ARGB(200,128,128,128);
        Rectangle(curxpos,y1pos,curxpos2,y2pos);
        Release;
        Pen.Style := apSolid;
      end;
    end;

    with Font do
    begin
      with TypeSetter as TAdSimpleTypeSetter do
      begin
        DrawMode := [dtCut, dtLeft, dtMiddle];
      end;
      s := Copy(FText, FTextStart, Length(FText) - FTextStart + 1);
      TextOut(rect, s);
    end;
  end;
  
  inherited;
end;

function TAdEdit.DoClick(X, Y:integer):boolean;
begin
  inherited DoClick(X, Y);
  result := true;
  SetFocused;
end;

function TAdEdit.DoKeyDown(Key:Word; Shift:TAdShiftState):boolean;
var
  pressed : boolean;
begin
  inherited DoKeyDown(Key, Shift);
  result := true;
  pressed := false;

  FCursorVisible := true;
  FCursorTime := 0;

  if Key = AVK_BACK then
  begin
    pressed := true;
    if GetSelCount > 0 then
      DeleteSelectedText
    else
    begin
      Delete(FText, FCursorPos, 1);
      FCursorPos := FCursorPos - 1;
    end;
  end else
  if Key = AVK_DELETE then
  begin
    pressed := true;
    if GetSelCount > 0 then
      DeleteSelectedText
    else
      Delete(FText, FCursorPos + 1, 1);
  end else
  if key = AVK_LEFT then
  begin
    pressed := true;
    FCursorPos := FCursorPos - 1;
  end else
  if key = AVK_RIGHT then
  begin
    pressed := true;
    FCursorPos := FCursorPos + 1;
  end else
  if key = AVK_HOME then
  begin
    pressed := true;
    FCursorPos := 0;
    FTextStart := 1;
  end else
  if key = AVK_END then
  begin
    pressed := true;
    FCursorPos := Length(FText) + 1;
  end;

  if FCursorPos < 0 then FCursorPos := 0;
  if FCursorPos > Length(FText) then FCursorPos := Length(FText);

  if pressed and not (asShift in Shift) then FSelStart := FCursorPos;
end;

function TAdEdit.IsNotSpecialCharacter(AChar:char):boolean;
begin
  result := (AChar > #31) and ((AChar < #127) or (AChar > #159));
end;

function TAdEdit.DoKeyPress(Key: Char):boolean;
begin
  inherited DoKeyPress(Key);
  result := false;

  FCursorVisible := true;
  FCursorTime := 0;

  if IsNotSpecialCharacter(Key) then
  begin
    DeleteSelectedText;

    FCursorPos := FCursorPos + 1;
    Insert(Key, FText, FCursorPos);
    FSelStart := FCursorPos;
  end;

  CheckRange;
end;

procedure TAdEdit.DoMove(timegap: double);
begin
  inherited;

  FCursorTime := FCursorTime + timegap;
  if FCursorTime > FCursorBlinkSpeed then
  begin
    FCursorTime := 0;
    FCursorVisible := not FCursorVisible;
  end;

  CheckRange;
end;

function TAdEdit.DoMouseMove(Shift: TAdShiftState; X, Y: Integer):boolean;
begin
  inherited DoMouseMove(Shift,X,Y);
  result := true;

  if asLeft in Shift then
  begin
    FSelStart := CalcCursorPos(x);
  end;
end;

function TAdEdit.DoMouseUp(Button: TAdMouseButton; Shift: TAdShiftState; X,
  Y: integer): boolean;
begin
  inherited DoMouseUp(Button, Shift, X, Y);
  result := true;
  MousePreview := false;
end;

function TAdEdit.DoMouseDown(Button:TAdMouseButton; Shift:TAdShiftState; X,Y:integer):boolean;
begin
  inherited DoMouseDown(Button,Shift,X,Y);
  result := true;

  if Button = abLeft then
  begin
    FCursorPos := CalcCursorPos(x);
    MousePreview := true;
  end;

  if not (asShift in Shift) then FSelStart := FCursorPos;
end;

procedure TAdEdit.LoadSkinItem;
begin
  FSkinItem := Skin.ItemNamed['edit'];
  SetSpacer(FSkinItem);
end;

function TAdEdit.SaveToXML(aroot: TAdSimpleXMLElems): TAdSimpleXMLElem;
begin
  result := inherited SaveToXML(aroot);
  result.Properties.Add('text',FText)
end;

procedure TAdEdit.LoadFromXML(aroot: TAdSimpleXMLElem);
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

function TAdSkinBtn.DoMouseDown(Button: TAdMouseButton; Shift: TAdShiftState; X,
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

function TAdSkinBtn.DoMouseUp(Button: TAdMouseButton; Shift: TAdShiftState; X,
  Y: Integer):boolean;
begin
  inherited DoMouseUp(Button,Shift,X,Y);
  result := true;
  FState := bsHover;
  GetStateNr;
end;

{ TAdTrackbar }

constructor TAdTrackControl.Create(AParent: TAdComponent);
begin
  inherited;

  FMin := 0;
  FMax := 100;
  FSmooth := true;
  Position := 0;

  AcceptChildComponents := false;
end;

procedure TAdTrackControl.DoDraw;
var
  sih,siw:integer;
  r:TAdRect;
begin
  inherited;

  sih := FSkinItem.BaseHeight;
  siw := FSkinItem.BaseWidth;

  r := BoundsRect;

  case FOrientation of
    orHorizontal:
    begin
      FSkinItem.Draw(FSkinState, AdBounds(
        r.Left, r.Top+(Height-sih) div 2, Width, sih), Alpha);

      FTrackItem.Draw(FTrackerState, TrackerPos, Alpha);
    end;
    orVertical:
    begin
      FSkinItem.Draw(FSkinState, AdBounds(
        r.Left + (Width-siw) div 2, r.Top, siw, Height), Alpha);

      FTrackItem.Draw(FTrackerState, TrackerPos, Alpha);
    end;
  end;
end;

function TAdTrackControl.DoMouseDown(Button: TAdMouseButton; Shift: TAdShiftState;
  X, Y: integer): boolean;
begin
  result := inherited DoMouseDown(Button, Shift, X, Y);

  if (Button = abLeft) and InRect(X, Y, TrackerPos) then
  begin
    FTrackerState := 2;  
    FOX := X; FOY := Y;
    FMX := 0; FMY := 0;
    FOPos := FTrackerPos;
    Draging := true;
  end;
end;

function TAdTrackControl.DoMouseUp(Button: TAdMouseButton; Shift: TAdShiftState; X,
  Y: integer): boolean;
begin
  result := inherited DoMouseUp(Button, Shift, X, Y);

  FTrackerState := 1;

  SetFocused;
end;

function TAdTrackControl.DoMouseWheel(Shift: TAdShiftState; WheelDelta, X,
  Y: integer): boolean;
begin
  result := inherited DoMouseWheel(Shift, WheelDelta, X, Y);

  Position := Position - WheelDelta div 120;
end;

function TAdTrackControl.DoResize: boolean;
begin
  result := inherited DoResize;
  SetPosition(FPosition);
end;

function TAdTrackControl.DoMouseEnter:boolean;
begin
  result := inherited DoMouseEnter;
  FSkinState := 1;
  FTrackerState := 1;
end;

function TAdTrackControl.DoMouseLeave:boolean;
begin
  result := inherited DoMouseLeave;
  FSkinState := 0;
  FTrackerState := 0;
end;

function TAdTrackControl.DoMouseMove(Shift: TAdShiftState; X, Y: Integer): boolean;
begin
  result := inherited DoMouseMove(Shift, X, Y);
  if (asLeft in Shift) and Draging then
  begin
    FMX := FMX + (X-FOX);
    FMY := FMY + (Y-FOY);

    case FOrientation of
      orHorizontal: FTrackerPos := FOPos + FMX;
      orVertical: FTrackerPos := FOPos + FMY;
    end;

    if FTrackerPos < 0 then
      FTrackerPos := 0;
    if (FTrackerPos > Width - FTrackItem.BaseWidth) and (FOrientation = orHorizontal) then
      FTrackerPos := Width - FTrackItem.BaseWidth;
    if (FTrackerPos > Height - FTrackItem.BaseHeight) and (FOrientation = orVertical) then
      FTrackerPos := Height - FTrackItem.BaseHeight;

    CalcPosition;

    if not Smooth then
      SetPosition(FPosition)
    else
      if Assigned(FOnChange) then
        FOnChange(Self);


    FOX := X;
    FOY := Y;
  end;
end;

procedure TAdTrackControl.CalcPosition;
begin
  case FOrientation of
    orHorizontal: FPosition :=
      round((FTrackerPos / (Width - FTrackItem.BaseWidth))   * (FMax - FMin) + FMin);
    orVertical:   FPosition :=
      round((FTrackerPos / (Height - FTrackItem.BaseHeight)) * (FMax - FMin) + FMin);
  end;
end;

procedure TAdTrackControl.SetMax(AValue: integer);
begin
  FMax := AValue;
  if AValue < FMin then
    FMax := FMin;

  SetPosition(FPosition);
end;

procedure TAdTrackControl.SetMin(AValue: integer);
begin
  FMin := AValue;
  if AValue > FMax then
    FMin := FMax;

  SetPosition(FPosition);
end;

procedure TAdTrackControl.SetOrientation(AValue: TAdOrientation);
var
  tmp: integer;
begin
  if AValue <> FOrientation then
  begin
    tmp := Width;
    Width := Height;
    Height := tmp;

    FOrientation := AValue;
    LoadSkinItem;
  end;
end;

procedure TAdTrackControl.SetPosition(AValue: integer);
begin
  if (FPosition <> AValue) and Assigned(FOnChange) then
    FOnChange(self);

  FPosition := AValue;

  if FPosition < FMin then
    FPosition := FMin;
  if FPosition > FMax then
    FPosition := FMax;

  if FMax <> FMin then
  begin
    case FOrientation of
      orHorizontal: FTrackerPos :=
        round((Width - FTrackItem.BaseWidth) / (FMax - FMin) * (FPosition - FMin));
      orVertical: FTrackerPos :=
      round((Height - FTrackItem.BaseHeight) / (FMax - FMin) * (FPosition - FMin));
    end;
  end else
    FTrackerPos := 0;
end;

function TAdTrackControl.TrackerPos: TAdRect;
begin
  case FOrientation of
    orHorizontal: result := AdBounds(
      BoundsRect.Left+FTrackerPos,
      BoundsRect.Top+(Height-FTrackItem.BaseHeight) div 2,
      FTrackItem.BaseWidth,
      FTrackItem.BaseHeight);
      
    orVertical: result := AdBounds(
      BoundsRect.Left+(Width-FTrackItem.BaseWidth) div 2,
      BoundsRect.Top+FTrackerPos,
      FTrackItem.BaseWidth,
      FTrackItem.BaseHeight);
  end;
end;

procedure TAdTrackControl.LoadFromXML(aroot: TAdSimpleXMLElem);
begin
  inherited;
  with aroot.Properties do
  begin
    FMin := IntValue('min',0);
    FMax := IntValue('max',100);
    FSmooth := BoolValue('smooth',true);
    FPosition := IntValue('position',0);
    FOrientation := TAdOrientation(IntValue('orientation',0));

    LoadSkinItem;
    SetPosition(FPosition);
  end;
end;

function TAdTrackControl.SaveToXML(aroot: TAdSimpleXMLElems): TAdSimpleXMLElem;
begin
  result := inherited SaveToXML(aroot);

  with result.Properties do
  begin
    Add('min', FMin);
    Add('max', FMax);
    Add('position', FPosition);
    Add('orientation', Ord(FOrientation));
    Add('smooth', FSmooth);    
  end;
end;

{ TAdTrackBar }

procedure TAdTrackBar.LoadSkinItem;
begin
  case FOrientation of
    orHorizontal:
    begin
      FSkinItem := Skin.ItemNamed['trackbar_horz'];
      FTrackItem := Skin.ItemNamed['trackbar_horz_handle'];
      MinWidth := FTrackItem.BaseWidth*2;
      MinHeight := FTrackItem.BaseHeight;
    end;
    orVertical:
    begin
      FSkinItem := Skin.ItemNamed['trackbar_vert'];
      FTrackItem := Skin.ItemNamed['trackbar_vert_handle'];
      MinWidth := FTrackItem.BaseWidth;
      MinHeight := FTrackItem.BaseHeight*2;
    end;
  end;
end;

{ TAdScrollBar }

procedure TAdScrollBar.LoadSkinItem;
begin
  case FOrientation of
    orHorizontal:
    begin
      FSkinItem := Skin.ItemNamed['scrollbar_horz'];
      FTrackItem := Skin.ItemNamed['scrollbar_horz_handle'];
      MinWidth := FTrackItem.BaseWidth;
      MinHeight := FSkinItem.BaseHeight;
    end;
    orVertical:
    begin
      FSkinItem := Skin.ItemNamed['scrollbar_vert'];
      FTrackItem := Skin.ItemNamed['scrollbar_vert_handle'];
      MinWidth := FSkinItem.BaseWidth;
      MinHeight := FTrackItem.BaseHeight;
    end;
  end;
end;

{ TAdGridBox }

constructor TAdListBox.Create(AParent: TAdComponent);
begin
  inherited Create(AParent);

  AcceptChildComponents := false;

  FStrings := TStringList.Create;
  TStringList(FStrings).OnChange := StringsChange;
end;

procedure TAdListBox.CreateScrollBars;
begin
  if FHorzScrollbar = nil then
  begin
    AcceptChildComponents := true;

    FHorzScrollBar := TAdScrollbar.Create(self);
    FHorzScrollBar.SubComponent := true;
    FHorzScrollBar.Orientation := orHorizontal;
    FHorzScrollBar.Height := FHorzScrollbar.MinHeight;
    FHorzScrollBar.Visible := false;

    FVertScrollBar := TAdScrollbar.Create(self);
    FVertScrollBar.SubComponent := true;
    FVertScrollBar.Orientation := orVertical;
    FVertScrollBar.Width := FVertScrollbar.MinWidth;
    FVertScrollBar.Visible := false;

    AcceptChildComponents := false;
  end;
end;

destructor TAdListBox.Destroy;
begin
  FStrings.Free;

  if FDisplayList <> nil then
  begin
    FDisplayList.Free;
    FDisplayList := nil;
  end;

  inherited;
end;

procedure TAdListBox.DoDraw;
var
  r: TAdRect;
begin
  r := ClientRect;
  if FSkinItem <> nil then
  begin
    FSkinItem.Draw(0, r, Alpha);
  end;

  if FDisplayList <> nil then
  begin
    FDisplayList.Draw;
  end;

  inherited;
end;

function TAdListBox.DoResize: boolean;
var
  r: TAdRect;
  w, h: integer;
begin
  CreateScrollbars;
  
  result := inherited DoResize;

  if FVertScrollbar.Visible then  
    SpacerRight := FVertScrollbar.Width
  else
    SpacerRight := 0;

  if FHorzScrollbar.Visible then
    SpacerBottom := FHorzScrollbar.Height
  else
    SpacerBottom := 0;

  r := BoundsRect;
  w := r.Right - r.Left;
  h := r.Bottom - r.Top;

  FVertScrollbar.X := w - FVertScrollbar.Width;
  FVertScrollbar.Y := 0;
  FVertScrollbar.Height := h - SpacerBottom;
  FVertScrollbar.OnChange := StringsChange;

  FHorzScrollbar.X := 0;
  FHorzScrollbar.Y := h - FHorzscrollbar.Height;
  FHorzScrollbar.Width := w - SpacerRight;
  FHorzScrollbar.OnChange := StringsChange;

  UpdateView;
end;

procedure TAdListBox.LoadSkinItem;
begin
  FSkinItem := Skin.ItemNamed['listbox_frame'];

  MinWidth := 100;
  MinHeight := 100;
end;

procedure TAdListBox.StringsChange(Sender: TObject);
begin
  UpdateView;
  if FItemIndex > FStrings.Count-1 then
    FItemIndex := -1;
end;

procedure TAdListBox.UpdateView;
var
  i : integer;
  py : integer;
  ch : integer;
  r : TAdRect;
  overflow: boolean;
begin
  if FDisplayList <> nil then
  begin
    FDisplayList.Free;
    FDisplayList := nil;
  end;

  FVertScrollbar.Max := FStrings.Count;

  with AdDraw.Canvas do
  begin
    py := 0;
    r := GetClientRect;
    ch := r.Bottom - r.Top;
    overflow := false;

    for i := FVertScrollbar.Position to FStrings.Count - 1 do
    begin
      TextOut(0, 0 + py, FStrings[i]);

      py := py + Font.TextHeight(FStrings[i]);
      if py > ch then
      begin
        overflow := true;
        break;
      end;
    end;

    if overflow then
    begin
      if not FVertScrollbar.Visible then
      begin
        FVertScrollBar.Visible := true;
        DoResize;
       end;
    end else
    begin
      if (FVertScrollBar.Position = 0) and (FVertScrollbar.Visible) then
      begin
        FVertScrollBar.Visible := false;
        DoResize;
      end;
    end;


    FDisplayList := ReturnDisplayList;
  end;
end;

initialization
  RegisterComponent(TAdListBox,'Standard');
  RegisterComponent(TAdProgressBar,'Standard');
  RegisterComponent(TAdTrackBar,'Standard');
  RegisterComponent(TAdScrollBar,'Standard');
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
