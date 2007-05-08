unit AdComponents;

interface

uses JvSimpleXML, AdGUI, AdXML, AdSkin, AdClasses, AdDraws, {$I AdTypes.inc}, Controls,
     Classes, Graphics;

type
  TAdAlignment = (alLeft,alCenter,alRight);
  TAdTextPos = (tpTop,tpCenter,tpBottom);
  TAdAlignmentEx = (axLeft, axRight, axTop, axBottom);
  
  TAdForm = class(TAdComponent)
    private
      FCaption:string;
      FSkinItem:TAdSkinItem;
      FFixedPosition:boolean;
    protected
      procedure LoadSkinItem;override;
      procedure DoDraw;override;
    public
      procedure LoadFromXML(aroot:TJvSimpleXMLElem);override;
      function SaveToXML(aroot:TJvSimpleXMLElems):TJvSimpleXMLElem;override;
    published
      property Caption:string read FCaption write FCaption;
      property FixedPosition:boolean read FFixedPosition write FFixedPosition;
  end;

  TAdPanel = class(TAdComponent)
    private
      FCaption:string;
      FSkinItem:TAdSkinItem;
      FAlignment:TAdAlignment;
      FTextPos:TAdTextPos;
    protected
      procedure LoadSkinItem;override;
      procedure DoDraw;override;
    public
      procedure LoadFromXML(aroot:TJvSimpleXMLElem);override;
      function SaveToXML(aroot:TJvSimpleXMLElems):TJvSimpleXMLElem;override;
    published
      property Caption:string read FCaption write FCaption;
      property TextPos:TAdTextPos read FTextPos write FTextPos;
      property Alignment:TAdAlignment read FAlignment write FAlignment;
  end;

  TAdButtonState = (bsNormal, bsDown, bsHover, bsFocus, bsDisabled);

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
      procedure DoMouseEnter;override;
      procedure DoMouseLeave;override;
      procedure DoMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);override;
      procedure DoMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);override;
    public
      constructor Create(AParent:TAdComponent);override;
      destructor Destroy;override;
      procedure LoadFromXML(aroot:TJvSimpleXMLElem);override;
      function SaveToXML(aroot:TJvSimpleXMLElems):TJvSimpleXMLElem;override;
      property State:TAdButtonState read FState;
    published
      property Caption:string read FCaption write FCaption;
  end;

  //Written by Michael Morstein alias Neutral General
  TAdCheckBox = class(TAdComponent)
    private
      FState: TAdButtonState;
      FCaption: String;
      FSkinItem: TAdSkinItem;
      FCheckedItem: TAdSkinItem;
      FChecked: Boolean;
      FAlignment: TAdAlignmentEx;
    protected
      procedure LoadSkinItem; override;
      procedure DoDraw; override;
      procedure DoMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
      procedure DoMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
      procedure DoMouseEnter; override;
      procedure DoMouseLeave; override;
    public
      constructor Create(AParent:TAdComponent);override;
      procedure LoadFromXML(aroot:TJvSimpleXMLElem); override;
      function SaveToXML(aroot:TJvSimpleXMLElems): TJvSimpleXMLElem;override;
    published
      property Checked:Boolean read FChecked write FChecked;
      property Caption:String read FCaption write FCaption;
      property Alignment:TAdAlignmentEx read FAlignment write FAlignment;
  end;

  TAdResourceImage = class
    private
      FImage:TAdImage;
      FParent:TAdDraw;
      FTransparent:boolean;
      FTransparentColor:TColor;
      FCompressor:TCompressorClass;
      procedure SetTransparent(AValue:boolean);
      procedure SetTransparentColor(AValue:TColor);
      procedure SetCompressor(ACompressor:TCompressorClass);
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
      property Compressor:TCompressorClass read FCompressor write SetCompressor;
      property Loaded:boolean read GetLoaded;
  end;

  TAdBitmapButton = class(TAdComponent)
    private
      FImgHover:TAdResourceImage;
      FImgDown:TAdResourceImage;
      FImgDisabled:TAdResourceImage;
      FImgNormal:TAdResourceImage;
      FState:TAdButtonState;
      FDown:boolean;
      FCheckButton:boolean;
      procedure SetDown(AValue:boolean);
    protected
      procedure DoDraw; override;
      procedure DoMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
      procedure DoMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
      procedure DoMouseEnter; override;
      procedure DoMouseLeave; override;
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
      property Down:boolean read FDown write SetDown;
      property CheckButton:boolean read FCheckButton write FCheckButton;
  end;

  const               
    SPACING = 5;

implementation

{ TAdPanel }

procedure TAdPanel.DoDraw;
var rect:TRect;
    ax,ay:integer;
begin
  if FSkinItem <> nil then
  begin
    rect := BoundsRect;
    FSkinItem.Draw(0,rect.Left,rect.Top,round(Width),round(Height),Alpha);

    rect := ClientRect;

    ax := 0;
    ay := 0;

    if FCaption <> '' then
    begin
      case FAlignment of
        alLeft: ax := rect.Left;
        alCenter: ax := rect.Left + ((rect.Right - rect.Left) - Font.TextWidth(FCaption)) div 2;
        alRight: ax := rect.Right - Font.TextWidth(FCaption);
      end;
      case FTextPos of
        tpTop: ay := rect.Top;
        tpCenter: ay := rect.Top + ((rect.Bottom - rect.Top) - Font.TextHeight(FCaption)) div 2;
        tpBottom: ay := rect.Bottom - Font.TextHeight(FCaption);
      end;

      Font.Alpha := Alpha;
      Font.TextOut(ax,ay,FCaption);
    end;
  end;

  inherited DoDraw;
end;

procedure TAdPanel.LoadSkinItem;
begin
  FSkinItem := Skin.ItemNamed['panel'];
  SetSpacer(FSkinItem);
end;

procedure TAdPanel.LoadFromXML(aroot: TJvSimpleXMLElem);
begin
  inherited;
  with aroot.Properties do
  begin
    FTextPos := TAdTextPos(IntValue('textpos',ord(tpCenter)));
    FAlignment := TAdAlignment(IntValue('alignment',ord(alCenter)));
    FCaption := Value('caption','');
  end;
end;

function TAdPanel.SaveToXML(aroot: TJvSimpleXMLElems): TJvSimpleXMLElem;
begin
  result := inherited SaveToXML(aroot);
  with result.Properties do
  begin
    Add('textpos',ord(FTextPos));
    Add('alignment',ord(FAlignment));
    Add('caption',FCaption);
  end;
end;

{ TAdButton }

constructor TAdButton.Create(AParent: TAdComponent);
begin
  inherited;
  FStateNr := 0;
  FState := bsNormal;
  AcceptChildComponents := false;
end;

destructor TAdButton.Destroy;
begin
  inherited;
end;

procedure TAdButton.DoDraw;
var rect:TRect;
    x,y:integer;
begin
  if FSkinItem <> nil then
  begin
    rect := BoundsRect;
    FSkinItem.Draw(FStateNr,rect.Left,rect.Top,round(Width),round(Height),Alpha);

    rect := ClientRect;
    x := rect.Left + ((rect.Right - rect.Left) - Font.TextWidth(FCaption)) div 2;
    y := rect.Top + ((rect.Bottom - rect.Top) - Font.TextHeight(FCaption)) div 2;
    Font.Alpha := Alpha;
    Font.TextOut(x,y,FCaption);
  end;
  inherited DoDraw;
end;

procedure TAdButton.DoMouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  FState := bsDown;
  GetStateNr;
end;

procedure TAdButton.DoMouseEnter;
begin
  inherited;
  FState := bsHover;
  GetStateNr;
end;

procedure TAdButton.DoMouseLeave;
begin
  inherited;
  if Focused then
  begin
    FState := bsFocus;
  end
  else
  begin
    FState := bsNormal;
  end;
  GetStateNr;
end;

procedure TAdButton.DoMouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
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

procedure TAdForm.DoDraw;
var
  rect:TRect;
begin
  rect := BoundsRect;
  FSkinItem.Draw(0,rect.Left,rect.Top,round(Width),round(Height),Alpha);
  Font.TextOut(rect.Left+SpacerLeft,rect.Top+(SpacerTop - Font.TextHeight(FCaption)) div 2,FCaption);
  inherited DoDraw;
end;

procedure TAdForm.LoadFromXML(aroot: TJvSimpleXMLElem);
begin
  inherited;
  with aroot.Properties do
  begin
    FCaption := Value('caption','');
  end;
end;

function TAdForm.SaveToXML(aroot: TJvSimpleXMLElems): TJvSimpleXMLElem;
begin
  result := inherited SaveToXML(aroot);
  with result.Properties do
  begin
    Add('caption',FCaption);
  end;
end;

procedure TAdForm.LoadSkinItem;
begin
  FSkinItem := Skin.ItemNamed['form'];
  SetSpacer(FSkinItem);
end;

{ TAdCheckBox }

constructor TAdCheckBox.Create(AParent: TAdComponent);
begin
  inherited;
  AcceptChildComponents := false;
end;

procedure TAdCheckBox.DoDraw;
var Rect: TRect;
begin
  if (FSkinItem <> nil) and (FCheckedItem <> nil) then
 begin
    Rect := BoundsRect;
    Font.Alpha := Alpha;
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

procedure TAdCheckBox.DoMouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  FState := bsDown;
end;

procedure TAdCheckBox.DoMouseEnter;
begin
  inherited;
  FState := bsHover;
end;

procedure TAdCheckBox.DoMouseLeave;
begin
  inherited;
  FState := bsNormal;
end;

procedure TAdCheckBox.DoMouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  FChecked := not FChecked;
  FState := bsHover;
end;

procedure TAdCheckBox.LoadSkinItem;
begin
  FSkinItem := Skin.ItemNamed['checkbox'];
  SetSpacer(FSkinItem);
  FCheckedItem := Skin.ItemNamed['checkboxhook'];
  SetSpacer(FCheckedItem);
end;

procedure TAdCheckBox.LoadFromXML(ARoot: TJvSimpleXMLElem);
begin
  inherited;
  with ARoot.Properties do
  begin
    FCaption := Value('caption','');
    FChecked := BoolValue('checked',false);
    FAlignment := TAdAlignmentEx(IntValue('align',Ord(axLeft)));
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

procedure TAdResourceImage.SetCompressor(ACompressor: TCompressorClass);
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
    adbmp.AssignToBitmap(bmp);
    adbmp.Free;
    bmp.Transparent := FTransparent;
    bmp.TransparentMode := tmFixed;
    bmp.TransparentColor := FTransparentColor;
    adbmp := TAdBitmap.Create;
    adbmp.AssignBitmap(bmp);
    FImage.Texture.Texture.LoadFromBitmap(adbmp,32);
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
  AcceptChildComponents := false;
end;

destructor TAdBitmapButton.Destroy;
begin
  FImgHover.Free;
  FImgDown.Free;
  FImgNormal.Free;
  FImgDisabled.Free;
  inherited;
end;

procedure TAdBitmapButton.DoDraw;
begin
  if DesignMode then
  begin
    with AdDraw.Canvas do
    begin
      Brush.Style := abClear;
      Pen.Color := Ad_RGB(0,0,0);
      Rectangle(BoundsRect);
    end;
  end;
  
  if (Enabled) or (not FImgDisabled.Loaded) then  
  begin
    if (State = bsNormal) and (FImgNormal.Loaded) then
    begin
      FImgNormal.Picture.StretchDraw(AdDraw,Boundsrect,0);
    end;
    if (State = bsDown) and (FImgDown.Loaded) then
    begin
      FImgDown.Picture.StretchDraw(AdDraw,Boundsrect,0);
    end;
    if (State = bsHover) and (FImgHover.Loaded) then
    begin
      FImgHover.Picture.StretchDraw(AdDraw,Boundsrect,0);
    end;
  end
  else
  begin
    FImgDisabled.Picture.StretchDraw(AdDraw,Boundsrect,0);
  end;

  inherited;
end;

procedure TAdBitmapButton.DoMouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if FImgDown.Loaded then
  begin
    FState := bsDown;
    if FCheckButton then
    begin
      FDown := not FDown;
    end;
  end;
end;

procedure TAdBitmapButton.DoMouseEnter;
begin
  inherited;
  if FImgHover.Loaded and ((not FCheckButton) or (not FDown)) then
  begin
    FState := bsHover;
  end;
end;

procedure TAdBitmapButton.DoMouseLeave;
begin
  inherited;
  if FImgNormal.Loaded and ((not FCheckButton) or (not FDown)) then
  begin
    FState := bsNormal;
  end;
end;

procedure TAdBitmapButton.DoMouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  if FImgNormal.Loaded then
  begin
    if (not FCheckButton) or (not FDown) then
    begin
      FState := bsHover;
    end;
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
    FCheckButton := BoolValue('checkbutton',false);
    FDown := BoolValue('down',false);
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
    Add('checkbutton',FCheckButton);
    Add('down',FDown);
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
end;

initialization
  RegisterComponent(TAdForm,'Standard');
  RegisterComponent(TAdPanel,'Standard');
  RegisterComponent(TAdButton,'Standard');
  RegisterComponent(TAdCheckbox,'Standard');
  RegisterComponent(TAdBitmapButton, 'Standard');

finalization

end.
