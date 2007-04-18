unit AdComponents;

interface

uses JvSimpleXML, AdGUI, AdSkin, AdClasses, AdDraws, {$I AdTypes.inc}, Controls,
     Classes;

type
  TAdAlignment = (alLeft,alCenter,alRight);
  TAdTextPos = (tpTop,tpCenter,tpBottom);

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

  TAdButtonState = (bsNormal, bsDown, bsHover, bsFocus);

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
      constructor Create(AParent:TAdComponent);
      destructor Destroy;override;
      procedure LoadFromXML(aroot:TJvSimpleXMLElem);override;
      function SaveToXML(aroot:TJvSimpleXMLElems):TJvSimpleXMLElem;override;
    published
      property Caption:string read FCaption write FCaption;
      property State:TAdButtonState read FState;
  end;

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

initialization
  RegisterComponent(TAdPanel,'Standard');
  RegisterComponent(TAdButton,'Standard');

finalization

end.
