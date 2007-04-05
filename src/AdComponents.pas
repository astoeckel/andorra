unit AdComponents;

interface

uses AdGUI, AdSkin, AdClasses, AdDraws, {$I AdTypes.inc};

type
  TAdAlignment = (alLeft,alCenter,alRight);
  TAdTextPos = (tpTop,tpCenter,tpBottom);

  TAdPanel = class(TAdComponent)
    private
      FCaption:string;
      FSkinItem:TAdSkinItem;
      FFont:TAdFont;
      FAlignment:TAdAlignment;
      FTextPos:TAdTextPos;
      procedure SetFont(AFont:TAdFont);
      procedure SetCaption(const Value: string);
    protected
      procedure LoadSkinItem;override;
      procedure DoDraw;override;
    public
      property Font:TAdFont read FFont write SetFont;
    published
      property Caption:string read FCaption write SetCaption;
      property TextPos:TAdTextPos read FTextPos write FTextPos;
      property Alignment:TAdAlignment read FAlignment write FAlignment;
  end;

implementation

{ TAdPanel }

procedure TAdPanel.DoDraw;
var rect:TRect;
    x,y:integer;
    afont:TAdFont;
begin
  inherited;
  rect := ClientRect;
  if FSkinItem <> nil then
  begin
    FSkinItem.Draw(0,rect.Left,rect.Top,round(Width),round(Height),Alpha);

    if FFont = nil then
    begin
      afont := AdDraw.Canvas.Font;
    end
    else
    begin
      afont := FFont;
    end;

    if FCaption <> '' then
    begin
      case FAlignment of
        alLeft: x := rect.Left + SpacerLeft;
        alCenter: x := rect.Left + ((rect.Right - rect.Left) - afont.TextWidth(FCaption)) div 2;
        alRight: x := rect.Right - SpacerRight - afont.TextWidth(FCaption);
      end;
      case FTextPos of
        tpTop: y := rect.Top + SpacerTop;
        tpCenter: y := rect.Top + ((rect.Bottom - rect.Top) - afont.TextHeight(FCaption)) div 2;
        tpBottom: y := rect.Right - SpacerBottom - afont.TextHeight(FCaption);
      end;

      FFont.TextOut(x,y,FCaption);
    end;
  end;
end;

procedure TAdPanel.LoadSkinItem;
begin
  FSkinItem := Skin.ItemNamed['panel'];
end;

procedure TAdPanel.SetCaption(const Value: string);
begin
  FCaption := Value;
end;

procedure TAdPanel.SetFont(AFont: TAdFont);
begin
  FFont := AFont;
end;

initialization
  RegisterComponent(TAdPanel);

finalization

end.
