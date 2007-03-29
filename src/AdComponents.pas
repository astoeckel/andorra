unit AdComponents;

interface

uses AdGUI, AdSkin, {$I AdTypes.inc};

type
  TAdPanel = class(TAdComponent)
    private
      FCaption:string;
      FSkinItem:TAdSkinItem;
      procedure SetCaption(const Value: string);
    protected
      procedure LoadSkinItem;override;
      procedure DoDraw;override;
    public
    published
      property Caption:string read FCaption write SetCaption;
  end;

implementation

{ TAdPanel }

procedure TAdPanel.DoDraw;
var rect:TRect;
begin
  inherited;
  rect := ClientRect;
  if FSkinItem <> nil then
  begin
    FSkinItem.Draw(0,rect.Left,rect.Top,round(Width),round(Height),Alpha);
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

initialization
  RegisterComponent(TAdPanel);

finalization

end.
