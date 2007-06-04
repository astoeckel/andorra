unit AdSpriteEngineEx;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses {$I AdTypes.inc}, SysUtils, AdDraws, AdClasses, AdSprites;

type
  TSpriteEngineEx = class(TSpriteEngine)
    private
      FZoom:single;
      FZDistanze:double;
      FRotation:single;
      FViewPort:TRect;
      procedure SetZoom(AValue:single);
      procedure SetRotation(AValue:single);
      procedure SetViewPort(AValue:TRect);
      procedure CalcSurfaceRect;
      procedure CalcZDistance;
      function Base:TRect;
    protected
      procedure Notify(Sender:TObject;AEvent:TSurfaceEventState);override;
      procedure SetSurface(AValue:TAdDraw);override;
    public
      procedure Draw;

      constructor Create(AParent:TAdDraw);

      function ScreenPointToSpriteCoords(p:TPoint):TPoint;

      property ViewPort:TRect read FViewPort write SetViewPort;
//      property Rotation:single read FRotation write SetRotation;
      property Zoom:single read FZoom write SetZoom;
  end;

implementation

{ TSpriteEngineEx }

constructor TSpriteEngineEx.Create(AParent: TAdDraw);
begin
  inherited;

  FRotation := 0;
  FViewPort := Rect(0,0,0,0);
  FZoom := 1;
end;

procedure TSpriteEngineEx.Draw;
var
  oldvp:TRect;
  projmat:TAdMatrix;
  viewmat:TAdMatrix;
  w,h:integer;
begin
  CalcSurfaceRect;

  oldvp := Surface.AdAppl.Viewport;
  Surface.AdAppl.GetScene(viewmat, projmat);
  Surface.AdAppl.Viewport := Base;

  w := Base.Right - Base.Left;
  h := Base.Bottom - Base.Top;
  Surface.AdAppl.Setup3DScene(w,h,AdVector3(w/2,h/2,-FZDistanze),AdVector3(w/2,h/2,0),AdVector3(sin(FRotation),sin(FRotation-pi/2),0));

  inherited;

  Surface.AdAppl.SetupManualScene(viewmat, projmat);
  Surface.AdAppl.Viewport := oldvp;
end;

function TSpriteEngineEx.Base: TRect;
begin
  if (Viewport.Left = 0) and (ViewPort.Top = 0) and
     (Viewport.Right = 0) and (ViewPort.Bottom = 0) then
  begin
    result := Surface.DisplayRect;
  end
  else
  begin
    result := FViewPort;
  end;
end;

procedure TSpriteEngineEx.CalcSurfaceRect;
var
  w,h:double;

begin

  w := (Base.Right - Base.Left)*Zoom;
  h := (Base.Bottom - Base.Top)*Zoom;


  with FSurfaceRect do
  begin
    Left   := round(- w / 2 + (Base.Right - Base.Left) / 2);
    Right  := round(  w / 2 + (Base.Right - Base.Left) / 2);
    Top    := round(- h / 2 + (Base.Bottom - Base.Top) / 2);
    Bottom := round(  h / 2 + (Base.Bottom - Base.Top) / 2);
  end;

end;

function TSpriteEngineEx.ScreenPointToSpriteCoords(p: TPoint): TPoint;
begin
  result.X := round((p.X - Base.Left) * FZoom + Base.Left) + SurfaceRect.Left;
  result.Y := round((p.Y - Base.Top) * FZoom + Base.Top) + SurfaceRect.Top;
end;

procedure TSpriteEngineEx.CalcZDistance;
begin
  FZDistanze := round(1.20*(Base.Bottom - Base.Top)*FZoom);
end;

procedure TSpriteEngineEx.SetRotation(AValue: single);
begin
  FRotation := AValue;
  if Surface <> nil then CalcSurfaceRect;
end;

procedure TSpriteEngineEx.SetSurface(AValue: TAdDraw);
begin
  inherited;
  if AValue <> nil then
  begin
    CalcZDistance;
  end;
end;

procedure TSpriteEngineEx.Notify(Sender: TObject; AEvent: TSurfaceEventState);
begin
  inherited;
  CalcZDistance;
end;

procedure TSpriteEngineEx.SetViewPort(AValue: TRect);
begin
  FViewPort := AValue;
  if Surface <> nil then
  begin
    CalcZDistance;
  end;
end;

procedure TSpriteEngineEx.SetZoom(AValue: single);
begin
  FZoom := AValue;
  if FZoom < 0.1 then
  begin
    FZoom := 0.1;
  end;
  if Surface <> nil then
  begin
    CalcZDistance;
  end;
end;

end.
