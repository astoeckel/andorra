{
* This program is licensed under the Common Public License (CPL) Version 1.0
* You should have recieved a copy of the license with this file.
* If not, see http://www.opensource.org/licenses/cpl1.0.txt for more informations.
* 
* Inspite of the incompatibility between the Common Public License (CPL) and the GNU General Public License (GPL) you're allowed to use this program 
* under the GPL. 
* You also should have recieved a copy of this license with this file. 
* If not, see http://www.gnu.org/licenses/gpl.txt for more informations.
*
* Project: Andorra 2D
* Author:  Andreas Stoeckel
* File: AdSpriteEngineEx.pas
* Comment: Contains an extended spriteengine that is capable of zooming in and rotating the scene.
}

{Contains an extended spriteengine that is capable of zooming in and rotating the scene.}
unit AdSpriteEngineEx;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Math, AdTypes, SysUtils, AdDraws, AdClasses, AdSprites;

type
  {An extended spriteengine that is able to zoom in and to rotate the scene. Absolute
   screen coordinates can be transformed into spriteengine coordinates using the
   "ScreenPointToSpriteCoords" function.}
  TSpriteEngineEx = class(TSpriteEngine)
    private
      FZoom:single;
      FZDistanze:double;
      FRotation:single;
      FViewPort:TAdRect;
      FChanged:boolean;
      FOurSurfaceRect: TAdRect;
      procedure SetZoom(AValue:single);
      procedure SetRotation(AValue:single);
      procedure SetViewPort(AValue:TAdRect);
      procedure CalcSurfaceRect;
      procedure CalcZDistance;
      function Base:TAdRect;
      procedure RotatePoint(cx, cy:integer; var p:TAdPoint);
    protected
      procedure SetSurface(AValue:TAdRenderingSurface);override;
    public    
      {Creates an instance of TSpriteEngineEx}
      constructor Create(AParent:TAdDraw);

      {Draws the scene}
      procedure Draw;override;

      {Transforms screen coordinates to sprite coordinates.}
      function ScreenPointToSpriteCoords(p:TAdPoint):TAdPoint;

      {Set this property to limit the viewport of the current scene.}
      property ViewPort:TAdRect read FViewPort write SetViewPort;
      {Use the rotation property to rotate the scene. The rotation must be 
       given in radian measure.}
      property Rotation:single read FRotation write SetRotation;
      {The zoom value. A value of e.g. two means that all things are twice
       as big is in original size.}
      property Zoom:single read FZoom write SetZoom;
  end;

implementation

{ TSpriteEngineEx }

constructor TSpriteEngineEx.Create(AParent: TAdDraw);
begin
  inherited;

  FRotation := 0;
  FViewPort := AdRect(0,0,0,0);
  Zoom := 1;
end;

procedure TSpriteEngineEx.Draw;
var
  oldvp:TAdRect;
  projmat:TAdMatrix;
  viewmat:TAdMatrix;
  w,h:integer;
begin
  CalcSurfaceRect;

  oldvp := Surface.Scene.Viewport;
  projmat := Surface.Scene.ProjectionMatrix;
  viewmat := Surface.Scene.ViewMatrix;

  w := Base.Right - Base.Left;
  h := Base.Bottom - Base.Top;
  Surface.Scene.Setup3DScene(
    w, h,
    AdVector3(w/2,h/2,-FZDistanze),
    AdVector3(w/2,h/2,0),
    AdVector3(sin(FRotation),sin(FRotation-pi/2),0));

  inherited;

  Surface.Scene.Viewport := oldvp;
  Surface.Scene.ViewMatrix := viewmat;
  Surface.Scene.ProjectionMatrix := projmat;      
end;

function TSpriteEngineEx.Base: TAdRect;
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

procedure TSpriteEngineEx.RotatePoint(cx, cy:integer; var p: TAdPoint);
var
  alpha:double;
  distance:double;
begin
  if FRotation <> 0 then
  begin
    p.X := p.X - cx;
    p.Y := p.Y - cy;
    
    //Calculate point polar coordinates
    distance := sqrt(sqr(p.X) + sqr(p.Y));
    alpha := arctan(p.Y/p.X);
    if p.X < 0 then
      alpha := alpha + PI;

    //Rotate point
    alpha := alpha + rotation;
    p.x := round(cos(alpha)*distance);
    p.y := round(sin(alpha)*distance);

    p.X := p.X + cx;
    p.Y := p.Y + cy;
  end;
end;

procedure TSpriteEngineEx.CalcSurfaceRect;
var
  w,h:double;
  ps:array[0..3] of TAdPoint;
  i:integer;
  maxw, minw, maxh, minh, cx, cy:integer;
begin
  if FChanged then
  begin
    w := (Base.Right - Base.Left)*Zoom;
    h := (Base.Bottom - Base.Top)*Zoom;

    if FRotation <> 0 then
    begin
      ps[0].X := 0; ps[0].Y := 0;
      ps[1].X := 0; ps[1].Y := round(h);
      ps[2].X := round(w); ps[2].Y := 0;
      ps[3].X := round(w); ps[3].Y := round(h);

      cx := round(w / 2);
      cy := round(h / 2);

      for i := 0 to 3 do RotatePoint(cx, cy, ps[i]);

      maxw := ps[0].x; minw := ps[0].x; maxh := ps[0].y; minh := ps[0].y;
      for i := 1 to 3 do
      begin
        if ps[i].x > maxw then maxw := ps[i].x;
        if ps[i].x < minw then minw := ps[i].x;
        if ps[i].y > maxh then maxh := ps[i].y;
        if ps[i].y < minh then minh := ps[i].y;
      end;

      w := maxw - minw;
      h := maxh - minh;
    end;

    with FSurfaceRect do
    begin
      Left   := round(- w / 2 + (Base.Right - Base.Left) / 2);
      Right  := round(  w / 2 + (Base.Right - Base.Left) / 2);
      Top    := round(- h / 2 + (Base.Bottom - Base.Top) / 2);
      Bottom := round(  h / 2 + (Base.Bottom - Base.Top) / 2);
    end;
    FOurSurfaceRect := FSurfaceRect;

    FChanged := false;
  end else
    FSurfaceRect := FOurSurfaceRect;
end;

function TSpriteEngineEx.ScreenPointToSpriteCoords(p: TAdPoint): TAdPoint;
var
  w, h, left, top:double;
begin
  if Rotation <> 0 then
  begin
    w := (Base.Right - Base.Left);
    h := (Base.Bottom - Base.Top);

    RotatePoint(round(w/2), round(h/2), p);

    left := round(- (w*Zoom) / 2 + (Base.Right - Base.Left) / 2);
    top := round(- (h*Zoom) / 2 + (Base.Bottom - Base.Top) / 2);
    with result do
    begin
      X := round((p.X - Base.Left) * FZoom + Base.Left + left);
      Y := round((p.Y - Base.Top) * FZoom + Base.Top + top);
    end;
  end
  else
  begin
    with result do
    begin
      X := round((p.X - Base.Left) * FZoom + Base.Left) + SurfaceRect.Left;
      Y := round((p.Y - Base.Top) * FZoom + Base.Top) + SurfaceRect.Top;
    end;
  end;
end;

procedure TSpriteEngineEx.CalcZDistance;
begin
  FZDistanze := round(1.20*(Base.Bottom - Base.Top)*FZoom);
  FChanged := true;
end;

procedure TSpriteEngineEx.SetRotation(AValue: single);
begin
  FRotation := AValue;
  FChanged := true;
end;

procedure TSpriteEngineEx.SetSurface(AValue: TAdRenderingSurface);
begin
  inherited;
  if AValue <> nil then
  begin
    CalcZDistance;
  end;
end;

procedure TSpriteEngineEx.SetViewPort(AValue: TAdRect);
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
    FZoom := 0.1;

  if Surface <> nil then
    CalcZDistance;
end;

end.
