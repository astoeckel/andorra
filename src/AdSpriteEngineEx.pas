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
  TAdCamera = class
    private
      FSpriteEngine: TSpriteEngine;
    public
      constructor Create(ASpriteEngine: TSpriteEngine);
      destructor Destroy; override;

      procedure Update;virtual;abstract;
      function IsVisible(AObj: TAdRect): boolean;virtual;abstract;
      function TranslatePoint(APnt: TAdPoint): TAdPoint;virtual;abstract;
      function MinMaxRect: TAdRect;virtual;abstract;

      property SpriteEngine: TSpriteEngine read FSpriteEngine;
  end;

  TAdSimpleCamera = class(TAdCamera)
    private
      FX, FY: double;
    public
      procedure Update;override;
      function IsVisible(AObj: TAdRect): boolean; override;
      function TranslatePoint(APnt: TAdPoint): TAdPoint; override;
      function MinMaxRect: TAdRect;override;

      property X: double read FX write FX;
      property Y: double read FY write FY;
    end;  

  {An extended spriteengine that is able to zoom in and to rotate the scene. Absolute
   screen coordinates can be transformed into spriteengine coordinates using the
   "ScreenPointToSpriteCoords" function.}
  TSpriteEngineEx = class(TSpriteEngine)
    private
      FCamera: TAdCamera;
      FOwnCamera: boolean;
      procedure SetCamera(AValue: TAdCamera);
    public
      constructor Create(AParent:TAdDraw);
      destructor Destroy;override;
      {Draws the scene.}
      procedure Draw;override;

      property Camera: TAdCamera read FCamera write SetCamera;
  end;

implementation

{ TSpriteEngineEx }

constructor TSpriteEngineEx.Create(AParent: TAdDraw);
begin
  inherited;

  FCamera := nil;
end;

destructor TSpriteEngineEx.Destroy;
begin
  inherited;
end;

procedure TSpriteEngineEx.Draw;
var
  oldvp: TAdRect;
  projmat: TAdMatrix;
  viewmat: TAdMatrix;
  w, h, i: integer;
begin
  if Visible then
  begin
    if (VisibilityTest) and (FCamera <> nil) then
    begin
      //Set the surface rectangle to the min max rectangle of the camera. The
      //surface rectangle will not be used for visibility tests. Furthermore, this
      //rectangle is only set for downward compatibility. Sprites may use the surface
      //rect to get the boundaries of the screen.
      FSurfaceRect := FCamera.MinMaxRect; 

      //Save the view and the projection matrix
      projmat := Surface.Scene.ProjectionMatrix;
      viewmat := Surface.Scene.ViewMatrix;

      //Let the camera set the view and the projection matrix
      FCamera.Update;

      //Draw all sprites that pass the camera visibility test
      for i := 0 to Items.Count - 1 do
        if FCamera.IsVisible(Items[i].BoundsRect) then
          Items[i].Draw;

      //Reset the matrices to the saved values
      Surface.Scene.ViewMatrix := viewmat;
      Surface.Scene.ProjectionMatrix := projmat;
    end else
      inherited;
  end;
end;

procedure TSpriteEngineEx.SetCamera(AValue: TAdCamera);
begin
  FCamera := AValue;
end;

{ TAdCamera }

constructor TAdCamera.Create(ASpriteEngine: TSpriteEngine);
begin
  inherited Create;
  FSpriteEngine := ASpriteEngine;
end;

destructor TAdCamera.Destroy;
begin
  inherited;
end;

{ TAdSimpleCamera }

function TAdSimpleCamera.IsVisible(AObj: TAdRect): boolean;
begin
  result := OverlapRect(FSpriteEngine.Surface.DisplayRect, AObj);
end;

function TAdSimpleCamera.MinMaxRect: TAdRect;
begin
  //Return the rectangle describing the viewport of the surface
  result := FSpriteEngine.Surface.Scene.Viewport;
end;

function TAdSimpleCamera.TranslatePoint(APnt: TAdPoint): TAdPoint;
begin
  result := APnt;
end;

procedure TAdSimpleCamera.Update;
begin
  //Setup a simple 2D scene
  with FSpriteEngine.Surface.Scene do
    Setup2DScene(
      Viewport.Right - Viewport.Left,
      Viewport.Bottom - Viewport.Top);
    
  FSpriteEngine.X := FX;
  FSpriteEngine.Y := FY;
end;

end.
