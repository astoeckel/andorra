{
* This program is licensed under the Common Public License (CPL) Version 1.0
* You should have recieved a copy of the license with this file.
* If not, see http://www.opensource.org/licenses/cpl1.0.txt for more informations.
* 
* Inspite of the incompatibility between the Common Public License (CPL) and the GNU General Public License (GPL) you're allowed to use this program * under the GPL. 
* You also should have recieved a copy of this license with this file. 
* If not, see http://www.gnu.org/licenses/gpl.txt for more informations.

* Project: Andorra 2D
* Authors:  Andreas Stoeckel
* File: AdPixelTest.pas
* Comment: Contains a class which allows simple and hardware accelerated collision detection.
}

{Contains a class which allows simple and hardware accelerated collision
 detection.}
unit AdPixelTest;

interface

uses
  AdClasses, AdTypes, AdDraws;

type
  TAdCollisionTestDrawEvent = procedure(ASurface: TAdSurface; AX, AY: integer) of object;

  TAdPixelCollisionTester = class
    private
      FSurface: TAdTextureSurface;
      FDraw: TAdDraw;
      FPixelCounter: TAd2dPixelCounter;

      procedure Notify(ASender: TObject; AEvent: TAdSurfaceEventState);
      function CalcOverlapRect(out AO: TAdRect; const AR1, AR2: TAdRect): boolean;
    protected
      procedure Initialize;
      procedure Finalize;
    public
      constructor Create(AParent: TAdDraw);
      destructor Destroy;override;

      property AdDraw: TAdDraw read FDraw;

      function CheckCollision(ABoundsRect1: TAdRect;
        ACallback1: TAdCollisionTestDrawEvent; ABoundsRect2: TAdRect;
        ACallback2: TAdCollisionTestDrawEvent): boolean;

      property Surface: TAdTextureSurface read FSurface;
  end;

implementation

{ TAdPixelCollisionTester }

constructor TAdPixelCollisionTester.Create(AParent: TAdDraw);
begin
  inherited Create;

  FDraw := AParent;
  FDraw.RegisterNotifyEvent(Notify);

  FSurface := TAdTextureSurface.Create(FDraw);
  FSurface.Options := [aoStencilBuffer, aoAlphaMask, aoZBuffer, aoTextures];
  FSurface.SetSize(32, 32);

  if FDraw.Initialized then  
    Initialize;
end;

destructor TAdPixelCollisionTester.Destroy;
begin
  Finalize;
  FSurface.Free;
  
  inherited;
end;

function TAdPixelCollisionTester.CheckCollision(ABoundsRect1: TAdRect;
  ACallback1: TAdCollisionTestDrawEvent; ABoundsRect2: TAdRect;
  ACallback2: TAdCollisionTestDrawEvent): boolean;
var
  AOverlapRect: TAdRect;
  w, h: integer;
begin
  result := false;

  if CalcOverlapRect(AOverlapRect, ABoundsRect1, ABoundsRect2) then
  begin
    //Clear surface
    FSurface.ClearSurface(0);

    //Set the size of the scene to the width of the overlap rect
    w := AOverlapRect.Right - AOverlapRect.Left;
    h := AOverlapRect.Bottom - AOverlapRect.Top;

    FSurface.Scene.Setup2DScene(w, h);
    if (w <= FSurface.Width) and (h <= FSurface.Height) then    
      FSurface.Scene.Viewport := AdRect(0, 0, w, h);

    //Setup stencil options. When a pixel is drawn, increment the stencil buffer
    FDraw.AdAppl.SetStencilOptions(0, $FFFF, asfAlways);
    FDraw.AdAppl.SetStencilEvent(asePass, asoIncrement);

    //Draw the first object
    ACallback1(FSurface,
      ABoundsRect1.Left - AOverlapRect.Left,
      ABoundsRect1.Top - AOverlapRect.Top);

    //Now, only draw a pixel, if the stencil buffer value is one, what means that
    //there is already another pixel
    FDraw.AdAppl.SetStencilOptions(1, $FFFF, asfEqual);
    FDraw.AdAppl.SetStencilEvent(asePass, asoKeep);

    //Start counting drawn pixels
    FPixelCounter.StartCount;

    //Draw the second object
    ACallback2(FSurface,
      ABoundsRect2.Left - AOverlapRect.Left,
      ABoundsRect2.Top - AOverlapRect.Top);

    //Stop counting - If at least one pixel had been drawn, there is a collision
    result := FPixelCounter.StopCount <> 0;

    //Reset stencil options
    FDraw.AdAppl.SetStencilOptions(0, $FFFF, asfAlways);
  end;
end;

procedure TAdPixelCollisionTester.Finalize;
begin
  if FPixelCounter <> nil then
  begin
    FPixelCounter.Free;
    FPixelCounter := nil;
  end;
end;

procedure TAdPixelCollisionTester.Initialize;
begin
  Finalize;

  FPixelCounter := FDraw.AdAppl.CreatePixelCounter;
end;

procedure TAdPixelCollisionTester.Notify(ASender: TObject;
  AEvent: TAdSurfaceEventState);
begin
  case AEvent of
    seInitialize: Initialize;
    seFinalize: Finalize;
  end;
end;

function TAdPixelCollisionTester.CalcOverlapRect(out AO: TAdRect; const AR1,
  AR2: TAdRect): boolean;
begin
  if OverlapRect(AR1, AR2) then
  begin
    result := true;

    //--------
    //|A1----|----
    //|  | AO|   |
    //-------- A2|
    //   |       |
    //   ---------

    if AR1.Left > AR2.Left then
      AO.Left := AR1.Left
    else
      AO.Left := AR2.Left;

    if AR1.Top > AR2.Top then
      AO.Top := AR1.Top
    else
      AO.Top := AR2.Top;

    if AR1.Bottom < AR2.Bottom then
      AO.Bottom := AR1.Bottom
    else
      AO.Bottom := AR2.Bottom;

    if AR1.Right < AR2.Right then
      AO.Right := AR1.Right
    else
      AO.Right := AR2.Right;       

  end else
  begin
    result := false;
  end;
end;

end.
