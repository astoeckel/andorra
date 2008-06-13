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
  AdClasses, AdTypes, AdDraws, AdContainers;

type
  TAdCollisionTestDrawProc = procedure(AObj: TObject; ASurface: TAdRenderingSurface;
    AX, AY: integer) of object;
  TAdCollisionTestCollisionProc = procedure(AObj1, AObj2: TObject) of object;

  TAdCollisionTesterItem = record
    Obj1, Obj2: TObject;
    CollisionCallback: TAdCollisionTestCollisionProc;
    PixelCounter: TAd2dPixelCounter;
  end;
  PAdCollisionTesterItem = ^TAdCollisionTesterItem;

  {This class can be used to see whether two user-defined objects collide. 
   TAdPixelCollisionTester manages its own surfaces where the test objects are
   temporaly drawn at. After sending all test to the class using the
   "CheckCollision" method, the results can be retrieved using the "GetCollisions"
   method.}
  TAdPixelCollisionTester = class
    private
      FSurface: TAdTextureSurface;
      FDraw: TAdDraw;
      FCollisionObjectList: TAdLinkedList;
      FCount: integer;

      procedure Notify(ASender: TObject; AEvent: TAdSurfaceEventState);
      function CalcOverlapRect(out AO: TAdRect; const AR1, AR2: TAdRect): boolean;
      procedure Clear;
      function AddTestItem: PAdCollisionTesterItem;
    protected
      procedure Initialize;
      procedure Finalize;
    public
      {Creates an instance of TAdPixelCollisionTester.}
      constructor Create(AParent: TAdDraw);
      {Destroys the instance of TAdPixelCollisionTester.}
      destructor Destroy;override;

      {}
      procedure CheckCollision(AObj1: TObject; ABoundsRect1: TAdRect;
        AObj2: TObject; ABoundsRect2: TAdRect;
        ADrawCallback: TAdCollisionTestDrawProc;
        ACollisionCallback: TAdCollisionTestCollisionProc);

      procedure GetCollisions;

      property Surface: TAdTextureSurface read FSurface;
      property AdDraw: TAdDraw read FDraw;      
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

  FCollisionObjectList := TAdLinkedList.Create;

  if FDraw.Initialized then  
    Initialize;
end;

destructor TAdPixelCollisionTester.Destroy;
begin
  Finalize;
  FSurface.Free;

  FCollisionObjectList.Free;
  
  inherited;
end;

procedure TAdPixelCollisionTester.CheckCollision(AObj1: TObject; ABoundsRect1: TAdRect;
  AObj2: TObject; ABoundsRect2: TAdRect;
  ADrawCallback: TAdCollisionTestDrawProc;
  ACollisionCallback: TAdCollisionTestCollisionProc);
var
  AOverlapRect: TAdRect;
  ATestItem: PAdCollisionTesterItem;
  w, h: integer;
begin
  if CalcOverlapRect(AOverlapRect, ABoundsRect1, ABoundsRect2) then
  begin
    //Get new list element
    ATestItem := AddTestItem;

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
    ADrawCallback(AObj1, FSurface,
      ABoundsRect1.Left - AOverlapRect.Left,
      ABoundsRect1.Top - AOverlapRect.Top);

    //Now, only draw a pixel, if the stencil buffer value is one, what means that
    //there is already another pixel
    FDraw.AdAppl.SetStencilOptions(1, $FFFF, asfEqual);
    FDraw.AdAppl.SetStencilEvent(asePass, asoKeep);

    //Start counting drawn pixels
    ATestItem^.PixelCounter.StartCount;

    //Draw the second object
    ADrawCallback(AObj2, FSurface,
      ABoundsRect2.Left - AOverlapRect.Left,
      ABoundsRect2.Top - AOverlapRect.Top);

    //Stop counting
    ATestItem^.PixelCounter.StopCount;

    //Set list item data
    ATestItem^.Obj1 := AObj1;
    ATestItem^.Obj2 := AObj2;
    ATestItem^.CollisionCallback := ACollisionCallback;

    //Reset stencil options
    FDraw.AdAppl.SetStencilOptions(0, $FFFF, asfAlways);

    FSurface.Image.Draw(FDraw, 0, 0, 0);
  end;
end;

procedure TAdPixelCollisionTester.Clear;
var
  pct: PAdCollisionTesterItem;
begin
  FCollisionObjectList.StartIteration;
  while not FCollisionObjectList.ReachedEnd do
  begin
    pct := FCollisionObjectList.GetCurrent;
    pct^.PixelCounter.Free;
    Dispose(pct);
  end;
  FCount := 0;
end;

procedure TAdPixelCollisionTester.Finalize;
begin
  Clear;
end;

procedure TAdPixelCollisionTester.GetCollisions;
var
  pct: PAdCollisionTesterItem;
  i: integer;
begin
  //Iterate through list
  i := 0;
  FCollisionObjectList.StartIteration;
  while (not FCollisionObjectList.ReachedEnd) and (i < FCount) do
  begin
    pct := FCollisionObjectList.GetCurrent;
    if pct^.PixelCounter.GetCount > 0 then
      pct^.CollisionCallback(pct^.Obj1, pct^.Obj2);

    i := i + 1;
  end;

  //Reset list
  FCollisionObjectList.StartIteration;
  FCount := 0;
end;

procedure TAdPixelCollisionTester.Initialize;
begin
  Finalize;
end;

procedure TAdPixelCollisionTester.Notify(ASender: TObject;
  AEvent: TAdSurfaceEventState);
begin
  case AEvent of
    seInitialize: Initialize;
    seFinalize: Finalize;
  end;
end;

function TAdPixelCollisionTester.AddTestItem: PAdCollisionTesterItem;
begin
  if not FCollisionObjectList.ReachedEnd then
  begin
    result := FCollisionObjectList.GetCurrent;
  end else
  begin
    New(result);
    result^.PixelCounter := AdDraw.AdAppl.CreatePixelCounter;
    FCollisionObjectList.Add(result);
  end;

  FCount := FCount + 1;
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
    result := false;
end;

end.
