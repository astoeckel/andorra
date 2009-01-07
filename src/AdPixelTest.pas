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

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  AdClasses, AdTypes, AdDraws, AdContainers;

type
  {Callback procedure type used by TAdPicelCollisionTester. Called when a
   object should be drawn.
   @param(AObj represents the object that should be drawn)
   @param(ASurface the surface it should be drawn to)
   @param(AX, AY the top corner edge of the object where it should be drawn to.)}
  TAdCollisionTestDrawProc = procedure(AObj: TObject; ASurface: TAdRenderingSurface;
    AX, AY: integer) of object;
  {Callback procedure type. Used by TAdPixelCollisionTester to indicate that two
   objects collided.}
  TAdCollisionTestCollisionProc = procedure(AObj1, AObj2: TObject) of object;

  {@exclude}
  TAdCollisionTesterItem = record
    Obj1, Obj2: TObject;
    CollisionCallback: TAdCollisionTestCollisionProc;
    PixelCounter: TAd2dPixelCounter;
  end;
  {@exclude}
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

      {The check collision function performs a collision test between two
       objects. The only thing you need to have, is a callback function that
       draws the objects on the surface to a specific position.
       @param(AObj1 is only a pointer that may contain user data that is in
         connection with the first collision object.)
       @param(ABoundsRect1 specifies the boundsrect of the first object.)
       @param(AObj2 is only a pointer that may contain user data that is in
         connection with the second collision object.)
       @param(ABoundsRect2 specifies the boundsrect of the second object.
       @param(ADrawCallback is called when one of the objects should be drawn
         on a specific surface on a specific position. The pointer given
         in AObj1 or AObj2 will be sent, to indicate which object should be
         drawn.)
       @param(ACollisionCallback is called when a collision between the two
         objects takes place. The pointers to the two given objects are sent.
         This callback function will be called after the "GetCollisions" function
         is called.)
       @seealso(TAdCollisionTestDrawProc)
       @seealso(TAdCollisionTestCollisionProc)}
      procedure CheckCollision(AObj1: TObject; ABoundsRect1: TAdRect;
        AObj2: TObject; ABoundsRect2: TAdRect;
        ADrawCallback: TAdCollisionTestDrawProc;
        ACollisionCallback: TAdCollisionTestCollisionProc);

      {Analyzes the last collision tests done by CheckCollision. If a collision
       took place, the "CollisionCallback" method given to CheckCollision is
       called.}
      procedure GetCollisions;

      {The surface all collision tests take place on. Increase the size of this
       surface by settings its size via "SetSize" to get more accurate pixel check
       results.}
      property Surface: TAdTextureSurface read FSurface;
      {The parent AdDraw given in the constructor.}
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

    //Setup stencil options. When a pixel is drawn, increment the stencil buffer
    FDraw.AdAppl.SetStencilOptions(1, $FFFF, asfAlways);
    FDraw.AdAppl.SetStencilEvent(asePass, asoReplace);

    //Draw the first object
    ADrawCallback(AObj1, FSurface,
      ABoundsRect1.Left - AOverlapRect.Left,
      ABoundsRect1.Top - AOverlapRect.Top);

    //Now, only draw a pixel, if the stencil buffer value is one, what means that
    //there is already another pixel
    FDraw.AdAppl.SetStencilOptions(0, $FFFF, asfLessThan);
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

end.
