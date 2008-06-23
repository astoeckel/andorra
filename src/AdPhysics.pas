{
* This program is licensed under the Common Public License (CPL) Version 1.0
* You should have recieved a copy of the license with this file.
* If not, see http://www.opensource.org/licenses/cpl1.0.txt for more informations.
* 
* Inspite of the incompatibility between the Common Public License (CPL) and the GNU General Public License (GPL) you're allowed to use this program * under the GPL. 
* You also should have recieved a copy of this license with this file. 
* If not, see http://www.gnu.org/licenses/gpl.txt for more informations.
*
* Project: Andorra 2D
* Author:  Andreas Stoeckel
* File: AdPhysics.pas
* Comment: Contains a link to newton
* To use the AdPhysics unit you'll need the Newton headers for Delphi from
*    http://newton.delphigl.de/
* and a version of Newton itsself from
*    http://www.newtongamedynamics.com/
* where you have to download the SDK for your platform and copy the .dll/.so/.dynlib to your Andorra 2D directory.
}

{ Contains a link to newton
To use the AdPhysics unit you'll need the Newton headers for Delphi from  http://newton.delphigl.de/ and a version of Newton itsself from   http://www.newtongamedynamics.com/ where you have to download the SDK for your platform and copy the .dll/.so/.dynlib to your Andorra 2D directory.}
unit AdPhysics;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  NewtonImport, Classes, Math,
  AdClasses, AdDraws, AdSprites, AdTypes, AdMath;

type
  //Contains data about a physical element.
  TPhysicalConstructData = class
     //The mass of an element
     Mass:single;
  end;
  
  //Contains data about a normal physical element.
  TPhysicalSimpleData = class(TPhysicalConstructData)
    //The size of an element
    Width,Height:single;
  end;  

  //A sprite which represents the physics engine.
  TPhysicalApplication = class;

  //A class which creates a physical element
  TPhysicalConstruct = class
    protected
      Parent:TPhysicalApplication;
    public
      //A pointer to newton's physical body/construct
      NewtonBody:PNewtonBody;
      //Creates the construct with the parameters given in AData
      procedure CreateConstruct(AData:TPhysicalConstructData);virtual;abstract;

      //Creates an instance of TPhysicalConstruct
      constructor Create(AParent:TPhysicalApplication);
      //Destroys an instance of TPhysicalConstruct
      destructor Destroy;override;
  end;

  //A class which creates a physical box
  TPhysicalBoxConstruct = class(TPhysicalConstruct)
    public
      //Creates the box with the parameters given in AData
      procedure CreateConstruct(AData:TPhysicalConstructData);override;
  end;

  //A class which creates a physical cylinder
  TPhysicalCylinderConstruct = class(TPhysicalConstruct)
    public
      //Creates the cylinder with the parameters given in AData
      procedure CreateConstruct(AData:TPhysicalConstructData);override;
  end;

  //A type which represents how accurate the simulation is
  TNewtonSolverModel = (
    smExact,//< Accurate but slow
    smAdaptive,//< Less accurate but still slow
    smLinear//< Unaccurate but fast
  );

  //A sprite which represents the physics engine.
  TPhysicalApplication = class(TSprite)
    private
      FLastSizeX:integer;
      FLastSizeY:integer;
      FTime:double;
      FInterval:single;
      FActive:boolean;
      FMinFrameRate:integer;
      FSolverModel:TNewtonSolverModel;
      procedure SetSolverModel(Value:TNewtonSolverModel);
    protected
      procedure DoMove(timegap:double);override;
    public
      // A pointer to newtons world
      NewtonWorld:PNewtonWorld;

      //Creates an instance of TPysicalApplication in the SpriteEngine
      constructor Create(AParent:TSprite);override;
      //Destroys the instance
      destructor Destroy;override;

      //Checks the bounds of the sprite world and expands is if neccesary
      procedure CheckBounds;

      //Sets wether the simulation is active or not
      property Active:boolean read FActive write FActive;
      //The interval of simulation in ms. Default is 10ms --> 100FPS.
      property Interval:single read FInterval write FInterval;
      //If the framerate is lower than this value the simulation is paused.
      property MinFrameRate:integer read FMinFrameRate write FMinFrameRate;
      //Sets the solver model. Have a look on the description of TNewtonSolverModel
      property SolverModel:TNewtonSolverModel read FSolverModel write SetSolverModel;
  end;

  //The typ of the physical sprite
  TPhysicalSpriteTyp = (
    ptDynamic,//< The sprite can move arround
    ptStatic//< The sprite is static, e.g. the landscape
  );

  //A sprite in the physical world. Don't use this sprite directly. Use e.g. TPhysicalBoxSprite or create a child class and override InitializeShape
  TPhysicalSprite = class(TImageSpriteEx)
    private
      FLastAx,FLastAy:double;
      FActive:boolean;
      FContinousActive:boolean;
      FBaseMatrix:TAdMatrix;
      FMass:single;
    protected
      Physics:TPhysicalApplication;
      Updating:Boolean;
      procedure DoMove(TimeGap:double);override;

      procedure SetX(Value:double);override;
      procedure SetY(Value:double);override;
      procedure SetAngle(Value:double);override;
      procedure UpdateNewtonMatrix;virtual;

      procedure SetActive(Value:boolean);virtual;
      function GetActive:boolean;virtual;
      procedure SetVelocity(Value:TAdVector3);virtual;
      function GetVelocity:TAdVector3;virtual;
      procedure SetOmega(Value:TAdVector3);virtual;
      function GetOmega:TAdVector3;virtual;

      procedure SetContinousActive(Value:boolean);
      function GetContinousActive:boolean;
    public
      //The creator of the physical form
      Construct:TPhysicalConstruct;
      //The typ of the sprite
      Typ:TPhysicalSpriteTyp;
      
      //Creates an instance of the sprite
      constructor Create(AParent:TSprite);override;
      //Destroys the instance
      destructor Destroy;override;
      
      //Frees the old construct if neccesary and waits for a child class to create the construct.
      procedure InitializeShape;virtual;
      
      //Wakes its neighbours up if they snooze at the moment.
      procedure ActivateNeighbours;

      //Sets the Sprite to the sleep modus (false)  or wakes it up (true).
      property Active:boolean read GetActive write SetActive;
      //Sets wether the sprite doesn't snooze. TRUE brings higher accuracy but it needs a lot of performance
      property ContinousActive:boolean read GetContinousActive write SetContinousActive;
      //The mass of the sprite
      property Mass:single read FMass write FMass;

      //Linear Velocity
      property Velocity:TAdVector3 read GetVelocity write SetVelocity;
      //Angulaer Velocity
      property Omega:TAdVector3 read GetOmega write SetOmega; 
  end;

  //A sprite with box form
  TPhysicalBoxSprite = class(TPhysicalSprite)
    public
      //Creates a box construct
      procedure InitializeShape;override;
  end;

  //A sprite with cylinder fotm
  TPhysicalCylinderSprite = class(TPhysicalSprite)
    public
      //Creates a cylinder construct
      procedure InitializeShape;override;
  end;

implementation

procedure ForceAndTorqueCallback(const body : PNewtonBody); cdecl;
var
 Mass : Single;
 Inertia : TAdVector3;
 Force : TAdVector3;
// i,j: integer;
begin
  NewtonBodyGetMassMatrix(Body, @Mass, @Inertia.x, @Inertia.y, @Inertia.z);
  with Force do
  begin
    X := 0;
    Y := 36 * Mass;
    Z := 0;
  end;
  NewtonBodySetForce(Body, @Force.x);
end;

{ TPhysicalApplication }

constructor TPhysicalApplication.Create(AParent: TSprite);
begin
  inherited;
  NewtonWorld := NewtonCreate(nil,nil);

  FLastSizeX := 0;
  FLastSizeY := 0;

  FTime := 0;
  FInterval := 10;
  FActive := true;
  FMinframeRate := 20;
  FSolverModel := smExact;

  CheckBounds;

//  NewtonImport.NewtonSetGlobalScale(
end;

destructor TPhysicalApplication.Destroy;
begin
  NewtonDestroy(NewtonWorld);
  inherited;
end;

procedure TPhysicalApplication.DoMove(timegap: double);
begin
  if FActive then
  begin
    if timegap < 1/FMinFramerate then
    begin
      FTime := FTime + timegap*1000;
      while FTime > FInterval do
      begin
        FTime := FTime - FInterval;
        NewtonUpdate(NewtonWorld, FInterval / 100);
      end;
    end;
    CheckBounds;
  end;
end;

procedure TPhysicalApplication.SetSolverModel(Value: TNewtonSolverModel);
begin
  FSolverModel := Value;
  case Value of
    smExact: NewtonSetSolverModel(NewtonWorld,0);
    smAdaptive: NewtonSetSolverModel(NewtonWorld,1);
    smLinear: NewtonSetSolverModel(NewtonWorld,2);
  end;
end;

procedure TPhysicalApplication.CheckBounds;
var vc1,vc2:TAdVector3;
begin
  if (FLastSizeX <> Engine.SpriteField.EndX - Engine.SpriteField.StartX) or
     (FLastSizeY <> Engine.SpriteField.EndY - Engine.SpriteField.StartY) then
  begin

    FLastSizeX := Engine.SpriteField.EndX - Engine.SpriteField.StartX;
    FLastSizeY := Engine.SpriteField.EndY - Engine.SpriteField.StartX;

    vc1 := AdVector3((Engine.SpriteField.StartX-1)*Engine.GridSize,
                    (Engine.SpriteField.StartY-1)*Engine.GridSize,-50);
    vc2 := AdVector3((Engine.SpriteField.EndX+1)*Engine.GridSize,
                    (Engine.SpriteField.EndY+1)*Engine.GridSize, 50);

    NewtonSetWorldSize(NewtonWorld, @vc1.X, @vc2.X);

  end;
end;

{ TPhysicalSprite }

constructor TPhysicalSprite.Create(AParent: TSprite);
var i:integer;
begin
  inherited;
  Physics := nil;
  for i := 0 to Engine.Items.Count-1 do
  begin
    if Engine.Items[i] is TPhysicalApplication then
    begin
      Physics := TPhysicalApplication(Engine.Items[i]);
      break;
    end;
  end;

  Construct := nil;
  FActive := true;
  FContinousActive := false;
end;

destructor TPhysicalSprite.Destroy;
begin
  inherited;
end;

procedure TPhysicalSprite.DoMove(TimeGap: double);
var Matrix:TAdMatrix;
    ax,ay:double;
begin
  inherited;
  if (FActive) and (Physics <> nil) and (Construct <> nil) and (Construct.NewtonBody <> nil) then
  begin
    NewtonBodyGetMatrix(Construct.NewtonBody, @Matrix[0,0]);
    Updating := true;

    X := Matrix[3,0] - Width / 2;
    Y := Matrix[3,1] - Height / 2;


    ax := (Matrix[0,0]) + (Matrix[1,0]);
    ay := (Matrix[0,1]) + (Matrix[1,1]);
    if (FLastAX <> ax) or (FLastAY <> ay) then
    begin
      FLastAx := ax;
      FLastAy := ay;
      if ay > 0 then
      begin
        Angle := RadToDeg(arccos(ax/(sqrt(sqr(ax)+sqr(ay)))))-45;
      end
      else
      begin
        Angle := 360-RadToDeg(arccos(ax/(sqrt(sqr(ax)+sqr(ay)))))-45;
      end;
    end;

    if (Matrix[2,0] <> FBaseMatrix[2,0]) or  (Matrix[2,1] <> FBaseMatrix[2,1]) or
       (Matrix[2,2] <> FBaseMatrix[2,2]) or  (Matrix[0,2] <> FBaseMatrix[0,2]) or
       (Matrix[1,2] <> FBaseMatrix[1,2]) then
    begin
      UpdateNewtonMatrix;
    end;


    Updating := false;
  end;
end;

function TPhysicalSprite.GetActive: boolean;
begin
  if FActive then
  begin
    result := NewtonBodyGetSleepingState(Construct.NewtonBody) = 1;
  end
  else
  begin
    result := false;
  end;
end;

function TPhysicalSprite.GetContinousActive: boolean;
begin
  if Construct <> nil then
  begin
    result := NewtonBodyGetAutoFreeze(Construct.NewtonBody) = 0;
  end
  else
  begin
    result := FContinousActive;
  end;
end;

procedure TPhysicalSprite.SetContinousActive(Value: boolean);
begin
  FContinousActive := Value;
  if Construct <> nil then
  begin
    if Value then
    begin
      NewtonBodySetAutoFreeze(Construct.NewtonBody,0)
    end
    else
    begin
      NewtonBodySetAutoFreeze(Construct.NewtonBody,1);
    end;
  end;
end;

function TPhysicalSprite.GetOmega: TAdVector3;
begin
  NewtonBodyGetOmega(Construct.NewtonBody,@Result.X);
end;

procedure TPhysicalSprite.SetOmega(Value: TAdVector3);
begin
  NewtonBodySetOmega(Construct.NewtonBody,@Value.X);
  SetActive(true);
end;

function TPhysicalSprite.GetVelocity: TAdVector3;
begin
  NewtonBodyGetVelocity(Construct.NewtonBody,@Result.X);
end;

procedure TPhysicalSprite.SetVelocity(Value: TAdVector3);
begin
  NewtonBodySetVelocity(Construct.NewtonBody,@Value.X);
  SetActive(true);
end;

procedure TPhysicalSprite.InitializeShape;
begin
  if Construct <> nil then
  begin
    Construct.Free;
    Construct := nil;
  end;
end;

procedure TPhysicalSprite.SetActive(Value: boolean);
begin
  if (Construct <> nil) and (Value <> Active) then
  begin
    FActive := Value;
    if Value then
    begin
      NewtonWorldUnfreezeBody(Physics.NewtonWorld, Construct.NewtonBody);
      ActivateNeighbours;
    end
    else
    begin
      NewtonWorldFreezeBody(Physics.NewtonWorld, Construct.NewtonBody);
    end;
  end;
end;

procedure TPhysicalSprite.SetAngle(Value: double);
begin
  inherited;
  if (not Updating) and (Construct <> nil) then
  begin
    UpdateNewtonMatrix;
  end;
end;

procedure TPhysicalSprite.SetX(Value: double);
begin
  inherited;
  if (not Updating) and (Construct <> nil) then
  begin
    UpdateNewtonMatrix;
  end;
end;

procedure TPhysicalSprite.SetY(Value: double);
begin
  inherited;
  if (not Updating) and (Construct <> nil) then
  begin
    UpdateNewtonMatrix;
  end;
end;

procedure TPhysicalSprite.ActivateNeighbours;
var r:TAdRect;
    ax,ay,i:integer;
    List:TSpriteList;
begin
  r := GetCollisionField;
  for ax := r.Left to r.Right do
  begin
    for ay := r.Top to r.Bottom do
    begin
      List := Parent.SpriteField.Items[ax,ay];
      for i := 0 to List.Count - 1 do
      begin
        if (List[i] <> self) and (List[i] is TPhysicalSprite) and 
           (TPhysicalSprite(List[i]).Typ <> ptStatic) then
        begin
          TPhysicalSprite(List[i]).Active := true;
        end;
      end;
    end;
  end;
end;

procedure TPhysicalSprite.UpdateNewtonMatrix;
var Mat1,Mat2:TAdMatrix;
begin
  Mat1 := AdMatrix_Translate(X + Width / 2, Y + Height / 2, 50);
  Mat2 := AdMatrix_RotationZ(DegToRad(Angle));
  Mat2 := AdMatrix_Multiply(Mat2,Mat1);
  NewtonBodySetMatrix(Construct.NewtonBody, @Mat2[0,0]);
end;

{ TPhysicalContstruct }

constructor TPhysicalConstruct.Create(AParent:TPhysicalApplication);
begin
  inherited Create;

  Parent := AParent;
  NewtonBody := nil;
end;

destructor TPhysicalConstruct.Destroy;
begin
  if NewtonBody <> nil then
  begin
    NewtonDestroyBody(Parent.NewtonWorld, NewtonBody);
  end;
  inherited;
end;

{ TPhysicalBoxConstruct }

procedure TPhysicalBoxConstruct.CreateConstruct(AData: TPhysicalConstructData);
var Collision:PNewtonCollision;
    Inertia:TAdVector3;
begin
  if AData is TPhysicalSimpleData then
  begin
    with AData as TPhysicalSimpleData do
    begin
      Collision := NewtonCreateBox(Parent.NewtonWorld, Width, Height, 100, nil);
      NewtonBody := NewtonCreateBody(Parent.NewtonWorld, Collision);
      NewtonReleaseCollision(Parent.NewtonWorld, Collision);

      with Inertia do
      begin
        x := Mass * (sqr(Height) + 10000)      / 12;
        y := Mass * (sqr(Width) + 10000)       / 12;
        z := Mass * (sqr(Width) + sqr(Height)) / 12;
        NewtonBodySetMassMatrix(NewtonBody, Mass, x, y, z);
      end;

      NewtonBodySetForceAndTorqueCallback(NewtonBody, ForceAndTorqueCallback);
    end;
  end;
end;

{ TPhysicalBoxSprite }

procedure TPhysicalBoxSprite.InitializeShape;
var Data:TPhysicalSimpleData;
begin
  inherited;
  Construct := TPhysicalBoxConstruct.Create(Physics);
  Data := TPhysicalSimpleData.Create;
  Data.Width := Width;
  Data.Height := Height;
  if Typ = ptDynamic then
  begin
    Data.Mass := Mass;
  end
  else
  begin
    Data.Mass := 0;
  end;

  Construct.CreateConstruct(Data);
  Data.Free;

  NewtonBodyGetMatrix(Construct.NewtonBody,@FBaseMatrix[0,0]);

  Physics.CheckBounds;

  SetX(X);
  SetY(Y);

  SetContinousActive(FContinousActive);
end;

{ TPhysicalZylinderConstruct }

procedure TPhysicalCylinderConstruct.CreateConstruct(
  AData: TPhysicalConstructData);
var Collision:PNewtonCollision;
    Inertia:TAdVector3;
    Mat1:TAdMatrix;
begin
  if AData is TPhysicalSimpleData then
  begin
    with AData as TPhysicalSimpleData do
    begin
      Mat1 := AdMatrix_RotationY(1/2*Pi);

      Collision := NewtonImport.NewtonCreateCylinder(Parent.NewtonWorld, Width / 2, 100, @Mat1[0,0]);
      NewtonBody := NewtonCreateBody(Parent.NewtonWorld, Collision);
      NewtonReleaseCollision(Parent.NewtonWorld, Collision);

      with Inertia do
      begin
        x := Mass * (sqr(Height)+ 10000)      / 12;
        y := Mass * (sqr(Width) + 10000)       / 12;
        z := Mass * (sqr(Width) + sqr(Height)) / 12;
        NewtonBodySetMassMatrix(NewtonBody, Mass, x, y, z);
      end;

      NewtonBodySetForceAndTorqueCallback(NewtonBody, ForceAndTorqueCallback);
    end;
  end;  
end;

{ TPhysicalCylinderSprite }

procedure TPhysicalCylinderSprite.InitializeShape;
var Data:TPhysicalSimpleData;
begin
  inherited;
  Construct := TPhysicalCylinderConstruct.Create(Physics);
  Data := TPhysicalSimpleData.Create;
  Data.Width := Width;
  Data.Height := Height;
  if Typ = ptDynamic then
  begin
    Data.Mass := Mass;
  end
  else
  begin
    Data.Mass := 0;
  end;

  Construct.CreateConstruct(Data);
  Data.Free;

  NewtonBodyGetMatrix(Construct.NewtonBody,@FBaseMatrix[0,0]);

  Physics.CheckBounds;

  SetX(X);
  SetY(Y);

  SetContinousActive(FContinousActive);
end;

end.
