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
* Author:  Andreas Stöckel
* File: AdSprites.pas
* Comment: Contains the Andorra SpriteEngine System.
}

{Contains the Andorra SpriteEngine System.}
unit AdSprites;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses 
  SysUtils, Classes, Math,
  AdBitmap, AdTypes, AdDraws, AdClasses, AdParticles, AdList, AdPixelTest,
  AdMessages;

type
  {The sprite engines base class.}
  TSprite = class;
  {A class of TSprite}
  TSpriteClass = class of TSprite;
  {The spriteengine itsself.}
  TSpriteEngine = class;
  
  {A list, which contains sprites.}
  TSpriteList = class(TAdList)
    private
    	function GetItem(AIndex:integer):TSprite;
    	procedure SetItem(AIndex:integer;AItem:TSprite);
    protected
    public
    	{Access on every item in the list.}
      property Items[AIndex:integer]:TSprite read GetItem write SetItem;default;
      {Returns all sprites, which are at the given position. X and Y are relative
       screen coordinates.}
      procedure GetSpritesAt(ASpriteList: TSpriteList; const AX,
        AY: Integer; const ASpriteClass: TSpriteClass);
      {Adds a sprite to the list and orders it by its Z value}
      procedure Add(ASprite:TSprite);
      {Removes a sprite from the list.}
      procedure Remove(ASprite:TSprite);
  end;

  {Array type used internally by TAd2dSpriteList}
  TIntegerArray = array of Integer;
  
  {Matrix that contains a bucket for sprites in each cell.}
  T2DSpriteListArray = array of array of TSpriteList;

  {A list used to optimize the spriteengine}
  TAd2DSpriteList = class
    private
      FStartX,FStartY:integer;
      FEndY,FEndX:integer;
      FUnoptimized:boolean;
      FOrganisationCols:TIntegerArray;
      FOrganisationRows:TIntegerArray;
      FDynField:T2DSpriteListArray;
      function GetItem(X,Y:integer):TSpriteList;
    protected
      function GetCell(X,Y:integer):TAdPoint;
      function GetCol(X:integer):Integer;
      function GetRow(Y:integer):Integer;
    public
      {Expands the sprite field by X/Y fields. A negative value expands the field
       in negative direction.}
      procedure Expand(X,Y:integer);
      {Creates the list and initializes all values}
      constructor Create;
      {Destroys the list and all sprites within}
      destructor Destroy;override;
      {Add a sprite on specific coordinates to the field}
      procedure Add(AItem:TSprite;X,Y:integer;Width:integer=1;Height:integer=1);
      {Delete a sprite item from the list}
      procedure Delete(AItem:TSprite;X,Y:integer;Width:integer=1;Height:integer=1);
      {Optimizes the field. Removes empty rows and reorganizes the map structure}
      procedure Optimize;
      {Returns the list of sprites in a field}
      property Items[X,Y:integer]:TSpriteList read GetItem;
      {The start of the x coordinate}
      property StartX:integer read FStartX;
      {The end of the x coordinate}
      property EndX:integer read FEndX;
      {The start of the y coordinate}
      property StartY:integer read FStartY;
      {The end of the y coordinate}
      property EndY:integer read FEndY;
      {Returns weather the field has to be optimized}
      property Unoptimized:boolean read FUnOptimized;
  end;

  {Used to specifie the collision optimization method for the sprite engine 
   system.}
  TCollisionTyp = (
    ctOptimized, {< The sprite system uses optimizations. This mode is up to
      five times faster than the normal mode. The optimized mode is best 
      suitable for games with large levels organized in the spriteengine.}
    ctNormal {< If you experience any problems with the optimized mode, use
      this mode to use the old DelphiX-like collision mode.}
  );

  TAdSpriteCollisionTester = class
    public
      procedure CheckCollision(ASprite1, ASprite2: TSprite);virtual;abstract;
      procedure StopTest;virtual;abstract;
  end;

  TAdSpritePixelCollisionTester = class(TAdSpriteCollisionTester)
    private
      FPixelTester: TAdPixelCollisionTester;
      FSurfaceWidth, FSurfaceHeight: integer;
      FDone: boolean;
      procedure DrawSpriteProc(AObj: TObject; ASurface: TAdRenderingSurface;
        AX, AY: integer);
      procedure SpriteCollisionProc(AObj1, AObj2: TObject);
      procedure SetSurfaceWidth(AValue: integer);
      procedure SetSurfaceHeight(AValue: integer);
    protected
    public
      constructor Create(AParent: TAdDraw);
      destructor Destroy;override;

      procedure CheckCollision(ASprite1, ASprite2: TSprite);override;
      procedure StopTest;override;

      property SurfaceWidth: integer read FSurfaceWidth write SetSurfaceWidth;
      property SurfaceHeight: integer read FSurfaceHeight write SetSurfaceHeight;
  end;

  {The sprite engines base class.}
  TSprite = class
    private
      FList:TSpriteList;
      FParent:TSprite;
      FEngine:TSpriteEngine;
      FX,FY:double;
      FWidth,FHeight:double;
      FZ:integer;
      FDead:boolean;
      FVisible:boolean;
      FDoMoving:boolean;
      FDoCollisions:boolean;
      FSpriteField:TAd2DSpriteList;
      FGridSize:integer;
      FAutoOptimize:boolean;
      FCollisionTyp:TCollisionTyp;
      FVisibilityTest:boolean;
      FCollisionTester: TAdSpriteCollisionTester;
      procedure SetParent(AParent:TSprite);
      procedure Add(ASprite:TSprite);
      procedure Remove(ASprite:TSprite);
      procedure SetZ(AValue:integer);
      function GetWorldY:double;
      function GetWorldX:double;
      procedure SetGridSize(Value:integer);
      procedure SetCollisionOptimization(Value:TCollisionTyp);
    protected
      OldFieldCoords:TAdRect;
      DidCollision:boolean;
      function GetBoundsRect:TAdRect;virtual;
      function GetFieldCoords:TAdRect;virtual;
      function TestCollision(Sprite:TSprite):boolean;virtual;
      procedure DoMove(TimeGap:double);virtual;
      procedure DoDraw;virtual;
      procedure DoCollision(Sprite: TSprite; var Done: Boolean); virtual;
      procedure DoRestore;virtual;
      procedure SetX(AValue:double);virtual;
      procedure SetY(AValue:double);virtual;
      procedure SetWidth(AValue:double);virtual;
      procedure SetHeight(AValue:double);virtual;
      procedure RemapField;
      procedure MoveInField(ASprite:TSprite);
      function GetCollisionField:TAdRect;virtual;
      procedure CheckCollisionWith(ASprite:TSprite);virtual;
    public
      {Creates an instance of TSprite.}
      constructor Create(AParent:TSprite);virtual;
      {Destroyes the instance.}
      destructor Destroy;override;
      {The parent sprite you've set into the constructor.}
      property Parent:TSprite read FParent write SetParent;
      {The sprite engine.}
      property Engine:TSpriteEngine read FEngine;

      {Calls the instance's DoMove function and all child's move function.}
      procedure Move(TimeGap:double);
      {Calls the instance's DoDraw function and all childs' draw function.}
      procedure Draw;virtual;
      {Tells the engine that this instance of TSprite wants to bee freed.}
      procedure Dead;virtual;
      {Gives all sprites the posibility to restore their setings - eg. for
       Lights in Splitscreenmode}
      procedure Restore;

      {Deletes all sprites}
      procedure Clear;

      {Checks whether this sprite collides with another sprite in the
       spriteengine.  Returns the count of sprites.}
      function Collision:integer;
      {Checks whether this sprite collides to the sprite, that wants to know
       whether it collides with another sprite.}
      procedure Collision2;

      {Returns the count of a sprite class}
      function CountOfClass(AClass:TSpriteClass):integer;

      {Optimzes the optimization field}
      procedure Optimize;

      {Returns one sprite at the specified position. X and Y are relative
       screen coordinates.}
      function GetSpriteAt(X,Y:integer):TSprite;virtual;
      {Returns all sprites, which are at the specified position and of the specified type. X and Y are relative
       screen coordinates.}
      procedure GetSpritesAt(const AX, AY: Integer; ASprites: TSpriteList;
        AClass : TSpriteClass); overload; virtual;
      {Returns all sprites, which are at the specified position and of the specified type. X and Y are relative
       screen coordinates.}
      procedure GetSpritesAt(const AX, AY: Integer; ASprites: TSpriteList);overload;virtual;
     
      {Returns a rect which contains the relative coordinates (relative to the
       screen) of the sprite.}
      property BoundsRect:TAdRect read GetBoundsRect;
      {Contains all child sprites.}
      property Items:TSpriteList read FList;

      {Sets the size of the sprite field}
      property GridSize:integer read FGridSize write SetGridSize;
      {Sets the type of the collision optimization.}
      property CollisionOptimizationTyp:TCollisionTyp read FCollisionTyp write SetCollisionOptimization;

      {Returns the 2D-Matrix in which all sprites are sorted by their position.
       This Matrix may be used for fast access on the sprites.}
      property SpriteField:TAd2DSpriteList read FSpriteField;

      property CollisionTester: TAdSpriteCollisionTester read FCollisionTester write FCollisionTester;
    published
      {The absolute X Position of the sprite.}
      property X:double read FX write SetX;
      {The absolute Y Position of the sprite.}
      property Y:double read FY write SetY;
      {The Z order of the sprite.}
      property Z:integer read FZ write SetZ;
      {The width of the sprite.}
      property Width:double read FWidth write SetWidth;
      {The height of the sprite.}
      property Height:double read FHeight write SetHeight;
      {The relative X Position of the sprite.}
      property WorldX:double read GetWorldX;
      {The relative Y Position of the sprite.}
      property WorldY:double read GetWorldY;
      {Returns wether this sprite wants to be freed.}
      property Deaded:boolean read FDead;
      {Disable to call the "draw" method of sprites although they aren't visible}
      property VisibilityTest:boolean read FVisibilityTest write FVisibilityTest;

      {Defines whether this sprite is included in the collision system. Equal to
       DelpiX's "Collisioned".}
      property CanDoCollisions:boolean read FDoCollisions write FDoCollisions;
      {Defines whether the "DoMove" function is called. Equal to DelphiX's "Moved".}
      property CanDoMoving:boolean read FDoMoving write FDoMoving;
      {Defines whether "DoDraw" is called.}
      property Visible:boolean read FVisible write FVisible;
      {True if the optimization field should be automatically optimized}
      property AutoOptimize:boolean read FAutoOptimize write FAutoOptimize;
  end;

  {The spriteengine itsself.}
  TSpriteEngine = class(TSprite)
    private
      FSurface:TAdRenderingSurface;
      FDeadList:TAdList;
      FCollisionCount:integer;
      FCollisionSprite:TSprite;
      FCollisionDone:boolean;
      FCollisionRect:TAdRect;
    protected
      FSurfaceRect:TAdRect;
      procedure SetSurface(AValue:TAdRenderingSurface);virtual;
    public
      //The count of sprites which collide to the collision sprite.
      property CollisionCount:integer read FCollisionCount write FCollisionCount;
      //The sprite which wants to know, whether it collides.
      property CollisionSprite:TSprite read FCollisionSprite write FCollisionSprite;
      //If this value is set to true, the collision aborts.
      property CollisionDone:boolean read FCollisionDone write FCollisionDone;
      //The rect the collision takes place in.
      property CollisionRect:TAdRect read FCollisionRect write FCollisionRect;

      //Creates an instance of TSprite
      constructor Create(AParent:TAdDraw);reintroduce;
      //Destroyes the instance
      destructor Destroy;override;
      //Kills all sprites which want to be dead.
      procedure Dead;override;
      {Calls the DoMove function of the instance and all child's move function.}
      procedure Move(TimeGap: double);

      //The size of the surface.
      property SurfaceRect:TAdRect read FSurfaceRect;
    published
      //The parent addraw surface.
      property Surface:TAdRenderingSurface read FSurface write SetSurface;
  end;

  {Type that defines the event that happened when the TImageSprite DoAnimEvent
   method is called.}
  TAdAnimEvent = (
    aaeStart,//< The animation starts to playback the first time
    aaeStop,//< The playback of the animation stopped
    aaeLoop//< The playback of the animation starts again
  );

  {A sprite which draws sprites.}
  TImageSprite = class(TSprite)
    private
      FImage:TAdImage;
      FAnimLoop: Boolean;
      FAnimPos: Double;
      FAnimSpeed: Double;
      FAnimStart: Integer;
      FAnimStop: Integer;
      FAnimActive: Boolean;
      FNewAnim: Boolean;
      function GetAnimCount:integer;
      procedure SetAnimStart(AValue:integer);
      procedure SetAnimStop(AValue:integer);
    protected
      procedure SetImage(AValue:TAdImage);virtual;
      procedure SetHeight(AValue:double);override;
      procedure SetWidth(AValue:double);override;
      procedure DoDraw;override;
      procedure DoMove(TimeGap:double);override;
      procedure DoAnim(AAnimEvent: TAdAnimEvent);virtual;
    public
      //Creates an instance of TImageSprite
      constructor Create(AParent:TSprite);override;
      //Destroys the instance of TImageSprite
      destructor Destroy;override;
      //The image which is drawn by the sprite.
      property Image:TAdImage read FImage write SetImage;
    published
      //The count of patterns the image has.
      property AnimCount:Integer read GetAnimCount;
      //The pattern where the animation starts.
      property AnimStart:Integer read FAnimStart write SetAnimStart;
      //The pattern where the animation ends.
      property AnimStop:Integer read FAnimStop write SetAnimStop;
      //The pattern where the animation is at this moment.
      property AnimPos:double read FAnimPos write FAnimPos;
      //Defines whether the animation loops or it is only played once.
      property AnimLoop:boolean read FAnimLoop write FAnimLoop;
      //Defines whether the animation is played (true) or paused (false).
      property AnimActive:boolean read FAnimActive write FAnimActive;
      //The animation speed in frames (patterns) per second.
      property AnimSpeed:double read FAnimSpeed write FAnimSpeed;
    end;

  {An extended sprite which draws sprites blended and rotatet.}
  TImageSpriteEx = class(TImageSprite)
    private
      FAngle:double;
      FAlpha:double;
      FRotationCenterX:double;
      FRotationCenterY:double;
      FColor:LongInt;
      FBoundsRect:TAdRect;
      FChangedRotation: boolean;
      procedure RecalcBoundsRect;
      procedure RotatePoint(cx, cy: integer; var p: TAdPoint);
    protected
      procedure DoDraw;override;
      procedure DoMove(ATimeGap: double);override;
      procedure SetHeight(AValue:double);override;
      procedure SetWidth(AValue:double);override;
      procedure SetAlpha(AValue:double);virtual;
      procedure SetAngle(AValue:double);virtual;
      procedure SetColor(AValue:longint);virtual;
      procedure SetRotationCenterX(AValue: double);virtual;
      procedure SetRotationCenterY(AValue: double);virtual;
      function GetBoundsRect:TAdRect;override;
    public
      //Creates an instance of TImageSpriteEx
      constructor Create(AParent:TSprite);override;
    published
      //The alpha blend value of the sprite
      property Alpha:double read FAlpha write SetAlpha;
      //The rotation angle from 0 to 360.
      property Angle:double read FAngle write SetAngle;
      //The color of the sprite
      property Color:LongInt read FColor write SetColor;
      //The x-rotation center
      property RotationCenterX:double read FRotationCenterX write SetRotationCenterX;
      //The y-rotation center
      property RotationCenterY:double read FRotationCenterY write SetRotationCenterY;
  end;

  {A sprite which draws the background of a scene.}
  TBackgroundSprite = class(TSprite)
    private
      FImage:TAdImage;
      FTile:boolean;
      FDepth:single;
      FXTiles,FYTiles:integer;
      FCenter:boolean;
      procedure SetDepth(AValue:single);
    protected
      function GetBoundsRect:TAdRect;override;
      procedure DoDraw;override;
    public
      {Creates an instance of TBackgroundSprite}
      constructor Create(AParent:TSprite);override;
    published
      {The virtual distance from the viewer. (May be a value bigger 0)}
      property Depth:single read FDepth write SetDepth;
      {Defines whether the sprites is drawn patterned.}
      property Tiled:boolean read FTile write FTile;
      {The image which is drawn}
      property Image:TAdImage read FImage write FImage;
      {The number of tiles in X direction if tiled is false.}
      property XTiles:integer read FXTiles write FXTiles;
      {The number of tiles in Y direction if tiled is false.}
      property YTiles:integer read FYTiles write FYTiles;
      {Whether the background should be drawn in the center.}
      property Center:boolean read FCenter write FCenter;
  end;

  {A sprite wrapping around the particle system}
  TParticleSprite = class(TSprite)
    private
      FPartSys:TAdParticleSystem;
      FTime,FWait:double;
      FEmissionCount:integer;
      FImage:TAdImage;
      FEmissionX,FEmissionY:double;
      FAutoDeath:boolean;
      procedure SetEmissionCount(AValue:integer);
      procedure SetImage(AValue:TAdImage);
    protected
      procedure DoDraw;override;
      procedure DoMove(TimeGap:double);override;
      function GetBoundsRect:TAdRect;override;
    public
      //Creates an instance of TParticleSprite
      constructor Create(AParent:TSprite);override;
      //Destroys the instance of TParticleSprite
      destructor Destroy;override;
      //Emits an amout of particles
      procedure Emit(ACount:integer);
      //Returns the particle system the sprite uses. Read only.
      property PartSys:TAdParticleSystem read FPartSys;
      //If the property is not zero, n particles may be created
      property EmissionCount:integer read FEmissionCount write SetEmissionCount;
      //The Image the particle system uses
      property Image:TAdImage read FImage write SetImage;
      //May be used to move the point where the particles are created. Normaly it is zero, and the particles are created in the center of the sprite
      property EmissionX:double read FEmissionX write FEmissionX;
      //May be used to move the point where the particles are created. Normaly it is zero, and the particles are created in the center of the sprite
      property EmissionY:double read FEmissionY write FEmissionY;
      //If true (preset), the sprite may automaticly free itsself, when there are no particles.
      property AutoDeath:boolean read FAutoDeath write FAutoDeath;
  end;

implementation


{ TSpriteList }
procedure TSpriteList.Add(ASprite: TSprite);
var I:integer;
begin
  I := 0;
  if Count > 0 then
  begin
    while I < Count do
    begin
      if Items[I].Z < ASprite.Z then
      begin
        I := I + 1;
      end
      else
      begin
        break;
      end;
    end;
    Insert(I,ASprite);
  end
  else
  begin
    inherited Add(ASprite);
  end;
end;

function TSpriteList.GetItem(AIndex:integer):TSprite;
begin
  result := TSprite(inherited Items[AIndex]);
end;

procedure TSpriteList.GetSpritesAt(ASpriteList: TSpriteList; const AX,
  AY: Integer; const ASpriteClass: TSpriteClass);
var
  i : Integer;
  Rect : TAdRect;
begin
  if ASpriteList = nil then
    raise Exception.Create(MsgSpriteListIsNil);

  for i := 0 to Count - 1 do
  begin
    Rect := Items[i].BoundsRect;
    if InRect(AX, AY, Rect) and
       (Items[i] is ASpriteClass) then
    begin
      ASpriteList.Add(Items[i]);
    end;
  end;
end;

procedure TSpriteList.Remove(ASprite: TSprite);
begin
  inherited Remove(ASprite);
end;

procedure TSpriteList.SetItem(AIndex:integer;AItem:TSprite);
begin
  inherited Items[AIndex] := AItem;
end;

{ TSprite }

constructor TSprite.Create(AParent: TSprite);
begin
  inherited Create;
  Parent := AParent;

  FDead := false;

  FList := TSpriteList.Create;
  FSpriteField := TAd2DSpriteList.Create;

  FX := 0; FY := 0;
  FWidth := 0; FHeight := 0;

  FDoCollisions := true;
  FDoMoving := true;
  FVisible := true;

  FGridSize := 128;
  FAutoOptimize := true;

  FVisibilityTest := true;
end;

procedure TSprite.Dead;
begin
  if (FParent <> nil) and (FEngine <> nil) and (not FDead) then
  begin
    FEngine.FDeadList.Add(self);
    FDead := true;
  end;
end;

destructor TSprite.Destroy;
begin
  Clear;
  FList.Free;
  FSpriteField.Free;
  inherited;
end;

procedure TSprite.Clear;
var
  i:integer;
begin
  for i := 0 to FList.Count - 1 do
  begin
    FList[i].Free;
  end;
  FList.Clear;
end; 

function TSprite.GetBoundsRect: TAdRect;
begin
  result := AdBounds(Round(WorldX),Round(WorldY),Round(Width),Round(Height));
end;

function TSprite.CountOfClass(AClass: TSpriteClass): integer;
var i:integer;
begin
  result := 0;
  for i := 0 to Items.Count - 1 do
  begin
    result := result + Items[i].CountOfClass(AClass);
    if (Items[i] <> self) and (Items[i].ClassType = AClass) then
    begin
      result := result + 1;
    end;
  end;
end;

function TSprite.GetFieldCoords: TAdRect;
var x1,y1:double;
begin
  x1 := X / FParent.GridSize;
  y1 := Y / FParent.GridSize;
  with Result do
  begin
    Left  := floor(x1);
    Top := floor(y1);
    Right := ceil(x1 + (Width / Parent.GridSize));
    Bottom := ceil(y1 + (Height / Parent.GridSize));
  end;
end;

function TSprite.GetSpriteAt(X, Y: integer): TSprite;
var i:integer;
    rect:TAdRect;
begin
  result := nil;
  for i := Items.Count - 1 downto 0 do
  begin
    rect := Items[i].BoundsRect;
    if (X >= rect.Left) and (X <= rect.Right) and
       (Y >= rect.Top) and (Y <= rect.Bottom) then
    begin
      result := Items[i];
      break;
    end;    
  end;
end;

procedure TSprite.GetSpritesAt(const AX, AY: Integer; ASprites: TSpriteList);
begin
  GetSpritesAt(AX, AY, ASprites, TSprite);
end;

procedure TSprite.GetSpritesAt(const AX, AY: Integer; ASprites: TSpriteList;
  AClass: TSpriteClass);
var
  Field: TSpriteList;
begin
  if CollisionOptimizationTyp = ctNormal then
    Items.GetSpritesAt(ASprites, AX, AY, AClass)
  else begin
    Field := SpriteField.GetItem(
      round(AX-WorldX) div FGridSize,
      round(AY-WorldY) div FGridSize);
    if Field <> nil then
      Field.GetSpritesAt(ASprites, AX, AY, AClass);
  end;
end;

function TSprite.GetWorldX: double;
begin
  if FParent <> nil then
    Result := FParent.WorldX + FX
  else
    Result := FX;
end;

function TSprite.GetWorldY: double;
begin
  if FParent <> nil then
  begin
    Result := FParent.WorldY + FY;
  end
  else
  begin
    Result := FY;
  end;
end;

procedure TSprite.Draw;
var i:integer;
begin
  if Visible then
  begin
    //Activate the target surface if it unequals nil
    if FEngine.Surface <> nil then
     FEngine.Surface.Activate;

    //Check if every element in the list is visible and draw it
    for i := 0 to FList.Count - 1 do
      if (not FVisibilityTest) or (OverlapRect(FEngine.SurfaceRect, FList[i].BoundsRect)) then
        FList[i].Draw;

    DoDraw;
  end;
end;

procedure TSprite.Move(TimeGap: double);
var
  i: Integer;
begin
  if CanDoMoving then
  begin
    for i := 0 to FList.Count - 1 do
    begin
      FList[i].Move(TimeGap);
    end;
    DoMove(TimeGap);
  end;
end;

procedure TSprite.MoveInField(ASprite: TSprite);
var r:TAdRect;
begin
  if ASprite <> nil then
  begin
    r := ASprite.GetFieldCoords;
    if not CompareRects(r,ASprite.OldFieldCoords) then
    begin
      with ASprite.OldFieldCoords do 
      begin
        //Remove the Sprite from the old position
        FSpriteField.Delete(ASprite,Left,Top,Right-Left,Bottom-Top);
      end;
      //Add the sprite on the new position
      FSpriteField.Add(ASprite,r.Left,r.Top,r.Right-r.Left,r.Bottom-r.Top);
      ASprite.OldFieldCoords := r;
    end;
  end;
end;

procedure TSprite.Optimize;
begin
  FSpriteField.Optimize;
end;

procedure TSprite.RemapField;
var i:integer;
    r:TAdRect;
begin
  FSpriteField.Free;
  FSpriteField := TAd2DSpriteList.Create;
  for i := 0 to Items.Count - 1 do
  begin
    r := Items[i].GetFieldCoords;
    FSpriteField.Add(Items[i],r.Left,r.Top,r.Right-r.Left,r.Bottom-r.Top);
    Items[i].OldFieldCoords := r;
    Items[i].RemapField;
  end;
end;

procedure TSprite.Remove(ASprite: TSprite);
begin
  FList.Remove(ASprite);
  if ASprite <> nil then
  begin
    with ASprite.OldFieldCoords do
    begin
      FSpriteField.Delete(ASprite,Left,Top,Right-Left,Bottom-Top);
    end;
  end;
end;

procedure TSprite.Restore;
var
  i:integer;
begin
  DoRestore;
  for i := 0 to Items.Count-1 do
  begin
    Items[i].Restore;
  end;
end;

procedure TSprite.Add(ASprite: TSprite);
var
  r:TAdRect;
begin
  if ASprite <> nil then
  begin
    FList.Add(ASprite);

    r := ASprite.GetFieldCoords;
    FSpriteField.Add(ASprite,r.Left,r.Top,r.Right-r.Left,r.Bottom-r.Top);
    ASprite.OldFieldCoords := r;
  end;
end;


procedure TSprite.Collision2;
var
  i: Integer;
begin
  //Cancel if this sprite doesn't support collisions.
  if CanDoCollisions then
  begin
    if self <> FEngine.FCollisionSprite then
    begin
      if FEngine.CollisionSprite.CollisionTester = nil then
      begin
        if  OverlapRect(FEngine.FCollisionRect,BoundsRect) and
            TestCollision(FEngine.FCollisionSprite) then
        begin
          FEngine.FCollisionCount := FEngine.CollisionCount + 1;
          FEngine.FCollisionSprite.DoCollision(self, FEngine.FCollisionDone);
          if FEngine.FCollisionSprite.Deaded or (not FEngine.FCollisionSprite.CanDoCollisions) then
          begin
            FEngine.CollisionDone := true;
          end;
        end;
      end else
      begin
        FEngine.CollisionSprite.CollisionTester.CheckCollision(
          FEngine.CollisionSprite, self);
      end;
    end;

    if not FEngine.FCollisionDone then
    begin
      for i := 0 to FList.Count - 1 do
      begin
        FList[i].Collision2;
        if FEngine.CollisionDone then
        begin
          break;
        end;
      end;
    end;
  end;
end;

function TSprite.GetCollisionField: TAdRect;
var
  r:TAdRect;
begin
  r := GetFieldCoords;

  result.Left := r.Left - 1;
  if result.Left < Engine.SpriteField.StartX then
  begin
    result.Left := FEngine.SpriteField.StartX;
  end;

  result.Top := r.Top - 1;
  if result.Top < Engine.SpriteField.StartY then
  begin
    result.Top := FEngine.SpriteField.StartY;
  end;

  result.Right := r.Right + 1;
  if result.Right > Engine.SpriteField.EndX then
  begin
    result.Right := FEngine.SpriteField.EndX;
  end;

  result.Bottom := r.Bottom + 1;
  if result.Bottom > Engine.SpriteField.EndY then
  begin
    result.Bottom := FEngine.SpriteField.EndY;
  end;
end;

function TSprite.Collision: integer;
var
  ax,ay,i: Integer;
  list:TSpriteList;
  r:TAdRect;
begin
  result := 0;
  
  if (not Deaded) and (FEngine <> nil) and (CanDoCollisions) then
  begin
    Engine.CollisionCount := 0;
    Engine.CollisionSprite := self;
    Engine.CollisionDone := false;
    Engine.CollisionRect := BoundsRect;

    if FEngine.CollisionOptimizationTyp = ctOptimized then
    begin
      r := GetCollisionField;
      
      //Do the collision
      for ax := r.Left to r.Right do
      begin
        for ay := r.Top to r.Bottom do
        begin
          list := Engine.SpriteField.Items[ax,ay];
          i := 0;
          while i < list.Count do
          begin
            if not list[i].DidCollision then
            begin
              list[i].DidCollision := true;
              CheckCollisionWith(list[i]);
              if Engine.CollisionDone then
              begin
                break;
              end;
            end;
            i := i + 1;   
          end;
          if Engine.CollisionDone then
            break;
        end;
        if Engine.CollisionDone then
          break;
      end;

      //Reset the "DidCollision" flag
      for ax := r.Left to r.Right do
      begin
        for ay := r.Top to r.Bottom do
        begin
          list := Engine.SpriteField.Items[ax,ay];
          i := 0;
          while i < list.Count do
          begin
            list[i].DidCollision := false;
            i := i + 1;
          end;
        end;
      end;   

    end
    else
    begin
      for i := 0 to Engine.Items.Count - 1 do
      begin
        CheckCollisionWith(Engine.Items[i]);
        if Engine.CollisionDone then
          break;
      end;
    end;
    Engine.CollisionSprite := nil;

    result := FEngine.CollisionCount;
  end;

  if FCollisionTester <> nil then
    FCollisionTester.StopTest;
end;

procedure TSprite.CheckCollisionWith(ASprite: TSprite);
begin
  ASprite.Collision2;
end;

procedure TSprite.DoDraw;
begin
  // Nothing to do yet
end;

procedure TSprite.DoMove(TimeGap: double);
begin
  if FAutoOptimize then
  begin
    Optimize;
  end;
end;

procedure TSprite.DoRestore;
begin
  //Nothing to do yet.
end;

procedure TSprite.DoCollision(Sprite: TSprite; var Done: Boolean);
begin
  //Nothing to do yet.
end;

procedure TSprite.SetCollisionOptimization(Value: TCollisionTyp);
var i:integer;
begin
  FCollisionTyp := Value;
  for i := 0 to Items.Count - 1 do
  begin
    Items[i].CollisionOptimizationTyp := Value;
  end;
end;

procedure TSprite.SetGridSize(Value: integer);
var i:integer;
begin
  FGridSize := Value;
  for i := 0 to Items.Count - 1 do
  begin
    Items[i].GridSize := Value;
  end;
  RemapField;
end;

procedure TSprite.SetParent(AParent: TSprite);
begin
  if AParent <> nil then
  begin
    FParent := AParent;
    FEngine := FParent.Engine;
    if FParent <> nil then
    begin
      FParent.FList.Remove(Self);
    end;
    AParent.Add(Self);
  end
  else
  begin
    FParent := nil;
    FEngine := nil;
  end;
end;

procedure TSprite.SetHeight(AValue: double);
begin
  FHeight := AValue;
  if Parent <> nil then Parent.MoveInField(self);
end;

procedure TSprite.SetWidth(AValue: double);
begin
  FWidth := AValue;
  if Parent <> nil then Parent.MoveInField(self);
end;

procedure TSprite.SetX(AValue: double);
begin
  FX := AValue;
  if Parent <> nil then Parent.MoveInField(self);
end;

procedure TSprite.SetY(AValue: double);
begin
  FY := AValue;
  if Parent <> nil then Parent.MoveInField(self);
end;

procedure TSprite.SetZ(AValue: integer);
begin
  if AValue <> FZ then
  begin
    FZ := AValue;
    if FParent <> nil then
    begin
      FParent.Remove(self);
      FParent.Add(self);
    end;
  end;
end;     

function TSprite.TestCollision(Sprite: TSprite): boolean;
begin
  result := true;
end;

{ TSpriteEngine }

constructor TSpriteEngine.Create(AParent: TAdDraw);
begin
  inherited Create(nil);
  FParent := nil;
  FEngine := Self;
  FDeadList := TAdList.Create;

  Surface := AParent;
end; 

procedure TSpriteEngine.Dead;
var i:integer;
begin
  for i := 0 to FDeadList.Count - 1 do
  begin
    Remove(TSprite(FDeadList[i]));
    TSprite(FDeadList[i]).Free;
  end;
  FDeadList.Clear;
end;

destructor TSpriteEngine.Destroy;
begin
  FDeadList.Free;
  inherited Destroy;
end;

procedure TSpriteEngine.Move(TimeGap: double);
begin
  FSurfaceRect := Surface.DisplayRect;
  inherited;
end;

procedure TSpriteEngine.SetSurface(AValue: TAdRenderingSurface);
begin
  if (AValue <> nil) and (AValue <> FSurface) then
  begin
    FSurface := AValue;
    FSurfaceRect := AValue.DisplayRect;
  end;
end;

{ TImageSprite }

constructor TImageSprite.Create(AParent: TSprite);
begin
  inherited Create(AParent);
  FAnimActive := true;
  FAnimSpeed := 25;
  FAnimLoop := true;
end;

destructor TImageSprite.Destroy;
begin
  inherited;
end;

procedure TImageSprite.DoAnim(AAnimEvent: TAdAnimEvent);
begin
  //Do nothing here
end;

procedure TImageSprite.DoDraw;
begin
  if FImage <> nil then
  begin
    FImage.StretchDraw(Engine.Surface,BoundsRect,Trunc(AnimPos));
  end;
end;

procedure TImageSprite.DoMove(TimeGap: double);
begin
  if AnimActive then
  begin
    if FNewAnim then
    begin
      DoAnim(aaeStart);
      FNewAnim := false;
    end;

    AnimPos := AnimPos + AnimSpeed * TimeGap;
    if trunc(AnimPos) > AnimStop then
    begin
      if AnimLoop then
      begin
        AnimPos := AnimStart;
        DoAnim(aaeLoop);
      end
      else
      begin
        AnimActive := False;
        DoAnim(aaeStop);
      end;
    end;
  end;
end;

function TImageSprite.GetAnimCount: integer;
begin
  result := 0;
  if FImage <> nil then
  begin
    result := FImage.PatternCount;
  end;
end;

procedure TImageSprite.SetAnimStart(AValue: integer);
begin
  if AValue >= AnimCount then
  begin
    AValue := AnimCount-1;
  end;
  FAnimStart := AValue;
end;

procedure TImageSprite.SetAnimStop(AValue: integer);
begin
  if Abs(AValue) >= AnimCount then
  begin
    AValue := AnimCount-1;
  end;
  FAnimStop := Abs(AValue);
end;

procedure TImageSprite.SetImage(AValue: TAdImage);
begin
  if (AValue <> nil) and (AValue <> FImage) then
  begin
    Width := AValue.Width;
    Height := AValue.Height;
    if FImage = nil then
    begin
      FAnimStart := 0;
      FAnimStop := AValue.PatternCount-1;
    end;
    FNewAnim := true;
  end;
  FImage := AValue;
end;

procedure TImageSprite.SetWidth(AValue: double);
begin
  inherited;
end;

procedure TImageSprite.SetHeight(AValue: double);
begin
  inherited;
end;

{ TImageSpriteEx }

constructor TImageSpriteEx.Create(AParent: TSprite);
begin
  inherited Create(AParent);
  FAlpha := 255;
  FColor := $FFFFFF;
  FRotationCenterX := 0.5;
  FRotationCenterY := 0.5;
  FChangedRotation := true;
end;

procedure TImageSpriteEx.DoDraw;
begin
  if FImage <> nil then
  begin   
    FImage.Color := FColor;
    if Alpha <> 255 then
    begin
      if Angle <> 0 then
      begin
        FImage.DrawRotateAlpha(Engine.Surface,
          round(WorldX), round(WorldY),
          round(Width), round(Height),
          Trunc(AnimPos),
          FRotationCenterX, FRotationCenterY, Round(Angle), Round(Alpha));
      end
      else
      begin
        FImage.DrawAlpha(Engine.Surface,BoundsRect,Trunc(AnimPos),Round(Alpha));
      end;
    end
    else
    begin
      if Angle <> 0 then
      begin
        FImage.DrawRotate(Engine.Surface,
          round(WorldX), round(WorldY),
          round(Width), round(Height),
          Trunc(AnimPos),
          FRotationCenterX, FRotationCenterY, Round(Angle));
      end
      else
      begin
        FImage.StretchDraw(Engine.Surface, BoundsRect, Trunc(AnimPos));
      end;
    end;
  end;
end;

procedure TImageSpriteEx.DoMove(ATimeGap: double);
begin
  inherited;

  if FChangedRotation then
  begin
    RecalcBoundsRect;
    FChangedRotation := false;
  end;
end;

function TImageSpriteEx.GetBoundsRect: TAdRect;
begin
  result :=
    AdRect(
      WorldX + FBoundsRect.Left,
      WorldY + FBoundsRect.Top,
      WorldX + FBoundsRect.Right,
      WorldY + FBoundsRect.Bottom)
end;

procedure TImageSpriteEx.RotatePoint(cx, cy: integer; var p: TAdPoint);
var
  alpha:double;
  distance:double;
begin
  p.X := p.X - cx;
  p.Y := p.Y - cy;
    
  //Calculate point polar coordinates
  distance := sqrt(sqr(p.X) + sqr(p.Y));
  alpha := arctan(p.Y/p.X);
  if p.X < 0 then
    alpha := alpha + PI;

  //Rotate point
  alpha := alpha + (FAngle / 180 * PI);
  p.x := round(cos(alpha) * distance);
  p.y := round(sin(alpha) * distance);

  p.X := p.X + cx;
  p.Y := p.Y + cy;
end;

procedure TImageSpriteEx.RecalcBoundsRect;
var
  ps: array[0..3] of TAdPoint;
  i: integer;
begin
  if not FloatsEqual(FAngle, 0, 0.001) then
  begin
    ps[0] := AdPoint(0, 0);
    ps[1] := AdPoint(Width, 0);
    ps[2] := AdPoint(0, Height);
    ps[3] := AdPoint(width, Height);

    for i := 0 to 3 do
      RotatePoint(
        round(FRotationCenterX * Width),
        round(FRotationCenterY * Height),
        ps[i]);

    for i := 0 to 3 do
    begin
      if (i = 0) or (ps[i].X > FBoundsRect.Right)  then FBoundsRect.Right  := ps[i].X;
      if (i = 0) or (ps[i].X < FBoundsRect.Left)   then FBoundsRect.Left   := ps[i].X;
      if (i = 0) or (ps[i].Y > FBoundsRect.Bottom) then FBoundsRect.Bottom := ps[i].Y;
      if (i = 0) or (ps[i].Y < FBoundsRect.Top)    then FBoundsRect.Top    := ps[i].Y;
    end;
      
  end else
  begin
    FBoundsRect :=
      AdRect(0, 0, Width, Height);
  end;
end;

procedure TImageSpriteEx.SetAlpha(AValue: double);
begin
  FAlpha := AValue;
end;

procedure TImageSpriteEx.SetAngle(AValue: double);
begin
  if FAngle <> AValue then
  begin
    FAngle := AValue;
    FChangedRotation := true;
  end;
end;

procedure TImageSpriteEx.SetColor(AValue: Integer);
begin
  FColor := AValue;
end;

procedure TImageSpriteEx.SetWidth(AValue: double);
begin
  if AValue <> Width then
  begin
    inherited;
    FChangedRotation := true;
  end;
end;

procedure TImageSpriteEx.SetHeight(AValue: double);
begin
  if AValue <> Height then
  begin
    inherited;
    FChangedRotation := true;
  end;
end;

procedure TImageSpriteEx.SetRotationCenterX(AValue: double);
begin
  if AValue <> FRotationCenterX then
  begin
    FRotationCenterX := AValue;
    FChangedRotation := true;
  end;
end;

procedure TImageSpriteEx.SetRotationCenterY(AValue: double);
begin
  if AValue <> FRotationCenterY then
  begin
    FRotationCenterY := AValue;
    FChangedRotation := true;
  end;
end;

{ TBackgroundSprite }

constructor TBackgroundSprite.Create(AParent: TSprite);
begin
  inherited Create(AParent);
  FCenter := true;
  FTile := true;
  FDepth := 1;
  FXTiles := 1;
  FYTiles := 1;
end;

procedure TBackgroundSprite.DoDraw;
var
  SourceRect:TAdRect;
  amx,amy:integer;
  ax,ay:double;
  
  procedure MoveRect(mx,my:integer;var ARect:TAdRect);
  begin
    ARect.Left := mx+ARect.Left;
    ARect.Top := my+ARect.Top;
    ARect.Right := mx+ARect.Right;
    ARect.Bottom := my+ARect.Bottom;
  end;
begin
  if FImage <> nil then
  begin
    if FTile then
    begin
      SourceRect := Engine.SurfaceRect;
    end
    else
    begin
      SourceRect := AdBounds(0,0,Image.Width*XTiles,Image.Height*YTiles);
    end;

    ax := Engine.X;
    ay := Engine.Y;

    //Calculate Depth
    amx := round((-round(ax) mod round(Image.Width*Depth)) * (1/Depth));
    amy := round((-round(ay) mod round(Image.Height*Depth))* (1/Depth));

    if FCenter then
    begin
      amx := amx-(SourceRect.Right-SourceRect.Left) div 2 + Image.Width div 2;
      amy := amy-(SourceRect.Bottom-SourceRect.Top) div 2 + Image.Height div 2;
    end;

    MoveRect(amx,amy,SourceRect);

    Image.DrawEx(Engine.Surface,SourceRect,
      AdRect(Engine.SurfaceRect.Left-1,Engine.SurfaceRect.Top-1,
             Engine.SurfaceRect.Right,Engine.SurfaceRect.Bottom), 0, 0, 0, 255,
             bmAlpha);
  end;
end;

function TBackgroundSprite.GetBoundsRect: TAdRect;
begin
  result := Engine.SurfaceRect;
end;      

procedure TBackgroundSprite.SetDepth(AValue: single);
begin
  if AValue > 0 then
  begin
    FDepth := AValue;
  end;
end;

{ TParticleSprite }

constructor TParticleSprite.Create(AParent: TSprite);
begin
  inherited;
  FPartSys := TAdParticleSystem.Create(Engine.Surface.Parent);
  EmissionCount := 0;
  FWait := 0;
  FAutoDeath := true;
  CanDoCollisions := false;
end;

destructor TParticleSprite.Destroy;
begin
  FPartSys.Free;
  inherited;
end;

procedure TParticleSprite.DoDraw;
begin
  FPartSys.Draw(FEngine.Surface, round(WorldX), round(WorldY));
end;

procedure TParticleSprite.DoMove(TimeGap: double);
var c:integer;
begin
  FTime := FTime + TimeGap*1000;
  if (FTime >= FWait) and (FWait > 0) then
  begin
    c := round(FTime / FWait);
    FTime := FTime - (FWait*c);
    Emit(c);
  end;
  FPartSys.Move(TimeGap);
  if (FAutoDeath) and (FPartSys.Items.Count = 0) and (FEmissionCount = 0) then
  begin
    Dead;
  end;
end;

procedure TParticleSprite.Emit(ACount: integer);
begin
  FPartSys.Emit(ACount, round(FEmissionX), round(FEmissionY));
end;

function TParticleSprite.GetBoundsRect: TAdRect;
var
  r:TAdRect;
  w,h:integer;
begin
  r := FPartSys.BoundsRect;
  w := r.Right - r.Left;
  h := r.Bottom - r.Top;
  result := AdBounds(r.Left + round(WorldX), r.Top + round(WorldY), w, h);
  FWidth := w;
  FHeight := h;
end;

procedure TParticleSprite.SetEmissionCount(AValue: integer);
begin
  if AValue > 0 then
  begin
    FEmissionCount := AValue;
    FWait := 1000/AValue;
  end
  else
  begin
    FWait := 0;
  end;
end;

procedure TParticleSprite.SetImage(AValue: TAdImage);
begin
  FImage := AValue;
  FPartSys.Texture := FImage.Texture;
end;

{ TAd2DSpriteList }

procedure TAd2DSpriteList.Add(AItem: TSprite; X, Y, Width, Height: integer);
var
  r:TAdRect;
  ax,ay:integer;
begin
  r := AdBounds(X,Y,Width,Height);

  //Resize the field if necessary
  if (r.Right-1 > FEndX) then Expand(r.Right-FEndX,0);
  if (r.Bottom-1 > FEndY) then Expand(0,r.Bottom-FEndY-1);
  if (r.Left < FStartX) then Expand(r.Left-FStartX,0);
  if (r.Top < FStartY) then Expand(0,r.Top-FStartY);

  for ax := r.Left to r.Right-1 do
  begin
    for ay := r.Top to r.Bottom-1 do
    begin
      Items[ax,ay].Add(AItem);
    end;
  end;
end;

procedure TAd2DSpriteList.Delete(AItem: TSprite; X, Y, Width, Height: integer);
var ax,ay:integer;
begin
  for ax := x to x + Width - 1 do
  begin
    for ay := y to y + Height - 1 do
    begin
      Items[ax,ay].Remove(AItem);
    end;
  end;
end;

constructor TAd2DSpriteList.Create;
begin
  inherited Create;
  SetLength(FDynField,1,1);
  FDynField[0,0] := TSpriteList.Create;
  SetLength(FOrganisationCols,1);
  SetLength(FOrganisationRows,1);
  FEndX := 0;
  FStartX := 0;
  FEndY := 0;
  FStartY := 0;
end;

destructor TAd2DSpriteList.Destroy;
var x,y:integer;
begin
  for x := 0 to high(FDynField) do
  begin
    for y := 0 to high(FDynField[x]) do
    begin
      FDynField[x][y].Free;
    end;
  end;
  inherited Destroy;
end;

procedure TAd2DSpriteList.Expand(X, Y: integer);
var i,j:integer;
begin
  if X <> 0 then
  begin
    //Resize the array
    SetLength(FDynField,Length(FDynField)+abs(X),Length(FDynField[0]));
    
    //Create TSpriteLists for all new fields
    for i := High(FDynField)-abs(x)+1 to High(FDynField) do
    begin
      for j := 0 to High(FDynField[i]) do
      begin
        FDynField[i][j] := TSpriteList.Create;
      end;
    end;

    //Make the organisation row field longer
    SetLength(FOrganisationCols,Length(FOrganisationCols)+abs(x));

    //Set new FEndX and FStartX
    if X > 0 then
    begin
      j := 0;
      for i := High(FOrganisationCols)-abs(x)+1 to High(FOrganisationCols) do
      begin
        j := j + 1;
        FOrganisationCols[i] := FEndX + j;
      end;
      FEndX := FEndX + X;
    end;

    if X < 0 then
    begin
      j := 0;
      for i := High(FOrganisationCols)-abs(x)+1 to High(FOrganisationCols) do
      begin
        j := j + 1;
        FOrganisationCols[i] := FStartX - j;
      end;
      FStartX := FStartX + X;
      FUnoptimized := true;
    end;
  end;

  if Y <> 0 then
  begin
    //Resize the array
    SetLength(FDynField,Length(FDynField),Length(FDynField[0])+abs(Y));
    
    //Create TSpriteLists for all new fields
    for i := 0 to High(FDynField) do
    begin
      for j := High(FDynField[i])-abs(y)+1 to High(FDynField[i]) do
      begin
        FDynField[i][j] := TSpriteList.Create;
      end;
    end;

    //Make the organisation col field longer
    SetLength(FOrganisationRows,Length(FOrganisationRows)+abs(Y));

    //Set new FEndY and FStartY
    if Y > 0 then
    begin
      j := 0;
      for i := High(FOrganisationRows)-abs(Y)+1 to High(FOrganisationRows) do
      begin
        j := j + 1;
        FOrganisationRows[i] := FEndY + j;
      end;
      FEndY := FEndY + Y;
    end;

    if Y < 0 then
    begin
      j := 0;
      for i := High(FOrganisationRows)-abs(Y)+1 to High(FOrganisationRows) do
      begin
        j := j + 1;
        FOrganisationRows[i] := FStartY - j;
      end;
      FStartY := FStartY + Y;
      FUnoptimized := true;
    end;
  end;  
end;

function TAd2DSpriteList.GetCell(X, Y: integer): TAdPoint;
begin
  if FUnOptimized then
  begin
    result.X := GetCol(X);
    result.Y := GetRow(Y);
  end
  else
  begin
    result.X := X - FStartX;
    result.Y := Y - FStartY;
  end;
end;

function TAd2DSpriteList.GetCol(X: integer): Integer;
var i:integer;
begin
  result := -1;
  for i := 0 to high(FOrganisationCols) do
  begin
    if FOrganisationCols[i] = X then
    begin
      result := i;
      break;
    end;
  end;
end;

function TAd2DSpriteList.GetItem(X, Y: integer): TSpriteList;
var
  cell:TAdPoint;
begin
  result := nil;
  if InRect(X, Y, AdRect(FStartX, FStartY, FEndX, FEndY)) then
  begin
    cell := GetCell(X,Y);
    result := FDynField[cell.X,cell.Y];
  end;
end;

function TAd2DSpriteList.GetRow(Y: integer): Integer;
var i:integer;
begin
  result := -1;
  for i := 0 to high(FOrganisationRows) do
  begin
    if FOrganisationRows[i] = Y then
    begin
      result := i;
      break;
    end;
  end;
end;

procedure TAd2DSpriteList.Optimize;
var x,y:integer;
    OrgRows,OrgCols:TIntegerArray;
    Field1:T2DSpriteListArray;
begin
  if UnOptimized then
  begin 
    //Create a new, tenporary buffer for the organisatrion and the field data
    SetLength(OrgRows,Length(FOrganisationCols));
    SetLength(OrgCols,Length(FOrganisationRows));
    SetLength(Field1,FEndX-FStartX+1,FEndY-FStartY+1);

    //Fill the organisation data
    for x := 0 to high(OrgRows) do
      OrgRows[x] := FStartX + x;

    for x := 0 to high(OrgCols) do
      OrgCols[x] := FStartY + x;

    //Re-Fill the field in optimized order
    for x := 0 to high(Field1) do
    begin
      for y := 0 to high(Field1[x]) do
      begin
        Field1[x,y] := FDynField[GetCol(x+FStartX)][GetRow(y+FStartY)];
      end;
    end;

    //Finalize the old buffers
    Finalize(FOrganisationRows);
    Finalize(FOrganisationCols);
    Finalize(FDynField);

    //Copy our temoprary buffer in the "global" buffer
    FDynField := Copy(Field1);
    FOrganisationRows := Copy(OrgCols);
    FOrganisationCols := Copy(OrgRows);

    FUnOptimized := false;      
  end;  
end;

{ TAdSpritePixelCollisionCheck }

constructor TAdSpritePixelCollisionTester.Create(AParent: TAdDraw);
begin
  inherited Create;

  FPixelTester := TAdPixelCollisionTester.Create(AParent);

  FSurfaceWidth := FPixelTester.Surface.Width;
  FSurfaceHeight := FPixelTester.Surface.Height;
end;

destructor TAdSpritePixelCollisionTester.Destroy;
begin
  FPixelTester.Free;
  inherited;
end;

procedure TAdSpritePixelCollisionTester.DrawSpriteProc(AObj: TObject;
  ASurface: TAdRenderingSurface; AX, AY: integer);
var
  oldx, oldy: double;
  oldsurface: TAdRenderingSurface;
begin
  with AObj as TSprite do
  begin
    //Temporarily set the new pixel check surface
    oldsurface := Engine.Surface;
    Engine.Surface := ASurface;

    //Calculate temporal position
    oldx := X;
    oldy := Y;

    X := X - BoundsRect.Left + AX;
    Y := Y - BoundsRect.Top + AY;

    //Draw the sprite
    Draw;

    //Reset position and surface
    Engine.Surface := oldsurface;
    X := oldx;
    Y := oldy;
  end;
end;

procedure TAdSpritePixelCollisionTester.SetSurfaceHeight(AValue: integer);
begin
  //Set the new surface height of the pixel tester
  FSurfaceHeight := AValue;
  FPixelTester.Surface.SetSize(FSurfaceWidth, FSurfaceHeight);
end;

procedure TAdSpritePixelCollisionTester.SetSurfaceWidth(AValue: integer);
begin
  //Set the new surface width of the pixel tester
  FSurfaceWidth := AValue;
  FPixelTester.Surface.SetSize(FSurfaceWidth, FSurfaceHeight);
end;

procedure TAdSpritePixelCollisionTester.SpriteCollisionProc(AObj1,
  AObj2: TObject);
begin
  if not FDone then  
    TSprite(AObj1).DoCollision(TSprite(AObj2), FDone);
end;

procedure TAdSpritePixelCollisionTester.StopTest;
begin
  FPixelTester.GetCollisions;
end;

procedure TAdSpritePixelCollisionTester.CheckCollision(ASprite1,
  ASprite2: TSprite);
begin
  FDone := false;

  //Call the collision test routine. The procedure "DrawSpriteProc" will be
  //called by the pixel collision tester.
  FPixelTester.CheckCollision(ASprite1, ASprite1.BoundsRect,
    ASprite2, ASprite2.BoundsRect, DrawSpriteProc,
    SpriteCollisionProc);
end;

end.
