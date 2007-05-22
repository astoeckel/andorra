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

uses {$INCLUDE AdTypes.inc}, SysUtils, Classes, AdDraws, AdClasses, AdParticles, Math;

type
  {The sprite engines base class.}
  TSprite = class;
  {A class of TSprite}
  TSpriteClass = class of TSprite;
  {The spriteengine itsself.}
  TSpriteEngine = class;
  
  {A list, which contains sprites.}
  TSpriteList = class(TList)
    private
    	function GetItem(AIndex:integer):TSprite;
    	procedure SetItem(AIndex:integer;AItem:TSprite);
    protected
    public
    	{Access on every item in the list.}
      property Items[AIndex:integer]:TSprite read GetItem write SetItem;default;
      {Adds a sprite to the list and orders it by its Z value}
      procedure Add(ASprite:TSprite);
      {Removes a sprite from the list.}
      procedure Remove(ASprite:TSprite);
  end;

  TIntegerArray = array of Integer;
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
      function GetCell(X,Y:integer):TPoint;
      function GetCol(X:integer):Integer;
      function GetRow(Y:integer):Integer;
    public
      {Expands the field.}
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

  TCollisionTyp = (ctOptimized, ctNormal);

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
      procedure SetParent(AParent:TSprite);
      procedure Add(ASprite:TSprite);
      procedure Remove(ASprite:TSprite);
      procedure SetZ(AValue:integer);
      function GetWorldY:double;
      function GetWorldX:double;
      procedure SetGridSize(Value:integer);
      procedure SetCollisionOptimization(Value:TCollisionTyp);
    protected
      OldFieldCoords:TRect;
      DidCollision:boolean;
      function GetBoundsRect:TRect;virtual;
      function GetFieldCoords:TRect;virtual;
      function TestCollision(Sprite:TSprite):boolean;virtual;
      procedure DoMove(TimeGap:double);virtual;
      procedure DoDraw;virtual;
      procedure DoCollision(Sprite: TSprite; var Done: Boolean); virtual;
      procedure SetX(AValue:double);virtual;
      procedure SetY(AValue:double);virtual;
      procedure SetWidth(AValue:double);virtual;
      procedure SetHeight(AValue:double);virtual;
      procedure RemapField;
      procedure MoveInField(ASprite:TSprite);
      function GetCollisionField:TRect;virtual;
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
      procedure Draw;
      {Tells the engine that this instance of TSprite wants to bee freed.}
      procedure Dead;virtual;

      {Checks whether this sprite collides with another sprite in the spriteengine.  Returns the count of sprites.}
      function Collision:integer;
      {Checks whether this sprite collides to the sprite, that wants to know whether it collides with another sprite.}
      procedure Collision2;

      {Returns the count of a sprite class}
      function GetCountOfClass(AClass:TSpriteClass):integer;

      {Optimzes the optimization field}
      procedure Optimize;

      {Returns one sprite at the specified position}
      function GetSpriteAt(X,Y:integer):TSprite;virtual;
      
      {Returns a rect which contains the relative coordinates of the sprite.}
      property BoundsRect:TRect read GetBoundsRect;
      {Contains all child sprites.}
      property Items:TSpriteList read FList;

      {Sets the size of the sprite field}
      property GridSize:integer read FGridSize write SetGridSize;
      {Sets the type of the collision optimization.}
      property CollisionOptimizationTyp:TCollisionTyp read FCollisionTyp write SetCollisionOptimization;

      property SpriteField:TAd2DSpriteList read FSpriteField;
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

      {Defines whether this sprite is included in the collision system. Equal to DelpiX's "Collisioned". Must have been a translation fault.}
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
      FSurface:TAdDraw;
      FDeadList:TList;
      FCollisionCount:integer;
      FCollisionSprite:TSprite;
      FCollisionDone:boolean;
      FCollisionRect:TRect;
      procedure SetSurface(AValue:TAdDraw);
    protected
      FSurfaceRect:TRect;
      procedure Notify(Sender:TObject;AEvent:TSurfaceEventState);
    public
      //The count of sprites which collide to the collision sprite.
      property CollisionCount:integer read FCollisionCount write FCollisionCount;
      //The sprite which wants to know, whether it collides.
      property CollisionSprite:TSprite read FCollisionSprite write FCollisionSprite;
      //If this value is set to true, the collision aborts.
      property CollisionDone:boolean read FCollisionDone write FCollisionDone;
      //The rect the collision takes place in.
      property CollisionRect:TRect read FCollisionRect write FCollisionRect;

      //Creates an instance of TSprite
      constructor Create(AParent:TAdDraw);reintroduce;
      //Destroyes the instance
      destructor Destroy;override;
      //Kills all sprites which want to be dead.
      procedure Dead;override;

      //The size of the surface.
      property SurfaceRect:TRect read FSurfaceRect;
    published
      //The parent application.
      property Surface:TAdDraw read FSurface write SetSurface;
  end;

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
      procedure SetImage(AValue:TAdImage);
      function GetAnimCount:integer;
      procedure SetAnimStart(AValue:integer);
      procedure SetAnimStop(AValue:integer);
    protected
      procedure DoDraw;override;
      procedure DoMove(TimeGap:double);override;
    public
      //Creates an instance of TImageSprite
      constructor Create(AParent:TSprite);override;
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
    protected
      procedure DoDraw;override;
      procedure SetAlpha(AValue:double);virtual;
      procedure SetAngle(AValue:double);virtual;
      procedure SetColor(AValue:longint);virtual;
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
      property RotationCenterX:double read FRotationCenterX write FRotationCenterX;
      //The y-rotation center
      property RotationCenterY:double read FRotationCenterY write FRotationCenterY;
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
      function GetBoundsRect:TRect;override;
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

  {A sprite which contains a light source.}
  TLightSprite = class(TSprite)
    private
      FRange:double;
      FFalloff:double;
      FColor:LongWord;
      FLight:TAdLight;
      procedure SetRange(AValue:double);
      procedure SetFalloff(AValue:double);
      procedure SetColor(AValue:LongWord);
    protected
      procedure DoDraw;override;
      function GetBoundsRect:TRect;override;
    public
      {Creates an instance of TLightSprite}
      constructor Create(AParent:TSprite);override;
      {Destroys the instance of TLightSprite}
      destructor Destroy;override;
    published
      {Sets the spot size of the light}
      property Range:double read FRange write SetRange;
      {Sets the falloff value of the light}
      property Falloff:double read FFalloff write SetFalloff;
      {Sets the color value of the light}
      property Color:LongWord read FColor write SetColor;
      {Link back to the TAdLight}
      property Light:TAdLight read FLight;
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
      function GetBoundsRect:TRect;override;
    public
      constructor Create(AParent:TSprite);override;
      destructor Destroy;override;
      procedure Emit(ACount:integer);
      property PartSys:TAdParticleSystem read FPartSys;
      property EmissionCount:integer read FEmissionCount write SetEmissionCount;
      property Image:TAdImage read FImage write SetImage;
      property EmissionX:double read FEmissionX write FEmissionX;
      property EmissionY:double read FEmissionY write FEmissionY;
      property AutoDeath:boolean read FAutoDeath write FAutoDeath;
  end;

implementation


function OverlapRect(const Rect1, Rect2: TRect): Boolean;
begin
  Result:=(Rect1.Left<Rect2.Right)and
    (Rect1.Right>Rect2.Left)and
    (Rect1.Top<Rect2.Bottom)and
    (Rect1.Bottom>Rect2.Top);
end;

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
var i:integer;
begin
  for i := 0 to FList.Count - 1 do
  begin
    FList[i].Free;
  end;    
  FList.Free;
  FSpriteField.Free;
  inherited Create;
end;

function TSprite.GetBoundsRect: TRect;
begin
  result := Bounds(Round(WorldX),Round(WorldY),Round(Width),Round(Height));
end;

function TSprite.GetCountOfClass(AClass: TSpriteClass): integer;
var i:integer;
begin
  result := 0;
  for i := 0 to Items.Count - 1 do
  begin
    result := result + Items[i].GetCountOfClass(AClass);
    if (Items[i] <> self) and (Items[i].ClassType = AClass) then
    begin
      result := result + 1;
    end;
  end;
end;

function TSprite.GetFieldCoords: TRect;
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
    rect:TRect;
begin
  result := nil;
  for i := Items.Count-1 downto 0 do
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

function TSprite.GetWorldX: double;
begin
  if FParent <> nil then
  begin
    Result := FParent.WorldX + FX;
  end
  else
  begin
    Result := FX;
  end;
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
    for i := 0 to FList.Count - 1 do
    begin
      if OverlapRect(FEngine.SurfaceRect,FList[i].BoundsRect) then
      begin
        FList[i].Draw;
      end;
    end;
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
var r:TRect;
begin
  if ASprite <> nil then
  begin
    r := ASprite.GetFieldCoords;
    if not CompRects(r,ASprite.OldFieldCoords) then
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
    r:TRect;
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

procedure TSprite.Add(ASprite: TSprite);
var r:TRect;
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
    if (Self<>FEngine.FCollisionSprite) and
        OverlapRect(FEngine.FCollisionRect,BoundsRect) and
        TestCollision(FEngine.FCollisionSprite) then
    begin
      FEngine.FCollisionCount := FEngine.CollisionCount + 1;
      FEngine.FCollisionSprite.DoCollision(self, FEngine.FCollisionDone);
      if FEngine.FCollisionSprite.Deaded or (not FEngine.FCollisionSprite.CanDoCollisions) then
      begin
        FEngine.CollisionDone := true;
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

function TSprite.GetCollisionField: TRect;
var r:TRect;
begin
  r := GetFieldCoords;

  result.Left := r.Left - 1;
  if result.Left < Engine.SpriteField.StartX then result.Left := FEngine.SpriteField.StartX;
  result.Top := r.Top - 1;
  if result.Top < Engine.SpriteField.StartY then result.Top := FEngine.SpriteField.StartY;
  result.Right := r.Right + 1;
  if result.Right > Engine.SpriteField.EndX then result.Right := FEngine.SpriteField.EndX;
  result.Bottom := r.Bottom + 1;
  if result.Bottom > Engine.SpriteField.EndY then result.Bottom := FEngine.SpriteField.EndY;
end;

function TSprite.Collision: integer;
var
  ax,ay,i: Integer;
  list:TSpriteList;
  r:TRect;
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
          begin
            break;
          end;
        end;
        if Engine.CollisionDone then
        begin
          break;
        end;
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
        begin
          break;
        end;
      end;
    end;
    Engine.CollisionSprite := nil;

    result := FEngine.CollisionCount;
  end;
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
  FZ := AValue;
  if FParent <> nil then
  begin
    FParent.Remove(self);
    FParent.Add(self);
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
  FDeadList := TList.Create;

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

procedure TSpriteEngine.Notify(Sender: TObject; AEvent: TSurfaceEventState);
begin
  if AEvent = seInitialize then
  begin
    FSurfaceRect := FSurface.DisplayRect;
  end;
end;

procedure TSpriteEngine.SetSurface(AValue: TAdDraw);
begin
  if (AValue <> nil) and (AValue <> FSurface) then
  begin
    if FSurface <> nil then
    begin
      FSurface.UnRegisterNotifyEvent(Notify);
    end;
    FSurface := AValue;
    FSurfaceRect := AValue.DisplayRect;
    FSurface.RegisterNotifyEvent(Notify);
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
    AnimPos := AnimPos + AnimSpeed*TimeGap;
    if trunc(AnimPos) > AnimStop then
    begin
      if AnimLoop then
      begin
        AnimPos := AnimStart;
      end
      else
      begin
        AnimActive := False;
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
  if AValue <> nil then
  begin
    Width := AValue.Width;
    Height := AValue.Height;
    if FImage = nil then
    begin
      FAnimStart := 0;
      FAnimStop := AValue.PatternCount-1;
    end;
  end;
  FImage := AValue;
end;

{ TImageSpriteEx }

constructor TImageSpriteEx.Create(AParent: TSprite);
begin
  inherited Create(AParent);
  FAlpha := 255;
  FColor := $FFFFFF;
  FRotationCenterX := 0.5;
  FRotationCenterY := 0.5;
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
        FImage.DrawRotateAlpha(Engine.Surface,BoundsRect.Left,BoundsRect.Top,
          BoundsRect.Right-BoundsRect.Left,BoundsRect.Bottom-BoundsRect.Top,
          Trunc(AnimPos),FRotationCenterX,FRotationCenterY,Round(Angle),Round(Alpha));
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
        FImage.DrawRotate(Engine.Surface,BoundsRect.Left,BoundsRect.Top,
          BoundsRect.Right-BoundsRect.Left,BoundsRect.Bottom-BoundsRect.Top,
          Trunc(AnimPos),0.5,0.5,Round(Angle));
      end
      else
      begin
        FImage.StretchDraw(Engine.Surface,BoundsRect,Trunc(AnimPos));
      end;
    end;
  end;
end;

procedure TImageSpriteEx.SetAlpha(AValue: double);
begin
  FAlpha := AValue;
end;

procedure TImageSpriteEx.SetAngle(AValue: double);
begin
  FAngle := AValue;
end;

procedure TImageSpriteEx.SetColor(AValue: Integer);
begin
  FColor := AValue;
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
var SourceRect:TRect;
    amx,amy:integer;
    ax,ay:double;
  procedure MoveRect(mx,my:integer;var ARect:TRect);
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
      SourceRect := bounds(0,0,Image.Width*XTiles,Image.Height*YTiles);
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

    Image.StretchBltAlpha(Engine.Surface,SourceRect,
      rect(Engine.SurfaceRect.Left-1,Engine.SurfaceRect.Top-1,
           Engine.SurfaceRect.Right,Engine.SurfaceRect.Bottom),0,0,0,255);
  end;
end;

function TBackgroundSprite.GetBoundsRect: TRect;
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

{ TLightSprite }

constructor TLightSprite.Create(AParent: TSprite);
begin
  inherited Create(AParent);
  FLight := TAdLight.Create(FEngine.Surface);
  CanDoCollisions := false;
end;

destructor TLightSprite.Destroy;
begin
  FLight.Free;
  inherited Destroy;
end;

procedure TLightSprite.DoDraw;
begin
  FLight.X := round(X+Engine.X);
  FLight.Y := round(Y+Engine.Y);
  FLight.Restore;
  FLight.Enable;
end;

function TLightSprite.GetBoundsRect: TRect;
var r:integer;
begin
  r := round(FRange);
  result := rect(round(x+Engine.X)-r,round(y+Engine.Y)-r,
                 round(x+Engine.X)+r,round(y+Engine.Y)+r);
end;

procedure TLightSprite.SetColor(AValue: LongWord);
begin
  FColor := AValue;
  FLight.Color := Ad_RGB(GetRValue(AValue),GetGValue(AValue),GetBValue(AValue));
end;

procedure TLightSprite.SetFalloff(AValue: double);
begin
  FFalloff := AValue;
  FLight.Falloff := AValue;
end;

procedure TLightSprite.SetRange(AValue: double);
begin
  FRange := AValue;
  FLight.Range := AValue;
end;

{ TParticleSprite }

constructor TParticleSprite.Create(AParent: TSprite);
begin
  inherited;
  FPartSys := TAdParticleSystem.Create(Engine.Surface);
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
  FPartSys.Draw(WorldX,WorldY);
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
  if (FAutoDeath) and (FPartSys.Items.Count = 0) then
  begin
    Dead;
  end;
  FPartSys.Dead;
end;

procedure TParticleSprite.Emit(ACount: integer);
begin
  FPartSys.CreateParticles(ACount,TAdParticle,round(FEmissionX),round(FEmissionY));
end;

function TParticleSprite.GetBoundsRect: TRect;
var r:TRect;
    w,h:integer;
begin
  r := FPartSys.BoundsRect;
  w := r.Right - r.Left;
  h := r.Bottom - r.Top;
  result := Bounds(r.Left + round(WorldX), r.Top + round(WorldY), w, h);
  FWidth := w;
  FHeight := h;
end;

procedure TParticleSprite.SetEmissionCount(AValue: integer);
begin
  if AValue > 0 then
  begin
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
var r:TRect;
    ax,ay:integer;
begin
  r := Bounds(X,Y,Width,Height);

  //Resize the field if necessary
  if (r.Right-1 > FEndX) then Expand(r.Right-FEndX-1,0);
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
      for j :=  High(FDynField[i])-abs(y)+1 to High(FDynField[i]) do
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

function TAd2DSpriteList.GetCell(X, Y: integer): TPoint;
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
var cell:TPoint;
begin
  cell := GetCell(X,Y);
  result := FDynField[cell.X,cell.Y];
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
    begin
      OrgRows[x] := FStartX + x;
    end;
    for x := 0 to high(OrgCols) do
    begin
      OrgCols[x] := FStartY + x;
    end;

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

end.
