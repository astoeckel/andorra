{
* This program is licensed under the to Common Public License (CPL) Version 1.0
* You should have recieved a copy of the license with this file.
* If not, see http://www.opensource.org/licenses/cpl1.0.txt for more informations
*
* Project: Andorra 2D
* Author:  Andreas Stoeckel
* File: AdSprites.pas
* Comment: This unit contains the sprite system. Please note that most of the things are
*          adapted from Un/DelphiX! 
}

{Contains the Andorra SpriteEngine System.}
unit AdSprites;

interface

uses Types,SysUtils,Classes, AdDraws, AdClasses, AdParticles;

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
      procedure SetParent(AParent:TSprite);
      procedure Add(ASprite:TSprite);
      procedure Remove(ASprite:TSprite);
      procedure SetZ(AValue:integer);
      function GetWorldY:double;
      function GetWorldX:double;
    protected
      function GetBoundsRect:TRect;virtual;
      function TestCollision(Sprite:TSprite):boolean;virtual;
      procedure DoMove(TimeGap:double);virtual;
      procedure DoDraw;virtual;
      procedure DoCollision(Sprite: TSprite; var Done: Boolean); virtual;
      procedure SetX(AValue:double);virtual;
      procedure SetY(AValue:double);virtual;
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
      
      {Returns a rect which contains the relative coordinates of the sprite.}
      property BoundsRect:TRect read GetBoundsRect;
      {Contains all child sprites.}
      property Items:TSpriteList read FList;
    published
      {The absolute X Position of the sprite.}
      property X:double read FX write SetX;
      {The absolute Y Position of the sprite.}
      property Y:double read FY write SetY;
      {The Z order of the sprite.}
      property Z:integer read FZ write SetZ;
      {The width of the sprite.}
      property Width:double read FWidth write FWidth;
      {The height of the sprite.}
      property Height:double read FHeight write FHeight;
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
      FSurfaceRect:TRect;
      procedure SetSurface(AValue:TAdDraw);
    protected
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
      constructor Create(AParent:TSprite);override;
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
      FImage:TPictureCollectionItem;
      FAnimLoop: Boolean;
      FAnimPos: Double;
      FAnimSpeed: Double;
      FAnimStart: Integer;
      FAnimStop: Integer;
      FAnimActive: Boolean;
      procedure SetImage(AValue:TPictureCollectionItem);
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
      property Image:TPictureCollectionItem read FImage write SetImage;
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
      FColor:LongInt;
    protected
      procedure DoDraw;override;
    public
      //Creates an instance of TImageSpriteEx
      constructor Create(AParent:TSprite);override;
    published
      //The alpha blend value of the sprite
      property Alpha:double read FAlpha write FAlpha;
      //The rotation angle from 0 to 360.
      property Angle:double read FAngle write FAngle;
      //The color of the sprite
      property Color:LongInt read FColor write FColor;
  end;

  {A sprite which draws the background of a scene.}
  TBackgroundSprite = class(TSprite)
    private
      FImage:TPictureCollectionItem;
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
      property Image:TPictureCollectionItem read FImage write FImage;
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
      FImage:TPictureCollectionItem;
      FEmissionX,FEmissionY:double;
      procedure SetEmissionCount(AValue:integer);
      procedure SetImage(AValue:TPictureCollectionItem); 
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
      property Image:TPictureCollectionItem read FImage write SetImage;
      property EmissionX:double read FEmissionX write FEmissionX;
      property EmissionY:double read FEmissionY write FEmissionY;
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

  FX := 0; FY := 0;
  FWidth := 0; FHeight := 0;

  FDoCollisions := true;
  FDoMoving := true;
  FVisible := true;
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
        FEngine.FCollisionDone := true;
      end;
    end;
    if not FEngine.FCollisionDone then
    begin
      for i := 0 to FList.Count - 1 do
      begin
        FList[i].Collision2;
        if FEngine.FCollisionDone then
        begin
          break;
        end;
      end;
    end;
  end;
end;

function TSprite.Collision: integer;
var
  i: Integer;
begin
  result := 0;
  
  if (not Deaded) and (FEngine <> nil) and (CanDoCollisions) then
  begin
    FEngine.FCollisionCount := 0;
    FEngine.FCollisionSprite := self;
    FEngine.FCollisionDone := false;
    FEngine.FCollisionRect := BoundsRect;

    for i := 0 to FEngine.FList.Count - 1 do
    begin
      FEngine.FList[i].Collision2;
      if FEngine.FCollisionDone then
      begin
        break;
      end;
    end;

    FEngine.FCollisionSprite := nil;

    result := FEngine.FCollisionCount;
  end;  
end;

procedure TSprite.DoDraw;
begin
  // Nothing to do yet
end;

procedure TSprite.DoMove(TimeGap: double);
begin
  // Nothing to do yet
end;

procedure TSprite.DoCollision(Sprite: TSprite; var Done: Boolean);
begin
  //Nothing to do yet.
end;

procedure TSprite.Remove(ASprite: TSprite);
begin
  FList.Remove(ASprite);
end;

procedure TSprite.Add(ASprite: TSprite);
begin
  if ASprite <> nil then
  begin
    FList.Add(ASprite);
  end;
end;

procedure TSprite.SetParent(AParent: TSprite);
begin
  if AParent <> nil then
  begin
    if FParent <> nil then
    begin
      FParent.FList.Remove(Self);
    end;
    AParent.Add(Self);
    FParent := AParent;
    FEngine := FParent.Engine;
  end
  else
  begin
    FParent := nil;
    FEngine := nil;
  end;
end;

procedure TSprite.SetX(AValue: double);
begin
  FX := AValue;
end;

procedure TSprite.SetY(AValue: double);
begin
  FY := AValue;
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

constructor TSpriteEngine.Create(AParent: TSprite);
begin
  inherited Create(nil);
  FParent := nil;
  FEngine := Self;

  FDeadList := TList.Create;
end; 

procedure TSpriteEngine.Dead;
var i:integer;
begin
  for i := 0 to FDeadList.Count - 1 do
  begin
    TSprite(FDeadList[i]).Free;
    Remove(TSprite(FDeadList[i]));
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
  if AValue > AnimStop then
  begin
    AValue := AnimStop;
  end;
  FAnimStart := AValue;
end;

procedure TImageSprite.SetAnimStop(AValue: integer);
begin
  if Abs(AValue) >= AnimCount then
  begin
    AValue := AnimCount-1;
  end;
  if Abs(AValue) < AnimStart then
  begin
    AValue := AnimStart;
  end;
  FAnimStop := Abs(AValue);
end;

procedure TImageSprite.SetImage(AValue: TPictureCollectionItem);
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
          Trunc(AnimPos),0.5,0.5,Round(Angle),Round(Alpha));
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

    Image.StretchBltAlpha(Engine.Surface,SourceRect,Engine.SurfaceRect,0,0,0,255);
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
  CanDoCollisions := false;
end;

destructor TParticleSprite.Destroy;
begin
  FPartSys.Free;
  inherited;
end;

procedure TParticleSprite.DoDraw;
begin
  FPartSys.Draw(round(Engine.X+WorldX),round(Engine.Y+WorldY));
  FPartSys.Dead;
end;

procedure TParticleSprite.DoMove(TimeGap: double);
var c:integer;
begin
  FTime := FTime + TimeGap*1000;
  if (FTime >= FWait) and (FWait > 0) then
  begin
    c := round(FTime / FWait);
    FTime := FTime - FWait;
    Emit(c);
  end;
  FPartSys.Move(TimeGap);
end;

procedure TParticleSprite.Emit(ACount: integer);
begin
  FPartSys.CreateParticles(ACount,TAdParticle,round(FEmissionX),round(FEmissionY));
end;

function TParticleSprite.GetBoundsRect: TRect;
var r:TRect;
begin
  r := FPartSys.BoundsRect;
  result := Rect(
              round(r.Left+Engine.X+WorldX),
              round(r.Top+Engine.Y+WorldY),
              round(r.Right+Engine.X+WorldX),
              round(r.Bottom+Engine.Y+WorldY));
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

procedure TParticleSprite.SetImage(AValue: TPictureCollectionItem);
begin
  FImage := AValue;
  FPartSys.Texture := FImage.Texture;
end;

end.
