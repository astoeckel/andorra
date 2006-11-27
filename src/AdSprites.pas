{
* This program is licensed under the GNU Lesser General Public License Version 2
* You should have recieved a copy of the license with this file.
* If not, see http://www.gnu.org/licenses/lgpl.html for more informations
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

uses Types,SysUtils,Classes,AdDraws,AndorraUtils;

type
  TSprite = class;
  TSpriteEngine = class;
  
  TSpriteList = class(TList)
    private
    	function GetItem(AIndex:integer):TSprite;
    	procedure SetItem(AIndex:integer;AItem:TSprite);
    protected
    public
    	property Items[AIndex:integer]:TSprite read GetItem write SetItem;default;
      procedure Add(ASprite:TSprite);
      procedure Remove(ASprite:TSprite);
    published
  end;

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
      constructor Create(AParent:TSprite);virtual;
      destructor Destroy;override;
      property Parent:TSprite read FParent write SetParent;
      property Engine:TSpriteEngine read FEngine;

      procedure Move(TimeGap:double);
      procedure Draw;
      procedure Dead;virtual;

      function Collision:integer;
      procedure Collision2;
    published
      property X:double read FX write SetX;
      property Y:double read FY write SetY;
      property Z:integer read FZ write SetZ;
      property Width:double read FWidth write FWidth;
      property Height:double read FHeight write FHeight;
      property WorldX:double read GetWorldX;
      property WorldY:double read GetWorldY;
      property Deaded:boolean read FDead;
      property BoundsRect:TRect read GetBoundsRect;
      //Equal to DelpiX's "Collisioned". Must have been a translation fault.
      property CanDoCollisions:boolean read FDoCollisions write FDoCollisions;
      //Equal to DelphiX's "Moved".
      property CanDoMoving:boolean read FDoMoving write FDoMoving;
      property Visible:boolean read FVisible write FVisible;
  end;

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
    public
      property CollisionCount:integer read FCollisionCount write FCollisionCount;
      property CollisionSprite:TSprite read FCollisionSprite write FCollisionSprite;
      property CollisionDone:boolean read FCollisionDone write FCollisionDone;
      property CollisionRect:TRect read FCollisionRect write FCollisionRect;

      constructor Create(AParent:TSprite);override;
      destructor Destroy;override;
      procedure Dead;override;

    published
      property Surface:TAdDraw read FSurface write SetSurface;
      property SurfaceRect:TRect read FSurfaceRect;
  end;

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
      constructor Create(AParent:TSprite);override;
      property Image:TPictureCollectionItem read FImage write SetImage;
    published
      property AnimCount:Integer read GetAnimCount;
      property AnimStart:Integer read FAnimStart write SetAnimStart;
      property AnimStop:Integer read FAnimStop write SetAnimStop;
      property AnimPos:double read FAnimPos write FAnimPos;
      property AnimLoop:boolean read FAnimLoop write FAnimLoop;
      property AnimActive:boolean read FAnimActive write FAnimActive;
      //The animation speed in frames per second.
      property AnimSpeed:double read FAnimSpeed write FAnimSpeed;
    end;

  TImageSpriteEx = class(TImageSprite)
    private
      FAngle:double;
      FAlpha:double;
    protected
      procedure DoDraw;override;
    public
      constructor Create(AParent:TSprite);override;
    published
      property Alpha:double read FAlpha write FAlpha;
      property Angle:double read FAngle write FAngle;
  end;

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
      constructor Create(AParent:TSprite);override;
    published
      {The virtual distance from the viewer. (May be a value bigger 0)}
      property Depth:single read FDepth write SetDepth;
      property Tiled:boolean read FTile write FTile;
      property Image:TPictureCollectionItem read FImage write FImage;
      property XTiles:integer read FXTiles write FXTiles;
      property YTiles:integer read FYTiles write FYTiles;
      property Center:boolean read FCenter write FCenter;
  end;

  TLightSprite = class(TSprite)
    private
      FRange:double;
      FFalloff:double;
      FColor:LongWord;
      procedure SetRange(AValue:double);
      procedure SetFalloff(AValue:double);
      procedure SetColor(AValue:LongWord);
    protected
      Light:TAdLight;
      procedure DoDraw;override;
      function GetBoundsRect:TRect;override;
    public
      constructor Create(AParent:TSprite);override;
      destructor Destroy;override;
    published
      property Range:double read FRange write SetRange;
      property Falloff:double read FFalloff write SetFalloff;
      property Color:LongWord read FColor write SetColor;
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
  result := inherited Items[AIndex];
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
    Remove(FDeadList[i]);
  end;
  FDeadList.Clear;
end;

destructor TSpriteEngine.Destroy;
begin
  FDeadList.Free;
  inherited Destroy;
end;

procedure TSpriteEngine.SetSurface(AValue: TAdDraw);
begin
  FSurface := AValue;
  FSurfaceRect := AValue.DisplayRect;
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
end;

procedure TImageSpriteEx.DoDraw;
begin
  if FImage <> nil then
  begin
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
  Light := TAdLight.Create(Engine.Surface);
  Light.Data.X1 := 0;
  Light.Data.Y1 := 0;
  Light.Data.Range := 100;
  Light.Data.Color := Ad_RGB(255,255,255);
  Light.Data.Falloff := 2;
  Light.Restore;
  FColor := $FFFFFF;
  FRange := 100;
  CanDoCollisions := false;
end;

destructor TLightSprite.Destroy;
begin
  Light.Free;
  inherited Destroy;
end;

procedure TLightSprite.DoDraw;
begin
  Light.Data.X1 := round(X+Engine.X);
  Light.Data.Y1 := round(Y+Engine.Y);
  Light.Restore;
  Light.Enable;
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
  Light.Data.Color := Ad_RGB(AValue, (AValue shr 8) and 255, (AValue shr 16) and 255);
  Light.Restore;
end;

procedure TLightSprite.SetFalloff(AValue: double);
begin
  FFalloff := AValue;
  Light.Data.Falloff := AValue;
  Light.Restore;
end;

procedure TLightSprite.SetRange(AValue: double);
begin
  FRange := AValue;
  Light.Data.Range := AValue;
  Light.Restore;
end;

end.
