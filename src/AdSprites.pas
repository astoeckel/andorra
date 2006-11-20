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
      property X:double read FX write FX;
      property Y:double read FY write FY;
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
      procedure SetImage(AValue:TPictureCollectionItem);
    protected
    public
      procedure DoDraw;override;
      property Image:TPictureCollectionItem read FImage write SetImage;
    published
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
begin
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

procedure TImageSprite.DoDraw;
begin
  if FImage <> nil then
  begin
    FImage.StretchDraw(Engine.Surface,BoundsRect,0);
  end;
end;

procedure TImageSprite.SetImage(AValue: TPictureCollectionItem);
begin
  FImage := AValue;
  if FImage <> nil then
  begin
    Width := FImage.Width;
    Height := FImage.Height;
  end;
end;

end.
