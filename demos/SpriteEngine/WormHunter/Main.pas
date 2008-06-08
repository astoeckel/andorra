unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, AdDraws, AdClasses, StdCtrls,
  AdPNG, AdTypes, AdSprites, AdParticles, AdPerformanceCounter, AdSetupDlg;

type
  TKey = (kyUp, kyDown, kyLeft, kyRight);

  TKeys = set of TKey;

  TDirection = (dirS, dirSW, dirW, dirNW, dirN, dirNE, dirE, dirSE);

  TState = (stStopped, stWalking, stDie, stBorn);

  TDieCallBack = procedure of object;

  TAnimationSprite = class(TImageSprite)
    private
      FImages:TAdImageList;
      FPrefix:string;
    public
      constructor Create(AParent:TSprite);override;
      destructor Destroy;override;
      procedure SetAnims(AList:TAdImageList; APrefix:string);
      procedure SetAnim(AName:string);
  end;

  TCharacter = class(TAnimationSprite)
    private
      FDirection:TDirection;
      FState:TState;
      FXSpeed:integer;
      FYSpeed:integer;
      
      procedure SetDirAnim;
    protected
      Speed:integer;

      property State:TState read FState;
      property Direction:TDirection read FDirection;

      procedure DoMove(TimeGap:double);override;
      procedure DoDraw;override;
      procedure SetDir(ADirection:TDirection);
      procedure SetState(AState:TState);
    public
      constructor Create(AParent:TSprite);override;
  end;

  TMainCharacter = class(TCharacter)
    private
      FKeys:TKeys;
    protected
      procedure DoMove(TimeGap:double);override;
      procedure DoCollision(Sprite:TSprite; var Done:boolean);override;
    public
      constructor Create(AParent:TSprite);override;      
      procedure Init;
      procedure SetKeys(Keys:TKeys);
  end;

  TWorm = class(TCharacter)
    private
      FNextDirChange:double;
      FLifeTime:double;
      FMaxLifeTime:integer;
    protected
      procedure DoMove(TimeGap:double);override;
    public
      constructor Create(AParent:TSprite);override;
      procedure Init;
      procedure Die;
  end;
  

  TDoodad = class(TImageSprite);

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    AdDraw:TAdDraw;
    AdPerCounter:TAdPerformanceCounter;
    AdImageList:TAdImageList;
    AdSpriteEngine:TSpriteEngine;
    AdPixelCollisionTester: TAdSpritePixelCollisionTester;

    MainCharacter:TMainCharacter;
    ActTime:double;

    procedure Idle(Sender:TObject;var Done:boolean);
    procedure AddWorm;
  end;

const
  path = '..\demos\SpriteEngine\WormHunter\';

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.AddWorm;
begin
  with TWorm.Create(AdSpriteEngine) do
  begin
    SetAnims(AdImageList,'worm');
    X := Random(5000)-2500;
    Y := Random(5000)-2500;
    Init;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i:integer;
  AdSetup: TAdSetup;
begin
  Randomize;

  AdPerCounter := TAdPerformanceCounter.Create;

  Cursor := crNone;

  AdDraw := TAdDraw.Create(self);
  AdDraw.Options := AdDraw.Options - [aoCulling];

  AdSetup := TAdSetup.Create(AdDraw);
  AdSetup.Image := 'logo1.png';
  if AdSetup.Execute then
  begin
    if AdDraw.Initialize then
    begin
      Application.OnIdle := Idle;

      AdImageList := TAdImageList.Create(AdDraw);
      AdImageList.LoadFromFile(path+'images.ail');

      AdPixelCollisionTester := TAdSpritePixelCollisionTester.Create(AdDraw);

      AdSpriteEngine := TSpriteEngine.Create(AdDraw);

      with TBackgroundSprite.Create(AdSpriteEngine) do
      begin
        Image := AdImageList.Find('gras');
        Z := -10000;
      end;

      MainCharacter := TMainCharacter.Create(AdSpriteEngine);
      with MainCharacter do
      begin
        SetAnims(AdImageList,'viking');
        SetKeys([]);
        CollisionTester := AdPixelCollisionTester;
        Init;
      end;

      for i := 0 to 200 do
      begin
        with TDoodad.Create(AdSpriteEngine) do
        begin
          case random(2) of
            0: Image := AdImageList.Find('objects');
            1: Image := AdImageList.Find('trees');
          end;
          AnimPos := Random(Image.PatternCount);
          AnimActive := false;
          X := Random(5000)-2500;
          Y := Random(5000)-2500;
          Z := round(Y+Height);
        end;
      end;

    end
    else
    begin
      ShowMessage('Error while initializing Andorra 2D. Try to use another display'+
                  'mode or another video adapter.');
      halt;
    end;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  AdPixelCollisionTester.Free;
  AdSpriteEngine.Free;
  AdPerCounter.Free;
  AdImageList.Free;
  AdDraw.Free;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  //Resize the Backbuffer
  AdDraw.Setup2DScene;
end;

procedure TForm1.Idle(Sender: TObject; var Done: boolean);
var
  keys:TKeys;
begin
  if AdDraw.CanDraw then
  begin
    AdPerCounter.Calculate;
    AdDraw.ClearSurface(clBlack);
    AdDraw.BeginScene;

    ActTime := ActTime + AdPerCounter.TimeGap;

    if ActTime > 100 then
    begin
      keys := [];
      if GetKeyState(VK_LEFT) < 0 then keys := keys + [kyLeft];
      if GetKeyState(VK_RIGHT) < 0 then keys := keys + [kyRight];
      if GetKeyState(VK_UP) < 0 then keys := keys + [kyUp];
      if GetKeyState(VK_DOWN) < 0 then keys := keys + [kyDown];

      MainCharacter.SetKeys(Keys);

      if AdSpriteEngine.CountOfClass(TWorm) < 200 then
      begin
        AddWorm;
      end;

      ActTime := 0;
    end;

    AdSpriteEngine.Move(AdPerCounter.TimeGap / 1000);
    AdSpriteEngine.Draw;
    AdSpriteEngine.Dead;

    with AdDraw.Canvas do
    begin
      Font.Textout(0,0,'FPS: '+inttostr(AdPerCounter.FPS));
      Font.Textout(0,20,'Sprites: '+inttostr(AdSpriteEngine.Items.Count));
      Font.Textout(0,40,'TimeGap: '+FormatFloat('#0.000',AdPerCounter.TimeGap));
    end;

    AdDraw.EndScene;
    AdDraw.Flip; 
  end;
  Done := false;
end;

{ TAnimCharacter }

constructor TAnimationSprite.Create(AParent: TSprite);
begin
  inherited Create(AParent);
  FImages := nil;
end;

destructor TAnimationSprite.Destroy;
begin
  if FImages <> nil then
    FreeAndNil(FImages);
  inherited;
end;

procedure TAnimationSprite.SetAnim(AName: string);
var
  i:integer;
begin
  Image := nil;
  for i := 0 to FImages.Count - 1 do
  begin
    if AName = Copy(FImages[i].Name,Length(FPrefix+'_')+1,Length(FImages[i].Name)) then
    begin
      Image := FImages[i];
      AnimLoop := true;
      exit;
    end;
  end;
end;

procedure TAnimationSprite.SetAnims(AList: TAdImageList; APrefix: string);
begin
  if FImages <> nil then
    FreeAndNil(FImages);
  FImages := AList.FindEx(APrefix+'_');
  FPrefix := APrefix;
end;

{ TCharacter }

constructor TCharacter.Create(AParent: TSprite);
begin
  inherited;
  
  Speed := 150;
  AnimSpeed := 15;
end;

procedure TCharacter.DoDraw;
begin
  inherited;
end;

procedure TCharacter.DoMove(TimeGap: double);
begin
  inherited;
  if FState = stWalking then
  begin
    X := X + FXSpeed * TimeGap;
    Y := Y + FYSpeed * TimeGap;
    Z := round(Y+Height-20);
  end;
end;

procedure TCharacter.SetDir(ADirection: TDirection);
var
  w:integer;
begin
  FYSpeed := 0;
  FXSpeed := 0;

  FDirection := ADirection;

  w := 0;
  
  case ADirection of
    dirS: w := -270;
    dirSW: w := -225;
    dirW: w := -180;
    dirNW: w := -135;
    dirN: w := -90;
    dirNE: w := -45;
    dirE: w := 0;
    dirSE: w := -315;
  end;

  SetDirAnim;

  FXSpeed := round(cos(PI/180*w)*Speed);
  FYSpeed := round(sin(PI/180*w)*Speed);
end;

procedure TCharacter.SetDirAnim;
begin
  case FState of
    stStopped:
    begin
      SetAnim('stop');
      AnimLoop := false;
      AnimActive := false;
      AnimPos := ord(FDirection);
    end;
    stWalking:
    begin
      case FDirection of
        dirS: SetAnim('s');
        dirSW: SetAnim('sw');
        dirW: SetAnim('w');
        dirNW: SetAnim('nw');
        dirN: SetAnim('n');
        dirNE: SetAnim('ne');
        dirE: SetAnim('e');
        dirSE: SetAnim('se');
      end;
      AnimLoop := true;
      AnimActive := true;
    end;
    stDie:
    begin
      SetAnim('die');
      AnimLoop := false;
      AnimActive := true;
    end;
    stBorn:
    begin
      SetAnim('born');
      AnimLoop := false;
      AnimActive := true;
    end;
  end;
end;

procedure TCharacter.SetState(AState: TState);
begin
  FState := AState;
  SetDirAnim;
end;

{ TMainCharacter }

constructor TMainCharacter.Create(AParent: TSprite);
begin
  inherited;
end;

procedure TMainCharacter.DoCollision(Sprite: TSprite; var Done: boolean);
begin
  if (Sprite is TWorm) and (TWorm(Sprite).State <> stDie) then
  begin
    with TParticleSprite.Create(Engine) do
    begin
      PartSys.LoadFromFile(path+'splatter.apf');
      Image := Form1.AdImageList.Find('particle');
      Emit(20);
      X := Sprite.X + Sprite.Width / 2;
      Y := Sprite.Y + Sprite.Height / 2;
      Z := Sprite.Z;
    end;
    TWorm(Sprite).Die;
  end;
end;

procedure TMainCharacter.DoMove(TimeGap: double);
begin
  inherited;

  Engine.X := -X + (Engine.SurfaceRect.Right - Width) / 2;
  Engine.Y := -Y + (Engine.SurfaceRect.Bottom - Height) / 2;

  Collision;
end;

procedure TMainCharacter.Init;
begin
  SetState(stStopped);
end;

procedure TMainCharacter.SetKeys(Keys: TKeys);
begin
  if Keys = FKeys then exit;
  
  FKeys := Keys;
  if (kyUp in Keys) then
  begin
    if (kyLeft in Keys) then
    begin
      SetDir(dirNW);
    end else
    if (kyRight in Keys) then
    begin
      SetDir(dirNE);
    end else
      SetDir(dirN);
  end else
  if (kyDown in Keys) then
  begin
    if (kyLeft in Keys) then
    begin
      SetDir(dirSW);
    end else
    if (kyRight in Keys) then
    begin
      SetDir(dirSE);
    end else
      SetDir(dirS);
  end else
  if kyLeft in Keys then
  begin
    SetDir(dirW);
  end else
  if kyRight in Keys then
  begin
    SetDir(dirE);
  end;

  if Keys = [] then
  begin
    SetState(stStopped);
  end
  else
  begin
    SetState(stWalking);
  end;
end;

{ TWorm }

constructor TWorm.Create(AParent: TSprite);
begin
  inherited;
  Speed := 50;
  FMaxLifeTime := random(40)+10;
end;

procedure TWorm.Die;
begin
  if State <> stDie then
  begin
    SetState(stDie);
  end;
end;

procedure TWorm.DoMove(TimeGap: double);
var
  d:integer;
begin
  inherited;
  
  if (State = stBorn) and (not AnimActive) then
  begin
    SetState(stWalking);
    SetDir(dirS);
    FNextDirChange := random(2000)+100;
    exit;
  end;

  if (State = stDie) and (not AnimActive) then
  begin
    Dead;
    exit;
  end;

  if (State = stWalking) then
  begin
    FNextDirChange := FNextDirChange - (TimeGap*1000);
    if FNextDirChange < 0 then
    begin
      d := ord(Direction);
      case Random(2) of
        0: d := d - 1;
        1: d := d + 1;
      end;
      if d < 0 then d := 7;
      if d > 7 then d := 0;
      SetDir(TDirection(d));
      FNextDirChange := random(2000)+100;
    end;
  end;

  FLifeTime := FLifeTime + TimeGap;

  if (FLifeTime > FMaxLifeTime) and (State <> stDie) then
  begin
    Die;
    FLifeTime := 0;
  end;
end;

procedure TWorm.Init;
begin
  SetState(stBorn);
end;

end.
