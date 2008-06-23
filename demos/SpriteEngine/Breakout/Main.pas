unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, AdDraws, AdSprites, AdSpriteEngineEx, AdParticles, AdClasses, AdPng, AdSetupDlg,
  AdPerformanceCounter, AdTypes, AdBitmap, AdCanvas;

type
  TBrickSprite = class(TImageSpriteEx)
    protected
      FWillDie:boolean;
    public
      procedure DoMove(TimeGap:double);override;
      procedure Die;
  end;

  TBall = class(TImageSpriteEx)
    public
      XDir,YDir:double;
      Speed:double;
      constructor Create(AParent:TSprite);override;
      destructor Destroy;override;
      procedure ChangeDir(top:boolean);
      procedure DoMove(TimeGap:double);override;
      procedure DoCollision(Sprite:TSprite; var done:boolean);override;
  end;

  TBat = class (TImageSprite)
    protected
      FDestX:integer;
      FSpeed:double;
    public
      constructor Create(AParent:TSprite);override;
      procedure MoveTo(AX:integer);
      procedure DoMove(TimeGap:double);override;
  end;

type
  TMainDlg = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  private
    { Private-Deklarationen }
  public
    AdDraw:TAdDraw;
    AdImgLst:TAdImageList;
    AdSpriteEngine:TSpriteEngine;
    AdPerCounter:TAdPerformanceCounter;
    Bat:TBat;
    CamPos:TAdVector3;
    VX,VY,VZ:integer;
    procedure CreateLevel;
    procedure Idle(Sender:TObject; var done:boolean);
  end;

var
  MainDlg: TMainDlg;

const
  path = './resources/';

implementation

{$R *.dfm}

procedure TMainDlg.CreateLevel;
var
  ax,ay:integer;
  c, w: integer;
begin
  c := ClientWidth div AdImgLst.Find('bricks').Width; 
  w := AdImgLst.Find('bricks').Width;
  w := round(w * (ClientWidth / ((c) * w)));
  for ax := 0 to c - 1 do
  begin
    for ay := 0 to 7 do
    begin
      with TBrickSprite.Create(AdSpriteEngine) do
      begin
        Image := AdImgLst.Find('bricks');
        Width := w;
        x := ax * w;
        y := ay * 20;
        case random(5) of
          0 :  Color := RGB(255,128,128);
          1 :  Color := RGB(128,128,255);
          2 :  Color := RGB(128,255,128);
          3 :  Color := RGB(255,200,128);
          4 :  Color := RGB(128,200,255);
        end;
      end;
    end;
  end;
end;

procedure TMainDlg.FormCreate(Sender: TObject);
var
  AdSetupDlg:TAdSetup;
begin
  AdPerCounter := TAdPerformanceCounter.Create;

  AdDraw := TAdDraw.Create(self);

  AdSetupDlg := TAdSetup.Create(AdDraw);
  AdSetupDlg.Image := 'logo1.png';
  AdSetupDlg.Sections := AdSetupDlg.Sections;

  if AdSetupDlg.Execute then
  begin
    Cursor := crNone;
    if AdDraw.Initialize then
    begin
      AdImgLst := TAdImageList.Create(AdDraw);

      with AdImgLst.Add('bricks') do
      begin
        Texture.LoadGraphicFromFile(path+'brick1.png', false, clNone);
      end;

      with AdImgLst.Add('ball') do
      begin
        Texture.LoadGraphicFromFile(path+'small_ball.png', true, clFuchsia);
      end;

      with AdImgLst.Add('bat') do
      begin
        Texture.LoadGraphicFromFile(path+'bat.png', false, clNone);
      end;

      AdImgLst.Restore;

      AdSpriteEngine := TSpriteEngine.Create(nil);
      AdSpriteEngine.Surface := AdDraw;

      Randomize;
      CreateLevel; 

      with TBall.Create(AdSpriteEngine) do
      begin
        Image := AdImglst.Find('ball');
        X := 294;
        Y := 295;
      end;

      Bat := TBat.Create(AdSpriteEngine);
      with Bat do
      begin
        Image := AdImglst.Find('bat');
        Y := ClientHeight-Height;
        X := (ClientWidth-Width) / 2;
      end;

      AdPerCounter := TAdPerformanceCounter.Create;

      Application.OnIdle := Idle;

      CamPos := AdVector3(ClientWidth / 2,ClientHeight / 2, -(ClientHeight * 1.3));
      VX := 100;
      VY := 100;
      VZ := 20;
    end
    else
    begin
      ShowMessage('Andorra 2D could not be initialized.');
      halt;
    end;
  end
  else
  begin
    halt;
  end;
end;

procedure TMainDlg.FormDestroy(Sender: TObject);
begin
  AdPerCounter.Free;
  AdSpriteEngine.Free;
  AdImgLst.Free;
  AdDraw.Free;
end;

procedure TMainDlg.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  Bat.MoveTo(X);
end;

procedure TMainDlg.Idle(Sender: TObject; var done: boolean);
begin
  AdPerCounter.Calculate;

  AdDraw.BeginScene;
  AdDraw.ClearSurface(clBlack);

  CamPos.x := CamPos.x + VX * AdPerCounter.TimeGap / 1000;
  CamPos.y := CamPos.y + VY * AdPerCounter.TimeGap / 1000;
  CamPos.z := CamPos.z - VZ * AdPerCounter.TimeGap / 1000;

  if (CamPos.x > ClientWidth) then
  begin
    CamPos.x := ClientWidth;
    VX := -VX;
  end;
  if (CamPos.x < 0) then
  begin
    CamPos.x := 0;
    VX := -VX;
  end;

  if (CamPos.y > ClientHeight) then
  begin
    CamPos.y := ClientHeight;
    VY := -VY;
  end;
  if (CamPos.y < 0) then
  begin
    CamPos.y := 0;
    VY := -VY;
  end;

  if (CamPos.z < - ClientHeight * 1.7) then
  begin
    CamPos.z := -ClientHeight * 1.7;
    VZ := -VZ;
  end;
  if (CamPos.z > -ClientHeight * 1.2) then
  begin
    CamPos.z := -ClientHeight * 1.2;
    VZ := -VZ;
  end;

  AdDraw.AdAppl.Setup3DScene(ClientWidth,ClientHeight,
      CamPos, AdVector3(ClientWidth / 2,ClientHeight / 2, 0), AdVector3(0,-1,0),
      -100, 100);

  AdSpriteEngine.Move(AdPerCounter.TimeGap/1000);
  AdSpriteEngine.Draw;
  AdSpriteEngine.Dead;

  if AdSpriteEngine.CountOfClass(TBrickSprite) = 0 then
  begin
    CreateLevel;
  end;

  with AdDraw.Canvas do
  begin
    TextOut(2,2,'FPS: '+inttostr(AdPerCounter.FPS));
    Brush.Style := abClear;
    Pen.Color := Ad_ARGB(255,255,255,255);
    Rectangle(AdRect(0,0,ClientWidth,ClientHeight));
    Release;
  end;                

  AdDraw.EndScene;
  AdDraw.Flip;

  Done := false;
end;

{ TBrickSprite }

procedure TBrickSprite.Die;
begin
  FWillDie := true;
  CanDoCollisions := false;
end;

procedure TBrickSprite.DoMove(TimeGap: double);
begin
  inherited;
  if FWillDie then
  begin
    Alpha := Alpha - TimeGap * 255;
    if Alpha <= 0 then Dead;
  end;
end;

{ TBall }

procedure TBall.ChangeDir(top:boolean);
begin
  if top then
  begin
    YDir := -YDir;
  end
  else
  begin
    XDir := -XDir;
  end;
end;

constructor TBall.Create(AParent:TSprite);
begin
  inherited;
  XDir := 1;
  YDir := 1;
  Speed := 150;
  Color := RGB(150,150,255);
end;

destructor TBall.Destroy;
begin
  inherited;
end;

procedure TBall.DoCollision(Sprite: TSprite; var done: boolean);
begin
  if Sprite is TBrickSprite then
  begin
    with Sprite as TBrickSprite do
    begin
      Die;
      self.Y := Y + Height;
      Speed := Speed + 5;
    end;
    ChangeDir(true);
  end;
  if Sprite is TBat then
  begin
    ChangeDir(true);
    with Sprite as TBat do
    begin
      self.Y := Y-Height;
      Speed := Speed + 1;
    end;
  end;
  Done := true;
end;

procedure TBall.DoMove(TimeGap: double);
begin
  inherited;
  X := X + XDir * Speed * TimeGap;
  Y := Y + YDir * Speed * TimeGap;

  Collision;

  if X+Width >= Engine.SurfaceRect.Right then
  begin
    ChangeDir(false);
    X := Engine.SurfaceRect.Right - Width;
  end;
  if X <= 0 then
  begin
    ChangeDir(false);
    X := 0;
  end;
  if Y+Height > Engine.SurfaceRect.Bottom then
  begin
    ChangeDir(true);
    Beep;
    Y := (Engine.SurfaceRect.Bottom-Height) /2;
    X := (Engine.SurfaceRect.Right-Width) /2;
  end;
  if Y < 0 then
  begin
    ChangeDir(true);
    Y := 0;
  end;
end;

{ TBar }

constructor TBat.Create(AParent: TSprite);
begin
  inherited;
  FSpeed := 1;
end;

procedure TBat.DoMove(TimeGap: double);
var mx:double;
begin
  inherited;
  mx := (FDestX - X);
  if abs(mx) < 1 then
  begin
    mx := 0;
    FDestX := round(X);
  end;
  X := X + mx*10*TimeGap;
  if X < 0 then X := 0;
  if X+Width > Engine.SurfaceRect.Right then X := Engine.SurfaceRect.Right-Width;
end;

procedure TBat.MoveTo(AX: integer);
begin
  FDestX := round(AX-Width / 2);
end;

end.
