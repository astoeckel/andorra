{
* This program is licensed under the Common Public License (CPL) Version 1.0
* You should have recieved a copy of the license with this file.
* If not, see http://www.opensource.org/licenses/cpl1.0.txt for more
* informations.
*
* Inspite of the incompatibility between the Common Public License (CPL) and
* the GNU General Public License (GPL) you're allowed to use this program
* under the GPL.
* You also should have recieved a copy of this license with this file.
* If not, see http://www.gnu.org/licenses/gpl.txt for more informations.
}
unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, AdDraws, AdSprites, AdParticles, AdClasses, AdPNG, AdSetupDlg,
  AdPerformanceCounter, AdTypes, AdConsts, AdBitmap, AdCanvas;

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
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  private
    { Private-Deklarationen }
  public
    AdDraw:TAdDraw;
    AdImageList:TAdImageList;
    AdSpriteEngine:TSpriteEngine;
    AdPerCounter:TAdPerformanceCounter;
    Bat:TBat;
    CamPos:TAdVector3;
    VX,VY,VZ:integer;
    procedure CreateLevel;
    procedure CameraMovement;
    procedure Idle(Sender:TObject; var done:boolean);
  end;

var
  Form1: TForm1;

const
  path = './resources/';

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  AdSetup: TAdSetup;
begin
  //Only for debuging - you can remove this line
  ReportMemoryLeaksOnShutdown := true;

  //Crate the main surface.
  AdDraw := TAdDraw.Create(self);

  //Create the setup dialog and pass the main surface
  AdSetup := TAdSetup.Create(AdDraw);
  AdSetup.Image := 'logo1.png';

  if AdSetup.Execute then
  begin
    //Free the setup dialog
    AdSetup.Free;
    
    //Try to initialize the TAdDraw
    if AdDraw.Initialize then
    begin
      //Create the performance counter. This class is used for measuring the time
      //that passes between two frames.
      AdPerCounter := TAdPerformanceCounter.Create;

      //Connect the on idle event
      AdDraw.Window.Events.OnIdle := Idle;

      //Create the image list and load the used images
      AdImageList := TAdImageList.Create(AdDraw);

      with AdImageList.Add('bricks') do
        Texture.LoadGraphicFromFile(path+'brick1.png', false, clNone);

      with AdImageList.Add('ball') do
        Texture.LoadGraphicFromFile(path+'small_ball.png', true, clFuchsia);

      with AdImageList.Add('bat') do
        Texture.LoadGraphicFromFile(path+'bat.png', false, clNone);

      AdImageList.Restore;

      //Create the sprite engine
      AdSpriteEngine := TSpriteEngine.Create(nil);
      AdSpriteEngine.Surface := AdDraw;

      //Create the first level
      Randomize;
      CreateLevel;


      //Create same basic sprites
      with TBall.Create(AdSpriteEngine) do
      begin
        Image := AdImageList.Find('ball');
        X := ClientWidth div 2;
        Y := ClientHeight div 2;
      end;

      Bat := TBat.Create(AdSpriteEngine);
      with Bat do
      begin
        Image := AdImageList.Find('bat');
        Y := ClientHeight-Height;
        X := (ClientWidth-Width) / 2;
      end;

      //Set a default camera position. This demo uses the 3D-space to create
      //a neat graphical effect.
      CamPos := AdVector3(ClientWidth / 2,ClientHeight / 2, -(ClientHeight * 1.3));

      //Speed of the camera movement
      VX := 100;
      VY := 100;
      VZ := 20;
    end
    else
    begin
      ShowMessage(AdDraw.GetLastError);
      halt;
    end;
  end
  else
  begin
    AdSetup.Free;
    halt;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  AdPerCounter.Free;
  AdSpriteEngine.Free;
  AdImageList.Free;
  AdDraw.Free;
end;

procedure TForm1.CreateLevel;
var
  ax,ay:integer;
  c, w: integer;
begin
  c := ClientWidth div AdImageList.Find('bricks').Width;
  w := AdImageList.Find('bricks').Width;
  w := round(w * (ClientWidth / (c * w)));
  for ax := 0 to c - 1 do
  begin
    for ay := 0 to 7 do
    begin
      with TBrickSprite.Create(AdSpriteEngine) do
      begin
        Image := AdImageList.Find('bricks');
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

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  Bat.MoveTo(X);
end;

procedure TForm1.CameraMovement;
begin
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
end;

procedure TForm1.Idle(Sender: TObject; var done: boolean);
begin
  //Draw on the TAdDraw if drawing is possible
  if AdDraw.CanDraw then
  begin
    //Calculate the time difference.
    AdPerCounter.Calculate;
    
    AdDraw.ClearSurface(AdCol24_Black);

    AdDraw.BeginScene;

    //Recreate the level if there are no brick sprites left
    if AdSpriteEngine.CountOfClass(TBrickSprite) = 0 then
    begin
      CreateLevel;

      //Add a new ball to the game
      with TBall.Create(AdSpriteEngine) do
      begin
        Image := AdImageList.Find('ball');
        X := ClientWidth div 2;
        Y := ClientHeight div 2;
      end;
    end;

    //Set the camera position
    CameraMovement;
    AdDraw.AdAppl.Setup3DScene(ClientWidth,ClientHeight,
        CamPos, AdVector3(ClientWidth / 2,ClientHeight / 2, 0), AdVector3(0,-1,0),
        -100, 100);

    AdSpriteEngine.Move(AdPerCounter.TimeGap/1000);
    AdSpriteEngine.Draw;
    AdSpriteEngine.Dead;

    //Draw a white rectangle around the playground
    with AdDraw.Canvas do
    begin          
      Brush.Style := abClear;
      Pen.Color := AdCol32_White;
      Rectangle(AdRect(0,0,ClientWidth,ClientHeight));
      Release;
    end;

    //Return to a simple 2D scene
    AdDraw.Setup2DScene;

    //Draw the FPS information
    with AdDraw.Canvas do
    begin
      TextOut(2,2,'FPS: '+IntToStr(AdPerCounter.FPS));
      TextOut(2,18,'Bricks left: '+IntToStr(AdSpriteEngine.CountOfClass(TBrickSprite)));
      Release;
    end;            

    AdDraw.EndScene;
    AdDraw.Flip;
  end;

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

{ TBat }

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
