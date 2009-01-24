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
  Windows, Dialogs, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  AdDraws, AdSprites, AdSpriteEngineEx, AdClasses, AdTypes, AdCanvas,
  AdSetupDlg, AdPNG, AdPerformanceCounter, AdEvents;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
    { Private-Deklarationen }
  public
    AdDraw:TAdDraw;
    AdSpriteEngine:TSpriteEngineEx;
    AdImageList:TAdImageList;
    AdPerCounter:TAdPerformanceCounter;
    firsttime:boolean;
    lx,ly:integer;
    procedure Idle(Sender:TObject;var Done:boolean);
    procedure LoadLevel;
    procedure LoadImages;
  end;

  TWall = class(TImageSprite);

  TBall = class(TImageSpriteEx)
    private
      Falling:boolean;
      WillDie:boolean;
    public
      SX,SY:double;
      SourceX,SourceY:integer;
      procedure DoDraw;override;
      constructor Create(AParent:TSprite);override;
      procedure Dead;override;
      procedure DoMove(TimeGap:double);override;
      procedure DoCollision(Sprite:TSprite; var Done:boolean);override;
      procedure Coll;
      procedure Reset;
  end;

var
  Form1: TForm1;

const
  path='./resources/';

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

      //Create an image list
      AdImageList := TAdImageList.Create(AdDraw);
      //Create the sprite engine object
      AdSpriteEngine := TSpriteEngineEx.Create(AdDraw);

      LoadImages;
      LoadLevel;

      //Set the ambient color of the scene. This color is used when lights are
      //turned.
      AdDraw.Scene.AmbientColor := Ad_ARGB(255, 96, 96, 96);

      Randomize;
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

procedure TForm1.Idle(Sender: TObject; var Done: boolean);
begin
  //Draw on the TAdDraw if drawing is possible
  if AdDraw.CanDraw then
  begin
    //Calculate the time difference.
    AdPerCounter.Calculate;

    //Clear the surface of the TAdDraw with black color
    AdDraw.ClearSurface(0);

    AdDraw.BeginScene;

    //Enable lights
    AdDraw.Options := AdDraw.Options + [aoLight];

    //Move all sprites
    AdSpriteEngine.Move(AdPerCounter.TimeGap/1000);
    //Draw them
    AdSpriteEngine.Draw;
    //And free all sprites that were marked as "dead"  
    AdSpriteEngine.Dead;

    //Disable lights
    AdDraw.Options := AdDraw.Options - [aoLight];

    with AdDraw.Canvas do
    begin
      Textout(0,0,'FPS: '+inttostr(AdPerCounter.FPS));
      Textout(0,16,'Use mousewheel to zoom, mousewheel and left mouse button to rotate');
      Release;
    end;

    AdDraw.EndScene;
    AdDraw.Flip;
  end;

  Done := false;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  //Handle key presses (I'm using the Andorra 2D virtual key codes here - if your
  //game is only for windows you can also use the native windows keycodes.
  if key = AVK_LEFT then
    AdSpriteEngine.X := AdSpriteEngine.X + 10;

  if key = AVK_RIGHT then
    AdSpriteEngine.X := AdSpriteEngine.X - 10;

  if key = AVK_UP then
    AdSpriteEngine.Y := AdSpriteEngine.Y + 10;

  if key = AVK_DOWN then
    AdSpriteEngine.Y := AdSpriteEngine.Y - 10;
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: Char);
var i:integer;
begin
  if key = 'r' then
  begin
    for i := 0 to AdSpriteEngine.Items.Count-1 do
    begin
      if AdSpriteEngine.Items[i] is TBall then
      begin
        TBall(AdSpriteEngine.Items[i]).Reset;
      end;
    end;
  end;
end;

procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  p:TAdPoint;
  lst: TSpriteList;
begin
  p := AdSpriteEngine.ScreenPointToSpriteCoords(AdPoint(X,Y));
  lx := p.x;
  ly := p.y;

  lst := TSpriteList.Create;
  AdSpriteEngine.GetSpritesAt(
    round(lx),
    round(ly), lst);
  if lst.Count > 0 then
  begin
    lst[0].Dead;
  end;
  lst.Free;
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  p:TAdPoint;
begin
  if ssLeft in Shift then
  begin
    p := AdSpriteEngine.ScreenPointToSpriteCoords(AdPoint(X,Y));
    AdSpriteEngine.X := AdSpriteEngine.X + p.x - lx;
    AdSpriteEngine.Y := AdSpriteEngine.Y + p.y - ly;
    Lx := p.x;
    Ly := p.y;
  end;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if ssLeft in Shift then
  begin
    AdSpriteEngine.Rotation := AdSpriteEngine.Rotation + WheelDelta / 5000;
  end
  else
  begin
    AdSpriteEngine.Zoom := AdSpriteEngine.Zoom + WheelDelta / (1000 / AdSpriteEngine.Zoom);
  end;
end;

procedure TForm1.LoadImages;
begin
  with AdImageList.Add('wall')do
  begin
    Texture.LoadGraphicFromFile(path+'texture.png', false, clWhite);
  end;
  with AdImageList.Add('wallgras')do
  begin
    Texture.LoadGraphicFromFile(path+'texture2.png', false, clWhite);
  end;
  with AdImageList.Add('ball') do
  begin
    Texture.LoadGraphicFromFile(path+'ball.png', true, clYellow);
    PatternWidth := 32;
    PatternHeight := 32;
  end;
  with AdImageList.Add('stars')do
  begin
    Texture.LoadGraphicFromFile(path+'stars.png', false, clWhite);
  end;

  AdImageList.Filter := atLinear;

  AdImageList.Restore;
end;

procedure TForm1.LoadLevel;
var
  level:TStringList;
  ax,ay:integer;
begin
  level := TStringList.Create;
  level.LoadFromFile(path+'level2.txt');

  with TBackgroundSprite.Create(AdSpriteEngine) do
  begin
    Image := AdImageList.Find('stars');
    z := -1;
    Depth := 10;
  end;

  for ay := 0 to level.Count - 1 do
  begin
    for ax := 1 to length(level[ay]) do
    begin
      case level[ay][ax] of
        'x':
        begin
          with TWall.Create(AdSpriteEngine) do
          begin
            Image := AdImageList.Find('wall');
            x := ax*128;
            y := ay*128;
            z := 0;
          end;
        end;
        'X':
        begin
          with TWall.Create(AdSpriteEngine) do
          begin
            Image := AdImageList.Find('wallgras');
            x := ax*128;
            y := ay*128;
            z := 0;
          end;
        end;
        'b':
        begin
          with TBall.Create(AdSpriteEngine) do
          begin
            Image := AdImageList.Find('ball');
            x := ax*128;
            y := ay*128+128-height;
            z := 2;
            sourcex := round(x);
            sourcey := round(y);
            z := 1;
           end;
        end;
      end;
    end;
  end;
  level.Free;
end;

{ TBall }

procedure TBall.Coll;
begin
  WillDie := true;
  with TBall.Create(Engine) do
  begin
    Image := self.Image;
    x := self.sourcex;
    y := self.sourcey;
    sourcex := round(x);
    sourcey := round(y);
    Color := self.Color;
  end;
  CanDoCollisions := false;
end;

constructor TBall.Create(AParent: TSprite);
begin
  inherited Create(AParent);

  sy := 200;
  if random(2) = 0 then sx := -200 else sx := 200;

  AnimSpeed := 4;

  Alpha := 0;

  Color := RGB(random(255),random(255),random(255));

  z := 1;
end;


procedure TBall.Dead;
begin
  //Light.Dead;
  inherited Dead;
end;

procedure TBall.DoCollision(Sprite: TSprite; var Done: boolean);
begin
  if Sprite is TWall then
  begin
    falling := false;
    SY := 128;
    if (Sprite.Y > Y) and (Sprite.X > X) and (Sprite.X+Sprite.Width < Y+Width) and
       (Sprite.Y+Sprite.Height < Y+Height) then
    begin
      Coll;
      Done := true;
      exit;
    end;

    if Sprite.Y > Y then
    begin
      Y := Sprite.Y-Height+1;
    end
    else
    begin
      if (Sprite.X+Sprite.Width > X) and (SX < 0) then
      begin
        SX := -SX;
        X := Sprite.X+Sprite.Width+1;
        exit;
      end;
      if (Sprite.X < X+Width) and (SX > 0) then
      begin
        SX := -SX;
        X := Sprite.X-Width-1;
        exit;
      end;
    end;   
  end;
  if Sprite is TBall then
  begin
    Coll;
    TBall(Sprite).Coll;
    Done := true;
  end;
end;

procedure TBall.DoDraw;
var
  old: TAndorraColor;
begin
  with Engine.Surface.Canvas do
  begin
    Brush.BlendMode := bmAdd;

    Brush.Color := Ad_ARGB(round(Alpha/2),255,255,255);
    Brush.GradientColor := Ad_ARGB(0,255,255,255);
    Pen.Style := apNone;
    Circle(round(BoundsRect.Left+Width/2),round(BoundsRect.Top+Height/2),300);
    Release;

    Brush.BlendMode := bmAlpha;
  end;
  Image.Color := Color;
  old := Engine.Surface.Scene.AmbientColor;
  Engine.Surface.Scene.AmbientColor := Ad_ARGB(255, 255, 255, 255);
  inherited DoDraw;
  Engine.Surface.Scene.AmbientColor := old;
end;

procedure TBall.DoMove(TimeGap: double);
begin
  inherited DoMove(TimeGap);
  if not WillDie then
  begin
    if Alpha < 255 then
    begin
      Alpha := Alpha + 1000*TimeGap;
    end
    else
    begin
      Alpha := 255;
    end;

    falling := true;
    Collision;

    if falling then
    begin
      SY := SY + SY * 0.1*TimeGap;
      Y := Y + SY*TimeGap;
    end
    else
    begin
      Angle := Angle + 360*(SX/abs(SX))*TimeGap;
      if Angle > 360 then Angle := 0;
      X := X + SX*TimeGap;
    end;
  end
  else
  begin
    Alpha := Alpha - 1000*TimeGap;
    if Alpha < 0 then Alpha := 0;    
    if Alpha <= 20 then Dead;
  end;
end;

procedure TBall.Reset;
begin
  X := sourcex;
  Y := sourcey;
end;

end.

