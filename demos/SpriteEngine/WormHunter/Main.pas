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
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, AdDraws, AdClasses, StdCtrls,
  AdPNG, AdTypes, AdSprites, AdParticles, AdPerformanceCounter, AdSetupDlg,
  AdConsts;

type
  //Types that are used to tell the character which key has been pressed
  TKey = (kyUp, kyDown, kyLeft, kyRight);
  TKeys = set of TKey;

  //TDirection is used to describe the current direction of the character
  TDirection = (dirS, dirSW, dirW, dirNW, dirN, dirNE, dirE, dirSE);

  //TState represents the current state of a character
  TState = (stStopped, stWalking, stDie, stBorn);

  TDieCallBack = procedure of object;

  //TAnimation sprite is a base class that manages animations
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

  //TCharacter is a base class that adds normal behaviour to TAnimationSprite
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

  //TMainCharacter represents the default character the player may steer through
  //the game.
  TMainCharacter = class(TCharacter)
    private
      FKeys:TKeys;
      FDefPartSys: TAdParticleSystem;
    protected
      procedure DoMove(TimeGap:double);override;
      procedure DoCollision(Sprite:TSprite; var Done:boolean);override;
    public
      constructor Create(AParent:TSprite);override;      
      procedure Init;
      procedure SetKeys(Keys:TKeys);
      procedure SetDefPartSys(APartSys: TAdParticleSystem);
  end;

  //TWorm represents an enemy character
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
  
  //TDoodad is used for entities spread over the whole map
  TDoodad = class(TImageSprite);

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure Idle(Sender:TObject;var Done:boolean);
    procedure AddWorm;
    procedure FreeObjects;
  public
    AdDraw:TAdDraw;
    AdPerCounter:TAdPerformanceCounter;
    AdImageList:TAdImageList;
    AdSpriteEngine:TSpriteEngine;
    AdPixelCollisionTester: TAdSpritePixelCollisionTester;
    AdSplatterEffect: TAdParticleSystem;

    MainCharacter:TMainCharacter;
    ActTime:double;
  end;

const
  path = './resources/';

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.AddWorm;
begin
  //Create a new worm sprite
  with TWorm.Create(AdSpriteEngine) do
  begin
    //Set the animation list name.
    //This function is provided by the "TAnimationSprite" class
    SetAnims(AdImageList,'worm');

    //Set a random position
    X := Random(5000)-2500;
    Y := Random(5000)-2500;

    //Initialize the worm
    Init;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i:integer;
  AdSetup: TAdSetup;
begin
  //Activate the memory leak reports. You may remove this line if your compiler doesn't support this
  ReportMemoryLeaksOnShutdown := true;
  
  //Initialize the random number generator
  Randomize;

  //Create the TAdDraw
  AdDraw := TAdDraw.Create(self);

  //Show the setup dialog
  AdSetup := TAdSetup.Create(AdDraw);
  AdSetup.Image := 'logo1.png';
  
  if AdSetup.Execute then
  begin
    //Free the setup dialog
    AdSetup.Free;

    if AdDraw.Initialize then
    begin
      //Connect the on idle (draw) event
      AdDraw.Window.Events.OnIdle := Idle;
      //Disable the cursor
      AdDraw.Window.CursorVisible := false;

      //Create the performance counter
      AdPerCounter := TAdPerformanceCounter.Create;
      //Set a maximum frame rate to save CPU power
      AdPerCounter.MaximumFrameRate := 100;

      //Load the imagelist
      AdImageList := TAdImageList.Create(AdDraw);
      AdImageList.LoadFromFile(path+'demo_wormhunter.ail');

      //Create the sprite engine
      AdSpriteEngine := TSpriteEngine.Create(AdDraw);

      //Create the collision tester
      AdPixelCollisionTester := TAdSpritePixelCollisionTester.Create(AdDraw);

      //Create the "blood" splatter particle effect
      AdSplatterEffect := TAdParticleSystem.Create(AdDraw);
      AdSplatterEffect.LoadFromFile(path + 'splatter.apf');
      AdSplatterEffect.Texture := AdImageList.Find('particle').Texture;
      
      //Create a background sprite
      with TBackgroundSprite.Create(AdSpriteEngine) do
      begin
        Image := AdImageList.Find('gras');
        Z := -10000;
      end;

      //Create the main character
      MainCharacter := TMainCharacter.Create(AdSpriteEngine);
      with MainCharacter do
      begin
        SetAnims(AdImageList,'viking');
        SetKeys([]);
        SetDefPartSys(AdSplatterEffect);
        CollisionTester := AdPixelCollisionTester;
        Init;
      end;

      //Create a few doodads and spread them on the map
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
      //Report the last error
      ShowMessage(AdDraw.GetLastError);
      FreeObjects; //Free all created objects
    end;
  end else
  begin
    AdSetup.Free;
    FreeObjects; //Free all created objects
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeObjects;
end;

procedure TForm1.FreeObjects;
begin
  if Assigned(AdSplatterEffect) then
    AdSplatterEffect.Free;
  if Assigned(AdPixelCollisionTester) then
    AdPixelCollisionTester.Free;
  if Assigned(AdSpriteEngine) then
    AdSpriteEngine.Free;
  if Assigned(AdPerCounter) then
    AdPerCounter.Free;
  if Assigned(AdImageList) then
    AdImageList.Free;
  if Assigned(AdDraw) then
    AdDraw.Free;  
end;

procedure TForm1.Idle(Sender: TObject; var Done: boolean);
var
  keys:TKeys;
begin
  if AdDraw.CanDraw then
  begin
    //Calculate the time that has gone by since the last frame
    AdPerCounter.Calculate;

    //Clear the surface with black color
    AdDraw.ClearSurface(AdCol24_Black);

    //Start the scene
    AdDraw.BeginScene;

    //Only perform the following actions every 100ms
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

    //Move all sprites...
    AdSpriteEngine.Move(AdPerCounter.TimeGap / 1000);
    //...draw them...
    AdSpriteEngine.Draw;
    //...and free the sprites that have been marked as dead
    AdSpriteEngine.Dead;

    //Write some information on the canvas
    with AdDraw.Canvas do
    begin
      Textout(0,0,'FPS: '+inttostr(AdPerCounter.FPS));
      Textout(0,20,'Sprites: '+inttostr(AdSpriteEngine.Items.Count));
      Textout(0,40,'TimeGap: '+FormatFloat('#0.000',AdPerCounter.TimeGap));

      Release; //This command makes all canvas command visible on the surface. 
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
  //Set the animation image to the image specified by the name and the stored prefix
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

  //Get the animation image list by storing all images in a list the contain
  //"APrefix_"
  FImages := AList.FindEx(APrefix+'_');
  FPrefix := APrefix;
end;

{ TCharacter }

constructor TCharacter.Create(AParent: TSprite);
begin
  inherited;
  
  //Set some default values
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

  //Move on if the character is currently walking
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
  //Reset the speed variables
  FYSpeed := 0;
  FXSpeed := 0;

  FDirection := ADirection;
  w := 0;

  //Set the movement angle  
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

  //Set the corresponding animation for the direction
  SetDirAnim;

  //Set the X and the Y speed
  FXSpeed := round(cos(PI/180*w)*Speed);
  FYSpeed := round(sin(PI/180*w)*Speed);
end;

procedure TCharacter.SetDirAnim;
begin
  case FState of
    //Set the image to "stop" and don't draw any animation
    stStopped:
    begin
      SetAnim('stop');
      AnimLoop := false;
      AnimActive := false;
      AnimPos := ord(FDirection);
    end;

    //Select the walking animation
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

    //Set the die animation
    stDie:
    begin
      SetAnim('die');
      AnimLoop := false;
      AnimActive := true;
    end;

    //Set the born animatoin
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

  //Set the animation
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
    //Create a splatter effect particle sprite
    with TParticleSprite.Create(Engine) do
    begin
      PartSys.Assign(FDefPartSys);
      Emit(20);
      X := Sprite.X + Sprite.Width / 2;
      Y := Sprite.Y + Sprite.Height / 2;
      Z := Sprite.Z;
    end;

    //Kill the worm
    TWorm(Sprite).Die;
  end;
end;

procedure TMainCharacter.DoMove(TimeGap: double);
begin
  inherited;

  //Center the engine to the main character
  Engine.X := -X + (Engine.SurfaceRect.Right - Width) / 2;
  Engine.Y := -Y + (Engine.SurfaceRect.Bottom - Height) / 2;

  //Check collisions
  Collision;
end;

procedure TMainCharacter.Init;
begin
  SetState(stStopped);
end;

procedure TMainCharacter.SetDefPartSys(APartSys: TAdParticleSystem);
begin
  FDefPartSys := APartSys;
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

  //Set initial values
  Speed := 50;
  FMaxLifeTime := random(40)+10;
end;

procedure TWorm.Die;
begin
  if State <> stDie then
    SetState(stDie);
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

    //Change direction by one step
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
