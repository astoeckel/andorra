unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, AdDraws, AdClasses, AdSprites, AdPhysics, AdPng, AdSetupDlg,
  AdPerformanceCounter, AdTypes;

type
  TPlayer = class(TPhysicalCylinderSprite);

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private-Deklarationen }
  public
    AdDraw:TAdDraw;
    AdPerCounter:TAdPerformanceCounter;
    AdSpriteEngine:TSpriteEngine;
    AdImageList:TAdImageList;
    Selected:TSprite;
    dx,dy:integer;
    player:TPlayer;
    procedure Idle(Sender:TObject;var Done:boolean);
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

const
  path = '..\demos\SpriteEngine\Physics\';

implementation

{$R *.dfm}

//Initialization
procedure TForm1.FormCreate(Sender: TObject);
var
  i:integer;
  AdSetupDlg:TAdSetup;
begin
  AdPerCounter := TAdPerformanceCounter.Create;

  AdDraw := TAdDraw.Create(self);

  AdSetupDlg := TAdSetup.Create(self);
  AdSetupDlg.Image := 'logo1.png';
  AdSetupDlg.AdDraw := AdDraw;
  AdSetupDlg.Form := self;
  AdSetupDlg.Sections := AdSetupDlg.Sections - [dlgResolution];

  if AdSetupDlg.Execute then
  begin
    if AdDraw.Initialize then
    begin
      Application.OnIdle := Idle;

      AdSpriteEngine := TSpriteEngine.Create(nil);
      AdSpriteEngine.Surface := AdDraw;

      AdImageList := TAdImageList.Create(AdDraw);
      AdImageList.LoadFromFile(path+'main.ail');
      AdImageList.Restore;

      with TPhysicalApplication.Create(AdSpriteEngine) do
      begin
        SolverModel := smExact;
      end;

      randomize;

      with TBackgroundSprite.Create(AdSpriteEngine) do
      begin
        Image := AdImageList.Find('newton');
        Z := -10;
      end;

      for i := 0 to 25 do
      begin
        case random(2) of
         0:with TPhysicalCylinderSprite.Create(AdSpriteEngine) do
           begin
             X := random(40)+(i mod 5 + 1) * 100;
             Y := i div 4 * 40;
             Image := AdImageList.Find('cylinder');
             Mass := 1;
             InitializeShape;
          end;
         1:with TPhysicalBoxSprite.Create(AdSpriteEngine) do
           begin
             X := random(40)+(i mod 5 + 1) * 100;
             Y := i div 4 * 40;
             Image := AdImageList.Find('box');
             Mass := 10;
             InitializeShape;
          end;      
        end;

      end;

      for i := 0 to 10 do
      begin
        with TPhysicalCylinderSprite.Create(AdSpriteEngine) do
        begin
          X := i * 60 + 32;
          Y := 400 - (i mod 3)*100;
          Image := AdImageList.Find('point');
          Typ := ptStatic;
          InitializeShape;
        end;
      end;

      with TPhysicalBoxSprite.Create(AdSpriteEngine) do
      begin
        Image := AdImageList.Find('plate');
        X := 0;
        Y := ClientHeight - Height;
        Width := ClientWidth;
        Typ := ptStatic;
        InitializeShape;
      end;

      player := TPlayer.Create(AdSpriteEngine);
      with player do
      begin
        X := 200;
        Y := 500;
        Image := AdImageList.Find('cylinder');
        Mass := 20;
        InitializeShape;
      end;

    end
    else
    begin
      ShowMessage('Error while initializing Andorra 2D. Try to use another display'+
                  'mode or another video adapter.');
      halt;
    end;
  end
  else
  begin
    halt;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  AdImageList.Free;
  AdSpriteEngine.Free;
  AdPerCounter.Free;
  AdDraw.Free;
end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  vec:TAdVector3;
begin
  vec := player.Omega;
  if Key = VK_LEFT then
  begin
    vec.z := vec.z - 0.1;
  end;
  if Key = VK_RIGHT then
  begin
    vec.z := vec.z + 0.1;
  end;
  Player.Omega := vec;

  vec := Player.Velocity;

  if Key = VK_UP then
  begin
    vec.y := vec.y - 10;
  end;

  Player.Velocity := vec;
end;

//Render
procedure TForm1.Idle(Sender: TObject; var Done: boolean);
begin
  if AdDraw.CanDraw then
  begin
    AdPerCounter.Calculate;
    Caption := 'FPS:'+inttostr(AdPerCounter.FPS);

    AdDraw.ClearSurface(clBlack);
    AdDraw.BeginScene;
    AdSpriteEngine.Move(AdPerCounter.TimeGap / 1000);
    AdSpriteEngine.Draw;
    AdSpriteEngine.Dead;
    AdDraw.EndScene;
    AdDraw.Flip;

  end;
  Done := false;
end;

//Mouse events for moving the elements
procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Selected := AdSpriteEngine.GetSpriteAt(X,Y);
  if (Selected <> nil) and (Selected is TPhysicalSprite) then
  begin
    dx := round(Selected.WorldX - X);
    dy := round(Selected.WorldY - Y);
    TPhysicalSprite(Selected).ActivateNeighbours;
    TPhysicalSprite(Selected).Active := false;
  end
  else
  begin
    Selected := nil;
  end;
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if Selected <> nil then
  begin
    Selected.X := X + dx;
    Selected.Y := Y + dy;
  end;
end;

procedure TForm1.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Selected <> nil then
  begin
    TPhysicalSprite(Selected).Active := true;
  end;
  Selected := nil;
end;

end.
