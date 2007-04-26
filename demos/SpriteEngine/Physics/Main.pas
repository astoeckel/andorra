unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, AdDraws, AdClasses, AdSprites, AdPhysics, AdPng;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private-Deklarationen }
  public
    AdDraw1:TAdDraw;
    AdPerCounter:TPerformanceCounter;
    AdSpriteEngine:TSpriteEngine;
    AdImageList:TAdImageList;
    Selected:TSprite;
    dx,dy:integer;
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
var i:integer;
begin
  AdPerCounter := TPerformanceCounter.Create;

  AdDraw1 := TAdDraw.Create(self);
  AdDraw1.DllName := 'AndorraDX93D.dll';
  AdDraw1.Options := AdDraw1.Options + [doAntialias];
  if AdDraw1.Initialize then
  begin
    Application.OnIdle := Idle;

    AdSpriteEngine := TSpriteEngine.Create(nil);
    AdSpriteEngine.Surface := AdDraw1;

    AdImageList := TAdImageList.Create(AdDraw1);
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

    for i := 0 to 35 do
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
      X := 0;
      Y := 690;
      Image := AdImageList.Find('plate');
      Width := ClientWidth;
      Typ := ptStatic;
      InitializeShape;
    end;

  end
  else
  begin
    ShowMessage('Error while initializing Andorra 2D. Try to use another display'+
                'mode or another video adapter.');
    halt;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  AdImageList.Free;
  AdSpriteEngine.Free;
  AdPerCounter.Free;
  AdDraw1.Free;
end;

//Render
procedure TForm1.Idle(Sender: TObject; var Done: boolean);
begin
  if AdDraw1.CanDraw then
  begin
    AdPerCounter.Calculate;
    Caption := 'FPS:'+inttostr(AdPerCounter.FPS);

    AdDraw1.ClearSurface(clBlack);
    AdDraw1.BeginScene;
    AdSpriteEngine.Move(AdPerCounter.TimeGap / 1000);
    AdSpriteEngine.Draw;
    AdSpriteEngine.Dead;
    AdDraw1.EndScene;
    AdDraw1.Flip;

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
