unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, AdDraws, AdClasses, AdSprites, AdPng, AdSetupDlg, AdPerformanceCounter;

type
  TAdTestSprite = class(TImageSpriteEx)
    protected
      procedure DoMove(timegap:double);override;
      procedure DoDraw;override;
    public
      col:boolean;
  end;

  TAdCursorSprite = class(TImageSprite)
    private
    protected
      procedure DoCollision(Sprite:TSprite; var Done:boolean);override;
      procedure DoMove(timegap:double);override;
  end;

  TMainFrm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  private
    { Private-Deklarationen }
  public
    AdDraw:TAdDraw;
    AdSpriteEngine:TSpriteEngine;
    AdPercounter:TAdPerformanceCounter;
    AdImageList:TAdImageList;
    ColTest: TAdSpritePixelCollisionTester;
    Sprite:TImageSprite;
    procedure Idle(Sender:TObject; var Done:boolean);
  end;

var
  MainFrm: TMainFrm;

const
  path = '../demos/SpriteEngine/PixelCheck/';

implementation

{$R *.dfm}

procedure TMainFrm.FormCreate(Sender: TObject);
var
  i:integer;
  AdSetupDlg:TAdSetup;  
begin
  Cursor := crNone;
  AdPerCounter := TAdPerformanceCounter.Create;

  AdDraw := TAdDraw.Create(self);

  AdSetupDlg := TAdSetup.Create(AdDraw);
  AdSetupDlg.Image := 'logo1.png';

  if AdSetupDlg.Execute then
  begin
    if AdDraw.Initialize then
    begin
      AdImageList := TAdImageList.Create(AdDraw);
      AdImageList.LoadFromFile(path+'misc.ail');

      ColTest := TAdSpritePixelCollisionTester.Create(AdDraw);

      AdSpriteEngine := TSpriteEngine.Create(AdDraw);

      Randomize;

      for i := 0 to 20 do
      begin
        with TAdTestSprite.Create(AdSpriteEngine) do
        begin
          Image := AdImageList.Items[random(AdImageList.Count)];
          X := random(round(ClientWidth - Width));
          Y := random(round(ClientHeight - Height));
          CollisionTester := ColTest;
        end;
      end;

      Sprite := TAdCursorSprite.Create(AdSpriteEngine);
      with Sprite do
      begin
        Image := AdImageList.Items[2];
        z := 1;
        CollisionTester := ColTest;
      end;

      Application.OnIdle := Idle;
    end
    else
    begin
      ShowMessage('Andorra 2D couldn''t be initialized. Please try another display mode or adapter');
      halt;
    end;
  end
  else
  begin
    halt;
  end;
end;

procedure TMainFrm.FormDestroy(Sender: TObject);
begin
  ColTest.Free;
  AdSpriteEngine.Free;
  AdPerCounter.Free;
  AdImageList.Free;
  AdDraw.Free;
end;

procedure TMainFrm.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  Sprite.X := X + Sprite.Width / 2;
  Sprite.Y := Y + Sprite.Height / 2;
end;

procedure TMainFrm.Idle(Sender: TObject; var Done: boolean);
begin
  AdPercounter.Calculate;

  AdDraw.ClearSurface(0);
  AdDraw.BeginScene;

  with AdDraw.Canvas do
  begin
    TextOut(0,0,inttostr(AdPercounter.FPS));
    Release;
  end;

  AdSpriteEngine.Move(AdPercounter.TimeGap/1000);
  AdSpriteEngine.Draw;
  AdSpriteEngine.Dead;

  AdDraw.EndScene;
  AdDraw.Flip;

  Done := false;
end;

{ TAdTestSprite }

procedure TAdTestSprite.DoDraw;
begin
  if not col then
    Color := rgb(200,200,200);
  inherited;
  Color := rgb(255,255,255);
end;

procedure TAdTestSprite.DoMove(timegap: double);
begin
  inherited;  
  col := false;
  Angle := Angle + timegap * 10;
end;

{ TAdCursorSprite }

procedure TAdCursorSprite.DoCollision(Sprite: TSprite; var Done: boolean);
begin
  inherited;
  if Sprite is TAdTestSprite then
  begin
    TAdTestSprite(Sprite).col := true;
  end;
end;

procedure TAdCursorSprite.DoMove(timegap: double);
begin
  inherited;
  Collision;
end;

end.
