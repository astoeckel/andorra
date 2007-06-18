unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, AdDraws, AdClasses, AdSprites, AdPng;

type
  TAdTestSprite = class(TImageSprite)
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

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  private
    { Private-Deklarationen }
  public
    AdDraw:TAdDraw;
    AdSpriteEngine:TSpriteEngine;
    AdPercounter:TPerformanceCounter;
    AdImageList:TAdImageList;
    Sprite:TImageSprite;
    procedure Idle(Sender:TObject; var Done:boolean);
  end;

var
  Form1: TForm1;

const
  path = '../demos/SpriteEngine/PixelCheck/';

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  i:integer;
begin
  AdDraw := TAdDraw.Create(self);
  AdDraw.DllName := 'AndorraDX93D.dll';

  Cursor := crNone;

  if AdDraw.Initialize then
  begin
    AdImageList := TAdImageList.Create(AdDraw);
    AdImageList.LoadFromFile(path+'misc.ail');

    AdPerCounter := TPerformanceCounter.Create;
    AdSpriteEngine := TSpriteEngine.Create(AdDraw);

    Randomize;

    for i := 0 to 20 do
    begin
      with TAdTestSprite.Create(AdSpriteEngine) do
      begin
        Image := AdImageList.Items[random(AdImageList.Count)];
        X := random(round(ClientWidth - Width));
        Y := random(round(ClientHeight - Height));
        PixelCheck := true;
      end;
    end;

    Sprite := TAdCursorSprite.Create(AdSpriteEngine);
    with Sprite do
    begin
      Image := AdImageList.Items[2];
      z := 1;
      PixelCheck := true;
    end;

    Application.OnIdle := Idle;
  end
  else
  begin
    ShowMessage('Andorra 2D couldn''t be initialized. Please try another display mode or adapter');
    halt;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  AdSpriteEngine.Free;
  AdPerCounter.Free;
  AdImageList.Free;
  AdDraw.Free;
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  Sprite.X := X + Sprite.Width / 2;
  Sprite.Y := Y + Sprite.Height / 2;
end;

procedure TForm1.Idle(Sender: TObject; var Done: boolean);
begin
  AdPercounter.Calculate;

  AdDraw.ClearSurface(clBlack);
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
    Image.Color := rgb(200,200,200);
  inherited;
  Image.Color := rgb(255,255,255);
end;

procedure TAdTestSprite.DoMove(timegap: double);
begin
  inherited;

  col := false;
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
  collision;
end;

end.
