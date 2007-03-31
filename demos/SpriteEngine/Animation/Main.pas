unit Main;

interface

uses
  Windows, Dialogs, SysUtils, Graphics, Classes, Forms, AdDraws, AdClasses,
  Controls, ExtCtrls, AdSprites;

type
  TFigur = class(TImageSprite)
    private
    protected
      procedure DoMove(TimeGap:double);override;
    public
      XSpeed:integer;
      constructor Create(AParent:TSprite);override;
      procedure SetLine;
  end;

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    AdDraw1:TAdDraw;
    AdPerCounter:TPerformanceCounter;
    AdImageList1:TPictureCollection;
    AdSpriteEngine:TSpriteEngine;
    procedure Idle(Sender:TObject;var Done:boolean);
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

const
  path = '..\demos\Simple\Animation\';

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var i:integer;
begin
  ReportMemoryLeaksOnShutdown := true;
  AdPerCounter := TPerformanceCounter.Create;

  AdDraw1 := TAdDraw.Create(self);
  AdDraw1.DllName := 'AndorraDX93D.dll';

  if AdDraw1.Initialize then
  begin
    Application.OnIdle := Idle;

    AdImageList1 := TPictureCollection.Create(AdDraw1);
    with AdImageList1.Add('figur') do
    begin
      Texture.LoadGraphicFromFile(path+'boy.bmp',true,clFuchsia);
      PatternWidth := 96;
      PatternHeight := 96;
    end;
    AdImageList1.Restore;

    AdSpriteEngine := TSpriteEngine.Create(nil);
    AdSpriteEngine.Surface := AdDraw1;

    Randomize;

    for i := 0 to 5 do
    begin
      with TFigur.Create(AdSpriteEngine) do
      begin
        Image := AdImageList1.Find('figur');
        AnimActive := true;
        AnimLoop := true;
        AnimSpeed := 15;
        XSpeed := -(random(100)+50);
        SetLine;
      end;
    end;

  end
  else
  begin
    ShowMessage('Error while initializing Andorra 2D. Try to use another display '+
                'mode or another video adapter.');
    Close;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  AdSpriteEngine.Free;
  AdImageList1.Free;
  AdPerCounter.Free;
  AdDraw1.Free;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  if AdDraw1.Initialized then
  begin
    AdDraw1.Finalize;
    AdDraw1.Initialize;
  end;
end;

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

    Done := false;
  end;
end;

{ TFigur }

constructor TFigur.Create(AParent: TSprite);
begin
  inherited;
  X := 0;
  Y := 0;
  XSpeed := -150;
end;

procedure TFigur.DoMove(TimeGap: double);
begin
  inherited;

  X := X + XSpeed*TimeGap;
  if ((X > Engine.SurfaceRect.Right) and (XSpeed > 0)) or
     ((X < -96) and (XSpeed < 0)) then
  begin
    SetLine;
  end;
end;

procedure TFigur.SetLine;
begin
  XSpeed := -XSpeed;
  if XSpeed > 0 then
  begin
    AnimStart := 0;
    AnimStop := 7;
    X := -96;
  end
  else
  begin
    AnimStart := 8;
    AnimStop := 15;
    X := Engine.SurfaceRect.Right+96;
  end;
  Y := Random(Engine.SurfaceRect.Right-96);
end;

end.
