unit Main;

interface

uses
  Windows, Dialogs, SysUtils, Graphics, Classes, Forms, AdDraws, AdClasses,
  Controls, ExtCtrls, AdSprites;

type
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
    Figur:TImageSprite;
    procedure Idle(Sender:TObject;var Done:boolean);
    procedure SetLine;
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;
  XSpeed:single;

const
  path = '..\demos\Simple\Animation\';

implementation

{$R *.dfm}

procedure TForm1.SetLine;
begin
  XSpeed := -XSpeed;
  if XSpeed > 0 then
  begin
    Figur.AnimStart := 0;
    Figur.AnimStop := 7;
    Figur.X := -96;
  end
  else
  begin
    Figur.AnimStart := 8;
    Figur.AnimStop := 15;
    Figur.X := ClientWidth+96;
  end;
  Figur.Y := Random(ClientHeight-96);
end;

procedure TForm1.FormCreate(Sender: TObject);
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

    Figur := TImageSprite.Create(AdSpriteEngine);
    with Figur do
    begin
      Image := AdImageList1.Find('figur');
      AnimActive := true;
      AnimLoop := true;
      AnimSpeed := 15;
    end;              

    XSpeed := -150;

    Randomize;
    SetLine;
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

    Figur.X := Figur.X + XSpeed*AdPerCounter.TimeGap/1000;
    if ((Figur.X > ClientWidth) and (XSpeed > 0)) or
       ((Figur.X < -96) and (XSpeed < 0)) then
    begin
      SetLine;
    end;


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

end.
