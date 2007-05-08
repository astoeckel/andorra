unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, AdDraws, AdClasses, StdCtrls, AdPNG;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  private
    { Private-Deklarationen }
  public
    AdDraw1:TAdDraw;
    AdPerCounter:TPerformanceCounter;
    AdImage:TAdImage;
    procedure Idle(Sender:TObject;var Done:boolean);
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;
  w,h:integer;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  AdPerCounter := TPerformanceCounter.Create;

  AdDraw1 := TAdDraw.Create(self);
  AdDraw1.DllName := 'AndorraDX93D.dll';
  if AdDraw1.Initialize then
  begin
    Application.OnIdle := Idle;
    AdImage := TAdImage.Create(AdDraw1);
    AdImage.Texture.LoadGraphicFromFile('Icon64.png',true,clNone);
    AdImage.Restore;
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
  AdImage.Free;
  AdPerCounter.Free;
  AdDraw1.Free;
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  w := x;
  h := y;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  //Resize the Backbuffer
  AdDraw1.Setup2DScene;
end;

procedure TForm1.Idle(Sender: TObject; var Done: boolean);
begin
  if AdDraw1.CanDraw then
  begin
    AdPerCounter.Calculate;
    AdDraw1.ClearSurface(clBlack);
    AdDraw1.BeginScene;
    AdImage.Draw(AdDraw1,0,0,0);
    with AdDraw1.Canvas.Font do
    begin
      TextOutEx(Rect(0,0,w,h),'Dies'+#10#13+'ist ein "Test Text", der mit '+#10#13+inttostr(AdPerCounter.FPS)+'FPS gezeichnet wird!',[dtCenter,dtMiddle,dtWordWrap,dtDoLineFeeds])
    end;
    AdDraw1.EndScene;
    AdDraw1.Flip; 
  end;
  Done := false;
end;

end.
