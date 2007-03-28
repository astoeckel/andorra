unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, AdDraws, AdClasses, StdCtrls;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  private
    { Private-Deklarationen }
  public
    AdDraw1:TAdDraw;
    AdPerCounter:TPerformanceCounter;
    procedure Idle(Sender:TObject;var Done:boolean);
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;
  mx,my:integer;

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
  AdPerCounter.Free;
  AdDraw1.Free;
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  mx := x;
  my := y;
end;

procedure TForm1.Idle(Sender: TObject; var Done: boolean);
var i:integer;
begin
  if AdDraw1.CanDraw then
  begin
    AdPerCounter.Calculate;
    AdDraw1.ClearSurface(clBlack);
    AdDraw1.BeginScene;
    with AdDraw1.Canvas do
    begin
      Pen.Color := Ad_ARGB(255,0,255,0);
      Brush.Style := abGradientHorizontal;
      Brush.Color := Ad_ARGB(255,255,0,0);
      Brush.GradientColor := Ad_ARGB(255,0,0,255);
      Circle(50,50,100);
      Rectangle(100,100,200,200);
      Pen.Style := apNone;
      BlendMode := bmAdd;
      Brush.Style := abGradientHorizontal;
      Brush.Color := Ad_ARGB(255,255,255,255);
      Brush.GradientColor := Ad_ARGB(0,0,0,0);
      Circle(mx,my,100);
      BlendMode := bmAlpha;
      Release;
      Pen.Style := apSolid;
      Textout(0,0,Inttostr(AdPerCounter.FPS));
    end;
    AdDraw1.EndScene;
    AdDraw1.Flip; 
  end;
  Done := false;
end;

end.
