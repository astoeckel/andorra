unit Main;

interface

uses
  Dialogs, SysUtils, Forms, Types, Classes, Graphics,  AdSimpleXML,  
  AdPNG, AdDraws, AdClasses, AdTypes, AdPerformanceCounter, AdSetupDlg, AdSprites,
  AdBitmap, AdCanvas;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
    { Private-Deklarationen }
  public
    AdDraw:TAdDraw;
    AdPerCounter:TAdPerformanceCounter;
    AdImage: TAdImage;
    mx, my: integer;
    a: double;
    framenr: integer;

    procedure Idle(Sender: TObject; var Done: boolean);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  AdSetup: TAdSetup;
begin
  ReportMemoryLeaksOnShutdown := true;

  AdPerCounter := TAdPerformanceCounter.Create;

  AdDraw := TAdDraw.Create(self);

  AdSetup := TAdSetup.Create(AdDraw);
  AdSetup.Image := 'logo1.png';
  if AdSetup.Execute then
  begin
    if AdDraw.Initialize then
    begin
      Application.OnIdle := Idle;

      AdDraw.Scene.AmbientColor := Ad_ARGB(255, 255, 255, 255);

      AdImage := TAdImage.Create(AdDraw);
      AdImage.Texture.LoadGraphicFromFile('icon64.png');
      AdImage.Restore;
    end
    else
    begin
      ShowMessage('Error while initializing Andorra 2D. Try to use another display '+
                'mode or another video adapter.');
      halt;
    end;
  end else
  begin
    AdSetup.Free;
    halt;
  end;
  AdSetup.Free;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  AdImage.Free;
  AdPerCounter.Free;
  AdDraw.Free;
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  mx := x;
  my := y;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  a := a + WheelDelta / 50;
end;

procedure TForm1.Idle(Sender: TObject; var Done: boolean);
begin
  if AdDraw.CanDraw then
  begin
    AdPerCounter.Calculate;

    AdDraw.ClearSurface(clWhite);

    AdDraw.BeginScene;

    with AdDraw.Canvas do
    begin
      Brush.BlendMode := bmSub;
      Pen.Style := apNone;

      Brush.Color := Ad_ARGB(255, 0, 0, 255);
      Brush.GradientColor := Ad_ARGB(0, 0, 0, 255);
      Circle(
        round(mx + cos(0) * a),
        round(my + sin(0) * a), 90);

      Brush.Color := Ad_ARGB(255, 0, 255, 0);
      Brush.GradientColor := Ad_ARGB(0, 0, 255, 0);
      Circle(
        round(mx + cos(2) * a),
        round(my + sin(2) * a), 90);

      Brush.Color := Ad_ARGB(255, 255, 0, 0);
      Brush.GradientColor := Ad_ARGB(0, 255, 0, 0);
      Circle(
        round(mx + cos(4) * a),
        round(my + sin(4) * a), 90);

      Release;
    end;

    AdDraw.EndScene;

    AdDraw.Flip;
  end;
  Done := false;
end;

end.

