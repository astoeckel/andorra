unit Main;

interface

uses
  Dialogs, SysUtils, Forms, Types, Classes, Graphics, AdSimpleXML,
  AdPNG, AdDraws, AdClasses, AdTypes, AdPerformanceCounter, AdSetupDlg, ExtCtrls;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    AdDraw:TAdDraw;
    AdPerCounter:TAdPerformanceCounter;
    procedure Idle(Sender: TObject; var Done: boolean);
  end;

var
  Form1: TForm1;

implementation

uses AdCanvas;

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  AdSetup: TAdSetup;
begin
  //Only for debuging - you can remove this line
  ReportMemoryLeaksOnShutdown := true;

  //Create the performance counter. This class is used for measuring the time
  //that passed between two frames.
  AdPerCounter := TAdPerformanceCounter.Create;

  //Crate the main surface.
  AdDraw := TAdDraw.Create(self);

  //Create the setup dialog and pass the main surface
  AdSetup := TAdSetup.Create(AdDraw);
  AdSetup.Image := 'logo1.png';

  if AdSetup.Execute then
  begin
    if AdDraw.Initialize then
    begin
      Application.OnIdle := Idle;
    end
    else
    begin
      ShowMessage(AdDraw.GetLastError);
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
  AdPerCounter.Free;
  AdDraw.Free;
end;

procedure TForm1.Idle(Sender: TObject; var Done: boolean);
begin
  if AdDraw.CanDraw then
  begin
    AdPerCounter.Calculate;

    AdDraw.ClearSurface(clBlack);

    AdDraw.BeginScene;

    with AdDraw.Canvas do
    begin
      Pen.Width := 1;
      Circle(50, 50, 10);
      Textout(0, 0, IntToStr(AdPerCounter.FPS));
      Textout(0, 16, FloatToStr(AdPerCounter.TimeGap));
    end;

    AdDraw.EndScene;

    AdDraw.Flip;
  end;
  Done := false;
end;

end.

