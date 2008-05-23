unit Main;

interface

uses
  Dialogs, SysUtils, Forms, Types, Classes, Graphics,  
  AdPNG, AdDraws, AdClasses, AdTypes, AdPerformanceCounter, AdSetupDlg;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  private
    { Private-Deklarationen }
  public
    AdDraw:TAdDraw;
    AdPerCounter:TAdPerformanceCounter;
    AdImage: TAdImage;
    AdTexture: TAdTexture;

    mx, my: integer;

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

  AdDraw.Options := AdDraw.Options;

  AdSetup := TAdSetup.Create(AdDraw);
  AdSetup.Image := 'logo1.png';
  if AdSetup.Execute then
  begin
    if AdDraw.Initialize then
    begin
      Application.OnIdle := Idle;

      AdImage := TAdImage.Create(AdDraw);
      AdImage.Texture.LoadGraphicFromFile('water.bmp');
      AdImage.Details := 16;
      AdImage.Filter := atLinear;
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

procedure TForm1.Idle(Sender: TObject; var Done: boolean);
begin
  if AdDraw.CanDraw then
  begin
    AdPerCounter.Calculate;

    AdDraw.ClearSurface(clBlack);

    AdDraw.BeginScene;

    AdImage.StretchDraw(AdDraw, AdRect(0, 0, mx, my), 0);

    AdDraw.EndScene;

    AdDraw.Flip;

    Caption := 'FPS: ' + IntToStr(AdPerCounter.FPS);
  end;
  Done := false;
end;

end.
