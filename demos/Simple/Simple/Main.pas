unit Main;

interface

uses
  Dialogs, SysUtils, Forms, Types, Classes, Graphics,
  AdCanvas, AdDraws, AdClasses, AdTypes, AdPNG, AdSetupDlg, AdPerformanceCounter,
  AdSprites, AdFont;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  private
    { Private-Deklarationen }
  public
    AdDraw:TAdDraw;
    AdImage:TAdImage;
    AdSurface:TAdTextureSurface;
    AdPerCounter:TAdPerformanceCounter;

    mx, my: integer;

    procedure Idle(Sender: TObject; var Done: boolean);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  AdSetupDlg:TAdSetup;
begin

  ReportMemoryLeaksOnShutdown := true;

  AdPerCounter := TAdPerformanceCounter.Create;

  AdDraw := TAdDraw.Create(self);

  AdSetupDlg := TAdSetup.Create(self);
  AdSetupDlg.Image := 'logo1.png';
  AdSetupDlg.AdDraw := AdDraw;
  AdSetupDlg.Form := self;

  if AdSetupDlg.Execute then
  begin
    if AdDraw.Initialize then
    begin
      Application.OnIdle := Idle;

      AdDraw.TextureFilter := atLinear;

      AdImage := TAdImage.Create(AdDraw);
      AdImage.Texture.LoadGraphicFromFile('icon64.png');
      AdImage.Restore;

      AdSurface := TAdTextureSurface.Create(AdDraw);
      AdSurface.SetSize(256, 256);
    end
    else
    begin
      ShowMessage('Error while initializing Andorra 2D. Try to use another display '+
                  'mode or another video adapter.');
      halt;
    end;
  end
  else
  begin
    halt;
  end;
  AdSetupDlg.Free;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  AdPerCounter.Free;
  AdSurface.Free;
  AdImage.Free;
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

    AdDraw.ClearSurface(0);
    AdDraw.BeginScene;


    AdSurface.ClearSurface(0);

    with AdSurface.Canvas do
    begin
      Pen.Style := apNone;
      Brush.Color := Ad_ARGB(255, 128, 0, 64);
      Brush.GradientColor := Ad_ARGB(255, 255, 0, 255);
      Rectangle(0,0,256,256);
      Release;

      AdImage.Draw(AdSurface, 192,192,0);

      Font := AdDraw.Fonts.GenerateFont('Comic Sans MS',10,[],clBlack,255,1,1);

      with TAdSimpleTypeSetter(Font.TypeSetter) do
      begin
        DrawMode := [dtWordWrap, dtMiddle, dtCenter];
      end;

      Font.TextOut(AdRect(0,0,256,256),
        'Dies ist eine eigene Zeichenoberfläche (Surface) der Klasse ' +
        'TAdTextureSurface. Jede Zeichenoberfläche kann wie ein ' +
        'eigenes TAdDraw verwedet werden. Der Clou an der Sache: ' +
        'selbst TAdDraw ist ein eigenes Surface! ' +
        'Mit der Surfacetechnologie ist es möglich aufwendige Spezialeffekte ' +
        'wie zum Beispiel Motionblur, Bloom oder HDR durchzuführen. '+
        'Außerdem sind Banalitäten, wie zum Beispiel ein virtueller '+
        'Monitor, nun endlich realisierbar.');
    end;

    with AdDraw.Canvas do
    begin
      Pen.Style := apNone;
      Brush.Texture := AdImage.Texture.Texture;
      Brush.TextureMode := tmTile;
      Rectangle(0,0,ClientWidth,ClientHeight);
      Release;

      TextOut(0,0, 'FPS: '+IntToStr(AdPerCounter.FPS));
    end;

    AdSurface.Image.DrawRotate(AdDraw, mx, my, 256, 256, 0, 0.5, 0.5, 64);

    AdDraw.EndScene;
    AdDraw.Flip;
  end;
  Done := false;
end;

end.
