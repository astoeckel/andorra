unit Main;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  AdSetupDlg, AdDraws, AdClasses, AdPerformanceCounter, AdTypes, AdCanvas,
  AdStdWindow, AdEvents{$IFNDEF FPC}, AdPNG{$ENDIF};

type
  TAdAppl = class
    private
      mx, my: integer;
      procedure MouseMove(Sender: TObject; Shift: TAdShiftState; X, Y: integer);
      procedure Idle(Sender: TObject; var Done: boolean);
      procedure DrawScene(Surface: TAdRenderingSurface);
    public
      AdDraw: TAdDraw;
      AdSurface: TAdTextureSurface;
      AdPerCounter: TAdPerformanceCounter;
      AdImage: TAdImage;
      procedure Run;
  end;

implementation

{ TAdAppl }

procedure TAdAppl.DrawScene(Surface: TAdRenderingSurface);
begin
  with Surface.Canvas do
  begin
    Pen.Style := apNone;
    Brush.Color := Ad_ARGB(255, 0, 0, 255);
    Brush.GradientColor := Ad_ARGB(255, 0, 255, 0);
    Rectangle(0, 0, Surface.Scene.Width, Surface.Scene.Height);
    Textout(0, 64, 'Andorra 2D!!!');
    Release;   
  end;
  AdImage.Draw(Surface, 0, 0, 0);
end;

procedure TAdAppl.Idle(Sender: TObject; var Done: boolean);
begin
  if AdDraw.CanDraw then
  begin
    AdPerCounter.Calculate;

    AdDraw.ClearSurface(0);
    AdDraw.BeginScene;

    AdSurface.ClearSurface(0);

    DrawScene(AdDraw);

    DrawScene(AdSurface);

    AdSurface.Image.Draw(AdDraw, mx, my, 0);

    AdDraw.EndScene;
    AdDraw.Flip;
  end;
end;

procedure TAdAppl.MouseMove(Sender: TObject; Shift: TAdShiftState; X,
  Y: integer);
begin
  mx := x; my := y;
end;

procedure TAdAppl.Run;
var
  AdSetup: TAdSetup;
begin
  AdDraw := TAdDraw.Create(nil);
  AdDraw.Options := AdDraw.Options;
  AdSetup := TAdSetup.Create(AdDraw);
  AdSetup.Image := 'logo1.png';
  
  if AdSetup.Execute then
  begin
    if AdDraw.Initialize then
    begin
      AdPerCounter := TAdPerformanceCounter.Create;

      //Create surface and set size
      AdSurface := TAdTextureSurface.Create(AdDraw);
      AdSurface.SetSize(128, 128);

      //Load image
      AdImage := TAdImage.Create(AdDraw);
      AdImage.Texture.LoadGraphicFromFile('icon64.png');
      AdImage.Restore;

      //Set events
      AdDraw.Window.Events.OnMouseMove := MouseMove;
      AdDraw.Window.Events.OnIdle := Idle;

      //Set window title
      AdDraw.Window.Title := 'Multiple Surface Demo';

      //Run the application
      AdDraw.Run;

      //Finally free all created objects
      AdImage.Free;
      AdSurface.Free;
      AdPerCounter.Free;
    end;
  end;

  AdSetup.Free;
  AdDraw.Free;
end;

end.
