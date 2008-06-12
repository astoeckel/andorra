unit Main;

interface

uses
  Dialogs, SysUtils, Forms, Types, Classes, Graphics,  AdSimpleXML,  
  AdPNG, AdDraws, AdClasses, AdTypes, AdPerformanceCounter, AdSetupDlg, AdSprites,
  AdBitmap;

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
  a := a + WheelDelta / 10;
end;

procedure TForm1.Idle(Sender: TObject; var Done: boolean);
begin
  if AdDraw.CanDraw then
  begin
    AdPerCounter.Calculate;

    AdDraw.ClearSurface(clBlack);

    AdDraw.BeginScene;

    inc(framenr);

    AdDraw.Options := AdDraw.Options + [aoStencilBuffer, aoAlphaMask];

    AdDraw.AdAppl.SetStencilOptions(0, $FFFF, asfAlways);
    AdDraw.AdAppl.SetStencilEvent(asePass, asoIncrement);

    with AdDraw.Canvas do
    begin
      Font := AdDraw.Fonts.GenerateFont('Times New Roman', 32, [afBold]);
      TextOut(mx, my, 'Mask functions using the stencil buffer');
      TextOut(ClientWidth - mx, ClientHeight - my, 'Andorra 2D');
      Circle(mx, mx, 10);
      Circle(my, my, 10);
      Release;
    end;

    AdDraw.AdAppl.SetStencilOptions(0, $FFFF, asfLessThan);
    AdDraw.AdAppl.SetStencilEvent(asePass, asoKeep);

    AdDraw.Options := AdDraw.Options - [aoAlphaMask];

    AdImage.StretchBltAlpha(AdDraw, AdRect(0, 0, ClientWidth, ClientHeight),
      AdRect(0, 0, ClientWidth, ClientHeight), 0, 0, 0, 255);

    AdDraw.Options := AdDraw.Options - [aoStencilBuffer];

    AdImage.Draw(AdDraw,  0, 0, 0);

    AdDraw.EndScene;

    AdDraw.Flip;
  end;
  Done := false;
end;

end.
