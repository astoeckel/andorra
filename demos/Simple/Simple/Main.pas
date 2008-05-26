unit Main;

interface

uses
  Dialogs, SysUtils, Forms, Types, Classes, Graphics,  AdSimpleXML,  
  AdPNG, AdDraws, AdClasses, AdTypes, AdPerformanceCounter, AdSetupDlg, AdSprites;

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
    AdLight: TAd2dLight;

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
  data: TAd2dLightData;
begin
  ReportMemoryLeaksOnShutdown := true;

  AdPerCounter := TAdPerformanceCounter.Create;

  AdDraw := TAdDraw.Create(self);

  AdDraw.Options := AdDraw.Options + [aoLight];

  AdSetup := TAdSetup.Create(AdDraw);
  AdSetup.Image := 'logo1.png';
  if AdSetup.Execute then
  begin
    if AdDraw.Initialize then
    begin
      Application.OnIdle := Idle;

      AdDraw.Scene.AmbientColor := AD_ARGB(255, 255, 255, 255);

      AdImage := TAdImage.Create(AdDraw);
      AdImage.Texture.LoadGraphicFromFile('water.bmp');
      AdImage.Details := 32;
      AdImage.Filter := atLinear;
      AdImage.Restore;

      AdLight := AdDraw.AdAppl.CreateLight;
      {with data do
      begin
        LightType := altPoint;
        Diffuse := AD_ARGB(255, 255, 255, 255);
        Specular := AD_ARGB(0, 0, 0, 0);
        Ambient := AD_ARGB(0, 255, 255, 255);
        Position := AdVector3(ClientWidth / 2, ClientHeight / 2, 0);
        Range := 100;
        ConstantAttenuation := 1;
        LinearAttenuation := 0.005;
        QuadraticAttenuation := 0.005;
      end;
      AdLight.Data := data;

      AdLight.EnableLight(0);}
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
  AdLight.Free;
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

    AdImage.StretchDraw(AdDraw, AdRect(0, 0, ClientWidth, ClientHeight), 0);

    AdDraw.EndScene;

    AdDraw.Flip;

    Caption := 'FPS: ' + IntToStr(AdPerCounter.FPS);
  end;
  Done := false;
end;

end.
