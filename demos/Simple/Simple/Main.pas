unit Main;

interface

uses
  Dialogs, SysUtils, Forms, Types, Classes, Graphics,  AdSimpleXML,  
  AdPNG, AdDraws, AdClasses, AdTypes, AdPerformanceCounter, AdSetupDlg, AdSprites,
  AdShader, AdJPEG;

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
    AdShaderSys: TAdShaderSystem;
    AdShader: TAdShaderEffect;
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

      AdDraw.Scene.AmbientColor := Ad_ARGB(255, 255, 255, 255);

      AdShaderSys := TAdShaderSystem.Create(AdDraw);

      AdShader := TAdShaderEffect.Create(AdShaderSys);

      AdShader.FragmentShader.LoadFromFile('bumpmap.cg');
      AdShader.FragmentShader.CompileProgram('cg', 'fragment_shader', astFragment);
      AdShader.VertexShader.LoadFromFile('bumpmap.cg');
      AdShader.VertexShader.CompileProgram('cg', 'vertex_shader', astVertex);

      AdImage := TAdImage.Create(AdDraw);
      AdImage.Texture.LoadGraphicFromFile('texture2.bmp');
      AdImage.Details := 1;
      AdImage.Filter := atLinear;
      AdImage.Restore;

      AdTexture := TAdTexture.Create(AdDraw);
      AdTexture.LoadGraphicFromFile('normal.bmp', false);

      AdShader.BindToObject(AdImage);
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
  AdTexture.Free;
  AdShader.Free;
  AdShaderSys.Free;
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
var
  lightpos: TAdVector3;
  px, py: integer;
begin
  if AdDraw.CanDraw then
  begin
    AdPerCounter.Calculate;

    AdDraw.ClearSurface(clBlack);

    AdDraw.BeginScene;

    px := (ClientWidth - AdImage.Width) div 2;
    py := (ClientHeight - AdImage.Height) div 2;

    lightpos := AdVector3(mx - px, my - py, -50);

    AdShader.FragmentShader.SetParameter('colormap', AdImage.Texture.Texture);
    AdShader.FragmentShader.SetParameter('normalmap', AdTexture.Texture);
    AdShader.FragmentShader.SetParameter('lightintensity', 0.5);
    AdShader.FragmentShader.SetParameter('ambientcolor', Ad_ARGB(255, 0, 0, 0));

    AdShader.VertexShader.SetParameter('lightpos', lightpos);
    AdShader.VertexShader.SetParameter('lightcolor', Ad_ARGB(255, 255, 255, 255));

    AdImage.Draw(AdDraw, px, py, 0);

    AdDraw.EndScene;

    AdDraw.Flip;

    Caption := 'FPS: ' + IntToStr(AdPerCounter.FPS);
  end;
  Done := false;
end;

end.
