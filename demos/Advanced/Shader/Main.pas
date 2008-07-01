unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, AdDraws, AdSetupDlg, AdTypes, AdClasses, AdShader, AdPNG, AdJPEG;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  private
    { Private-Deklarationen }
  public
    mx, my: integer;
    AdDraw: TAdDraw;
    AdShaderSys: TAdShaderSystem;
    AdShader: TAdShaderEffect;
    AdTextureImage, AdNormalImage: TAdImage;
    procedure Idle(Sender: TObject; var Done:boolean);
  end;

var
  Form1: TForm1;

const
  path = './resources/';

implementation

uses AdCanvas;

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  AdSetup: TAdSetup;
begin
  AdDraw := TAdDraw.Create(self);
  AdSetup := TAdSetup.Create(AdDraw);
  AdSetup.Image := 'logo1.png';
  if AdSetup.Execute then
  begin
    if AdDraw.Initialize then
    begin
      Application.OnIdle := Idle;

      AdShaderSys := TAdShaderSystem.Create(AdDraw);
      AdShader := TAdShaderEffect.Create(AdShaderSys);

      AdShader.VertexShader.LoadFromFile(path + 'bumpmap.cg');
      AdShader.VertexShader.CompileProgram('cg', 'vertex_shader', astVertex);

      AdShader.FragmentShader.LoadFromFile(path + 'bumpmap.cg');
      AdShader.FragmentShader.CompileProgram('cg', 'fragment_shader', astFragment);

      AdNormalImage := TAdImage.Create(AdDraw);
      AdNormalImage.Texture.LoadGraphicFromFile(path + 'normalmap.png');
      AdNormalImage.Restore;

      AdTextureImage := TAdImage.Create(AdDraw);
      AdTextureImage.Texture.LoadGraphicFromFile(path + 'colormap.png');
      AdTextureImage.Details := 8;
      AdTextureImage.Restore;
    end else
    begin
      AdDraw.Free;
      AdSetup.Free;
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
  AdTextureImage.Free;
  AdNormalImage.Free;
  AdShader.Free;
  AdShaderSys.Free;
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
begin
  if AdDraw.CanDraw then
  begin
    AdDraw.ClearSurface(clBlack);
    AdDraw.BeginScene;

    lightpos := AdVector3(mx, 512 - my, 25);

    AdShader.VertexShader.SetParameter('lightpos', lightpos);
    AdShader.VertexShader.SetParameter('lightcolor', Ad_ARGB(255, 255, 255, 255));

    AdShader.FragmentShader.SetParameter('colormap', AdTextureImage.Texture.Texture);
    AdShader.FragmentShader.SetParameter('normalmap', AdNormalImage.Texture.Texture);
    AdShader.FragmentShader.SetParameter('lightintensity', 1);
    AdShader.FragmentShader.SetParameter('ambientcolor', Ad_ARGB(255, 64, 64, 64));

    AdShader.BindToObject(AdTextureImage);
    AdTextureImage.Draw(AdDraw, 0, 0, 0);

    AdDraw.EndScene;
    AdDraw.Flip;
  end;

  Done := false;
end;

end.
