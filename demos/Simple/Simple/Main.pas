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
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
    { Private-Deklarationen }
  public
    AdDraw:TAdDraw;
    AdPerCounter:TAdPerformanceCounter;
    AdImage: TAdImage;
    AdShaderSys: TAdShaderSystem;
    AdShader: TAdShaderEffect;
    mx, my: integer;
    a: double;

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

      AdShader.FragmentShader.LoadFromFile('bloom.cg');
      AdShader.FragmentShader.CompileProgram('cg', 'fragment_shader', astFragment);
      AdShader.VertexShader.LoadFromFile('bloom.cg');
      AdShader.VertexShader.CompileProgram('cg', 'vertex_shader', astVertex);

      AdImage := TAdImage.Create(AdDraw);
      AdImage.Texture.LoadGraphicFromFile('texture2.bmp',false);
      AdImage.Texture.Filter := atLinear;
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

    AdShader.FragmentShader.SetParameter('bloompower', 2);
    AdShader.FragmentShader.SetParameter('bloomalpha', a / 255);
    AdShader.FragmentShader.SetParameter('texture', AdImage.Texture.Texture);

    AdShader.BindToObject(AdImage);
    AdImage.Draw(AdDraw, mx, my, 0);
    AdShader.Unbind;

    AdImage.Draw(AdDraw, 0, 0, 0);

    with AdDraw.Canvas do
    begin
      TextOut(0, 0, 'HALLO!');
    end;

    AdDraw.EndScene;

    AdDraw.Flip;

    Caption := 'FPS: ' + FloatToStr(AdPerCounter.FPS);
  end;
  Done := false;
end;

end.
