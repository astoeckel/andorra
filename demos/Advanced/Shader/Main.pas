{
* This program is licensed under the Common Public License (CPL) Version 1.0
* You should have recieved a copy of the license with this file.
* If not, see http://www.opensource.org/licenses/cpl1.0.txt for more
* informations.
*
* Inspite of the incompatibility between the Common Public License (CPL) and
* the GNU General Public License (GPL) you're allowed to use this program
* under the GPL.
* You also should have recieved a copy of this license with this file.
* If not, see http://www.gnu.org/licenses/gpl.txt for more informations.
}
unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,
  AdDraws, AdSetupDlg, AdTypes, AdClasses, AdShader, AdPNG, AdConsts;

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
  //Only for debuging - you can remove this line
  ReportMemoryLeaksOnShutdown := true;

  //Crate the main surface.
  AdDraw := TAdDraw.Create(self);

  //Create the setup dialog and pass the main surface
  AdSetup := TAdSetup.Create(AdDraw);
  AdSetup.Image := 'logo1.png';

  if AdSetup.Execute then
  begin
    //Free the setup dialog
    AdSetup.Free;

    //Try to initialize the TAdDraw
    if AdDraw.Initialize then
    begin
      //Connect the on idle event
      AdDraw.Window.Events.OnIdle := Idle;

      //Create the shader system
      AdShaderSys := TAdShaderSystem.Create(AdDraw);
      //Create a shader object
      AdShader := TAdShaderEffect.Create(AdShaderSys);

      //Load and compile the vertex shader
      AdShader.VertexShader.LoadFromFile(path + 'bumpmap.cg');
      AdShader.VertexShader.CompileProgram('cg', 'vertex_shader', astVertex);

      //Load and compile the pixel shader
      AdShader.FragmentShader.LoadFromFile(path + 'bumpmap.cg');
      AdShader.FragmentShader.CompileProgram('cg', 'fragment_shader', astFragment);

      //Load the normal texture
      AdNormalImage := TAdImage.Create(AdDraw);
      AdNormalImage.Texture.LoadGraphicFromFile(path + 'normalmap.png');
      AdNormalImage.Restore;

      //Load the color texture
      AdTextureImage := TAdImage.Create(AdDraw);
      AdTextureImage.Texture.LoadGraphicFromFile(path + 'colormap.png');
      AdTextureImage.Restore;
      AdTextureImage.Details := 8;
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
  //Draw on the TAdDraw if drawing is possible
  if AdDraw.CanDraw then
  begin
    //Clear the surface of the TAdDraw with black color. You may also use delphi
    //colors here
    AdDraw.ClearSurface(AdCol24_Black);
    
    AdDraw.BeginScene;

    lightpos := AdVector3(mx, 512 - my, 25);

    //Set the vertex shader parameters
    AdShader.VertexShader.SetParameter('lightpos', lightpos);
    AdShader.VertexShader.SetParameter('lightcolor', Ad_ARGB(255, 255, 255, 255));

    //Set the fragment shader parameters
    AdShader.FragmentShader.SetParameter('colormap', AdTextureImage.Texture.Texture);
    AdShader.FragmentShader.SetParameter('normalmap', AdNormalImage.Texture.Texture);
    AdShader.FragmentShader.SetParameter('lightintensity', 1);
    AdShader.FragmentShader.SetParameter('ambientcolor', Ad_ARGB(255, 64, 64, 64));

    //Assign the shader to a object - in this case we are using the image mesh
    AdShader.BindToObject(AdTextureImage);

    AdTextureImage.Draw(AdDraw, 0, 0, 0);

    AdDraw.EndScene;
    
    //Bring the drawn things on the screen
    AdDraw.Flip;
  end;

  Done := false;
end;

end.
