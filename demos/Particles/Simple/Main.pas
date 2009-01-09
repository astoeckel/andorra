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
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  AdDraws, AdClasses, AdTypes, AdParticles, AdPNG, AdSetupDlg, AdPerformanceCounter,
  AdCanvas, AdConsts;

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
    AdPartSys:TAdParticleSystem;
    AdImageList:TAdImageList;
    AdDefaultParticle: TAdBillboardParticle;
    MouseX,MouseY:integer;
    procedure Idle(Sender:TObject;var Done:boolean);
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

const
  path = './resources/';

implementation

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
      //Create the performance counter. This class is used for measuring the time
      //that passes between two frames.
      AdPerCounter := TAdPerformanceCounter.Create;

      //Connect the on idle event
      AdDraw.Window.Events.OnIdle := Idle;

      //Create an image list and load the texture that will be used for the particles
      AdImageList := TAdImageList.Create(AdDraw);
      with AdImageList.Add('particle') do
      begin
        Texture.LoadGraphicFromFile(path+'part2.png', true, clNone);
        Restore;
      end;       
      
      //Create the particle system
      AdPartSys := TAdParticleSystem.Create(AdDraw);

      //Load the texture image
      AdPartSys.Texture := AdImageList.Items[0].Texture;
      
      AdDefaultParticle := TAdBillboardParticle.Create(AdPartSys);
      AdPartSys.DefaultParticle := AdDefaultParticle;

      with AdDefaultParticle do
      begin
        Size.Start := 1;
        Size.Stop := 0;
        Size.Variation := 0.5;
      end;

      //Setup the particle system color gradient
      AdPartSys.Colors.Clear;
      AdPartSys.Colors.Add(AdSetAlpha(255, AdCol32_CornflowerBlue));
      AdPartSys.Colors.Add(AdSetAlpha(0, AdCol32_Goldenrod));
    end
    else
    begin
      ShowMessage(AdDraw.GetLastError);
      halt;
    end;
  end
  else
  begin
    AdSetup.Free;
    halt;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  AdPartSys.Free;
  AdDefaultParticle.Free;
  AdImageList.Free;
  AdPerCounter.Free;
  AdDraw.Free;
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  //Emit some paticles when the mouse is moving
  MouseX := X; MouseY := Y;
  AdPartSys.Emit(5, MouseX, MouseY);
end;

procedure TForm1.Idle(Sender: TObject; var Done: boolean);
begin
  //Draw on the TAdDraw if drawing is possible
  if AdDraw.CanDraw then
  begin
    //Calculate the time difference.
    AdPerCounter.Calculate;

    //Clear the surface of the TAdDraw with black color
    AdDraw.ClearSurface(AdCol24_Black);

    AdDraw.BeginScene;

    //Calculate the particle motion
    AdPartSys.Move(AdPerCounter.TimeGap / 1000);

    //Draw the particle system
    AdPartSys.Draw(AdDraw, 0, 0);

    with AdDraw.Canvas do
    begin
      Pen.Color := AdCol32_BlueViolet;
      Brush.Style := abClear;
      Rectangle(AdPartSys.BoundsRect);
    end;

    AdDraw.EndScene;
    AdDraw.Flip;  
  end;
  Done := false;
end;

end.
