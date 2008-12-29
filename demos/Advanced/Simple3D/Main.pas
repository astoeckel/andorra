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

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  SysUtils, Classes, Dialogs,
  AdDraws, AdTypes, AdClasses, AdPerformanceCounter, AdStdWindow, AdSetupDlg,
  AdPNG, Ad3DObj, AdEvents, AdConsts;

type
  TAdAppl = class
    private
      AdPerCounter: TAdPerformanceCounter;
      AdDraw: TAdDraw;
      AdMesh: TAdMesh;
      AdLight: TAd2dLight;
      AdTexture: TAdTexture;

      FMX, FMY: integer;
      FMouseDown: boolean;
      FMatIndex: integer;
      dx, dy: Single;
      tz: Single;

      procedure SlowDownRotation;

      procedure Idle(Sender: TObject; var Done: boolean);
      procedure MouseMove(Sender: TObject; Shift: TAdShiftState; X, Y: integer);
      procedure MouseDown(Sender: TObject; Button: TAdMouseButton;
        Shift: TAdShiftState; X, Y: integer);
      procedure MouseUp(Sender: TObject; Button: TAdMouseButton;
        Shift: TAdShiftState; X, Y: integer);
      procedure KeyPress(Sender: TObject; Key: Char);
      procedure MouseWheel(Sender: TObject; Shift: TAdShiftState;
    WheelDelta: integer; X, Y: integer);
      procedure SetMaterial;
    public
      procedure Run;
  end;

const
  path = './resources/';

implementation

uses AdCanvas;

{ TAdAppl }

procedure TAdAppl.Run;
var
  AdSetup: TAdSetup;
  data: TAd2dLightData;
begin
  //Only for debuging - you can remove this line
  ReportMemoryLeaksOnShutdown := true;

  //Crate the main surface.
  AdDraw := TAdDraw.Create(nil);
  
  //Create the setup dialog and pass the main surface
  AdSetup := TAdSetup.Create(AdDraw);
  AdSetup.Image := 'logo1.png';
  
  if AdSetup.Execute then
  begin
    //Try to initialize the TAdDraw
    if AdDraw.Initialize then
    begin
      //Create the performance counter. This class is used for measuring the time
      //that passes between two frames.
      AdPerCounter := TAdPerformanceCounter.Create;
      AdPerCounter.MaximumFrameRate := 100;

      //Connect all window events
      AdDraw.Window.Events.OnIdle := Idle;
      AdDraw.Window.Events.OnMouseMove := MouseMove;
      AdDraw.Window.Events.OnMouseDown := MouseDown;
      AdDraw.Window.Events.OnMouseUp := MouseUp;
      AdDraw.Window.Events.OnKeyPress := KeyPress;
      AdDraw.Window.Events.OnMouseWheel := MouseWheel;
      AdDraw.Window.Title := 'Andorra 2D Simple 3D';

      AdTexture := TAdTexture.Create(AdDraw);
      AdTexture.LoadGraphicFromFile(path + 'normalmap.png');
      AdTexture.Filter := atAnisotropic;

      //Create a new "Teapot-Mesh"
      AdMesh := TAdPlaneMesh.Create(AdDraw);
      AdMesh.Texture := AdTexture;

      //Set the material of the teapot
      FMatIndex := -1;
      SetMaterial;

      //Setup a light
      AdLight := AdDraw.AdAppl.CreateLight;
      FillChar(data, SizeOf(TAd2dLightData), 0);
      with data do
      begin
        LightType := altDirectional;
        Diffuse := Ad_ARGB(255, 255, 255, 255);
        Specular := Ad_ARGB(255, 255, 255, 255);

        //The position vector represents the light direction when using
        //a directional light
        Position := AdVector3(0, 1, 1);
      end;
      
      //Store the light data in the light
      AdLight.Data := data;      

      //Actually run the application
      AdDraw.Run;

      //Free all created objects
      AdTexture.Free;
      AdLight.Free;
      AdMesh.Free;
      AdPerCounter.Free;
    end else
    begin
      ShowMessage(AdDraw.GetLastError);
    end;
  end;

  //Free the setup dialog
  AdSetup.Free;
  //Free the main surface
  AdDraw.Free;
end;

procedure TAdAppl.Idle(Sender: TObject; var Done: boolean);
begin
  //Draw on the TAdDraw if drawing is possible
  if AdDraw.CanDraw then
  begin
    //Calculate the time difference.
    AdPerCounter.Calculate;

    //Clear the surface of the TAdDraw with black color. You may also use delphi
    //colors here
    AdDraw.ClearSurface(AdCol24_Black);

    AdDraw.BeginScene;

    //Switch to the 3D mode
    AdDraw.Scene.Setup3DScene(800, 600,
      AdVector3(0, 0, -200 + tz),
      AdVector3(0, 0, 0),
      AdVector3(0, -1 , 0));

    //Activate the ZBuffer on to prevent the graphic engine from producing graphic
    //problems
    AdDraw.Options := AdDraw.Options + [aoZBuffer, aoLight];

    //Rotate the mesh
    AdMesh.RotationX := AdMesh.RotationX + (AdPerCounter.TimeGap * dx);
    AdMesh.RotationY := AdMesh.RotationY + (AdPerCounter.TimeGap * dy);

    //Draw the mesh
    AdLight.EnableLight(0);
    AdMesh.Draw(AdDraw);
    AdLight.DisableLight;

    SlowDownRotation;

    //Deactivate the ZBuffer again - we don't need it when drawing in the 2D mode
    AdDraw.Options := AdDraw.Options - [aoZBuffer, aoLight];

    //Switch to the 2D Mode again
    AdDraw.Setup2DScene;
    
    with AdDraw.Canvas do
    begin
      TextOut(0, 0, 'FPS: ' + inttostr(AdPerCounter.FPS));
      TextOut(0, 16, 'Rotation X: ' + inttostr(Round(AdMesh.RotationX * 180 / PI) mod 360) + '°');
      TextOut(0, 32, 'Rotation Y: ' + inttostr(Round(AdMesh.RotationY * 180 / PI) mod 360) + '°');
      TextOut(0, 64, 'Press "m" to change the material of the mesh');
      Release;
    end;

    AdDraw.EndScene;

    AdDraw.Flip;
  end;

  Done := false;
end;

procedure TAdAppl.KeyPress(Sender: TObject; Key: Char);
begin
  if Key = 'm' then
    SetMaterial;
end;

procedure TAdAppl.MouseDown(Sender: TObject; Button: TAdMouseButton;
  Shift: TAdShiftState; X, Y: integer);
begin
  FMX := X;
  FMY := Y;

  FMouseDown := Button = abLeft;
end;

procedure TAdAppl.MouseMove(Sender: TObject; Shift: TAdShiftState; X,
  Y: integer);
begin
  if (asLeft in Shift) then
  begin
    dx := dx + (Y - FMY) * 0.000015;
    dy := dy + (X - FMX) * 0.000015;

    FMX := X; FMY := Y;

    FMouseDown := false;
  end;
end;

procedure TAdAppl.MouseUp(Sender: TObject; Button: TAdMouseButton;
  Shift: TAdShiftState; X, Y: integer);
begin
  FMouseDown := FMouseDown and not (Button = abLeft);
end;

procedure TAdAppl.MouseWheel(Sender: TObject; Shift: TAdShiftState; WheelDelta,
  X, Y: integer);
begin
  tz := tz + WheelDelta * 0.01;
end;

procedure TAdAppl.SetMaterial;
begin
  //Select the next material
  inc(FMatIndex);
  if FMatIndex > 6 then
    FMatIndex := 0;
    
  case FMatIndex of
    0: AdMesh.Material := AdMat_Gold_Polished;
    1: AdMesh.Material := AdMat_Chrome;
    2: AdMesh.Material := AdMat_Rubber_Black;
    3: AdMesh.Material := AdMat_Plastic_Black;
    4: AdMesh.Material := AdMat_Plastic_Blue;
    5: AdMesh.Material := AdMat_Plastic_Green;
    6: AdMesh.Material := AdMat_Plastic_Red;
  end;
end;

procedure TAdAppl.SlowDownRotation;
var
  sd: single;
begin
  if FMouseDown then
    sd := 20
  else
    sd := 1;
    
  if dx > 0 then
    dx := dx - 0.000005 * AdPerCounter.TimeGap * sd
  else
    dx := dx + 0.000005 * AdPerCounter.TimeGap * sd;
  if abs(dx) < 0.00001 * AdPerCounter.TimeGap * sd then
    dx := 0;

  if dy > 0 then
    dy := dy - 0.000005 * AdPerCounter.TimeGap * sd
  else
    dy := dy + 0.000005 * AdPerCounter.TimeGap * sd;
  if abs(dy) < 0.00001 * AdPerCounter.TimeGap * sd then
    dy := 0;
end;

end.
