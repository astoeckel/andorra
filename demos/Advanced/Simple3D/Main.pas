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
      AdTexture: TAdTexture;
      AdLight: TAd2dLight;

      FMX, FMY: integer;
      FMouseDown: boolean;
      dx, dy: single;

      procedure SlowDownRotation;

      procedure Idle(Sender: TObject; var Done: boolean);
      procedure MouseMove(Sender: TObject; Shift: TAdShiftState; X, Y: integer);
      procedure MouseDown(Sender: TObject; Button: TAdMouseButton;
        Shift: TAdShiftState; X, Y: integer);
      procedure MouseUp(Sender: TObject; Button: TAdMouseButton;
        Shift: TAdShiftState; X, Y: integer);
    public
      procedure Run;
  end;

const
  path = './resources/';


implementation

uses AdCanvas;

{ TAdAppl }

procedure TAdAppl.Idle(Sender: TObject; var Done: boolean);
begin
  if AdDraw.CanDraw then
  begin
    AdPerCounter.Calculate;

    AdDraw.BeginScene;

    AdDraw.AdAppl.ClearSurface(AdDraw.SurfaceRect, [alZBuffer], AdCol32_AliceBlue, 1, 0);
    with AdDraw.Canvas do
    begin
      Pen.Width := 0;
      Brush.Color := AdCol32_Bisque;
      with Brush.Color do
        a := 100;
      Rectangle(AdDraw.SurfaceRect);

      Release;
    end;

    //Switch to the 3D mode
    AdDraw.Scene.Setup3DScene(800, 600,
      AdVector3(0, 0, -200),
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
    
    AdDraw.Canvas.TextOut(0, 0, 'FPS: ' + inttostr(AdPerCounter.FPS));
    AdDraw.Canvas.TextOut(0, 16, 'Rotation X: ' + inttostr(Round(AdMesh.RotationX * 180 / PI) mod 360) + '�');
    AdDraw.Canvas.TextOut(0, 32, 'Rotation Y: ' + inttostr(Round(AdMesh.RotationY * 180 / PI) mod 360) + '�');
    AdDraw.Canvas.Release;

    AdDraw.EndScene;

    AdDraw.Flip;
  end;

  Done := false;
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

procedure TAdAppl.Run;
var
  AdSetup: TAdSetup;
  data: TAd2dLightData;
  mat: TAd2dMaterial;
begin
  AdDraw := TAdDraw.Create(nil);
  
  AdSetup := TAdSetup.Create(AdDraw);
  AdSetup.Image := 'logo1.png';
  if AdSetup.Execute then
  begin
    if AdDraw.Initialize then
    begin
      AdPerCounter := TAdPerformanceCounter.Create;
      AdPerCounter.MaximumFrameRate := 100;

      AdDraw.Window.Events.OnIdle := Idle;
      AdDraw.Window.Events.OnMouseMove := MouseMove;
      AdDraw.Window.Events.OnMouseDown := MouseDown;
      AdDraw.Window.Events.OnMouseUp := MouseUp;
      AdDraw.Window.Title := 'Andorra 2D Simple 3D';

      AdDraw.Options := AdDraw.Options;

      AdTexture := TAdTexture.Create(AdDraw);
      AdTexture.LoadGraphicFromFile(path + 'floor_tex.png');
      AdTexture.Filter := atAnisotropic;

      AdMesh := TAdTeapotMesh.Create(AdDraw);

      FillChar(mat, SizeOf(TAd2dMaterial), 0);

      mat.Ambient := Ad_ARGB(255, 63, 56, 15);
      mat.Diffuse := Ad_ARGB(255, 90, 79, 23);
      mat.Specular := Ad_ARGB(255, 204, 186, 54);
      mat.Power := 5;

      AdMesh.Material := mat;

      AdLight := AdDraw.AdAppl.CreateLight;
      FillChar(data, SizeOf(TAd2dLightData), 0);
      with data do
      begin
        LightType := altDirectional;
        Diffuse := Ad_ARGB(255, 255, 255, 255);
        Specular := Ad_ARGB(255, 255, 255, 255);
        Position := AdVector3(0, 1, 1);
      end;

      AdLight.Data := data;      

      AdDraw.Run;

      AdLight.Free;
      AdTexture.Free;
      AdMesh.Free;
      AdPerCounter.Free;
    end else
    begin
      ShowMessage(AdDraw.GetLastError);
    end;
  end;

  AdDraw.Free;
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
