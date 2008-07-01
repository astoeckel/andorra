unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  AdDraws, AdClasses, AdTypes, AdParticles, AdPng, AdSetupDlg, AdPerformanceCounter,
  AdCanvas;

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
    PartSys:TAdParticleSystem;
    AdImageList:TAdImageList;
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
  AdSetupDlg:TAdSetup;
begin
  ReportMemoryLeaksOnShutdown := true;

  AdPerCounter := TAdPerformanceCounter.Create;

  AdDraw := TAdDraw.Create(self);

  AdSetupDlg := TAdSetup.Create(AdDraw);
  AdSetupDlg.Image := 'logo1.png';

  if AdSetupDlg.Execute then
  begin
    if AdDraw.Initialize then
    begin
      Application.OnIdle := Idle;

      AdImageList := TAdImageList.Create(AdDraw);
      with AdImageList.Add('particle') do
      begin
        Texture.LoadGraphicFromFile(path+'part2.png', true, clNone);
        Restore;
      end;
      PartSys := TAdParticleSystem.Create(AdDraw);
      PartSys.Texture := AdImageList.Items[0].Texture;
    end
    else
    begin
      ShowMessage(AdDraw.GetLastError);
      halt;
    end;
  end
  else
  begin
    halt;
  end;

  AdSetupDlg.Free;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  PartSys.Free;
  AdImageList.Free;
  AdPerCounter.Free;
  AdDraw.Free;
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  MouseX := X;
  MouseY := Y;
  PartSys.Emit(5, MouseX, MouseY);
end;

procedure TForm1.Idle(Sender: TObject; var Done: boolean);
begin
  if AdDraw.CanDraw then
  begin
    AdPerCounter.Calculate;
    Caption := 'FPS:'+inttostr(AdPerCounter.FPS) + ' C:'+inttostr(PartSys.Items.Count);

    AdDraw.ClearSurface(0);
    AdDraw.BeginScene;

    PartSys.Move(AdPerCounter.TimeGap / 1000);

    PartSys.Draw(AdDraw, 0, 0);

    with AdDraw.Canvas do
    begin
      Pen.Color := Ad_ARGB(255, 255, 0, 0);
      Brush.Style := abClear;
      Rectangle(PartSys.BoundsRect);
    end;

    AdDraw.EndScene;
    AdDraw.Flip;  
  end;
  Done := false;
end;

end.
