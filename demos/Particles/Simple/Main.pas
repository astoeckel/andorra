unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  AdDraws, AdClasses, AdParticles, AdPng, AdSetupDlg, AdPerformanceCounter;

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
  path = '..\demos\Particles\Simple\';

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  AdSetupDlg:TAdSetup;
begin
  AdPerCounter := TAdPerformanceCounter.Create;

  AdDraw := TAdDraw.Create(self);

  AdSetupDlg := TAdSetup.Create(self);
  AdSetupDlg.Image := 'logo1.png';
  AdSetupDlg.AdDraw := AdDraw;
  AdSetupDlg.Form := self;
  AdSetupDlg.Sections := [dlgResolution, dlgPlugin];

  if AdSetupDlg.Execute then
  begin
    if AdDraw.Initialize then
    begin
      Application.OnIdle := Idle;

      AdImageList := TAdImageList.Create(AdDraw);
      with AdImageList.Add('particle') do
      begin
        Texture.LoadGraphicFromFile(path+'part2.png',true,clNone);
        Restore;
      end;
      PartSys := TAdParticleSystem.Create(AdDraw);
      PartSys.Texture := AdImageList.Items[0].Texture;
    end
    else
    begin
      ShowMessage('Error while initializing Andorra 2D. Try to use another display'+
                  'mode or another video adapter.');
      halt;
    end;
  end
  else
  begin
    halt;
  end;
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
end;

procedure TForm1.Idle(Sender: TObject; var Done: boolean);
begin
  if AdDraw.CanDraw then
  begin
    AdPerCounter.Calculate;
    Caption := 'FPS:'+inttostr(AdPerCounter.FPS);

    AdDraw.ClearSurface(clBlack);
    AdDraw.BeginScene;
    PartSys.CreateParticles(1,TAdParticle,MouseX,MouseY);
    PartSys.Move(AdPerCounter.TimeGap / 1000);
    PartSys.Draw(0, 0);
    PartSys.Dead;
    AdDraw.EndScene;
    AdDraw.Flip;

  end;
  Done := false;
end;

end.
