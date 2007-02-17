unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, AdDraws, AdClasses, AdParticles, AdPng;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    AdDraw1:TAdDraw;
    AdPerCounter:TPerformanceCounter;
    PartSys:TAdParticleSystem;
    AdImageList:TPictureCollection;
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
begin
  AdPerCounter := TPerformanceCounter.Create;

  AdDraw1 := TAdDraw.Create(self);
  AdDraw1.DllName := 'AndorraDX93D.dll';
  if AdDraw1.Initialize then
  begin
    Application.OnIdle := Idle;

    AdImageList := TPictureCollection.Create(AdDraw1);
    with AdImageList.Add('particle') do
    begin
      Texture.LoadGraphicFromFile(path+'part2.png',true,clNone);
      Restore;
    end;
    PartSys := TAdParticleSystem.Create(AdDraw1);
    PartSys.Texture := AdImageList.Items[0].Texture;
    PartSys.DefaultParticle
  end
  else
  begin
    ShowMessage('Error while initializing Andorra 2D. Try to use another display'+
                'mode or another video adapter.');
    halt;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  PartSys.Free;
  AdImageList.Free;
  AdPerCounter.Free;
  AdDraw1.Free;
end;

procedure TForm1.Idle(Sender: TObject; var Done: boolean);
begin
  if AdDraw1.CanDraw then
  begin
    AdPerCounter.Calculate;
    Caption := 'FPS:'+inttostr(AdPerCounter.FPS);

    AdDraw1.ClearSurface(clBlack);
    AdDraw1.BeginScene;
    PartSys.CreateParticles(1,TAdParticle,0,0);
    PartSys.Move(AdPerCounter.TimeGap / 1000);
    PartSys.Draw(AdDraw1.DisplayRect.Right / 2, AdDraw1.DisplayRect.Bottom / 2);
    AdDraw1.EndScene;
    AdDraw1.Flip;

  end;
  Done := false;
end;

end.
