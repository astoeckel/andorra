unit Main;

interface

uses
  Dialogs, SysUtils, Forms, Types, Classes, Graphics,  AdSimpleXML,  
  AdPNG, AdDraws, AdClasses, AdTypes, AdPerformanceCounter, AdSetupDlg, AdSprites,
  AdBitmap, AdCanvas;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    AdDraw:TAdDraw;
    AdPerCounter:TAdPerformanceCounter;
    AdImage: TAdImage;

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

  AdSetup := TAdSetup.Create(AdDraw);
  AdSetup.Image := 'logo1.png';
  if AdSetup.Execute then
  begin
    if AdDraw.Initialize then
    begin
      Application.OnIdle := Idle;

      AdDraw.Scene.AmbientColor := Ad_ARGB(255, 255, 255, 255);

      AdImage := TAdImage.Create(AdDraw);
      AdImage.Texture.LoadGraphicFromFile('icon64.png');
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
  AdImage.Free;
  AdPerCounter.Free;
  AdDraw.Free;
end;

procedure TForm1.Idle(Sender: TObject; var Done: boolean);
begin
  if AdDraw.CanDraw then
  begin
    AdPerCounter.Calculate;

    AdDraw.ClearSurface(clBlack);

    AdDraw.BeginScene;

    AdDraw.EndScene;

    AdDraw.Flip;
  end;
  Done := false;
end;

end.

