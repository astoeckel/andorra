unit Main;

interface

uses
  Windows, Dialogs, SysUtils, Graphics, Classes, Forms,
  AdDraws, AdClasses, AdPNG, AdSetupDlg, AdPerformanceCounter;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    AdDraw:TAdDraw;
    AdPerCounter:TAdPerformanceCounter;
    AdImageList1:TAdImageList;
    procedure Idle(Sender:TObject;var Done:boolean);
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  AdSetupDlg : TAdSetup;
  bmp:TBitmap;
begin
  ReportMemoryLeaksOnShutdown := true;
  AdPerCounter := TAdPerformanceCounter.Create;

  AdDraw := TAdDraw.Create(self);

  AdSetupDlg := TAdSetup.Create(self);
  AdSetupDlg.Image := 'logo1.png';
  AdSetupDlg.AdDraw := AdDraw;
  AdSetupDlg.Form := self;

  if AdSetupDlg.Execute then
  begin
    if AdDraw.Initialize then
    begin
      Application.OnIdle := Idle;

      AdImageList1 := TAdImageList.Create(AdDraw);
      with AdImageList1.Add('logo') do
      begin
        Texture.LoadGraphicFromFile('icon64.png');
      end;
      AdImageList1.Restore;
    end
    else
    begin
      ShowMessage('Error while initializing Andorra 2D. Try to use another display '+
                  'mode or another video adapter.');
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
  AdImageList1.Free;
  AdPerCounter.Free;
  AdDraw.Free;
end;

procedure TForm1.Idle(Sender: TObject; var Done: boolean);
begin
  if AdDraw.CanDraw then
  begin
    AdPerCounter.Calculate;
    Caption := 'FPS: '+inttostr(AdPerCounter.FPS);

    AdDraw.ClearSurface(clSilver);
    AdDraw.BeginScene;

    AdImageList1.Find('logo').Draw(AdDraw,0,0,0);
    
    AdDraw.EndScene;
    AdDraw.Flip;

    Done := false;
  end;
end;

end.
