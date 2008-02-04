unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,
  AdDraws, AdClasses, AdPNG, AdSetupDlg, AdPerformanceCounter, AdSimpleCompressors,
  AdTypes, StdCtrls, ExtCtrls, AdVCLWindow, AdGUIConnector, AdGUI, AdComponents;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    AdDraw:TAdDraw;
    AdPerCounter:TAdPerformanceCounter;

    AdGUI:TAdGUI;
    AdGUIConnector:TAdGUIConnector;
    procedure Idle(Sender:TObject;var Done:boolean);
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  AdSetupDlg:TAdSetup;
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

      AdGUI := TAdGUI.Create(AdDraw);
      AdGUI.Cursors.LoadFromFile('cursors.xml');
      AdGUI.Skin.LoadFromFile('sunna.axs');
      AdGUI.LoadFromFile('test.axg');

      AdGUIConnector := TAdGUIConnector.Create(AdGUI);
      AdGUIConnector.ConnectEventHandlers(AdDraw.Window);
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
  AdGUIConnector.Free;
  AdGUI.Free;
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

    AdGUI.Move(AdPerCounter.TimeGap / 1000);
    AdGUI.Draw;

    AdDraw.EndScene;
    AdDraw.Flip;
  end;

  Done := false;
end;

end.
