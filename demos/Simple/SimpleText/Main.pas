unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,
  AdDraws, AdClasses, AdSetupDlg, AdPng, AdCanvas, AdPerformanceCounter,
  AdTypes, AdFont, AdFontFactory, AdStandardFontGenerator, ExtCtrls;

type
  TForm1 = class(TForm)
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    AdDraw1:TAdDraw;
    AdPerCounter:TAdPerformanceCounter;
    timestr:string;
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

  AdDraw1 := TAdDraw.Create(self);


  AdSetupDlg := TAdSetup.Create(self);
  AdSetupDlg.Image := 'logo1.png';
  AdSetupDlg.AdDraw := AdDraw1;
  AdSetupDlg.Form := self;

  if AdSetupDlg.Execute then
  begin
    if AdDraw1.Initialize then
    begin
      Application.OnIdle := Idle;

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
  AdSetupDlg.Free;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  AdPerCounter.Free;
  AdDraw1.Free;
end;

procedure TForm1.Idle(Sender: TObject; var Done: boolean);
begin
  if AdDraw1.CanDraw then
  begin
    AdPerCounter.Calculate;
    AdDraw1.ClearSurface(clBlack);
    AdDraw1.BeginScene;

    with AdDraw1.Fonts.GenerateFont('Verdana',36,[]) do
    begin
      with TypeSetter as TAdSimpleTypeSetter do
      begin
        DrawMode := [dtCenter, dtMiddle];
      end;
      Color := Ad_ARGB(128,0,0,255);
      TextOut(AdRect(0,0,ClientWidth,ClientHeight),timestr);
    end;

    AdDraw1.EndScene;
    AdDraw1.Flip;
  end;

  //Because we inserted a timer which uses a window message, OnIdle will
  //automaticly be called after each "OnTimer"
  //Done := false;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  timestr := FormatDateTime('hh:nn:ss',time);
end;

end.
