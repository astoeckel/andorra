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
    AdDraw:TAdDraw;
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

  AdDraw := TAdDraw.Create(self);


  AdSetupDlg := TAdSetup.Create(AdDraw);
  AdSetupDlg.Image := 'logo1.png';

  if AdSetupDlg.Execute then
  begin
    if AdDraw.Initialize then
    begin
      Application.OnIdle := Idle;

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

    with AdDraw.Fonts.GenerateFont('Verdana',36,[],clLime,128,-5,-5,5) do
    begin
      with TypeSetter as TAdSimpleTypeSetter do
      begin
        DrawMode := [dtCenter, dtMiddle];
      end;
      Color := Ad_ARGB(255,255,255,255);
      TextOut(AdRect(0,0,ClientWidth,ClientHeight),timestr);
    end;

    AdDraw.EndScene;
    AdDraw.Flip;
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
