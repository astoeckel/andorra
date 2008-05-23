unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, AdDraws, AdClasses, StdCtrls, AdSetupNew, AdPng, AdCanvas,
  AdPerformanceCounter, AdTypes;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  private
    { Private-Deklarationen }
  public
    AdDraw1:TAdDraw;
    AdPerCounter:TAdPerformanceCounter;

    procedure Idle(Sender:TObject;var Done:boolean);
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;
  w,h:integer;
  s:double;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  AdSetupDlg:TAdSetup;
begin
  ReportMemoryLeaksOnShutdown := true;

  AdPerCounter := TAdPerformanceCounter.Create;

  AdDraw1 := TAdDraw.Create(self);

  AdSetupDlg := TAdSetup.Create(AdDraw1);
  AdSetupDlg.Image := 'logo1.png';

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

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  w := x;
  h := y;
end;

procedure TForm1.Idle(Sender: TObject; var Done: boolean);
var
  cx, cy, ax, ay:integer;
  i: integer;
  start:integer;
begin
  if AdDraw1.CanDraw then
  begin
    AdPerCounter.Calculate;
    AdDraw1.ClearSurface(clBlack);
    AdDraw1.BeginScene;

    cx := w;
    cy := h;

    s := s + AdPerCounter.TimeGap / 20;

    start := round(s);

    with AdDraw1.Canvas do
    begin
      Pen.Width := 16;
      Pen.TextureMode := tmTile;
      Pen.TexturePosition := tpDynamic;
      Pen.BlendMode := bmAdd;

      for i := start to start + 360 do
      begin
        Pen.Color := Ad_ARGB(255-round((i-start)/360*255),255,100,100);
        ax := cx + round(cos(pi/18*i)*(i-start));
        ay := cy + round(sin(pi/18*i)*(i-start));
        if i = start then
        begin
          MoveTo(ax,ay);
        end
        else
        begin
          LineTo(ax,ay);
        end;
      end;

      cx := h;
      cy := w;
      for i := -start to -start + 360 do
      begin
        Pen.Color := Ad_ARGB(255-round((i+start)/360*255),100,100,255);
        ax := cx + round(cos(pi/18*i)*(i+start));
        ay := cy + round(sin(pi/18*i)*(i+start));
        if i = -start then
        begin
          MoveTo(ax,ay);
        end
        else
        begin
          LineTo(ax,ay);
        end;
      end;

      //It would be better to use "ReturnDisplayList" and draw each spiral once.
      //Rotation is possible by using TAdDisplayList.RotateZ.
      Release;
    end;

    Caption := 'FPS: '+IntToStr(AdPerCounter.FPS);

    AdDraw1.EndScene;
    AdDraw1.Flip; 
  end;
  Done := false;
end;

end.
