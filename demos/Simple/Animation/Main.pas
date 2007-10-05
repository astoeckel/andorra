unit Main;

interface

uses
  Windows, Dialogs, SysUtils, Graphics, Classes, Forms,
  Controls, ExtCtrls, AdPng, AdDraws, AdClasses, AdSetupDlg;

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
    procedure SetLine;
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;
  Pattern:single;
  StartPt,EndPt:integer;
  Y,X:single;
  XSpeed:single;

const
  path = '..\demos\Simple\Animation\';

implementation

{$R *.dfm}

procedure TForm1.SetLine;
begin
  XSpeed := -XSpeed;
  if XSpeed > 0 then
  begin
    StartPt := 0;
    EndPt := 7;
    X := -96;
  end
  else
  begin
    StartPt := 8;
    EndPt := 15;
    X := ClientWidth+96;
  end;
  Y := Random(ClientHeight-96);
end;

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

  if AdSetupDlg.Execute then
  begin
    if AdDraw.Initialize then
    begin
      Application.OnIdle := Idle;

      AdImageList1 := TAdImageList.Create(AdDraw);
      with AdImageList1.Add('figur') do
      begin
        Texture.LoadGraphicFromFile(path+'boy.bmp',true,clFuchsia);
        PatternWidth := 96;
        PatternHeight := 96;
      end;
      AdImageList1.Restore;

      XSpeed := -150;

      Randomize;
      SetLine;
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

    Pattern := Pattern + 15*AdPerCounter.TimeGap/1000;
    if Pattern >= EndPt then Pattern := StartPt;

    X := X + XSpeed*AdPerCounter.TimeGap/1000;
    if ((X > ClientWidth) and (XSpeed > 0)) or ((X < -96) and (XSpeed < 0))  then SetLine;

    AdDraw.ClearSurface(clBlack);
    AdDraw.BeginScene;

    AdImageList1.Find('figur').Draw(AdDraw,round(X),round(Y),round(Pattern));

    with AdDraw.Canvas do
    begin
      TextOut(5,5,'FPS: '+inttostr(AdPerCounter.FPS));
      Release;
    end;

    AdDraw.EndScene;
    AdDraw.Flip;

    Done := false;
  end;
end;

end.
