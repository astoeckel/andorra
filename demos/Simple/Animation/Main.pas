unit Main;

interface

uses
  Windows, Dialogs, SysUtils, Graphics, Classes, Forms, AdDraws, AdClasses,
  Controls, ExtCtrls;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    AdDraw1:TAdDraw;
    AdPerCounter:TPerformanceCounter;
    AdImageList1:TPictureCollection;
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
begin
  ReportMemoryLeaksOnShutdown := true;
  AdPerCounter := TPerformanceCounter.Create;

  AdDraw1 := TAdDraw.Create(self);
  AdDraw1.DllName := 'AndorraDX93D.dll';

  if AdDraw1.Initialize then
  begin
    Application.OnIdle := Idle;

    AdImageList1 := TPictureCollection.Create(AdDraw1);
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
    Close;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  AdImageList1.Free;
  AdPerCounter.Free;
  AdDraw1.Free;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  if AdDraw1.Initialized then
  begin
    AdDraw1.Finalize;
    AdDraw1.Initialize;
  end;
end;

procedure TForm1.Idle(Sender: TObject; var Done: boolean);
begin
  if AdDraw1.CanDraw then
  begin
    AdPerCounter.Calculate;
    Caption := 'FPS:'+inttostr(AdPerCounter.FPS);

    Pattern := Pattern + 15*AdPerCounter.TimeGap/1000;
    if Pattern >= EndPt then Pattern := StartPt;

    X := X + XSpeed*AdPerCounter.TimeGap/1000;
    if ((X > ClientWidth) and (XSpeed > 0)) or ((X < -96) and (XSpeed < 0))  then SetLine;


    AdDraw1.ClearSurface(clBlack);
    AdDraw1.BeginScene;

    AdImageList1.Find('figur').Draw(AdDraw1,round(X),round(Y),round(Pattern));
    AdDraw1.EndScene;
    AdDraw1.Flip;

    Done := false;
  end;
end;

end.
