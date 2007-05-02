unit Main;

interface

uses
  Windows, Dialogs, SysUtils, Graphics, Classes, Forms, AdDraws, AdClasses, AdPNG,
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
    AdImageList1:TAdImageList;
    procedure Idle(Sender:TObject;var Done:boolean);
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := true;
  AdPerCounter := TPerformanceCounter.Create;

  AdDraw1 := TAdDraw.Create(self);
  AdDraw1.DllName := 'AndorraDX93D.dll';

  if AdDraw1.Initialize then
  begin
    Application.OnIdle := Idle;

    AdImageList1 := TAdImageList.Create(AdDraw1);
    with AdImageList1.Add('logo') do
    begin
      Texture.LoadGraphicFromFile('icon64.png',true,clWhite);
    end;
    AdImageList1.Restore;
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

    AdDraw1.ClearSurface(clSilver);
    AdDraw1.BeginScene;

    AdImageList1.Find('logo').DrawMask(AdDraw1,rect(100,100,300,300),0,128);
    AdDraw1.EndScene;
    AdDraw1.Flip;

    Done := false;
  end;
end;

end.
