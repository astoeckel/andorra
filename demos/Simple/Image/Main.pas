unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, AdDraws, AdClasses;

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
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var bmp:TBitmap;
    adbmp:TAdBitmap;
begin
  AdPerCounter := TPerformanceCounter.Create;

  AdDraw1 := TAdDraw.Create(self);
  AdDraw1.Options := AdDraw1.Options;
  AdDraw1.DllName := 'AndorraDX93D.dll';
  if AdDraw1.Initialize then
  begin
    Application.OnIdle := Idle;

    AdImageList1 := TPictureCollection.Create(AdDraw1);
    with AdImageList1.Add('logo') do
    begin
      Texture.LoadGraphicFromFile('C:\fc.bmp',True,clWhite);
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
  AdDraw1.Finalize;
  AdDraw1.Initialize;
end;

procedure TForm1.Idle(Sender: TObject; var Done: boolean);
begin
  if AdDraw1.CanDraw then
  begin
    AdPerCounter.Calculate;
    Caption := 'FPS:'+inttostr(AdPerCounter.FPS);

    AdDraw1.ClearSurface(clGray);
    AdDraw1.BeginScene;

    AdImageList1.Find('logo').Draw(AdDraw1,0,0,0);

    AdDraw1.EndScene;
    AdDraw1.Flip;

    Done := false;
  end;
end;

end.
