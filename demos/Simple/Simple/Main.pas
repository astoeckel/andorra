unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, AdDraws, AdClasses, StdCtrls, AdPNG;

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
    AdImage:TAdImage;
    procedure Idle(Sender:TObject;var Done:boolean);
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  AdPerCounter := TPerformanceCounter.Create;

  AdDraw1 := TAdDraw.Create(self);
  AdDraw1.DllName := 'AndorraOGL.dll';
  if AdDraw1.Initialize then
  begin
    Application.OnIdle := Idle;
    AdImage := TAdImage.Create(AdDraw1);
    AdImage.Texture.LoadGraphicFromFile('Icon64.png',true,clNone);
    AdImage.Restore;
  end
  else
  begin
    ShowMessage('Error while initializing Andorra 2D. Try to use another display'+
                'mode or another video adapter.');
    halt;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  AdImage.Free;
  AdPerCounter.Free;
  AdDraw1.Free;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  //Resize the Backbuffer
  AdDraw1.Setup2DScene;
end;

procedure TForm1.Idle(Sender: TObject; var Done: boolean);
begin
  if AdDraw1.CanDraw then
  begin
    AdPerCounter.Calculate;
    AdDraw1.ClearSurface(clBlack);
    AdDraw1.BeginScene;
    AdImage.Draw(AdDraw1,0,0,0);
    AdDraw1.EndScene;
    AdDraw1.Flip; 
  end;
  Done := false;
end;

end.
