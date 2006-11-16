unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, AdDraws;

type
  TFmMain = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    AdDraw1:TAdDraw;
    procedure ApplicationIdle(Sender:TObject;var Done:boolean);
  end;

var
  FmMain: TFmMain;

implementation

{$R *.dfm}

procedure TFmMain.FormCreate(Sender: TObject);
begin
  Application.OnIdle := ApplicationIdle;

  AdDraw1 := TAdDraw.Create(Handle);
  AdDraw1.Options := [doHardware,doFullscreen];
  AdDraw1.DllName := 'AndorraDX93D.dll';
  if not AdDraw1.Initialize then
  begin
    Application.Terminate;
  end;
end;

procedure TFmMain.FormDestroy(Sender: TObject);
begin
  AdDraw1.Finalize;
  AdDraw1.Free;
end;

procedure TFmMain.ApplicationIdle(Sender: TObject; var Done: Boolean);
begin
  AdDraw1.BeginScene;
  AdDraw1.ClearSurface(clBlack);
  AdDraw1.EndScene;

  AdDraw1.Flip;
  Done := false;
end;

end.
