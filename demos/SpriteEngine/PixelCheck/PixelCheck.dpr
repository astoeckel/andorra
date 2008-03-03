program PixelCheck;

uses
  Forms,
  Main in 'Main.pas' {MainFrm};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Andorra 2D - Pixelcheck Demo';
  Application.CreateForm(TMainFrm, MainFrm);
  Application.Run;
end.
