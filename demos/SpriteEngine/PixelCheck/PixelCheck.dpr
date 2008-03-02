program PixelCheck;

uses
  Forms,
  Main in 'Main.pas' {MainFrm},
  AdShapesNew in '..\..\..\src\AdShapesNew.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Andorra 2D - Pixelcheck Demo';
  Application.CreateForm(TMainFrm, MainFrm);
  Application.Run;
end.
