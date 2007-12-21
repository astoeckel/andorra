program MovingPlanet;

uses
  Forms,
  Main in 'Main.pas' {MainFrm};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Moving Planet';
  Application.CreateForm(TMainFrm, MainFrm);
  Application.Run;
end.
