program PartEd;

uses
  Forms,
  Main in 'Main.pas' {MainFrm},
  PartParam in 'PartParam.pas' {ParamEdt};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Andorra 2D - Particle Editor';
  Application.CreateForm(TMainFrm, MainFrm);
  Application.Run;
end.
