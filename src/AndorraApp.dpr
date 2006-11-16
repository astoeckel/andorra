program AndorraApp;

uses
  Forms,
  AndorraUtils in 'AndorraUtils.pas',
  Andorra in 'Andorra.pas',
  AdDraws in 'AdDraws.pas',
  Main in 'Main.pas' {FmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Anorra 2D - The Next Generation Graphics Engine';
  Application.CreateForm(TFmMain, FmMain);
  Application.Run;
end.
