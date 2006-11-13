program AndorraApp;

uses
  Forms,
  AndorraUtils in 'AndorraUtils.pas',
  Andorra in 'Andorra.pas',
  AdDraws in 'AdDraws.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Anorra 2D - The Next Generation Graphics Engine';
  Application.Run;
end.
