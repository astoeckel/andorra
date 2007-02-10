program Breakout;

uses
  Forms,
  Main in 'Main.pas' {MainDlg},
  Ad2DList in '..\..\..\src\Ad2DList.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Breakout';
  Application.CreateForm(TMainDlg, MainDlg);
  Application.Run;
end.
