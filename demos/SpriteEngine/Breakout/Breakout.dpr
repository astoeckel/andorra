program Breakout;

uses
  Forms,
  Main in 'Main.pas' {MainDlg};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Breakout';
  Application.CreateForm(TMainDlg, MainDlg);
  Application.Run;
end.
