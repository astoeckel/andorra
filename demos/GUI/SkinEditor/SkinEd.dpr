program SkinEd;

uses
  Forms,
  MainEd in 'MainEd.pas' {MainDlg};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Andorra 2D - Skineditor';
  Application.CreateForm(TMainDlg, MainDlg);
  Application.Run;
end.
