program ImgEdit;

uses
  Forms,
  Main in 'Main.pas' {MainDlg},
  SetDlg in 'SetDlg.pas' {Settings},
  VarDlg in 'VarDlg.pas' {Variations},
  CompDlg in 'CompDlg.pas' {Compressors},
  Progress in 'Progress.pas' {ProgressDlg};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Andorra Image List Editor';
  Application.CreateForm(TMainDlg, MainDlg);
  Application.CreateForm(TProgressDlg, ProgressDlg);
  Application.Run;
end.
