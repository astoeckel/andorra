program GUIEdit;

uses
  Forms,
  Designer in 'Designer.pas' {DesignerDlg},
  Main in 'Main.pas' {MainDlg},
  Objects in 'Objects.pas' {ObjectsDlg},
  Structure in 'Structure.pas' {StructureDlg};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Andorra 2D GUI Demo';
  Application.CreateForm(TMainDlg, MainDlg);
  Application.Run;
end.
