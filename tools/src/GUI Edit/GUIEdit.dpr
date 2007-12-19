program GUIEdit;

uses
  Forms,
  Designer in 'Designer.pas' {DesignerDlg},
  Main in 'Main.pas' {MainDlg},
  Objects in 'Objects.pas' {ObjectsDlg},
  Structure in 'Structure.pas' {StructureDlg},
  XMLEdit in 'XMLEdit.pas' {XMLEditor},
  ImageEditor in 'ImageEditor.pas' {Images},
  FontCollection in 'FontCollection.pas' {FontCollectionDlg},
  FontEdit in 'FontEdit.pas' {FontEditDlg};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Andorra 2D GUI Demo';
  Application.CreateForm(TMainDlg, MainDlg);
  Application.CreateForm(TFontCollectionDlg, FontCollectionDlg);
  Application.CreateForm(TFontEditDlg, FontEditDlg);
  Application.Run;
end.
