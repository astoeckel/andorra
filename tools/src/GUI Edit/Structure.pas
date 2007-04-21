unit Structure;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ImgList, AdGUI;

type
  TStructureDlg = class(TForm)
    TreeView1: TTreeView;
    ImageList1: TImageList;
    procedure TreeView1Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    GUI:TAdGUI;
    OnClickListEntry:TNotifyEvent;
    { Public-Deklarationen }
  end;

implementation

{$R *.dfm}

procedure TStructureDlg.TreeView1Click(Sender: TObject);
begin
  if TreeView1.Selected <> nil then
  begin
    if Assigned(OnClickListEntry) then
    begin
      OnClickListEntry(GUI.FindComponent(TreeView1.Selected.Text));
    end;
  end;
end;

end.
