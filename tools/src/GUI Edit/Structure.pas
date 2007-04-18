unit Structure;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ImgList;

type
  TStructureDlg = class(TForm)
    TreeView1: TTreeView;
    ImageList1: TImageList;
    procedure TreeView1Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    OnClickListEntry:TNotifyEvent;
    { Public-Deklarationen }
  end;

implementation

{$R *.dfm}

procedure TStructureDlg.TreeView1Click(Sender: TObject);
begin
  //
end;

end.
