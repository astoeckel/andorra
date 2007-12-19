unit CompDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, AdDraws, AdBitmap;

type
  TCompressors = class(TForm)
    Button1: TButton;
    Button2: TButton;
    GroupBox1: TGroupBox;
    ListBox1: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Compressors: TCompressors;

implementation

{$R *.dfm}

procedure TCompressors.FormCreate(Sender: TObject);
var i:integer;
begin
  ListBox1.Clear;
  for i := 0 to RegisteredGraphicCompressors.Count - 1 do
  begin
    ListBox1.Items.Add(RegisteredGraphicCompressors.ValueFromIndex[i]);
  end;
  ListBox1.ItemIndex := 0;
end;

procedure TCompressors.ListBox1DblClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

end.
