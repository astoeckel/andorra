unit FontCollection;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, FontEdit, AdFont, AdFontList;

type
  TFontCollectionDlg = class(TForm)
    GroupBox1: TGroupBox;
    ListBox1: TListBox;
    Button1: TButton;
    Button3: TButton;
    Button6: TButton;
    procedure Button3Click(Sender: TObject);
    procedure ListBox1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Button6Click(Sender: TObject);
  private
    Fonts:TAdFontList;
    procedure ShowList;
  public
    function ShowDlg(AFonts:TAdFontList):TModalResult;
    procedure DeleteItem;
  end;

var
  FontCollectionDlg: TFontCollectionDlg;

implementation

{$R *.dfm}

procedure TFontCollectionDlg.Button3Click(Sender: TObject);
var
  dlg:TFontEditDlg;
  fnt:TAdFont;
  fntname:string;
begin
  dlg := TFontEditDlg.Create(self);
  if dlg.ShowDlg(Fonts, fnt, fntname) = mrOk then
  begin
    Fonts.Add(fntname, fnt, true);
    ShowList;
  end;
  dlg.Free;
end;

procedure TFontCollectionDlg.Button6Click(Sender: TObject);
begin
  DeleteItem;
end;

procedure TFontCollectionDlg.DeleteItem;
begin
  if (ListBox1.ItemIndex > -1) then
  begin
    Fonts.Delete(ListBox1.ItemIndex);
    ListBox1.Items.Delete(ListBox1.ItemIndex);
    ShowList;
  end;
end;

procedure TFontCollectionDlg.ListBox1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_DELETE then DeleteItem;
end;

function TFontCollectionDlg.ShowDlg(AFonts: TAdFontList): TModalResult;
begin
  Fonts := AFonts;
  ShowList;  
  result := ShowModal;
end;

procedure TFontCollectionDlg.ShowList;
var
  i:integer;
begin
  ListBox1.Clear;
  for i := 0 to Fonts.Count - 1 do
  begin
    ListBox1.Items.Add(Fonts.Names[i]);
  end;    
end;

end.
