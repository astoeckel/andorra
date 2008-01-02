unit FontEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, AdClasses, AdFont, AdStandardFontGenerator,
  AdFontList, AdFontFactory, AdTypes, ExtCtrls;

type
  TFontEditDlg = class(TForm)
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Edit1: TEdit;
    Button1: TButton;
    Button2: TButton;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Edit2: TEdit;
    Label3: TLabel;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    Label4: TLabel;
    Label5: TLabel;
    Edit3: TEdit;
    Label6: TLabel;
    Edit4: TEdit;
    Label7: TLabel;
    ComboBox2: TComboBox;
    Label8: TLabel;
    Panel1: TPanel;
    Label9: TLabel;
    Edit5: TEdit;
    ColorDialog1: TColorDialog;
    procedure FormCreate(Sender: TObject);
    procedure ComboBox1DrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure Button1Click(Sender: TObject);
    procedure Panel1DblClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    function ShowDlg(AFonts:TAdFontList; var AFont:TAdFont;
      var AName:string):TModalResult;
  end;

var
  FontEditDlg: TFontEditDlg;

implementation

{$R *.dfm}

procedure TFontEditDlg.Button1Click(Sender: TObject);
begin
  if Edit1.Text = '' then
  begin
    Beep;
    Edit1.SetFocus;
  end
  else
    ModalResult := mrOk;             
end;

procedure TFontEditDlg.ComboBox1DrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
  if Control = Combobox1 then
  begin
    Combobox1.Canvas.Brush.Color := clWhite;
    Combobox1.Canvas.FillRect(Rect);
    Combobox1.Canvas.Font.Name := Combobox1.Items[Index];
    Combobox1.Canvas.TextOut(Rect.Left, Rect.Top, Combobox1.Items[Index]);
  end;
end;

procedure TFontEditDlg.FormCreate(Sender: TObject);
begin
  Combobox1.Items.Assign(Screen.Fonts);
  Combobox1.ItemIndex := 0;
end;

procedure TFontEditDlg.Panel1DblClick(Sender: TObject);
begin
  if ColorDialog1.Execute then
  begin
    Panel1.Color := ColorDialog1.Color;
  end;
end;

function TFontEditDlg.ShowDlg(AFonts:TAdFontList; var AFont:TAdFont;
  var AName:string): TModalResult;
var
  style:TAdFontStyles;
  x, y, alpha: integer;
begin
  result := ShowModal;
  if result = mrOk then
  begin
    AName := Edit1.Text;

    style := [];
    if CheckBox1.Checked then style := style + [afItalic];
    if CheckBox2.Checked then style := style + [afBold];
    if CheckBox3.Checked then style := style + [afUnderline];

    x := StrToIntDef(Edit3.Text, 0);
    y := StrToIntDef(Edit4.Text, 0);
    alpha := StrToIntDef(Edit5.Text, 128);
    if alpha > 255 then alpha := 255;
    if alpha < 0 then alpha := 0;

    AFont := AFonts.Fonts.GenerateFont(Combobox1.Items[Combobox1.Itemindex],
      StrToIntDef(Edit2.Text,12), style, Panel1.Color, alpha, x, y,
      StrToInt(Combobox2.Items[Combobox2.ItemIndex]));
  end;
end;

end.
