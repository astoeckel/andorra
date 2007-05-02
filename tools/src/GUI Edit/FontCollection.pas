unit FontCollection;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, AdDraws, AdGUI;

type
  TFontColl = class(TForm)
    ListBox1: TListBox;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    FontDialog1: TFontDialog;
    Button4: TButton;
    procedure ListBox1DrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure ListBox1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    Fonts:TAdFontCollection;
    GUI:TAdGUI;
    { Private-Deklarationen }
  public
    function ShowDlg(AGUI:TAdGUI):TModalResult;
    procedure UpdateList;
end;

implementation

{$R *.dfm}

procedure TFontColl.Button1Click(Sender: TObject);
var
  aname:string;
begin
  if FontDialog1.Execute then
  begin
    aname := FontDialog1.Font.Name+inttostr(FontDialog1.Font.Size);
    Fonts.Add(aname,FontDialog1.Font.Name,FontDialog1.Font.Style,FontDialog1.Font.Size);
    UpdateList;
  end;
end;

procedure TFontColl.Button2Click(Sender: TObject);
begin
  if GUI.FocusedComponent.Font = Fonts.Items[ListBox1.ItemIndex] then
  begin
    GUI.FocusedComponent.Font := nil;
  end;
  Fonts.Delete(ListBox1.ItemIndex);
  UpdateList;
end;

procedure TFontColl.ListBox1Click(Sender: TObject);
begin
  Button2.Enabled := ListBox1.ItemIndex <> -1;
  Button3.Enabled := ListBox1.ItemIndex <> -1;
end;

procedure TFontColl.ListBox1DrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
  with (Control as TListbox).Canvas do
  begin
    if not (odSelected in State) then
    begin
      Brush.Color := clWindow;
      Font.Color := clBlack;
    end
    else
    begin
      Brush.Color := clHighLight;
      Font.Color := clWhite;
    end;
    FillRect(Rect);
    Font.Name := Fonts.Items[Index].Metadata.Name;
    Brush.Style := bsClear;
    Textout(rect.Left+2,rect.Top,ListBox1.Items[Index]);
  end;
end;

function TFontColl.ShowDlg(AGUI:TAdGUI):TModalResult;
begin
  GUI := AGUI;
  Fonts := AGUI.Fonts;
  UpdateList;
  result := ShowModal;
end;

procedure TFontColl.UpdateList;
var i:integer;
begin
  ListBox1.Clear;
  Button2.Enabled := false;
  Button3.Enabled := false;
  for i := 0 to Fonts.Count - 1 do
  begin
    ListBox1.Items.Add(Fonts.Items[i].Name);
  end;
end;

end.
