unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ComCtrls, XPMan, Designer, ImgList, Objects, Structure,

  //Add all units with components here
  AdGUI, AdComponents;

type
  TMainDlg = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    SaveGUIas1: TMenuItem;
    Savefileas1: TMenuItem;
    N1: TMenuItem;
    NewGUI1: TMenuItem;
    N2: TMenuItem;
    Exit1: TMenuItem;
    Loadrecent1: TMenuItem;
    Norecentfilesfound1: TMenuItem;
    Loadfile1: TMenuItem;
    N3: TMenuItem;
    PageControl1: TPageControl;
    XPManifest1: TXPManifest;
    Windows1: TMenuItem;
    Designer1: TMenuItem;
    Objectinspector1: TMenuItem;
    Project1: TMenuItem;
    Designmode1: TMenuItem;
    N4: TMenuItem;
    Grid1: TMenuItem;
    Off1: TMenuItem;
    N5: TMenuItem;
    N3x31: TMenuItem;
    N5x51: TMenuItem;
    N10x101: TMenuItem;
    ImageList1: TImageList;
    Structure1: TMenuItem;
    procedure Exit1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Windows1Click(Sender: TObject);
    procedure Designer1Click(Sender: TObject);
    procedure Project1Click(Sender: TObject);
    procedure Designmode1Click(Sender: TObject);
    procedure Grid1Click(Sender: TObject);
    procedure N10x101Click(Sender: TObject);
    procedure Objectinspector1Click(Sender: TObject);
    procedure Structure1Click(Sender: TObject);
  private
    procedure ToolButtonClick(Sender: TObject);
    procedure GetComponents(Component:TAdComponent;Node:TTreeNode);
  public
    Designer:TDesignerDlg;
    Objects:TObjectsDlg;
    Structure:TStructureDlg;
    procedure FocusObject(Sender:TObject);
    procedure ChangeList(Sender:TObject);
    procedure ClickListEntry(Sender:TObject);
  end;

var
  MainDlg: TMainDlg;

implementation

{$R *.dfm}

procedure TMainDlg.ToolButtonClick(Sender: TObject);
begin
  Designer.AddComponent(TAdComponentClass(GetClass((Sender as TToolButton).Hint)));
  SetForegroundWindow(Designer.Handle);
end;

procedure TMainDlg.Designmode1Click(Sender: TObject);
begin
  Designer.AdGUI.DesignMode := not Designmode1.Checked;
  Objects.Inspector.Enabled := not Designmode1.Checked;
end;

procedure TMainDlg.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TMainDlg.FocusObject(Sender: TObject);
begin
  Objects.Inspector.InspectObject := Designer.AdGUI.FocusedComponent;
  Objects.Change := true;
  Objects.ComboBox1.ItemIndex :=
    Objects.ComboBox1.Items.IndexOfObject(Objects.Inspector.InspectObject);
  Objects.Change := false;
end;

procedure TMainDlg.GetComponents(Component: TAdComponent;Node:TTreeNode);
var i:integer;
    anode:TTreeNode;
begin
  Objects.ComboBox1.Items.AddObject(Component.Name + ' ['+Component.ClassName+']',Component);
  anode := Structure.TreeView1.Items.AddChild(Node,Component.Name);
  with anode do
  begin
    ImageIndex := 0;
  end;
  for i:= 0 to Component.Components.Count - 1 do
  begin
    GetComponents(Component.Components[i],anode);
  end;
end;

procedure TMainDlg.ChangeList(Sender: TObject);
begin
  Structure.TreeView1.Items.Clear;
  Objects.ComboBox1.Items.Clear;
  GetComponents(Designer.AdGUI,nil);
  Objects.Change := true;
  Objects.ComboBox1.ItemIndex := Objects.ComboBox1.Items.IndexOfObject(Objects.Inspector.InspectObject);
  Objects.Change := false;
  Structure.TreeView1.Items[0].Expand(true);
end;

procedure TMainDlg.ClickListEntry(Sender: TObject);
begin
  if Sender is TAdComponent then
  begin
    (Sender as TAdComponent).SetFocused;
    FocusObject(nil);
  end;
end;

procedure TMainDlg.FormCreate(Sender: TObject);
var i,img:integer;
    page:TTabSheet;
    bar:TToolBar;
    bmp:TBitmap;

  function GetPage(name:string):TTabSheet;
  var i:integer;
  begin
    result := nil;
    for i := 0 to PageControl1.PageCount - 1 do
    begin
      if PageControl1.Pages[i].Caption = name then
      begin
        result := PageControl1.Pages[i];
        break;
      end;
    end;
  end;
begin
  Designer := TDesignerDlg.Create(self);
  Designer.Show;
  Designer.OnFocus := FocusObject;
  Designer.OnChangeList := ChangeList;

  Objects := TObjectsDlg.Create(self);
  Objects.Show;
  Objects.OnClickListEntry := ClickListEntry;

  Structure := TStructureDlg.Create(self);
  Structure.Show;
  Structure.OnClickListEntry := ClickListEntry;

  Top := 0;
  Left := 0;

  Structure.Top := Height;
  Objects.Top := Structure.Height + Height;
  Designer.Top := Height;

  Designer.Left := Objects.Width;
  Objects.Left := 0;
  Structure.Left := 0;
  Objects.Height := Designer.Height - Structure.Height;
  Width := Objects.Width + Designer.Width;

  img := 0;

  for i := 0 to RegisteredComponents.Count-1 do
  begin
    if RegisteredComponents.ValueFromIndex[i] <> '' then
    begin
      page := GetPage(RegisteredComponents.ValueFromIndex[i]);
      if page = nil then
      begin
        page := TTabSheet.Create(self);
        with page do
        begin
          Parent := self;
          PageControl := PageControl1;
          Caption := RegisteredComponents.ValueFromIndex[i];
        end;
        with TToolBar.Create(Page) do
        begin
          Parent := Page;
          Align := alClient;
          ButtonHeight := 26;
          ButtonWidth := 24;
          Images := ImageList1;
        end;
      end;

      bar := page.Controls[0] as TToolBar;

      bmp := TBitmap.Create;
      if FileExists('glyphs\'+RegisteredComponents.Names[i]+'.bmp') then
      begin
        bmp.LoadFromFile('glyphs\'+RegisteredComponents.Names[i]+'.bmp');
      end
      else
      begin
        bmp.LoadFromFile('glyphs\none.bmp');
      end;
      bmp.Transparent := true;
      bmp.TransparentMode := tmAuto;
      ImageList1.AddMasked(bmp,bmp.TransparentColor);
      bmp.Free;

      with TToolButton.Create(bar) do
      begin
        Parent := bar;
        ImageIndex := img;
        Hint := RegisteredComponents.Names[i];
        ShowHint := true;
        OnClick := ToolButtonClick;
      end;

      img := img + 1;
    end;
  end;

  ChangeList(nil);
  ClickListEntry(Designer.AdGUI);
end;

procedure TMainDlg.FormDestroy(Sender: TObject);
begin
  Designer.Free;
end;

procedure TMainDlg.Grid1Click(Sender: TObject);
begin
  if Designer.AdGUI.Grid then
  begin
    case Designer.AdGUI.GridX of
      3:N3x31.Checked := true;
      5:N5x51.Checked := true;
      10:N10x101.Checked := true;
    end;
  end
  else
  begin
    Off1.Checked := true;
  end;
end;

procedure TMainDlg.N10x101Click(Sender: TObject);
begin
  Designer.AdGUI.Grid := TMenuItem(Sender).Tag <> 1;
  Designer.AdGUI.GridX := TMenuItem(Sender).Tag;
  Designer.AdGUI.GridY := TMenuItem(Sender).Tag;
end;

procedure TMainDlg.Project1Click(Sender: TObject);
begin
  Designmode1.Checked := Designer.AdGUI.DesignMode;
end;

procedure TMainDlg.Windows1Click(Sender: TObject);
begin
  Designer1.Checked := Designer.Visible;
  Objectinspector1.Checked := Objects.Visible;
  Structure1.Checked := Structure.Visible;
end;

procedure TMainDlg.Designer1Click(Sender: TObject);
begin
  Designer.Visible := not Designer1.Checked;
end;

procedure TMainDlg.Objectinspector1Click(Sender: TObject);
begin
  Objects.Visible := not Objectinspector1.Checked;
end;

procedure TMainDlg.Structure1Click(Sender: TObject);
begin
  Structure.Visible := not Structure1.Checked;
end;

end.
