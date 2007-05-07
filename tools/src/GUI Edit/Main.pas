unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ComCtrls, XPMan, Designer, ImgList, Objects, Structure, ExtDlgs,
  FontCollection, ImageEditor, AdClasses, AdPNG, PngImage,

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
    Help1: TMenuItem;
    About1: TMenuItem;
    N6: TMenuItem;
    BgImage1: TMenuItem;
    OpenPictureDialog1: TOpenPictureDialog;
    SaveDialog1: TSaveDialog;
    OpenDialog1: TOpenDialog;
    ImageList2: TImageList;
    Clearbackgroundimage1: TMenuItem;
    N7: TMenuItem;
    Loadskin1: TMenuItem;
    OpenDialog2: TOpenDialog;
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
    procedure BgImage1Click(Sender: TObject);
    procedure NewGUI1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Loadfile1Click(Sender: TObject);
    procedure SaveGUIas1Click(Sender: TObject);
    procedure Savefileas1Click(Sender: TObject);
    procedure Clearbackgroundimage1Click(Sender: TObject);
    procedure Loadskin1Click(Sender: TObject);
  private
    procedure ToolButtonClick(Sender: TObject);
    procedure GetComponents(Component:TAdComponent;Node:TTreeNode);
  public
    Designer:TDesignerDlg;
    Objects:TObjectsDlg;
    Structure:TStructureDlg;
    OpenedFile:string;
    Saved:boolean;
    procedure FocusObject(Sender:TObject);
    procedure ChangeList(Sender:TObject);
    procedure ClickListEntry(Sender:TObject);
    function CheckSaved:boolean;
    function Save:boolean;
    function SaveAs:boolean;
    procedure Load(AFile:string);
    procedure RecentClick(Sender:TObject);
    procedure AddRecent(AFile:string);
    procedure DblClickFont(Sender:TObject);
    procedure DblClickImage(Sender:TObject);
  end;

var
  MainDlg: TMainDlg;

implementation

{$R *.dfm}

procedure TMainDlg.ToolButtonClick(Sender: TObject);
begin
  Designer.AddComponent(TAdComponentClass(GetClass((Sender as TToolButton).Hint)));
  SetForegroundWindow(Designer.Handle);
  Saved := false;
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

procedure TMainDlg.Clearbackgroundimage1Click(Sender: TObject);
begin
  Designer.AdImage.Texture.Clear;
end;

procedure TMainDlg.ClickListEntry(Sender: TObject);
begin
  if Sender is TAdComponent then
  begin
    (Sender as TAdComponent).SetFocused;
    FocusObject(nil);
  end;
end;

procedure TMainDlg.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if CheckSaved then
  begin
    Action := caFree;
  end
  else
  begin
    Action := caNone;
  end;
end;

procedure TMainDlg.FormCreate(Sender: TObject);
var i,img:integer;
    page:TTabSheet;
    bar:TToolBar;
    bmp:TBitmap;
    recent:TStringList;
    mi:TMenuItem;

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
  ReportMemoryLeaksOnShutdown := true;

  recent := TStringList.Create;
  if FileExists('gui.recent') then
  begin
    recent.LoadFromFile('gui.recent');
    if recent.Count > 0 then
    begin
      LoadRecent1.Clear;
    end;
    for i := 0 to recent.Count - 1 do
    begin
      if FileExists(recent[i]) then
      begin
        mi := TMenuItem.Create(LoadRecent1);
        with mi do
        begin
          Caption := recent[i];
          OnClick := RecentClick;
        end;
        LoadRecent1.Add(mi);
      end;
    end;
  end;
  recent.Free;

  OpenedFile := '';
  Saved := true;

  Designer := TDesignerDlg.Create(self);
  Designer.Show;
  Designer.OnFocus := FocusObject;
  Designer.OnChangeList := ChangeList;

  Objects := TObjectsDlg.Create(self);
  Objects.Show;
  Objects.OnClickListEntry := ClickListEntry;
  Objects.OnDblClickFont := DblClickFont;
  Objects.OnDblClickImage := DblClickImage;

  Structure := TStructureDlg.Create(self);
  Structure.Show;
  Structure.OnClickListEntry := ClickListEntry;
  Structure.GUI := Designer.AdGUI;

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
  Objects.Free;
  Structure.Free;
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

procedure TMainDlg.Load(AFile: string);
begin
  if CheckSaved then
  begin
    AddRecent(AFile);
    Designer.AdGUI.LoadFromFile(AFile);
    OpenedFile := AFile;
    Saved := true;
    ChangeList(nil);
  end;
end;

procedure TMainDlg.Loadfile1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    Load(OpenDialog1.FileName);
  end;
end;


procedure TMainDlg.Loadskin1Click(Sender: TObject);
begin
  if OpenDialog2.Execute then
  begin
    Designer.AdGUI.Skin.LoadFromFile(OpenDialog2.FileName);
  end;
end;

procedure TMainDlg.AddRecent(AFile: string);
var recent:TStringlist;
begin
  recent := TStringlist.Create;
  if FileExists('gui.recent') then
  begin
    recent.LoadFromFile('gui.recent');
  end;
  if recent.IndexOf(AFile) > -1 then
  begin
    recent.Delete(recent.IndexOf(AFile));
  end;
  recent.Insert(0,AFile);
  if recent.Count > 20 then
  begin
    recent.Delete(recent.Count-1);
  end;
  try
    recent.SaveToFile('gui.recent');
  finally
    recent.Free;
  end;
end;

procedure TMainDlg.BgImage1Click(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
  begin
    Designer.AdImage.Texture.LoadGraphicFromFile(OpenPictureDialog1.FileName,true,clNone);
    Designer.AdImage.Restore;
  end;
end;

procedure TMainDlg.N10x101Click(Sender: TObject);
begin
  Designer.AdGUI.Grid := TMenuItem(Sender).Tag <> 1;
  Designer.AdGUI.GridX := TMenuItem(Sender).Tag;
  Designer.AdGUI.GridY := TMenuItem(Sender).Tag;
end;

procedure TMainDlg.NewGUI1Click(Sender: TObject);
begin
  if CheckSaved then
  begin
    Designer.AdGUI.Clear;
  end;
end;

procedure TMainDlg.Project1Click(Sender: TObject);
begin
  Designmode1.Checked := Designer.AdGUI.DesignMode;
end;

procedure TMainDlg.RecentClick(Sender: TObject);
begin
  if FileExists(TMenuItem(sender).Caption) then
  begin
    Load(TMenuItem(sender).Caption);
  end;
end;

procedure TMainDlg.Windows1Click(Sender: TObject);
begin
  Designer1.Checked := Designer.Visible;
  Objectinspector1.Checked := Objects.Visible;
  Structure1.Checked := Structure.Visible;
end;

procedure TMainDlg.DblClickFont(Sender: TObject);
var
  dlg:TFontColl;
begin
  dlg := TFontColl.Create(nil);
  if dlg.ShowDlg(Designer.AdGUI) = mrOk then
  begin
    Designer.AdGUI.FocusedComponent.Font := Designer.AdGUI.Fonts.Items[dlg.ListBox1.ItemIndex];
  end;
  dlg.Free;
end;

procedure TMainDlg.DblClickImage(Sender: TObject);

  procedure AddAlpha(APNG:TPNGObject;ABMP:TAdBitmap);
  var x,y:integer;
      sl1:PByteArray;
      sl2:PRGBARec;
  begin
    sl2 := ABMP.ScanLine;
    for y := 0 to APNG.Height-1 do
    begin
      sl1 := APNG.AlphaScanline[y];
      for x := 0 to APNG.Width - 1 do
      begin
        sl1[x] := sl2^.a;
        inc(sl2);
      end;
    end;
  end;

  procedure AddRGB(APNG:TPNGObject;ABMP:TAdBitmap);
  var Bmp:TBitmap;
  begin
    Bmp := TBitmap.Create;
    ABMP.AssignToBitmap(Bmp);
    APNG.Assign(Bmp);
    Bmp.Free;
  end;

var
  dlg:TImages;
  png:TPngObject;
  adbmp:TAdBitmap;
begin
  png := nil;
  dlg := TImages.Create(nil);
  
  if TAdResourceImage(Sender).Loaded then
  begin
    png := TPngObject.Create;
    adbmp := TAdBitmap.Create;

    with TAdResourceImage(Sender).Picture.Texture.Texture do
    begin
      adbmp.ReserveMemory(BaseWidth,BaseHeight);
      SaveToBitmap(adbmp);
    end;

    AddRGB(PNG,adbmp);
    PNG.CreateAlpha;
    AddAlpha(PNG,adbmp);

    dlg.Image1.Picture.Graphic := png;

    adbmp.Free;
  end;

  if dlg.ShowModal = mrOk then
  begin
    if dlg.Image1.Picture.Graphic <> nil then
    begin
      with TAdResourceImage(Sender).Picture do
      begin
        Texture.LoadFromGraphic(dlg.Image1.Picture.Graphic);
        Restore;
      end;
      TAdResourceImage(Sender).Compressor := TPNGCompressor;
    end
    else
    begin
      with TAdResourceImage(Sender).Picture do
      begin
        Texture.Clear;
        Restore;
      end;
    end;
  end;

  if png <> nil then
  begin
    png.Free;
  end;
  dlg.Free;
end;

procedure TMainDlg.Designer1Click(Sender: TObject);
begin
  Designer.Visible := not Designer1.Checked;
end;

procedure TMainDlg.Objectinspector1Click(Sender: TObject);
begin
  Objects.Visible := not Objectinspector1.Checked;
end;

function TMainDlg.Save:boolean;
begin
  if OpenedFile = '' then
  begin
    result := SaveAs;
  end
  else
  begin
    AddRecent(OpenedFile);
    Designer.AdGUI.SaveToFile(OpenedFile);
    result := true;
    Saved := true;
  end;
end;

function TMainDlg.SaveAs:boolean;
begin
  result := false;
  if SaveDialog1.Execute then
  begin
    if SaveDialog1.FilterIndex = 1 then
    begin
      OpenedFile := ChangeFileExt(SaveDialog1.FileName,'.axg');
    end
    else
    begin
      OpenedFile := SaveDialog1.FileName;
    end;
    Save;
    result := true;
  end;
end;

procedure TMainDlg.Savefileas1Click(Sender: TObject);
begin
  SaveAs;
end;

procedure TMainDlg.SaveGUIas1Click(Sender: TObject);
begin
  Save;
end;

function TMainDlg.CheckSaved: boolean;
var
  id:integer;
begin
  if saved then
  begin
    result := true;
  end
  else
  begin
    id := Application.MessageBox('You probably did''t save your project. Do you want to save it now?',
          'Question',MB_YESNOCANCEL or MB_ICONQUESTION);
    if id = ID_YES then
    begin
      result := Save;
    end else
    if id = ID_NO then
    begin
      result := true;
    end else
      result := false;       
  end;
end;

procedure TMainDlg.Structure1Click(Sender: TObject);
begin
  Structure.Visible := not Structure1.Checked;
end;

end.
