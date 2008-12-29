unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, ComCtrls, Menus, ToolWin, ExtCtrls, XPMan, StdCtrls,
  ActnList, AdDraws, SetDlg, AdClasses, ExtDlgs, CompDlg,
  AdCanvas, AdBitmap, AdVCLFormats, AdSimpleCompressors, AdTypes, AdPerformanceCounter,
  Progress, AdPersistent, AdDevIL;

type
  TMainDlg = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    ToolBar1: TToolBar;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    LoadnewImageLibrary1: TMenuItem;
    LoadLibrary1: TMenuItem;
    N2: TMenuItem;
    SaveLibrary1: TMenuItem;
    SaveLibraryas1: TMenuItem;
    Exit1: TMenuItem;
    Image1: TMenuItem;
    Add1: TMenuItem;
    Imgs_Menu: TImageList;
    GroupBox2: TGroupBox;
    ListView1: TListView;
    Imgs_Preview: TImageList;
    PopupMenu1: TPopupMenu;
    Add2: TMenuItem;
    N5: TMenuItem;
    Delete2: TMenuItem;
    Settings2: TMenuItem;
    XPManifest1: TXPManifest;
    OpenPictureDialog1: TOpenPictureDialog;
    N4: TMenuItem;
    SaveDialog1: TSaveDialog;
    ODImageList: TOpenDialog;
    Setcompressorforallpictures1: TMenuItem;
    GroupBox1: TGroupBox;
    Splitter1: TSplitter;
    ListView2: TListView;
    ToolBar2: TToolBar;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    Imgs_Anim: TImageList;
    Panel3: TPanel;
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    N6: TMenuItem;
    Compoundselectedpictures1: TMenuItem;
    N7: TMenuItem;
    Export1: TMenuItem;
    SavePictureDialog1: TSavePictureDialog;
    View1: TMenuItem;
    Tiledbackground1: TMenuItem;
    Image2: TImage;
    ColorDialog1: TColorDialog;
    bgcolor1: TMenuItem;
    Imgs_Toolbar: TImageList;
    ToolButton1: TToolButton;
    ToolButton3: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    StatusBar1: TStatusBar;
    ToolButton2: TToolButton;
    N1: TMenuItem;
    Clearalphachannel1: TMenuItem;
    Setalphachannel1: TMenuItem;
    OpenPictureDialog2: TOpenPictureDialog;
    N8: TMenuItem;
    Library1: TMenuItem;
    Exportimages1: TMenuItem;
    N3: TMenuItem;
    Importlibrary1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListView1CustomDrawItem(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure CreateFilter;
    procedure ImportClick(Sender: TObject);
    procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ListView1Change(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure ListView1Click(Sender: TObject);
    procedure DeleteClick(Sender: TObject);
    procedure ListView1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Settings2Click(Sender: TObject);
    procedure ListView1DblClick(Sender: TObject);
    procedure NewClick(Sender: TObject);
    procedure OpenClick(Sender: TObject);
    procedure SaveClick(Sender: TObject);
    procedure SaveLibraryas1Click(Sender: TObject);
    procedure Setcompressorforallpictures1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure ListView2Change(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure Compoundselectedpictures1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Export1Click(Sender: TObject);
    procedure Tiledbackground1Click(Sender: TObject);
    procedure bgcolor1Click(Sender: TObject);
    procedure ListView1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure ListView1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure Panel1Resize(Sender: TObject);
    procedure Panel1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Clearalphachannel1Click(Sender: TObject);
    procedure Setalphachannel1Click(Sender: TObject);
    procedure Exportlibrary1Click(Sender: TObject);
    procedure Importlibrary1Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    AdDraw:TAdDraw;
    AdImageList:TAdImageList;
    AdPerCounter:TAdPerformanceCounter;
    Tiles:TAdImage;
    AColor:TColor;

    OffsetX,OffsetY:integer;
    mx,my:integer;
    pattern:double;

    CurrentFile:string;

    Changed:boolean;

    function CheckSaved:boolean;

    function Save:boolean;
    function SaveAs:boolean;
    procedure Load(AFile:string);

    procedure Idle(Sender:TObject; var Done:boolean);
    procedure ViewContent;
    procedure UpdateState;
    procedure CreateAnimPatterns;

    procedure ExportImage(const AIndex : Integer; const AFilename : string);
  end;

var
  MainDlg: TMainDlg;

implementation
uses
  {$WARNINGS OFF}FileCtrl{$WARNINGS ON};

{$R *.dfm}

function TMainDlg.CheckSaved: boolean;
var
  id:integer;
begin
  result := true;
  if Changed then
  begin
    id := Application.MessageBox('You probably did not save your changes. Do you want to save them now?','Confirm',MB_YESNOCANCEL);
    if id = ID_YES then
    begin
      result := Save;
    end
    else
    begin
      if id = ID_CANCEL then
      begin
        result := false;
      end;
    end;
  end;
end;

procedure TMainDlg.Clearalphachannel1Click(Sender: TObject);
var
  i:integer;
  adbmp:TAdBitmap;
begin
  for i := 0 to ListView1.Items.Count-1 do
  begin
    if ListView1.Items[i].Selected then
    begin
      adbmp := TAdBitmap.Create;
      with AdImageList[i].Texture.Texture do
      begin
        adbmp.ReserveMemory(basewidth, baseheight);
        SaveToBitmap(adbmp);
        adbmp.ClearAlphaChannel;
        LoadFromBitmap(adbmp, AdImageList[i].Texture.BitDepth);
      end;
      adbmp.Free;
    end;
  end;
end;

procedure TMainDlg.Compoundselectedpictures1Click(Sender: TObject);
var i,x,y,j,c:integer;
    xcount,ycount:double;
    prop:single;
    w,h:integer;
    bmp,bmp2,abmp,abmp2:TBitmap;
    adbmp:TAdBitmap;
begin
  w := 0;
  h := 0;
  c := 0;
  for i := 0 to ListView1.Items.Count-1 do
  begin
    with AdImageList do
    begin
      if ((w <> 0) and (h <> 0)) and ((ListView1.Items[i].Selected) and
         ((Items[i].Width <> w) or
         (Items[i].Height <> h))) then
      begin
        ShowMessage('All selected pictures must have the same size!');
        exit;
      end
      else
      begin
        if ListView1.Items[i].Selected then
        begin
          w := Items[i].Width;
          h := Items[i].Height;
          c := c + 1;
        end;
      end;
    end;
  end;
  prop := w/h;
  xcount := round(sqrt(listview1.SelCount)*(1/prop));
  ycount := round(sqrt(listview1.SelCount)*(prop));
  if xcount*ycount < ListView1.SelCount then xcount := xcount + 1;
  

  bmp := TBitmap.Create;
  bmp.Width := round(xcount*w);
  bmp.Height := round(ycount*h);

  abmp := TBitmap.Create;
  abmp.Width := round(xcount*w);
  abmp.Height := round(ycount*h);

  x := 0;
  y := 0;
  for i := 0 to ListView1.Items.Count - 1 do
  begin
    if ListView1.Items[i].Selected then
    begin
      if x >= xcount then
      begin
        y := y + 1;
        x := 0;
      end;
      bmp2 := TBitmap.Create;
      abmp2 := TBitmap.Create;
      adbmp := TAdBitmap.Create;

      with AdImageList[i] do
      begin
        adbmp.ReserveMemory(Texture.Texture.BaseWidth,Texture.Texture.BaseHeight);
        Texture.Texture.SaveToBitmap(adbmp);
      end;
      adbmp.AssignTo(bmp2);
      adbmp.AssignAlphaChannelTo(abmp2);
      adbmp.Free;

      if (x = 0) and (y = 0) then
      begin
        bmp.Canvas.Brush.Color := bmp2.Canvas.Pixels[0,0];
        bmp.Canvas.FillRect(rect(0,0,round(w*xcount),round(h*ycount)));
      end;

      bmp.Canvas.Draw(x*w,y*h,bmp2);
      abmp.Canvas.Draw(x*w,y*h,abmp2);

      bmp2.Free;
      abmp2.Free;

      x := x + 1;
    end;
  end;

  j := 0;
  for i := 0 to ListView1.Items.Count - 1 do
  begin
    if ListView1.Items[i].Selected then
    begin
      AdImageList.Delete(j);
    end
    else
    begin
      j := j + 1;
    end;
  end;

  adbmp := TAdBitmap.Create;
  adbmp.Assign(bmp);
  adbmp.AssignAlphaChannel(abmp);

  with AdImageList.Add('animation') do
  begin
    Texture.Texture.LoadFromBitmap(adbmp, ad32Bit);
    PatternWidth := w;
    PatternHeight := h;
    Restore;
    PatternStop := PatternCount - c;
  end;


  adbmp.Free;

  bmp.Free;
  abmp.Free;

  ViewContent;
end;

procedure TMainDlg.CreateFilter;
var i,j:integer;
    str:TStringList;
    fmt:string;
    cref:TAdGraphicFormatClass;
    c:integer;
begin
  OpenPictureDialog1.Filter := '';
  fmt := '';
  c := 0;
  for i := 0 to RegisteredGraphicFormats.Count-1 do
  begin
    str := TStringList.Create;
    cref := TAdGraphicFormatClass(AdGetClass(RegisteredGraphicFormats[i]));
    if cref <> nil then
    begin
      cref.FileExts(str);
      for j := 0 to str.Count - 1 do
      begin
        OpenPictureDialog1.Filter := OpenPictureDialog1.Filter+
          uppercase(copy(str[j],2,length(str[j])-1))+' File (*'+str[j]+')|*'+str[j]+'|';
        fmt := fmt+'*'+str[j]+'; ';
        c := c+1;
      end;
    end;
    str.Free;
  end;
  OpenPictureDialog1.Filter := OpenPictureDialog1.Filter+'All supportet files ('+fmt+')|'+fmt+'|';
  OpenPictureDialog1.Filter := OpenPictureDialog1.Filter+'All files (*.*)|*.*';
  OpenPictureDialog1.FilterIndex := c+1;
end;

procedure TMainDlg.DeleteClick(Sender: TObject);
var
  i:integer;
begin

  ListView1.Items.BeginUpdate;

  try
    for i := ListView1.Items.Count-1 downto 0 do
    begin
      if ListView1.Items[i].Selected then
      begin
        AdImageList.Delete(i);
        ListView1.Items.Delete(i);
      end;
    end;
  finally
    ListView1.Items.EndUpdate;
  end;


  UpdateState;
end;

procedure TMainDlg.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TMainDlg.Export1Click(Sender: TObject);
begin
  if ListView1.ItemIndex <> -1 then
  begin
    if SavePictureDialog1.Execute then
    begin
      ExportImage(ListView1.ItemIndex, SavePictureDialog1.FileName);
    end;
  end;
end;

procedure TMainDlg.ExportImage(const AIndex: Integer; const AFilename : string);
var
  adbmp:TAdBitmap;
  bmp:TBitmap;
begin
  adbmp := TAdBitmap.Create;
  bmp := TBitmap.Create;
  try
    with AdImageList[AIndex] do
    begin
      adbmp.ReserveMemory(Texture.Texture.BaseWidth, Texture.Texture.BaseHeight);
      Texture.Texture.SaveToBitmap(adbmp);
    end;

    adbmp.AssignTo(bmp);
    bmp.SaveToFile(AFilename);
  finally
    bmp.Free;
    adbmp.Free;
  end;
end;

procedure TMainDlg.Exportlibrary1Click(Sender: TObject);
var
  i: Integer;
  fn, Path : string;
begin
  if SelectDirectory('Please select the outputpath of the images', '', Path, [sdNewFolder,sdNewUI]) then
  begin
    Path := Path + '\';
    for i := 0 to ListView1.Items.Count - 1 do
    begin
      fn := AdImageList[i].Name;
      if fn = '' then
      begin
        fn := ExtractFileName(CurrentFile);
        if LastDelimiter('.', fn) > 0 then
        begin
          fn := Copy(fn, 1, LastDelimiter('.', fn) - 1);
        end;
        fn := fn + '_' + IntToStr(i);
      end;
      fn := fn + '.bmp';
      ExportImage(i, Path + fn);
    end;
  end;
end;

procedure TMainDlg.FormClose(Sender: TObject; var Action: TCloseAction);
var i:integer;
begin
  if CheckSaved then
  begin
    for i := 0 to ListView1.Items.Count - 1 do
    begin
      ListView1.Items[i].Selected := false;
    end;
  end
  else
  begin
    Action := caNone;
  end;
end;

procedure TMainDlg.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;

  ChDir(ExtractFilePath(Application.ExeName));

  AdPerCounter := TAdPerformanceCounter.Create;

  AdDraw := TAdDraw.Create(Panel1);
  AdDraw.DllName := 'AndorraOGL.dll';
  if AdDraw.Initialize then
  begin
    Application.OnIdle := Idle;
    AdImageList := TAdImageList.Create(AdDraw);
    AColor := ColorToRGB(clBtnFace);

    CreateFilter;

    Tiles := TAdImage.Create(AdDraw);
    Tiles.Texture.LoadFromGraphic(Image2.Picture.Bitmap);    
    Tiles.Restore;

    if ParamCount > 0 then
    begin
      Load(ParamStr(1));
    end;

    UpdateState;
  end
  else
  begin
    ShowMessage(AdDraw.GetLastError);
    Close;
  end;
end;

procedure TMainDlg.FormDestroy(Sender: TObject);
begin
  Tiles.Free;
  AdPerCounter.Free;
  AdImageList.Free;
  AdDraw.Free;
end;

procedure TMainDlg.Idle(Sender: TObject; var Done: boolean);
var
  c:TAndorraColor;
begin
  if AdDraw.CanDraw then
  begin
    AdPerCounter.Calculate;

    AdDraw.ClearSurface(AColor);
    AdDraw.BeginScene;

    if TiledBackground1.Checked then
    begin
      Tiles.DrawEx(AdDraw,
        AdRect(0,0,Panel1.Width,Panel1.Height),
        AdRect(0,0,Panel1.Width,Panel1.Height), 0.5, 0.5, 0, 255,
        bmAlpha);
    end;
    
    if ListView1.ItemIndex <> -1 then
    begin
      with AdImageList[ListView1.ItemIndex] do
      begin
        if Toolbutton7.Down then
        begin
          Pattern := Pattern + StrToIntDef(Edit1.Text,0) * (AdPerCounter.TimeGap/1000);
          if Pattern > PatternCount-1 then Pattern := 0;
          Done := false;
        end
        else
        begin
          Done := true;
        end;

        Draw(AdDraw,(AdDraw.Window.ClientWidth - Width) div 2 + OffsetX,(AdDraw.Window.ClientHeight - Height) div 2 + OffsetY,round(pattern));
      end;
    end;

    with AdDraw.Canvas do
    begin
      c := ColorToAdColor(ColorToRGB(clBtnFace));
      Brush.Color := c;
      c.a := 0;
      Brush.GradientColor := c;
      Brush.GradientDirecton := gdVertical;
      Pen.Width := 0;

      Rectangle(0,0,5,Panel1.ClientHeight);

      Release;
    end;

    AdDraw.EndScene;
    AdDraw.Flip;
  end;
end;

procedure TMainDlg.Tiledbackground1Click(Sender: TObject);
begin
  TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
end;

procedure TMainDlg.ListView1Change(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  if (AdImageList.Count > 0) and (Change = ctState) then
  begin
    OffsetX := 0;
    OffsetY := 0;
    UpdateState;
  end;
end;

procedure TMainDlg.ListView1Click(Sender: TObject);
begin
  UpdateState;
end;

procedure TMainDlg.ListView1CustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  x,y,i:integer;
  s:string;
begin
  with Sender as TListView do
  begin
    with Canvas do
    begin

      if Item.Index mod 2 = 1 then
      begin
        Brush.Color := rgb(240,240,255);
      end
      else
      begin
        Brush.Color := rgb(255,255,255);
      end;

      if Item.Selected then
      begin
        Brush.Color := rgb(220,220,255);
        Pen.Style := psDot;
        Pen.Color := clGray;
      end
      else
      begin
        Pen.Style := psSolid;
        Pen.Color := Brush.Color;
      end;

      Rectangle(Item.DisplayRect(drLabel).Left+2,Item.DisplayRect(drLabel).Top+2,
                Item.DisplayRect(drBounds).Right-2,Item.DisplayRect(drLabel).Bottom-2);

      Font.Style := [fsBold];

      x := Item.DisplayRect(drLabel).Left+4;
      y := Item.DisplayRect(drLabel).Top+4;

      for i := 0 to Item.SubItems.Count-1 do
      begin
        s := Item.SubItems[i];
        TextOut(x,y,s);
        y := y + TextHeight(s);
        Font.Style := [];
        Font.Color := clGray;
        Sender.Canvas.Refresh;
      end;

      SmallImages.Draw(Canvas,Item.DisplayRect(drIcon).Left,
       Item.DisplayRect(drIcon).Top,Item.ImageIndex,dsTransparent,itImage);
    end;
  end;
  DefaultDraw := false;
end;

procedure TMainDlg.ListView1DblClick(Sender: TObject);
begin
  if ListView1.ItemIndex <> -1 then
  begin
    Settings2Click(nil);
  end
  else
  begin
    ImportClick(nil);
  end;
end;

procedure TMainDlg.ListView1DragDrop(Sender, Source: TObject; X, Y: Integer);
var index1,index2:integer; 
    item:TAdImage;
begin
  Changed := true;
  index1 := ListView1.ItemFocused.Index;
  if ListView1.GetItemAt(X,Y) <> nil then
  begin
    index2 := ListView1.GetItemAt(X,Y).Index;
  end
  else
  begin
    index2 := ListView1.Items.Count-1;
  end;
  item := AdImageList.Items[index1];
  item.FreeByList := nil;
  AdImageList.Delete(index1);
  AdImageList.Insert(index2,item);
  item.FreeByList := AdImageList;
  ViewContent;
  ListView1.ItemIndex := index2;
  ListView1.Items[index2].MakeVisible(false);
end;

procedure TMainDlg.ListView1DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := false;
  if Source = Sender then
  begin
    Accept := true;
  end;
end;

procedure TMainDlg.ListView1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  i:integer;
begin
  if key = VK_DELETE then
  begin
    i := ListView1.ItemIndex;
    if i <> -1 then
    begin
      DeleteClick(nil);
      if i < ListView1.Items.Count then
      begin
        ListView1.ItemIndex := i;
      end
      else
      begin
        ListView1.ItemIndex := i - 1;
      end;
    end;
    UpdateState;
  end;
end;

procedure TMainDlg.ListView2Change(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  if Item <> nil then
  begin
    Pattern := Item.Index;
  end
  else
  begin
    Pattern := 0;
  end;
end;

procedure TMainDlg.Load(AFile: string);
begin
  Screen.Cursor := crHourglass;
  try
    AdImageList.LoadFromFile(AFile);
    CurrentFile := AFile;
    Changed := false;
    ViewContent;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainDlg.bgcolor1Click(Sender: TObject);
begin
  if ColorDialog1.Execute then
  begin
    AColor := ColorDialog1.Color;
  end;
end;

procedure TMainDlg.Panel1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mx := x;
  my := y;
  Screen.Cursor := crSizeAll;
end;

procedure TMainDlg.Panel1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if ssLeft in Shift then
  begin
    OffsetX := OffsetX + (x-mx);
    mx := x;
    OffsetY := OffsetY + (y-my);
    my := y;
  end;
end;

procedure TMainDlg.Panel1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Screen.Cursor := crDefault;
end;

procedure TMainDlg.Panel1Resize(Sender: TObject);
begin
  if AdDraw.Initialized then
  begin
    AdDraw.Setup2DScene;
  end;
end;

procedure TMainDlg.SaveLibraryas1Click(Sender: TObject);
begin
  SaveAs;
end;

procedure TMainDlg.Setalphachannel1Click(Sender: TObject);
var
  i:integer;
  adbmp:TAdBitmap;
  bmp:TBitmap;
begin
  if OpenPictureDialog2.Execute then
  begin
    bmp := TBitmap.Create;
    bmp.LoadFromFile(OpenPictureDialog2.FileName);

    for i := 0 to ListView1.Items.Count-1 do
    begin
      if (ListView1.Items[i].Selected) and (AdImageList[i].Width = bmp.Width) and (AdImageList[i].Height = bmp.Height)then
      begin
        adbmp := TAdBitmap.Create;
        with AdImageList[i].Texture.Texture do
        begin
          adbmp.ReserveMemory(basewidth, baseheight);
          SaveToBitmap(adbmp);
          adbmp.AssignAlphaChannel(bmp);
          LoadFromBitmap(adbmp, AdImageList[i].Texture.BitDepth);
        end;
        adbmp.Free;
      end;
    end;

    bmp.Free;
  end;
  ViewContent;
end;

procedure TMainDlg.Setcompressorforallpictures1Click(Sender: TObject);
var
  comp:TCompressors;
begin
  comp := TCompressors.Create(self);
  if comp.ShowModal = mrOk then
  begin
    AdImageList.Compressor := TAdGraphicCompressorClass(AdGetClass(Comp.ListBox1.Items[Comp.ListBox1.ItemIndex]));
  end;
  comp.Free;  
end;

procedure TMainDlg.Settings2Click(Sender: TObject);
var SetDlg:TSettings;
    ii:integer;
begin
  if ListView1.ItemIndex > -1 then
  begin
    ii := ListView1.ItemIndex;
    SetDlg := TSettings.Create(self);
    try
      if SetDlg.InspectImage(AdImageList.Items[ListView1.ItemIndex]) = mrOk then
      begin
        Changed := true;
      end;
      ViewContent;
    finally
      SetDlg.Free;
    end;
    ListView1.Selected := ListView1.Items[ii];
    ListView1.Selected.MakeVisible(false);
  end;
end;

procedure TMainDlg.NewClick(Sender: TObject);
begin
  if CheckSaved then
  begin
    Changed := false;
    CurrentFile := '';
    AdImageList.Clear;
    ViewContent;
  end;
end;

procedure TMainDlg.OpenClick(Sender: TObject);
begin
  if CheckSaved and ODImageList.Execute then
  begin
    Load(ODImageList.FileName);
  end;
end;

function TMainDlg.Save:boolean;
begin
  if CurrentFile <> '' then
  begin
    result := true;
    Screen.Cursor := crHourglass;
    AdImageList.SaveToFile(CurrentFile);
    Changed := false;
    Screen.Cursor := crDefault;
  end
  else
  begin
    result := SaveAs;
  end;
end;

function TMainDlg.SaveAs: boolean;
begin
  result := false;
  if SaveDialog1.Execute then
  begin
    CurrentFile := ChangeFileExt(SaveDialog1.FileName,'.ail');
    AdImageList.SaveToFile(CurrentFile);
    Changed := false;
    result := true;
  end;
end;

procedure TMainDlg.SaveClick(Sender: TObject);
begin
  Save;
end;

procedure TMainDlg.ImportClick(Sender: TObject);
var
  i:integer;
  progdlg:TProgressDlg;
begin
  if OpenPictureDialog1.Execute then
  begin
    progdlg := TProgressDlg.Create(self);
    try
      progdlg.Show;
      progdlg.ProgressBar1.Max := OpenPictureDialog1.Files.Count;

      for i := 0 to OpenPictureDialog1.Files.Count - 1 do
      begin
        with AdImageList.Add(extractfilename(copy(OpenPictureDialog1.Files[i],1,
              length(OpenPictureDialog1.Files[i])-
              length(ExtractFileExt(OpenPictureDialog1.Files[i]))))) do
        begin
          Screen.Cursor := crHourglass;
          try
            Texture.LoadGraphicFromFile(OpenPictureDialog1.Files[i],true,clNone);
          finally
            Screen.Cursor := crDefault;
          end;

          Changed := true;

          progdlg.ProgressBar1.Position := progdlg.ProgressBar1.Position + 1;
          progdlg.Repaint;
        end;
      end;
      AdImageList.Restore;
      ViewContent;
    finally
      progdlg.Free;
    end;
  end;
end;

procedure TMainDlg.Importlibrary1Click(Sender: TObject);
var
  AdBufferImages : TAdImageList;
  i: Integer;
  ProgDlg : TProgressDlg;
begin
  if ODImageList.Execute then
  begin
    AdBufferImages := TAdImageList.Create(AdDraw);
    ProgDlg := TProgressDlg.Create(self);
    try
      ProgDlg.Show;
      AdBufferImages.LoadFromFile(ODImageList.FileName);
      ProgDlg.ProgressBar1.Max := AdBufferImages.Count;
      for i := 0 to AdBufferImages.Count - 1 do
      begin
        with AdImageList.Add(AdBufferImages[i].Name) do
        begin
          Assign(AdBufferImages[i]);
        end;
        //Sleep(100);
        ProgDlg.ProgressBar1.StepIt;
        ProgDlg.Repaint;
      end;
      if AdBufferImages.Count > 0 then
        Changed := true;
      AdImageList.Restore;
      ViewContent;
    finally
      ProgDlg.Free;
      AdBufferImages.Free;
    end;
  end;
end;

procedure TMainDlg.UpdateState;
var
  SelectedItem : Boolean;
begin
  SelectedItem := ListView1.ItemIndex <> -1;
  Delete2.Enabled := SelectedItem;
  Settings2.Enabled := SelectedItem;
  Export1.Enabled := SelectedItem;
  Compoundselectedpictures1.Enabled := ListView1.SelCount > 1;
  ToolButton1.Enabled := SelectedItem;
  ToolButton6.Enabled := SelectedItem;
  ToolButton13.Enabled := SelectedItem;

  CreateAnimPatterns;
end;

procedure TMainDlg.CreateAnimPatterns;
var i,w,h:integer;
    bmp,bmp2:TBitmap;
    adbmp:TAdBitmap;
    r:TAdRect;
begin
  if (ListView1.ItemIndex <> -1) and (ListView1.SelCount = 1) and (AdImageList.Items[ListView1.ItemIndex].PatternCount > 1) then
  begin
    GroupBox1.Visible := true;
    Splitter1.Visible := true;
    Splitter1.Top := GroupBox1.Top;

    //Create Pattern-Images

    Screen.Cursor := crHourglass;

    with AdImageList[ListView1.ItemIndex] do
    begin
      adbmp := TAdBitmap.Create;
      adbmp.ReserveMemory(Texture.Texture.BaseWidth,Texture.Texture.BaseHeight);
      Texture.Texture.SaveToBitmap(adbmp);

      bmp := TBitmap.Create;
      adbmp.AssignTo(bmp);

      Imgs_Anim.Clear;
      ListView2.Clear;

      ListView2.Items.BeginUpdate;

      for i := 0 to PatternCount - 1 do
      begin

        if PatternWidth > PatternHeight then
        begin
          w := 32;
          h := Round(PatternHeight / PatternWidth * 32);
        end
        else
        begin
          h := 32;
          w := Round(PatternWidth / PatternHeight * 32);
        end;

        bmp2 := TBitmap.Create;
        bmp2.Width := 32;
        bmp2.Height := 32;

        r := GetPatternRect(i);
        SetStretchBltMode(bmp2.Canvas.Handle, Halftone);
        StretchBlt(bmp2.Canvas.Handle,(32-w) div 2,(32-h) div 2,w,h,bmp.Canvas.Handle,r.Left,r.Top,r.Right-r.Left,r.Bottom-r.Top,SRCCOPY);

        Imgs_Anim.Add(bmp2,nil);
        with ListView2.Items.Add do
        begin
          ImageIndex := i;
          Caption := 'Frame ' + FormatFloat('000',i+1);
        end;
        bmp2.Free;
      end;
      bmp.Free;
      adbmp.Free;

      ListView2.Items.EndUpdate;
    end;

    Screen.Cursor := crDefault;
  end
  else
  begin
    ToolButton8.Down := true;
    Splitter1.Visible := false;
    GroupBox1.Visible := false;
  end;
  Pattern := 0;
end;

procedure TMainDlg.ViewContent;
var i:integer;
    bmp,bmp2:TBitmap;
    abmp:TAdBitmap;
    w,h:integer;
begin
  Screen.Cursor := crHourglass;

  Caption := 'Image List Editor';
  StatusBar1.SimpleText := 'No file opened';
  if CurrentFile <> '' then
  begin
    Caption := Caption + ' ['+CurrentFile+']';
    StatusBar1.SimpleText := CurrentFile;
  end;
  
  Imgs_Preview.Clear;
  ListView1.Clear;
  
  ListView1.Items.BeginUpdate;

  for i := 0 to AdImageList.Count - 1 do
  begin
    with AdImageList[i] do
    begin
      if Texture.Texture.BaseWidth > Texture.Texture.BaseHeight then
      begin
        w := 32;
        h := Round(Texture.Texture.BaseHeight / Texture.Texture.BaseWidth * 32);
      end
      else
      begin
        h := 32;
        w := Round(Texture.Texture.BaseWidth / Texture.Texture.BaseHeight * 32);
      end;
      bmp := TBitmap.Create;
      bmp.Width := 32;
      bmp.Height := 32;

      bmp2 := TBitmap.Create;

      abmp := TAdBitmap.Create;
      Abmp.ReserveMemory(Texture.Texture.BaseWidth,Texture.Texture.BaseHeight);
      Texture.Texture.SaveToBitmap(ABmp);
      abmp.AssignTo(bmp2);
      abmp.Free;

      bmp.Canvas.StretchDraw(bounds((32-w) div 2,(32-h) div 2,w,h),bmp2);
      bmp2.Free;

      Imgs_Preview.Add(bmp,nil);

      bmp.Free;

      with ListView1.Items.Add do
      begin
        if Name <> '' then
        begin
          SubItems.Add(name)
        end
        else
        begin
          SubItems.Add('No name')
        end;
        SubItems.Add('w:'+inttostr(Texture.Texture.BaseWidth)+'px h:'+inttostr(Texture.Texture.BaseHeight)+'px');
        ImageIndex := i;
      end;
    end;
  end;

  ListView1.Items.EndUpdate;

  Screen.Cursor := crDefault;
end;

end.
