unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, ComCtrls, Menus, ToolWin, ExtCtrls, XPMan, StdCtrls,
  ActnList, AdDraws, SetDlg, AdClasses, ExtDlgs, CompDlg, AdPNG, AdJPEG;

type
  TMainDlg = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    ToolBar1: TToolBar;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    LoadnewImageLibrary1: TMenuItem;
    N1: TMenuItem;
    LoadLibrary1: TMenuItem;
    N2: TMenuItem;
    SaveLibrary1: TMenuItem;
    SaveLibraryas1: TMenuItem;
    N3: TMenuItem;
    Exit1: TMenuItem;
    Image1: TMenuItem;
    Add1: TMenuItem;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ImageList1: TImageList;
    ToolButton6: TToolButton;
    GroupBox2: TGroupBox;
    ListView1: TListView;
    ImageList2: TImageList;
    PopupMenu1: TPopupMenu;
    Add2: TMenuItem;
    N5: TMenuItem;
    Delete2: TMenuItem;
    Settings2: TMenuItem;
    ProgressBar1: TProgressBar;
    XPManifest1: TXPManifest;
    OpenPictureDialog1: TOpenPictureDialog;
    N4: TMenuItem;
    SaveDialog1: TSaveDialog;
    OpenDialog1: TOpenDialog;
    Setcompressorforallpictures1: TMenuItem;
    GroupBox1: TGroupBox;
    Splitter1: TSplitter;
    ListView2: TListView;
    ToolBar2: TToolBar;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ImageList3: TImageList;
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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListView1CustomDrawItem(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure CreateFilter;
    procedure ToolButton5Click(Sender: TObject);
    procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ListView1Change(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure ListView1Click(Sender: TObject);
    procedure Delete2Click(Sender: TObject);
    procedure ListView1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Settings2Click(Sender: TObject);
    procedure ListView1DblClick(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure ToolButton4Click(Sender: TObject);
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
  private
    { Private-Deklarationen }
  public
    AdDraw1:TAdDraw;
    AdImageList:TAdImageList;
    AdPerCounter:TAdPerformanceCounter;
    TextLabel:TAdImage;
    Tiles:TAdImage;
    AColor:TColor;

    OffsetX,OffsetY:integer;
    mx,my:integer;
    pattern:double;

    CurrentFile:string;

    procedure Idle(Sender:TObject; var Done:boolean);
    procedure ViewContent;
    procedure UpdateState;
    procedure WriteText(s:string);
  end;

var
  MainDlg: TMainDlg;

implementation

{$R *.dfm}

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
      adbmp.AssignToBitmap(bmp2);
      adbmp.AssignAlphaChannelToBitmap(abmp2);
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
  adbmp.AssignBitmap(bmp);
  adbmp.AssignAlphaChannel(abmp);

  with AdImageList.Add('animation') do
  begin
    Texture.Texture.LoadFromBitmap(adbmp);
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
    temp:TPictFormat;
    cref:TPersistentClass;
    c:integer;
begin
  OpenPictureDialog1.Filter := '';
  fmt := '';
  c := 0;
  for i := 0 to RegisteredFormats.Count-1 do
  begin
    str := TStringList.Create;
    cref := GetClass(RegisteredFormats[i]);
    if cref <> nil then
    begin
      temp := TPictFormat(TPictFormatClass(cref).Create);
      temp.FileExts(str);
      for j := 0 to str.Count - 1 do
      begin
        OpenPictureDialog1.Filter := OpenPictureDialog1.Filter+
          uppercase(copy(str[j],2,length(str[j])-1))+' File (*'+str[j]+')|*'+str[j]+'|';
        fmt := fmt+'*'+str[j]+'; ';
        c := c+1;
      end;
      temp.Free;
    end;
    str.Free;
  end;
  OpenPictureDialog1.Filter := OpenPictureDialog1.Filter+'All supportet files ('+fmt+')|'+fmt+'|';
  OpenPictureDialog1.Filter := OpenPictureDialog1.Filter+'All files (*.*)|*.*';
  OpenPictureDialog1.FilterIndex := c+1;
end;

procedure TMainDlg.Delete2Click(Sender: TObject);
begin
  if ListView1.ItemIndex <> -1 then
  begin
    AdImageList.Delete(ListView1.ItemIndex);
    ViewContent;
  end;
end;

procedure TMainDlg.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TMainDlg.Export1Click(Sender: TObject);
var adbmp:TAdBitmap;
    bmp:TBitmap;
begin
  if ListView1.ItemIndex <> -1 then
  begin
    if SavePictureDialog1.Execute then
    begin
      adbmp := TAdBitmap.Create;
      with AdImageList[ListView1.ItemIndex] do
      begin
        adbmp.ReserveMemory(Texture.Texture.BaseWidth,Texture.Texture.BaseHeight);
        Texture.Texture.SaveToBitmap(adbmp);
      end;
      bmp := TBitmap.Create;
      adbmp.AssignToBitmap(bmp,false);
      bmp.SaveToFile(changefileext(SavePictureDialog1.FileName,'.bmp'));
      bmp.Free;
      adbmp.Free;
    end;
  end;
end;

procedure TMainDlg.FormClose(Sender: TObject; var Action: TCloseAction);
var i:integer;
begin
  for i := 0 to ListView1.Items.Count - 1 do
  begin
    ListView1.Items[i].Selected := false;
  end;
end;

procedure TMainDlg.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;

  AdPerCounter := TAdPerformanceCounter.Create;

  AdDraw1 := TAdDraw.Create(Panel1);
  AdDraw1.DllName := 'AndorraOGL.dll';
  if AdDraw1.Initialize then
  begin
    Application.OnIdle := Idle;
    AdImageList := TAdImageList.Create(AdDraw1);
    AColor := ColorToRGB(clBtnFace);

    CreateFilter;

    TextLabel := TAdImage.Create(AdDraw1);

    Tiles := TAdImage.Create(AdDraw1);
    Tiles.Texture.LoadFromGraphic(Image2.Picture.Bitmap);    
    Tiles.Restore;
    
  end
  else
  begin
    ShowMessage('Couldn''t initialize Andorra 2D.');
    Close;
  end;
end;

procedure TMainDlg.FormDestroy(Sender: TObject);
begin
  Tiles.Free;
  AdPerCounter.Free;
  TextLabel.Free;
  AdImageList.Free;
  AdDraw1.Free;
end;

procedure TMainDlg.Idle(Sender: TObject; var Done: boolean);
begin
  if AdDraw1.CanDraw then
  begin
    AdPerCounter.Calculate;

    AdDraw1.ClearSurface(AColor);
    AdDraw1.BeginScene;

    if TiledBackground1.Checked then
    begin
      Tiles.StretchBltAlpha(AdDraw1,rect(0,0,Panel1.Width,Panel1.Height),rect(0,0,Panel1.Width,Panel1.Height),0.5,0.5,0,255);
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

        Draw(AdDraw1,(AdDraw1.Parent.Width - Width) div 2 + OffsetX,(AdDraw1.Parent.Height - Height) div 2 + OffsetY,round(pattern));
        TextLabel.Draw(AdDraw1,0,0,0);
      end;
    end;

    AdDraw1.EndScene;
    AdDraw1.Flip;
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
    if (ListView1.SelCount = 1) and (ListView1.ItemIndex <> -1)  then
    begin
      WriteText(AdImageList[ListView1.ItemIndex].Name+' Width: '+inttostr(AdImageList[ListView1.ItemIndex].Width)+
                'px, Height: '+inttostr(AdImageList[ListView1.ItemIndex].Height)+'px');
    end;
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
    ToolButton5Click(nil);
  end;
end;

procedure TMainDlg.ListView1DragDrop(Sender, Source: TObject; X, Y: Integer);
var index1,index2:integer; 
    item:TAdImage;
begin
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
      Delete2Click(nil);
      if i < ListView1.Items.Count then
      begin
        ListView1.ItemIndex := i;
      end
      else
      begin
        ListView1.ItemIndex := i-1;
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

procedure TMainDlg.Panel1Resize(Sender: TObject);
begin
  if AdDraw1.Initialized then
  begin
    AdDraw1.Setup2DScene;
  end;
end;

procedure TMainDlg.SaveLibraryas1Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    CurrentFile := ChangeFileExt(SaveDialog1.FileName,'.ail');
    AdImageList.SaveToFile(CurrentFile);
  end;
end;

procedure TMainDlg.Setcompressorforallpictures1Click(Sender: TObject);
var comp:TCompressors;
    i:integer;
begin
  comp := TCompressors.Create(self);
  if comp.ShowModal = mrOk then
  begin
    for i := 0 to AdImageList.Count - 1 do
    begin
      AdImageList.Compressor := TCompressorClass(GetClass(Comp.ListBox1.Items[Comp.ListBox1.ItemIndex]));
    end;
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
    SetDlg.InsprectImage(AdImageList.Items[ListView1.ItemIndex]);
    ViewContent;
    SetDlg.Free;
    ListView1.Selected := ListView1.Items[ii];
    ListView1.Selected.MakeVisible(false);
  end;
end;

procedure TMainDlg.ToolButton1Click(Sender: TObject);
begin
  CurrentFile := '';
  AdImageList.Clear;
  ViewContent;
end;

procedure TMainDlg.ToolButton2Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    Screen.Cursor := crHourglass;
    AdImageList.LoadFromFile(OpenDialog1.FileName);
    CurrentFile := OpenDialog1.FileName;
    ViewContent;
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainDlg.ToolButton4Click(Sender: TObject);
begin
  if CurrentFile <> '' then
  begin
    Screen.Cursor := crHourglass;
    AdImageList.SaveToFile(CurrentFile);
    Screen.Cursor := crDefault;
  end
  else
  begin
    SaveLibraryAs1Click(nil);
  end;
end;

procedure TMainDlg.ToolButton5Click(Sender: TObject);
var i:integer;
begin
  if OpenPictureDialog1.Execute then
  begin
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
      end;
    end;
    AdImageList.Restore;
    ViewContent;
  end;
end;

procedure TMainDlg.UpdateState;
var i,w,h:integer;
    bmp,bmp2:TBitmap;
    adbmp:TAdBitmap;
    r:TRect;
begin
  Delete2.Enabled := ListView1.ItemIndex <> -1;
  Settings2.Enabled := ListView1.ItemIndex <> -1;
  Export1.Enabled := ListView1.ItemIndex <> -1;
  Compoundselectedpictures1.Enabled := ListView1.SelCount > 1;

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
      adbmp.AssignToBitmap(bmp,false);

      ImageList3.Clear;
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

        ImageList3.Add(bmp2,nil);
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
  if CurrentFile <> '' then Caption := Caption + ' ['+CurrentFile+']';
  ImageList2.Clear;
  ListView1.Clear;
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
      abmp.AssignToBitmap(bmp2,true);
      abmp.Free;

      bmp.Canvas.StretchDraw(bounds((32-w) div 2,(32-h) div 2,w,h),bmp2);
      bmp2.Free;

      ImageList2.Add(bmp,nil);

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
  Screen.Cursor := crDefault;
end;

procedure TMainDlg.WriteText(s: string);
var bmp:TBitmap;
begin
  bmp := TBitmap.Create;

  bmp.Transparent := true;
  bmp.TransparentColor := clWhite;
  bmp.TransparentMode := tmFixed;

  bmp.Canvas.Font.Style := [fsBold];
  bmp.Width := bmp.Canvas.TextWidth(s);
  bmp.Height := bmp.Canvas.TextHeight(s);
  bmp.Canvas.TextOut(0,0,s);
  TextLabel.Texture.LoadFromGraphic(bmp);
  TextLabel.Restore;
  bmp.Free;
end;

end.
