unit MainEd;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Menus, XPMan, ComCtrls, Tabs, AdSkin, AdDraws,
  AdClasses, ExtDlgs, AdPng, Buttons;

type
  TRHandleTyp = (rhLT,rhRT,rhLB,rhRB);
  
  TRHandle = record
    Top,Left,Bottom,Right:integer;
    Typ:TRHandleTyp;
    Item:TAdSkinElem;
  end;

  PRHandle = ^TRHandle;
  
  THandleList = class(TList)
    private
      procedure SetItem(Index:integer;Value:TRHandle);
      function GetItem(Index:integer):TRHandle;
    protected
      procedure Notify(Ptr: Pointer; Action: TListNotification);override;
    public
      procedure Add(Item:TRHandle);
      property Items[Index:integer]:TRHandle read GetItem write SetItem;default;
  end;

  TMainDlg = class(TForm)
    Panel1: TPanel;
    Splitter1: TSplitter;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Splitter2: TSplitter;
    Panel2: TPanel;
    ListBox1: TListBox;
    Panel3: TPanel;
    Panel4: TPanel;
    Label1: TLabel;
    XPManifest1: TXPManifest;
    Panel6: TPanel;
    StatusBar1: TStatusBar;
    Panel7: TPanel;
    Button5: TSpeedButton;
    Button6: TSpeedButton;
    Panel8: TPanel;
    GroupBox4: TGroupBox;
    GroupBox6: TGroupBox;
    Panel9: TPanel;
    Panel10: TPanel;
    ScrollBox1: TScrollBox;
    Button9: TSpeedButton;
    Button10: TSpeedButton;
    Splitter3: TSplitter;
    GroupBox7: TGroupBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    GroupBox8: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    GroupBox9: TGroupBox;
    OpenPictureDialog1: TOpenPictureDialog;
    Splitter4: TSplitter;
    GroupBox5: TGroupBox;
    GroupBox10: TGroupBox;
    Panel5: TPanel;
    Splitter5: TSplitter;
    Splitter6: TSplitter;
    ListBox2: TListBox;
    Panel11: TPanel;
    Button1: TSpeedButton;
    Button4: TSpeedButton;
    Panel12: TPanel;
    Image2: TImage;
    Panel13: TPanel;
    ListBox3: TListBox;
    Splitter7: TSplitter;
    Panel14: TPanel;
    Button7: TSpeedButton;
    Button8: TSpeedButton;
    Button11: TSpeedButton;
    Splitter8: TSplitter;
    GroupBox3: TGroupBox;
    Label6: TLabel;
    Label7: TLabel;
    Edit5: TEdit;
    Edit6: TEdit;
    Label8: TLabel;
    Label9: TLabel;
    Edit7: TEdit;
    Edit8: TEdit;
    GroupBox11: TGroupBox;
    Label10: TLabel;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    Label11: TLabel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SaveDialog1: TSaveDialog;
    OpenDialog1: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Panel10Resize(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Panel10MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Panel10MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel10MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Button9Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure ListBox2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure ListBox3Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure Edit5Change(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    AdDraw1:TAdDraw;
    AdSkin:TAdSkin;
    Color:TColor;
    SelItem:TAdSkinItem;
    HandleList:THandleList;
    OverHandle:boolean;
    HandleOver:TRHandle;
    dx,dy:integer;
    ChangedSize:boolean;
    SelElem:TAdSkinElem;
    SetOptions:boolean;
    Saved:boolean;
    Currentfile:string;
    procedure DrawHandeledRect(ARect:TRect);
    procedure RefreshItemList;
    procedure RefreshStatusList;
    procedure RefreshImageList;
    procedure Idle(Sender:TObject;var Done:boolean);
    procedure Select;
    procedure SelectElem;
    procedure SelectImage;
    procedure SelectStatus;
    procedure AddHandleRect(Rect:TRect;Elem:TAdSkinElem);
    procedure BuildHandleList;
    function GetSnapPoint(P:TPoint):TPoint;
    procedure CreateFilter;
  end;

var
  MainDlg: TMainDlg;

const
  hs = 3;

implementation

{$R *.dfm}

procedure TMainDlg.AddHandleRect(Rect: TRect; Elem: TAdSkinElem);
var h:TRHandle;
begin
  with h do
  begin
    h.Left := Rect.Left;
    h.Top := Rect.Top;
    h.Right := Rect.Left + hs;
    h.Bottom := Rect.Top + hs;
    h.Typ := rhLT;
    h.Item := Elem;
  end;
  HandleList.Add(h);

  with h do
  begin
    h.Left := Rect.Right-hs;
    h.Top := Rect.Top;
    h.Right := Rect.Right;
    h.Bottom := Rect.Top + hs;
    h.Typ := rhRT;
    h.Item := Elem;
  end;
  HandleList.Add(h);

  with h do
  begin
    h.Left := Rect.Left;
    h.Top := Rect.Bottom - hs;
    h.Right := Rect.Left + hs;
    h.Bottom := Rect.Bottom;
    h.Typ := rhLB;
    h.Item := Elem;
  end;
  HandleList.Add(h);

  with h do
  begin
    h.Left := Rect.Right - hs;
    h.Top := Rect.Bottom - hs;
    h.Right := Rect.Right;
    h.Bottom := Rect.Bottom;
    h.Typ := rhRB;
    h.Item := Elem;
  end;
  HandleList.Add(h);
end;

procedure TMainDlg.BuildHandleList;
var i:integer;
begin
  HandleList.Clear;
  with SelItem do
    AddHandleRect(Rect(0,0,BaseWidth,BaseHeight),nil);
  for i := 0 to SelItem.Elements.Count - 1 do
  begin
    with SelItem.Elements[i] do
    begin
      AddHandleRect(Rect(Round(X1),Round(Y1),Round(X2),Round(Y2)),SelItem.Elements[i]);
    end;
  end;
end;

procedure TMainDlg.Button10Click(Sender: TObject);
var i:integer;
begin
  for i := 0 to SelItem.Elements.Count-1 do
  begin
    if SelItem.Elements[i].Selected then
    begin
      SelItem.Elements.Delete(i);
      break;
    end;
  end;
  SelElem := nil;
  SelectElem;
end;

procedure TMainDlg.Button11Click(Sender: TObject);
var s:string;
begin
  s := InputBox('Rename status','Enter the new name of the status',SelItem.States[ListBox3.ItemIndex]);
  if s <> '' then
  begin
    saved := false;
    SelItem.States[ListBox3.ItemIndex] := s;
    RefreshStatusList;
  end;
end;

procedure TMainDlg.Button1Click(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
  begin
    with SelItem.Images.Add(ExtractFileName(OpenPictureDialog1.Filename)) do
    begin
      Texture.LoadGraphicFromFile(OpenPictureDialog1.FileName,true,clNone);
      Restore;
    end;
    saved := false;
    RefreshImageList;
  end;
end;

procedure TMainDlg.Button2Click(Sender: TObject);
begin
  AdSkin.SaveToFile('test.xml');
end;

procedure TMainDlg.Button3Click(Sender: TObject);
begin
  AdSkin.LoadFromFile('test.xml');
  RefreshItemList;
end;

procedure TMainDlg.Button4Click(Sender: TObject);
begin
  if ListBox2.ItemIndex > -1 then
  begin
    SelItem.Images.Delete(ListBox2.ItemIndex);
    RefreshImageList;
  end;
end;

procedure TMainDlg.Button5Click(Sender: TObject);
var s:string;
begin
  s := InputBox('New skin item','Enter the name of the new skin item','');
  if s <> '' then
  begin
    AdSkin.Add(s);
    RefreshItemList;
  end;
end;

procedure TMainDlg.Button6Click(Sender: TObject);
begin
  AdSkin.Remove(SelItem);
  RefreshItemList;
end;

procedure TMainDlg.Button7Click(Sender: TObject);
var s:string;
begin
  s := InputBox('Name','Please enter the name of the new state.','');
  if s <> '' then
  begin
    Saved := false;
    SelItem.States.Add(s);
    RefreshStatusList;
  end;
end;

procedure TMainDlg.Button8Click(Sender: TObject);
var delindex:integer;
    i,j:integer;
begin
  delindex := listbox3.ItemIndex;
  for i := 0 to SelItem.Elements.Count-1 do
  begin
    for j := delindex to SelItem.States.Count-2 do
    begin
      SelItem.Elements[i].Images[j] := SelItem.Elements[i].Images[j+1];
    end;
  end;
  SelItem.States.Delete(delindex);
  RefreshStatusList;
end;

procedure TMainDlg.Button9Click(Sender: TObject);
var elem:TAdSkinElem;
begin
  elem := TAdSkinElem.Create;
  elem.X1 := 0;
  elem.X2 := 100;
  elem.Y1 := 0;
  elem.Y2 := 100;
  with SelItem do
  begin
    Elements.Add(elem);
  end;
  BuildHandleList;
  RefreshImageList;
end;

procedure TMainDlg.CheckBox1Click(Sender: TObject);
begin
  if (SelElem <> nil) and (not SetOptions) then
  begin
    SelElem.Anchors := [];
    Saved := false;
    if CheckBox1.Checked then
    begin
      SelElem.Anchors := SelElem.Anchors + [aaLeft];
    end;
    if CheckBox2.Checked then
    begin
      SelElem.Anchors := SelElem.Anchors + [aaRight];
    end;
    if CheckBox3.Checked then
    begin
      SelElem.Anchors := SelElem.Anchors + [aaTop];
    end;
    if CheckBox4.Checked then
    begin
      SelElem.Anchors := SelElem.Anchors + [aaBottom];
    end;
  end;
end;

procedure TMainDlg.ComboBox1Change(Sender: TObject);
begin
  SetOptions := true;
  Combobox2.ItemIndex := SelElem.Images[Combobox1.ItemIndex]+1;
  SetOptions := false;
end;

procedure TMainDlg.ComboBox2Change(Sender: TObject);
begin
  if not SetOptions then
  begin
    SelElem.Images[Combobox1.ItemIndex] := Combobox2.ItemIndex-1;
  end;
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

procedure TMainDlg.DrawHandeledRect(ARect:TRect);
begin
  with AdDraw1.Canvas do
  begin
    Rectangle(ARect);
    Brush.Style := abSolid;
    Rectangle(Bounds(ARect.Left,ARect.Top,hs,hs));
    Rectangle(Bounds(ARect.Right-hs,ARect.Top,hs,hs));
    Rectangle(Bounds(ARect.Left,ARect.Bottom-hs,hs,hs));
    Rectangle(Bounds(ARect.Right-hs,ARect.Bottom-hs,hs,hs));
  end;
end;

procedure TMainDlg.Edit1Change(Sender: TObject);
begin
  if not SetOptions then
  begin
    Saved := false;
    SelElem.X1 := StrToIntDef(Edit1.Text,0);
    SelElem.Y1 := StrToIntDef(Edit2.Text,0);
    SelElem.X2 := StrToIntDef(Edit3.Text,0);
    SelElem.Y2 := StrToIntDef(Edit4.Text,0);
  end;
end;

procedure TMainDlg.Edit5Change(Sender: TObject);
begin
  if not SetOptions then
  begin
    Saved := false;
    SelElem.ImgSrcX1 := StrToIntDef(Edit5.Text,0);
    SelElem.ImgSrcY1 := StrToIntDef(Edit6.Text,0);
    SelElem.ImgSrcX2 := StrToIntDef(Edit7.Text,0);
    SelElem.ImgSrcY2 := StrToIntDef(Edit8.Text,0);
  end;
end;

procedure TMainDlg.FormClose(Sender: TObject; var Action: TCloseAction);
var res:integer;
begin
  if not Saved then
  begin
    res := Application.MessageBox('You probably did not save your work. Do you '+
                                  'want to save it now?','Warning',MB_YESNOCANCEL);
    if res = ID_CANCEL then
    begin
      Action := caNone;
    end
    else
    begin
      if res = ID_YES then
      begin
        SpeedButton2Click(nil);
      end;
    end;
  end;
end;

procedure TMainDlg.FormCreate(Sender: TObject);
begin
  AdDraw1 := TAdDraw.Create(Panel10);
  AdDraw1.DllName := 'AndorraDX93D.dll'; 

  HandleList := THandleList.Create;

  CreateFilter;

  Saved := true;

  if AdDraw1.Initialize then
  begin
    Application.OnIdle := Idle;
    AdSkin := TAdSkin.Create(AdDraw1);
    Color := ColorToRGB(clBtnFace);
    Select;
    SelectElem;
  end
  else
  begin
    ShowMessage('Error while initializing Andorra 2D!');
    halt;
  end;
end;

procedure TMainDlg.FormDestroy(Sender: TObject);
begin
  HandleList.Free;
  AdSkin.Free;
  AdDraw1.Free;
end;

function TMainDlg.GetSnapPoint(P: TPoint): TPoint;
var i:integer;

  function InArea(Val1,Val2,Ar:single):boolean;
  begin
    result := (Val1 >= Val2-(Ar / 2)) and
              (Val1 <= Val2+(Ar / 2));
  end;
begin
  result := p;
  if InArea(P.X,0,4) then
  begin
    result.X := 0;
  end;
  if InArea(P.Y,0,4) then
  begin
    result.Y := 0;
  end;
  if InArea(P.X,SelItem.BaseWidth,4) then
  begin
    result.X := SelItem.BaseWidth;
  end;
  if InArea(P.Y,SelItem.BaseHeight,4) then
  begin
    result.Y := SelItem.BaseHeight;
  end;
  for i := 0 to SelItem.Elements.Count-1 do
  begin
    with SelItem.Elements[i] do
    begin
      if (P.X <> round(X1)) and (P.Y <> round(Y1)) and
         (P.X <> round(X2)) and (P.Y <> round(Y2)) then
      begin
        if InArea(P.X,X1,4) then
        begin
          result.X := round(X1);
        end;
        if InArea(P.Y,Y1,4) then
        begin
          result.Y := round(Y1);
        end;
        if InArea(P.X,X2,4) then
        begin
          result.X := round(X2);
        end;
        if InArea(P.Y,Y2,4) then
        begin
          result.Y := round(Y2);
        end;
      end;
    end;    
  end;
end;

procedure TMainDlg.Idle(Sender: TObject; var Done: boolean);
var i,state:integer;
    cx,cy:integer;
begin
  AdDraw1.BeginScene;

  AdDraw1.ClearSurface(Color);

  if SelItem <> nil then
  begin
    with SelItem do
    begin
      cx := (Panel10.ClientWidth-BaseWidth) div 2;
      cy := (Panel10.ClientHeight-BaseHeight) div 2;
      state := ListBox3.ItemIndex;
      if state < 0 then state := 0;      
      Draw(state,cx,cy,BaseWidth,BaseHeight);
    end;

    with AdDraw1.Canvas do
    begin
    
      if ChangedSize then
      begin
        Brush.Style := abSolid;
        Brush.Color := ad_argb(64,255,64,64);
        Pen.Color := ad_argb(128,255,64,64);
        Rectangle(Bounds(cx,cy-16,100,16));
        with SelItem do
          TextOut(cx+2,cy-16,inttostr(BaseWidth)+'x'+inttostr(BaseHeight));
      end;

      Pen.Color := ad_argb(128,255,64,64);
      Brush.Color := ad_argb(64,255,64,64);
      Brush.Style := abClear;
      with SelItem do
      begin
        DrawHandeledRect(Bounds(cx,cy,BaseWidth,BaseHeight));

        for i := 0 to Elements.Count-1 do
        begin
          Pen.Color := ad_argb(128,64,64,255);
          Brush.Color := ad_argb(64,64,64,255);
          if Elements[i].Selected then
          begin
            Brush.Style := abSolid;
          end
          else
          begin
            Brush.Style := abClear;
          end;
          with Elements[i] do
          begin
            DrawHandeledRect(Rect(round(cx+x1),round(cy+y1),round(cx+x2),round(cy+y2)));
          end;
        end;
      end;
      Release;
    end;
  end;

  AdDraw1.EndScene;
  AdDraw1.Flip;

  //Done := false;
end;

procedure TMainDlg.ListBox1Click(Sender: TObject);
begin
  Select;
end;

procedure TMainDlg.ListBox2Click(Sender: TObject);
begin
  SelectImage;
end;

procedure TMainDlg.ListBox3Click(Sender: TObject);
begin
  SelectStatus;
end;

function InRect(X,Y:integer;R:TRect):boolean;
begin
  result := (X >= R.Left) and
            (X <= R.Right) and
            (Y >= R.Top) and
            (Y <= R.Bottom);
end;

procedure TMainDlg.Panel10MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var i:integer;
    cx,cy:integer;
    hs:boolean;
begin
  if (SelItem <> nil) then
  begin
    Saved := false;
    SelElem := nil;
    cx := (Panel10.Width-SelItem.BaseWidth) div 2;
    cy := (Panel10.Height-SelItem.BaseHeight) div 2;
    hs := false;
    for i := SelItem.Elements.Count-1 downto 0 do
    begin
      with SelItem.Elements[i] do
      begin
        if InRect(X-cx,Y-cy,Rect(round(x1),round(y1),round(x2),round(y2))) then
        begin
          if not hs then
          begin
            Selected := true;
            SelElem := SelItem.Elements[i];
          end;
          hs := true;
        end
        else
        begin
          Selected := false;
        end;
      end;
    end;
    SelectElem;
    dx := x;
    dy := y;
  end;
end;

procedure TMainDlg.Panel10MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var i:integer;
    r:TRect;
    cx,cy:integer;
    mx,my:integer;
    sp:TPoint;
begin
  if SelItem <> nil then
  begin
    cx := (Panel10.Width-SelItem.BaseWidth) div 2;
    cy := (Panel10.Height-SelItem.BaseHeight) div 2;
    if not (ssLeft in Shift) then
    begin
      Panel10.Cursor := crDefault;
      OverHandle := false;
      for i := HandleList.Count-1 downto 0 do
      begin
        with HandleList.Items[i] do
        begin
          r := Rect(Left+cx,Top+cy,Right+cx,Bottom+cy);
          if InRect(X,Y,r) then
          begin
            case Typ of
              rhLT: Panel10.Cursor := crSizeNWSE;
              rhRT: Panel10.Cursor := crSizeNESW;
              rhLB: Panel10.Cursor := crSizeNESW;
              rhRB: Panel10.Cursor := crSizeNWSE;
            end;
            HandleOver := HandleList.Items[i];
            OverHandle := true;
            break;
          end;
        end;
      end;
    end
    else
    begin
      if (OverHandle) and (ssLeft in Shift) then
      begin
        mx := x-dx;
        my := y-dy;
        if (HandleOver.Item = nil) or (ssShift in Shift) then
        begin
          case HandleOver.Typ of
            rhLT:
            begin
              mx := -mx;
              my := -my;
            end;
            rhRT:
            begin
              my := -my;
            end;
            rhLB: 
            begin
              mx := -mx;
            end;
          end;
          ChangedSize := true;
          SelItem.BaseWidth := SelItem.BaseWidth + mx*2;
          SelItem.BaseHeight := SelItem.BaseHeight + my*2;
        end
        else
        begin
          with HandleOver do
          begin
            case Typ of
              rhLT:
              begin
                Item.X1 := Item.X1 + mx;
                Item.Y1 := Item.Y1 + my;
                if not (ssAlt in Shift) then
                begin
                  sp := GetSnapPoint(Point(round(Item.X1),round(Item.Y1)));
                  Item.X1 := sp.X;
                  Item.Y1 := sp.Y;
                end;
              end;
              rhRT:
              begin
                Item.X2 := Item.X2 + mx;
                Item.Y1 := Item.Y1 + my;
                if not (ssAlt in Shift) then
                begin
                  sp := GetSnapPoint(Point(round(Item.X2),round(Item.Y1)));
                  Item.X2 := sp.X;
                  Item.Y1 := sp.Y;
                end;
              end;
              rhLB:
              begin
                Item.X1 := Item.X1 + mx;
                Item.Y2 := Item.Y2 + my;
                if not (ssAlt in Shift) then
                begin
                  sp := GetSnapPoint(Point(round(Item.X1),round(Item.Y2)));
                  Item.X1 := sp.X;
                  Item.Y2 := sp.Y;
                end;
              end;
              rhRB:
              begin
                Item.X2 := Item.X2 + mx;
                Item.Y2 := Item.Y2 + my;
                if not (ssAlt in Shift) then
                begin
                  sp := GetSnapPoint(Point(round(Item.X2),round(Item.Y2)));
                  Item.X2 := sp.X;
                  Item.Y2 := sp.Y;
                end;
              end;
            end;
          end;
          if (SelElem <> nil) then
          begin
            Edit1.Text := IntToStr(Round(SelElem.X1));
            Edit2.Text := IntToStr(Round(SelElem.Y1));
            Edit3.Text := IntToStr(Round(SelElem.X2));
            Edit4.Text := IntToStr(Round(SelElem.Y2));
          end;
        end;
        BuildHandleList;
        dx := x;
        dy := y;
      end;
    end;
  end;
end;

procedure TMainDlg.Panel10MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ChangedSize := false;
end;

procedure TMainDlg.Panel10Resize(Sender: TObject);
begin
  if AdDraw1.Initialized then
  begin
    AdDraw1.Finalize;
    AdDraw1.Initialize;
  end;
end;

procedure TMainDlg.RefreshImageList;
var i:integer;
begin
  ListBox2.Clear;
  for i := 0 to SelItem.Images.Count-1 do
  begin
    ListBox2.Items.Add('['+inttostr(i)+']'+' '+SelItem.Images[i].Name);
  end;
  ComboBox2.Items.Assign(ListBox2.Items);
  Combobox2.Items.Insert(0,'[No Image]');
  SelectImage;
end;

procedure TMainDlg.RefreshStatusList;
begin
  ListBox3.Items.Assign(SelItem.States);
  ComboBox1.Items.Assign(SelItem.States);
  SelectStatus;
end;

procedure TMainDlg.RefreshItemList;
var i:integer;
begin
  ListBox1.Clear;
  for i := 0 to AdSkin.Count - 1 do
  begin
    ListBox1.Items.Add(AdSkin[i].Name);
  end;
  Select;
end;

procedure TMainDlg.Select;
begin
  if ListBox1.ItemIndex <> -1 then
  begin
    SelItem := AdSkin[ListBox1.ItemIndex];
    Button6.Enabled := true;
    Panel4.Enabled := true;
    Label1.Caption := 'GUI Item: '+SelItem.Name;
    BuildHandleList;
    SelElem := nil;
    SelectElem;
    RefreshImageList;
    RefreshStatusList;
  end
  else
  begin
    HandleList.Clear;
    SelItem := nil;
    Panel4.Enabled := false;
    Button6.Enabled := false;
    Label1.Caption := 'No item selected';
  end;
end;

procedure TMainDlg.SelectElem;
begin
  if SelElem <> nil then
  begin
    Button10.Enabled := true;
    GroupBox7.Visible := true;
    GroupBox8.Visible := true;
    GroupBox9.Visible := true;
    SetOptions := true;
    CheckBox1.Checked := aaLeft in SelElem.Anchors;
    CheckBox2.Checked := aaRight in SelElem.Anchors;
    CheckBox3.Checked := aaTop in SelElem.Anchors;
    CheckBox4.Checked := aaBottom in SelElem.Anchors;
    Edit1.Text := IntToStr(Round(SelElem.X1));
    Edit2.Text := IntToStr(Round(SelElem.Y1));
    Edit3.Text := IntToStr(Round(SelElem.X2));
    Edit4.Text := IntToStr(Round(SelElem.Y2));
    Edit5.Text := IntToStr(Round(SelElem.ImgSrcX1));
    Edit6.Text := IntToStr(Round(SelElem.ImgSrcY1));
    Edit7.Text := IntToStr(Round(SelElem.ImgSrcX2));
    Edit8.Text := IntToStr(Round(SelElem.ImgSrcY2));
    SetOptions := false;
    Combobox1.ItemIndex := ListBox3.ItemIndex;
    Combobox1Change(nil);
  end
  else
  begin
    Button10.Enabled := false;
    GroupBox7.Visible := false;
    GroupBox8.Visible := false;
    GroupBox9.Visible := false;
  end;
end;

procedure TMainDlg.SelectImage;
var
  bmp:TBitmap;
  abmp:TAdBitmap;
begin
  if ListBox2.ItemIndex > -1 then
  begin
    with SelItem.Images[ListBox2.ItemIndex] do
    begin
      bmp := TBitmap.Create;
      if Texture.Texture.Loaded then
      begin
        bmp.Width := SelItem.Images[ListBox2.ItemIndex].Width;
        bmp.Height := SelItem.Images[ListBox2.ItemIndex].Height;
        bmp.Canvas.Brush.Color := clBtnFace;
        bmp.Canvas.FillRect(bmp.Canvas.ClipRect);
        abmp := TAdBitmap.Create;
        abmp.ReserveMemory(SelItem.Images[ListBox2.ItemIndex].Width,
                           SelItem.Images[ListBox2.ItemIndex].Height);
        Texture.Texture.SaveToBitmap(abmp);
        abmp.AssignToBitmap(bmp,false);
        abmp.Free;
        Image2.Picture.Bitmap.Assign(bmp);
      end
      else
      begin
        bmp.Width := Image2.Width;
        bmp.Height := Image2.Height;
        bmp.Canvas.Brush.Color := clBtnFace;
        bmp.Canvas.FillRect(bmp.Canvas.ClipRect);
        bmp.Canvas.TextOut(0,0,'No picture loaded!');
        Image2.Picture.Bitmap.Assign(bmp);
      end;
      bmp.Free;
    end;
    Button4.Enabled := true;
  end
  else
  begin
    Button4.Enabled := false;
  end;
end;

procedure TMainDlg.SelectStatus;
begin
  if ListBox3.ItemIndex > 0 then
  begin
    Button11.Enabled := true;
    Button8.Enabled := true;
  end
  else
  begin
    Button11.Enabled := false;
    Button8.Enabled := false;
  end;
end;

procedure TMainDlg.SpeedButton1Click(Sender: TObject);
var res:integer;
begin
  if not Saved then
  begin
    res := Application.MessageBox('You probably did not save your work. Do you '+
                                  'want to save it now?','Warning',MB_YESNOCANCEL);
    if res = ID_CANCEL then
    begin
      Exit;
    end
    else
    begin
      if res = ID_YES then
      begin
        SpeedButton2Click(nil);
      end;
    end;
  end;
  if OpenDialog1.Execute then
  begin
    CurrentFile := OpenDialog1.FileName;
    Saved := true;
    AdSkin.LoadFromFile(CurrentFile);
    StatusBar1.SimpleText := CurrentFile;
    RefreshItemList;
  end;
end;

procedure TMainDlg.SpeedButton2Click(Sender: TObject);
begin
  if CurrentFile = '' then
  begin
    SpeedButton5Click(nil);
  end
  else
  begin
    Saved := true;
    AdSkin.SaveToFile(CurrentFile);
    StatusBar1.SimpleText := CurrentFile;
  end;
end;

procedure TMainDlg.SpeedButton4Click(Sender: TObject);
begin
  Close;
end;

procedure TMainDlg.SpeedButton5Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    CurrentFile := ChangeFileExt(SaveDialog1.FileName,'.axs');
    AdSkin.SaveToFile(CurrentFile);
    Saved := true;
    StatusBar1.SimpleText := CurrentFile;
  end;
end;

{ THandleList }

procedure THandleList.Add(Item: TRHandle);
var p:PRHandle;
begin
  new(p);
  p^ := Item;
  inherited Add(p);
end;

function THandleList.GetItem(Index: integer): TRHandle;
begin
  result := PRHandle(inherited Items[Index])^; 
end;

procedure THandleList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then
  begin
    Dispose(PRHandle(Ptr));
  end;
end;

procedure THandleList.SetItem(Index: integer; Value: TRHandle);
begin
  PRHandle(inherited Items[Index])^ := Value;
end;

end.
