unit SetDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, AdDraws, AdClasses, VarDlg, ExtDlgs, Buttons,
  AdBitmap, AdTypes, AdPersistent;

type
  TMyImage = class(TImage)
    public
      function DestRect:TRect;
  end;

type
  TSettings = class(TForm)
    Button1: TButton;
    Button2: TButton;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Edit1: TEdit;
    RadioGroup1: TRadioGroup;
    GroupBox3: TGroupBox;
    ListBox1: TListBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    Image1: TImage;
    Image2: TImage;
    Button3: TButton;
    Button5: TButton;
    GroupBox6: TGroupBox;
    Edit2: TEdit;
    Label1: TLabel;
    Edit3: TEdit;
    Label2: TLabel;
    GroupBox7: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    Edit4: TEdit;
    Edit5: TEdit;
    Button6: TButton;
    Label5: TLabel;
    OpenPictureDialog1: TOpenPictureDialog;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    ColorDialog1: TColorDialog;
    GroupBox8: TGroupBox;
    Label6: TLabel;
    Edit6: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Button6Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    Setting:boolean;
    { Private-Deklarationen }
  public
    ChangedImages:boolean;
    procedure ViewCompressors;
    procedure MakeMask(c:TColor;add:boolean);
    function InspectImage(AImage:TAdImage):TModalResult;
    { Public-Deklarationen }
  end;

var
  Settings: TSettings;
  Var1,Var2:integer;
  
implementation

{$R *.dfm}

{ TSettings }

procedure TSettings.Button3Click(Sender: TObject);
var bmp:TBitmap;
begin
  if OpenPictureDialog1.Execute then
  begin
    bmp := TBitmap.Create;
    bmp.LoadFromFile(OpenPictureDialog1.FileName);
    with Image2.Picture.Bitmap do
    begin
      Canvas.StretchDraw(rect(0,0,Width,Height),bmp);
    end;
    bmp.Free;
    ChangedImages := true;
  end;
end;

procedure TSettings.Button4Click(Sender: TObject);
var p,pt:TPoint;
begin
  Image1.Cursor := crCross;
  Label5.Visible := true;
  with Image1 do
  begin
    pt.X := Width div 2;
    pt.Y := Height div 2;
    p := ClientToScreen(pt);
  end;
  SetCursorPos(p.X,p.Y);
end;

procedure TSettings.Button5Click(Sender: TObject);
begin
  with Image2.Picture.Bitmap.Canvas do
  begin
    Brush.Color := clWhite;
    FillRect(ClipRect);
  end;
  ChangedImages := true;
end;


procedure TSettings.Button6Click(Sender: TObject);
var Dlg:TVariations;
begin
  Dlg := TVariations.Create(self);
  Dlg.Values(Var1,Var2);
  Dlg.Free;
end;

procedure TSettings.FormCreate(Sender: TObject);
var b:TRect;
begin
  b := Image1.BoundsRect;
  Image1.Free;

  Image1 := TMyImage.Create(GroupBox4);
  Image1.Parent := GroupBox4;
  Image1.BoundsRect := b;
  Image1.Stretch := true;
  Image1.Center := true;
  Image1.Proportional := true;
  Image1.OnMouseDown := Image1MouseDown;

  ViewCompressors;
end;

procedure TSettings.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var px,py,ax,ay:integer;
begin
  if Image1.Cursor = crCross then
  begin
    with Image1 as TMyImage do
    begin
      px := X-DestRect.Left;
      py := Y-DestRect.Top;
      ax := round(Picture.Width/(DestRect.Right-DestRect.Left)*px);
      ay := round(Picture.Height/(DestRect.Bottom-DestRect.Top)*py);
      MakeMask(Picture.Bitmap.Canvas.Pixels[ax,ay], ssCtrl in Shift);
      Label5.Visible := false;
      Cursor := crDefault;
    end;
  end;
end;

function TSettings.InspectImage(AImage: TAdImage):TModalResult;
var bmp:TBitmap;
    adbmp:TAdBitmap;
    bits:byte;
begin
  Setting := true;
  Edit1.Text := AImage.Name;
  case AImage.Texture.BitDepth of
    16:Radiogroup1.ItemIndex := 0;
    32:Radiogroup1.ItemIndex := 1;
  end;
  if AImage.Texture.Compressor <> nil then  
    ListBox1.ItemIndex := ListBox1.Items.IndexOf(AImage.Texture.Compressor.ClassName);
  Edit2.Text := inttostr(AImage.PatternWidth);
  Edit3.Text := inttostr(AImage.PatternHeight);
  Edit4.Text := inttostr(AImage.SkipWidth);
  Edit5.Text := inttostr(AImage.SkipHeight);
  Edit6.Text := inttostr(AImage.PatternStop);
  if AImage.Texture.Initialized then
  begin
    bmp := TBitmap.Create;
    adbmp := TAdBitmap.Create;
    adbmp.ReserveMemory(AImage.Texture.Texture.BaseWidth,AImage.Texture.Texture.BaseHeight);
    AImage.Texture.Texture.SaveToBitmap(adbmp);
    adbmp.AssignTo(bmp);
    Image1.Picture.Assign(bmp);
    adbmp.AssignAlphaChannelTo(bmp);
    Image2.Picture.Assign(bmp);
    bmp.Free;
    adbmp.Free;
  end;
  Setting := false;
  ChangedImages := false;
  result := ShowModal;
  if result = mrOk then
  begin
    AImage.Name := Edit1.Text;
    case RadioGroup1.ItemIndex of
      0:bits := 16;
      1:bits := 32;
    else
      bits := 32;
    end;

    if AImage.Texture.BitDepth <> bits then
    begin
      if (AImage.Texture.BitDepth < bits) or (Application.MessageBox('You''re changing the bit depth of this'+
      ' texture. This means, that you may loose picture quality. Do you really'+
      ' want to apply these settings?','Alert',mb_YesNo) = idYes) then
      begin
        AImage.Texture.BitDepth := bits;
      end
      else
      begin
        bits := AImage.Texture.BitDepth;
      end;
    end
    else
    begin
      bits := AImage.Texture.BitDepth;
    end;

    AImage.Texture.Compressor := TAdGraphicCompressorClass(AdGetClass(ListBox1.Items[ListBox1.ItemIndex]));
    AImage.PatternWidth := strtointdef(Edit2.Text,0);
    AImage.PatternHeight := strtointdef(Edit3.Text,0);
    AImage.SkipWidth := strtointdef(Edit4.Text,0);
    AImage.SkipWidth := strtointdef(Edit5.Text,0);
    AImage.PatternStop := strtointdef(Edit6.Text,0);


    if ChangedImages then
    begin
      adbmp := TAdBitmap.Create;
      adbmp.Assign(Image1.Picture.Bitmap);
      adbmp.AssignAlphaChannel(Image2.Picture.Bitmap);
      AImage.Texture.Texture.LoadFromBitmap(AdBmp,AImage.Parent.GetTextureParams(bits));
      adbmp.Free;
    end;
    AImage.Restore;
  end;
end;

procedure TSettings.MakeMask(c:TColor;add:boolean);
var p1,p2:PRGBRec;
    x,y:integer;
    r,g,b:byte;
    e,res:integer;
begin
  r := GetRValue(c);
  g := GetGValue(c);
  b := GetBValue(c);
  with Image2.Picture.Bitmap do
  begin
    PixelFormat := pf24Bit;
    Image1.Picture.Bitmap.PixelFormat := pf24Bit;
    for y := 0 to Height - 1 do
    begin
      p2 := Scanline[y];
      p1 := Image1.Picture.Bitmap.ScanLine[y];
      for x := 0 to Width - 1 do
      begin
        e := round(sqrt(sqr(p1^.b-r)+sqr(p1^.g-g)+sqr(p1^.r-b)));
        if e <= var1 then
        begin
          res := 0;
        end else
        if e <= var2 then
        begin
          res := cut(round(255/(var2-var1) * (e-var1)));
        end else
        begin
          res := 255;
        end;
        if (not add) or (p2^.r > res) then
        begin
          p2^.r := res;
          p2^.g := res;
          p2^.b := res;
        end;
        inc(p1);
        inc(p2);
      end;
    end;
  end;
  ChangedImages := true;
  Repaint;
end;

procedure TSettings.RadioGroup1Click(Sender: TObject);
begin
  ChangedImages := true;
end;

procedure TSettings.SpeedButton1Click(Sender: TObject);
begin
  if ColorDialog1.Execute then
  begin
    MakeMask(ColorDialog1.Color,true);
  end;
end;

procedure TSettings.ViewCompressors;
var i:integer;
begin
  ListBox1.Clear;
  for i := 0 to RegisteredGraphicCompressors.Count - 1 do
  begin
    ListBox1.Items.Add(RegisteredGraphicCompressors.ValueFromIndex[i]);
  end;
  ListBox1.ItemIndex := 0;
end;

{ TMyImage }

function TMyImage.DestRect: TRect;
begin
  result := inherited DestRect;
end;

initialization
  Var1 := 40;
  Var2 := 40;

finalization

end.
