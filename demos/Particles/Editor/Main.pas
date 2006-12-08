{
* This program is licensed under the GNU Lesser General Public License Version 2
* You should have recieved a copy of the license with this file.
* If not, see http://www.gnu.org/licenses/lgpl.html for more informations
*
* Project: Andorra 2D
* Authors:  Andreas Stoeckel
* File: Main.pas
* Comment: This is a particle editor.
}

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, AdDraws, AdParticles, AndorraUtils, ExtCtrls, StdCtrls, Menus, XPMan,
  ComCtrls, ExtDlgs;

type
  TForm1 = class(TForm)
    Timer1: TTimer;
    Panel1: TPanel;
    Panel2: TPanel;
    MainMenu1: TMainMenu;
    Datei1: TMenuItem;
    PartikelLaden1: TMenuItem;
    N1: TMenuItem;
    Saveparticles1: TMenuItem;
    Saveparticlesas1: TMenuItem;
    N2: TMenuItem;
    Close1: TMenuItem;
    Images1: TMenuItem;
    Loadnewimagefile1: TMenuItem;
    Includeimageinparticlefile1: TMenuItem;
    N3: TMenuItem;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    GroupBox1: TGroupBox;
    Edit1: TEdit;
    GroupBox2: TGroupBox;
    Button1: TButton;
    CheckBox1: TCheckBox;
    Image1: TImage;
    ListBox1: TListBox;
    Button2: TButton;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Edit5: TEdit;
    Image2: TImage;
    XPManifest1: TXPManifest;
    CheckBox2: TCheckBox;
    Label5: TLabel;
    ComboBox1: TComboBox;
    Button3: TButton;
    Button4: TButton;
    Environment1: TMenuItem;
    Addparticlesystem1: TMenuItem;
    N4: TMenuItem;
    Backgroundcolor1: TMenuItem;
    ColorDialog1: TColorDialog;
    OpenPictureDialog1: TOpenPictureDialog;
    GroupBox3: TGroupBox;
    Label6: TLabel;
    Edit6: TEdit;
    Label7: TLabel;
    Label8: TLabel;
    Edit7: TEdit;
    Label9: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ListBox1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ListBox1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure CheckBox2Click(Sender: TObject);
    procedure Backgroundcolor1Click(Sender: TObject);
    procedure ListBox1DrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure ListBox1DblClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel1Resize(Sender: TObject);
  private
    { Private-Deklarationen }
  public

    AdDraw1:TAdDraw;
    AdImg1:TPictureCollectionItem;

    PerCount:TPerformanceCounter;
    PartSys:TAdParticleSystem;

    AdColList:TAdColorList;

    pc:double;

    mx,my:integer;
    BackgroundColor:TColor;
    procedure ApplicationIdle(Sender:TObject; var Done:boolean);
    procedure Render;
    procedure DrawColorPreview;
    procedure UpdateControls;
  end;

var
  Form1: TForm1;

const
  path = '..\demos\Particles\media\';

implementation

{$R *.dfm}

procedure TForm1.ApplicationIdle(Sender: TObject; var Done: boolean);
begin
  Render;
  done := false;
end;


procedure TForm1.Backgroundcolor1Click(Sender: TObject);
begin
  if ColorDialog1.Execute then
  begin
    BackgroundColor := ColorDialog1.Color;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var bmp:TBitmap;
begin
  if OpenPictureDialog1.FileName = '' then
  begin
    if not DirectoryExists(path) then
    begin
      OpenPictureDialog1.InitialDir := ExtractFilePath(ParamStr(0));
    end
    else
    begin
      OpenPictureDialog1.InitialDir := ExtractFilePath(ParamStr(0))+path;
    end;
  end;
  if OpenPictureDialog1.Execute then
  begin
    bmp := TBitmap.Create;
    bmp.LoadFromFile(OpenPictureDialog1.FileName);
    AdImg1 := TPictureCollectionItem.Create(AdDraw1);
    AdImg1.Texture.LoadFromBitmap(bmp);
    AdImg1.Texture.AddAlphaChannel(bmp);
    AdImg1.Color := clWhite;
    AdImg1.Restore;
    PartSys.Texture := AdImg1.Texture;
    Image2.Picture.Bitmap.Assign(bmp);
    bmp.Free;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  PartSys.DefaultParticle.Colors.Add(Ad_ARGB(StrToIntDef(Edit5.Text,255),
                                             StrToIntDef(Edit2.Text,255),
                                             StrToIntDef(Edit3.Text,255),
                                             StrToIntDef(Edit4.Text,255)));
  UpdateControls;
  ListBox1Click(nil);  
end;

procedure TForm1.Button3Click(Sender: TObject);
var i,e:integer;
begin
  i := ListBox1.ItemIndex;
  e := ListBox1.ItemIndex - 1;
  ListBox1.Items.Exchange(i,e);
  PartSys.DefaultParticle.Colors.Exchange(i,e);
  ListBox1.ItemIndex := e;
  ListBox1Click(nil);
  ListBox1.Repaint;
  DrawColorPreview;
end;

procedure TForm1.Button4Click(Sender: TObject);
var i,e:integer;
begin
  i := ListBox1.ItemIndex;
  e := ListBox1.ItemIndex + 1;
  ListBox1.Items.Exchange(i,e);
  PartSys.DefaultParticle.Colors.Exchange(i,e);
  ListBox1.ItemIndex := e;
  ListBox1Click(nil);
  ListBox1.Repaint;
  DrawColorPreview;
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
  PartSys.DefaultParticle.DrawMask := CheckBox2.Checked;
end;

procedure TForm1.DrawColorPreview;
var x,y,i:integer;
    acol:TColor;
    acol2:TAndorraColor;
    a:double;
begin
  with Image1.Picture.Bitmap.Canvas do
  begin
    //First draw a black and white chess field
    i := 0;
    for x := 0 to 54 do
      for y := 0 to 6 do
      begin
        i := i + 1;
        if i mod 2 = 0 then
        begin
          brush.Color := clBlack;
          pen.Color := clBlack;
        end
        else
        begin
          brush.Color := clGray;
          pen.Color := clGray;
        end;
        rectangle(x*4,y*4,(x+1)*4,(y+1)*4);
      end;

    //Then draw the colors
    for x := 0 to 216 do
    begin
      for y := 0 to 24 do
      begin
        acol2 := PartSys.DefaultParticle.Colors.GetColor(216,x);
        a := acol2.a / 255;
        acol := RGB(round(GetRValue(Pixels[x,y])*(1-a)+acol2.r*a),
                    round(GetGValue(Pixels[x,y])*(1-a)+acol2.g*a),
                    round(GetBValue(Pixels[x,y])*(1-a)+acol2.b*a));
        Pixels[x,y] := acol;
      end;
    end;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var bmp:TBitmap;
    i:integer;
begin
  Randomize;

  //Initialize Andorra 2D
  AdDraw1 := TAdDraw.Create(Panel1);
  AdDraw1.DllName := 'AndorraDX93D.dll';
  AdDraw1.Options := AdDraw1.Options;
  AdDraw1.Initialize;

  //Load the texture
  bmp := TBitmap.Create;
  bmp.Width := 32;
  bmp.Height := 32;
  with bmp.Canvas do
  begin
    Brush.Color := clBlack;
    Pen.Color := clBlack;
    Rectangle(0,0,32,32);
    Pen.Color := clWhite;
    Brush.Style := bsClear;
    for i := 0 to 16 do
    begin
      Pen.Color := RGB(round(255 / 16 * i),round(255 / 16 * i),round(255 / 16 * i));      
      Ellipse(i,i,32-i,32-i);
      Ellipse(i,i,32-i-1,32-i-1);
      Ellipse(i,i,32-i-2,32-i-2);
      Ellipse(i,i,32-i+1,32-i+1);
      Ellipse(i,i,32-i+2,32-i+2);
    end;
  end;
  Image2.Picture.Bitmap.Assign(bmp);
  AdImg1 := TPictureCollectionItem.Create(AdDraw1);
  AdImg1.Texture.LoadFromBitmap(bmp);
  AdImg1.Texture.AddAlphaChannel(bmp);
  AdImg1.Color := clWhite;
  AdImg1.Restore;
  bmp.Free;

  Application.OnIdle := ApplicationIdle;

  PerCount := TPerformanceCounter.Create;

  PartSys := TAdParticleSystem.Create(AdDraw1);
  PartSys.Texture := AdImg1.Texture;

  Image1.Picture.Bitmap.Width := 216;
  Image1.Picture.Bitmap.Height := 23;
  UpdateControls;

  mx := Panel1.Width div 2;
  my := Panel1.Height div 2;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  //Free everything
  PartSys.Free;
  PerCount.Free;
  AdImg1.Free;
  AdDraw1.Free;
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if ssLeft in Shift then
  begin
    mx := x;
    my := y;
  end;
end;

procedure TForm1.ListBox1Click(Sender: TObject);
begin
  if ListBox1.ItemIndex <> -1 then
  begin
    if ListBox1.ItemIndex < ListBox1.Count-1 then Button4.Enabled := true else Button4.Enabled := false;
    if ListBox1.ItemIndex > 0                then Button3.Enabled := true else Button3.Enabled := false;    
  end;
end;

procedure TForm1.ListBox1DblClick(Sender: TObject);
begin
  if ListBox1.ItemIndex <> -1 then
  begin
    Edit2.Text := inttostr(PartSys.DefaultParticle.Colors[ListBox1.ItemIndex].r);
    Edit3.Text := inttostr(PartSys.DefaultParticle.Colors[ListBox1.ItemIndex].g);
    Edit4.Text := inttostr(PartSys.DefaultParticle.Colors[ListBox1.ItemIndex].b);
    Edit5.Text := inttostr(PartSys.DefaultParticle.Colors[ListBox1.ItemIndex].a);
  end;
end;

procedure TForm1.ListBox1DrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
  with ListBox1.Canvas do
  begin
    Brush.Color := AdColorToColor(PartSys.DefaultParticle.Colors.Items[Index]);
    Pen.Color := Brush.Color;
    Rectangle(Rect);
    Font.Color := RGB(255-GetRValue(Pen.Color),255-GetGValue(Pen.Color),255-GetBValue(Pen.Color));
    Textout(Rect.Left,Rect.Top,ListBox1.Items[index]);
  end;
end;

procedure TForm1.ListBox1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_DELETE) and (ListBox1.ItemIndex <> -1) and (ListBox1.Items.Count > 1)then
  begin
    PartSys.DefaultParticle.Colors.Delete(ListBox1.ItemIndex);
    ListBox1.Items.Delete(ListBox1.ItemIndex);
    DrawColorPreview;
    ListBox1.Repaint;
  end
  else
  begin
    beep;
  end;
end;

procedure TForm1.Panel1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then
  begin
    mx := x;
    my := y;
  end;
end;

procedure TForm1.Panel1Resize(Sender: TObject);
begin
  if AdDraw1.Initialized then
  begin
    AdDraw1.Setup2DScene;
  end;
end;

procedure TForm1.Render;
begin
  PerCount.Calculate;
  Caption := 'FPS: '+inttostr(PerCount.FPS)+' Particle Count: '+inttostr(PartSys.Items.Count);

  AdDraw1.BeginScene;
  AdDraw1.ClearSurface(BackgroundColor);
  PartSys.CreateParticles(1,TAdParticle,mx,my);
  PartSys.Move(PerCount.TimeGap/1000);
  PartSys.Draw(0,0);
  PartSys.Dead;
  AdDraw1.EndScene;
  AdDraw1.Flip;
end;

procedure TForm1.UpdateControls;
var i:integer;
begin
  with PartSys.DefaultParticle do
  begin
    ListBox1.Clear;
    for i := 0 to Colors.Count - 1 do
    begin
      ListBox1.Items.Add('a: '+inttostr(Colors[i].a)+' '+
                         'r: '+inttostr(Colors[i].r)+' '+
                         'g: '+inttostr(Colors[i].g)+' '+
                         'b: '+inttostr(Colors[i].b)+' ');
    end;
    DrawColorPreview;
    ListBox1.Repaint;
  end;
end;

end.
