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
  Dialogs, ExtCtrls, StdCtrls, Menus, XPMan,
  ComCtrls, ExtDlgs, Math,
  Buttons, ImgList, ToolWin, Tabs, JvInspector, TypInfo, PartParam,
  AdPerformanceCounter, AdTypes, AdVCLFormats, AdBitmap, AdPersistent, AdCanvas,
  AdDraws, AdParticles, AdClasses;

type
  TMainFrm = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    New1: TMenuItem;
    N1: TMenuItem;
    Open1: TMenuItem;
    N2: TMenuItem;
    Save1: TMenuItem;
    Saveas1: TMenuItem;
    N3: TMenuItem;
    Close1: TMenuItem;
    View1: TMenuItem;
    Setbackgroundcolor1: TMenuItem;
    Image1: TMenuItem;
    Loadparticleimage1: TMenuItem;
    ToolBar1: TToolBar;
    ImageList1: TImageList;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    Surface: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    XPManifest1: TXPManifest;
    N5: TMenuItem;
    Grid1: TMenuItem;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    cmb_classes: TComboBox;
    StatusBar1: TStatusBar;
    PageControl1: TPageControl;
    tbs_Particle: TTabSheet;
    tbs_Colors: TTabSheet;
    tbs_Editor: TTabSheet;
    GroupBox2: TGroupBox;
    Button1: TButton;
    GroupBox3: TGroupBox;
    rb_emittime: TRadioButton;
    Label3: TLabel;
    edt_emittime: TEdit;
    edt_emitcount: TEdit;
    Label4: TLabel;
    rb_emitcount: TRadioButton;
    Panel1: TPanel;
    img_part: TImage;
    OpenPictureDialog1: TOpenPictureDialog;
    Panel3: TPanel;
    GroupBox4: TGroupBox;
    pntb_pre: TPaintBox;
    GroupBox5: TGroupBox;
    lb_colors: TListBox;
    btn_delete: TButton;
    GroupBox6: TGroupBox;
    pnl_r: TPanel;
    pnl_g: TPanel;
    pnl_b: TPanel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    pnl_a: TPanel;
    Label8: TLabel;
    trbr_r: TTrackBar;
    trbr_g: TTrackBar;
    trbr_b: TTrackBar;
    trbr_a: TTrackBar;
    pntb_a: TPaintBox;
    btn_add: TButton;
    Timer1: TTimer;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Label2: TLabel;
    Particleorign1: TMenuItem;
    Boundsrect1: TMenuItem;
    ColorDialog1: TColorDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure edt_emitcountChange(Sender: TObject);
    procedure SurfaceResize(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure cmb_classesChange(Sender: TObject);
    procedure SurfaceMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SurfaceMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure trbr_rChange(Sender: TObject);
    procedure lb_colorsClick(Sender: TObject);
    procedure lb_colorsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure pntb_prePaint(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure btn_deleteClick(Sender: TObject);
    procedure btn_addClick(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure Saveas1Click(Sender: TObject);
    procedure Close1Click(Sender: TObject);
    procedure Boundsrect1Click(Sender: TObject);
    procedure Setbackgroundcolor1Click(Sender: TObject);
    private
      FMX, FMY: integer;
      JvInspector: TJvInspector;
      JvPainter: TJvInspectorBorlandPainter;
    public
      AdDraw: TAdDraw;
      AdPartSys: TAdParticleSystem;
      AdPerformanceCounter: TAdPerformanceCounter;
      AdDefaultPart: TAdParticle;
      Gap: double;
      EmissionTime: double;
      EmissionCount: integer;

      Updating: boolean;

      CenterX, CenterY: integer;
      EmitterX, EmitterY: integer;

      SelectedColor: PAndorraColor;
      SelectedColorIndex: integer;

      CurrentFileName: string;
      Saved: boolean;

      function CheckSaved: boolean;
      function Save: boolean;
      function SaveAs: boolean;
      procedure Load;

      procedure InitInspector;
      procedure InitAndorra;
      procedure LoadImage;
      procedure New;
      procedure Emit;
      procedure Idle(Sender: TObject; var Done: boolean);
      procedure FillClassCmb;
      procedure SetDefaultPart;
      procedure DrawGrid;
      procedure DrawBoundsRect;
      procedure DrawEmitter;
      procedure DrawAlphaPreview;
      procedure DrawColorband;
      procedure UpdateColorListbox;
      procedure UpdateSaved(Sender: TObject; Item: TJvCustomInspectorItem);
      procedure UpdateStatusbar;
  end;

  TJvInspectorParticleParameter = class(TJvInspectorClassItem)
  protected
    procedure Edit; override;
    procedure SetFlags(const Value: TInspectorItemFlags); override;
  end;

var
  MainFrm: TMainFrm;

const
  path = 'particles\';

implementation

{$R *.dfm}

{ TMainFrm }

procedure TMainFrm.Button1Click(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
  begin
    img_part.Picture.LoadFromFile(OpenPictureDialog1.FileName);
    LoadImage;
  end;
end;

procedure TMainFrm.btn_addClick(Sender: TObject);
begin
  Saved := false;
  AdPartSys.Colors.Add(Ad_ARGB(255, 255, 255, 255));
  UpdateColorListBox;
  UpdateStatusbar;
end;

procedure TMainFrm.Boundsrect1Click(Sender: TObject);
begin
  TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
end;

procedure TMainFrm.btn_deleteClick(Sender: TObject);
begin
  if AdPartSys.Colors.Count > 2 then
  begin
    if lb_colors.ItemIndex > -1 then
    begin
      AdPartSys.Colors.Delete(lb_colors.ItemIndex);
      UpdateColorListbox;
    end;
    lb_colorsClick(nil);
    Saved := false;
    UpdateStatusbar;
  end;
end;

procedure TMainFrm.cmb_classesChange(Sender: TObject);
begin
  if not Updating then  
    SetDefaultPart;
end;

procedure TMainFrm.DrawAlphaPreview;
var
  i, x, y, r, g, b: integer;
begin
  i := 0;
  for y := 0 to pntb_a.ClientHeight - 1 do
  begin
    for x := 0 to pntb_a.ClientWidth - 1 do
    begin
      if x mod 5 = 0 then i := i + 1;
      if (i mod 2 = 0) xor (trunc(y / 5) * 5 mod 2 = 0) then
      begin
        r := 128; g := 128; b := 128;
      end
      else
      begin
        r := 0; g := 0; b := 0;
      end;

      r := round(r * (1 - trbr_a.Position / 255) + trbr_r.Position * (trbr_a.Position / 255));
      g := round(g * (1 - trbr_a.Position / 255) + trbr_g.Position * (trbr_a.Position / 255));
      b := round(b * (1 - trbr_a.Position / 255) + trbr_b.Position * (trbr_a.Position / 255));
      pntb_a.Canvas.Pixels[x, y] := RGB(r, g, b);
    end;
  end;
end;

procedure TMainFrm.DrawBoundsRect;
var
  r: TAdRect;
begin
  with AdDraw.Canvas do
  begin
    Pen.Color := AD_ARGB(128, 255, 0, 0);
    Brush.Style := abClear;
    r := AdPartSys.BoundsRect;
    AdOffsetRect(r, CenterX, CenterY);
    Rectangle(r);
    Release;
  end;
end;

procedure TMainFrm.DrawColorband;
var
  i, x, y, r, g, b: integer;
  c: TAndorraColor;
begin
  i := 0;
  for y := 0 to pntb_pre.ClientHeight - 1 do
  begin
    for x := 0 to pntb_pre.ClientWidth - 1 do
    begin
      if x mod 5 = 0 then i := i + 1;
      if (i mod 2 = 0) xor (trunc(y / 5) * 5 mod 2 = 0) then
      begin
        r := 128; g := 128; b := 128;
      end
      else
      begin
        r := 0; g := 0; b := 0;
      end;

      c := AdPartSys.Colors.GetColor(pntb_pre.ClientWidth, x);

      r := round(r * (1 - c.a / 255) + c.r * c.a / 255);
      g := round(g * (1 - c.a / 255) + c.g * c.a / 255);
      b := round(b * (1 - c.a / 255) + c.b * c.a / 255);
      pntb_pre.Canvas.Pixels[x, y] := RGB(r, g, b);
    end;
  end;
end;

procedure TMainFrm.DrawEmitter;
begin
  with AdDraw.Canvas do
  begin
    Pen.Color := AD_ARGB(128, 0, 255, 0);
    Brush.Style := abClear;
    Circle(EmitterX + CenterX, EmitterY + CenterY, 5); 
    Release;
  end;
end;

procedure TMainFrm.DrawGrid;
begin
  with AdDraw.Canvas do
  begin
    Pen.Color := AD_ARGB(128, 0, 0, 255);
    Brush.Style := abClear;
    Line(CenterX, 0, CenterX, Surface.ClientHeight);
    Line(0, CenterY, Surface.ClientWidth, CenterY);
    Release;
  end;
end;

procedure TMainFrm.edt_emitcountChange(Sender: TObject);
begin
  EmissionTime := StrToFloatDef(edt_emittime.Text, 10);
  EmissionCount := StrToIntDef(edt_emitcount.Text, 100);
end;

procedure TMainFrm.Emit;
begin
  if rb_emittime.Checked then
  begin
    Gap := Gap + AdPerformanceCounter.TimeGap;
    if Gap > EmissionTime then
    begin
      AdPartSys.Emit(Round(Gap / EmissionTime), EmitterX, EmitterY);
      Gap := Gap - EmissionTime * Round(Gap / EmissionTime);
    end;
  end else
  begin
    if AdPartSys.Items.Count = 0 then
    begin
      AdPartSys.Emit(EmissionCount, EmitterX, EmitterY);
    end;
  end;
end;

procedure TMainFrm.FillClassCmb;
begin
  cmb_classes.Clear;
  cmb_classes.Items.Assign(RegisteredParticleClasses);
  cmb_classes.ItemIndex := 0;
end;

procedure TMainFrm.FormCreate(Sender: TObject);
begin
  InitAndorra;
  InitInspector;
  New;

  EmissionTime := 10;
  EmissionCount := 100;
end;

procedure TMainFrm.FormDestroy(Sender: TObject);
begin
  if AdDefaultPart <> nil then
    AdDefaultPart.Free;

  AdPartSys.Free;
  AdPerformanceCounter.Free;
  AdDraw.Free;
end;

procedure TMainFrm.Idle(Sender: TObject; var Done: boolean);
begin
  if AdDraw.CanDraw then
  begin
    AdPerformanceCounter.Calculate;
    Emit;

    AdDraw.ClearSurface(ColorDialog1.Color);
    AdDraw.BeginScene;

    if Grid1.Checked then
      DrawGrid;

    AdPartSys.Move(AdPerformanceCounter.TimeGap / 1000);

    AdPartSys.Draw(AdDraw, CenterX, CenterY);

    if Boundsrect1.Checked then
      DrawBoundsRect;

    if Particleorign1.Checked then
      DrawEmitter;

    with AdDraw.Canvas do
    begin
      Pen.Color := ColorToAdColor(not ColorDialog1.Color);
      TextOut(0, 0,  'FPS: '+inttostr(AdPerformanceCounter.FPS));
      TextOut(0, 16, 'Particle System FPS: '+inttostr(AdPartSys.FPS));
      Release;
    end;

    AdDraw.EndScene;

    AdDraw.Flip;

    Done := false;
  end;
end;

procedure TMainFrm.InitAndorra;
begin
  AdDraw := TAdDraw.Create(Surface);
  try
    AdDraw.DllName := 'AndorraOGL.dll';
    if not AdDraw.Initialize then
    begin
      ShowMessage('Error while initializing Andorra 2D');
      Halt;
    end else
    begin
      AdPartSys := TAdParticleSystem.Create(AdDraw);
      AdPerformanceCounter := TAdPerformanceCounter.Create;

      Application.OnIdle := Idle;
    end;
  except
    ShowMessage('Error while initializing Andorra 2D');
    Halt;
  end;
end;

procedure TMainFrm.InitInspector;
begin
  JvInspector := TJvInspector.Create(self);
  JvInspector.Parent := tbs_Particle;
  JvInspector.Align := alClient;
  JvPainter := TJvInspectorBorlandPainter.Create(self);
  JvInspector.Painter := JvPainter;
  JvInspector.OnItemValueChanged := UpdateSaved;
end;

procedure TMainFrm.lb_colorsClick(Sender: TObject);
begin
  if lb_colors.ItemIndex <> -1 then
  begin
    Updating := true;

    trbr_r.Position := AdPartSys.Colors[lb_colors.ItemIndex].r;
    trbr_g.Position := AdPartSys.Colors[lb_colors.ItemIndex].g;
    trbr_b.Position := AdPartSys.Colors[lb_colors.ItemIndex].b;
    trbr_a.Position := AdPartSys.Colors[lb_colors.ItemIndex].a;

    SelectedColor := TList(AdPartSys.Colors).Items[lb_colors.ItemIndex];

    Updating := false;
  end else
    SelectedColor := nil;

  btn_delete.Enabled := SelectedColor <> nil;

  SelectedColorIndex := lb_colors.ItemIndex;
  lb_colors.Repaint;
end;

procedure TMainFrm.lb_colorsDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
  with lb_colors.Canvas do
  begin
    if (odSelected in State) or (odFocused in State) or (SelectedColorIndex = Index) then
      Brush.Color := clHighlight
    else
      Brush.Color := clWhite;

    FillRect(Rect);
    Pen.Color := clBlack;
    Brush.Color := AdColorToColor(AdPartSys.Colors[Index]);
    Rectangle(Rect.Left, Rect.Top + 3, Rect.Left + 20, Rect.Bottom - 3);

    Brush.Style := bsClear;
    TextOut(Rect.Left + 25, Rect.Top, lb_colors.Items[Index]);
  end; 
end;

procedure TMainFrm.LoadImage;
begin
  if img_part.Picture.Graphic <> nil then
  begin
    TAdTexture(AdPartSys.Texture).LoadFromGraphic(img_part.Picture.Graphic);
  end;
end;

procedure TMainFrm.New;
begin
  //Renew particle system
  AdPartSys.Free;
  AdPartSys := TAdParticleSystem.Create(AdDraw);

  LoadImage;
  FillClassCmb;
  SetDefaultPart;
  UpdateColorListbox;

  Saved := true;
  UpdateStatusbar;
end;

procedure TMainFrm.pntb_prePaint(Sender: TObject);
begin
  DrawColorband;
end;

procedure TMainFrm.Setbackgroundcolor1Click(Sender: TObject);
begin
  ColorDialog1.Execute;
end;

procedure TMainFrm.SetDefaultPart;
var
  tmp: TAdParticle;
begin
  AdPartSys.Free;

  AdPartSys := TAdParticleSystem.Create(AdDraw);
  LoadImage;

  tmp := TAdParticleClass(
    AdGetClass(cmb_classes.Items[cmb_classes.ItemIndex])).Create(AdPartSys);
  AdPartSys.DefaultParticle := tmp;
  JvInspector.InspectObject := tmp;

  tmp.Assign(AdDefaultPart);

  if AdDefaultPart <> nil then
    AdDefaultPart.Free;

  AdDefaultPart := tmp;

  SelectedColorIndex := -1;
  SelectedColor := nil;
  UpdateColorListbox;
  Timer1.Enabled := true;
end;

procedure TMainFrm.SurfaceMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FMX := X;
  FMY := Y;
end;

procedure TMainFrm.SurfaceMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if ssRight in Shift then
  begin
    CenterX := CenterX + (X - FMX);
    CenterY := CenterY + (Y - FMY);
  end else
  if ssLeft in Shift then
  begin
    EmitterX := EmitterX + (X - FMX);
    EmitterY := EmitterY + (Y - FMY);
  end;

  FMX := X;
  FMY := Y;
end;

procedure TMainFrm.SurfaceResize(Sender: TObject);
begin
  CenterX := Surface.ClientWidth div 2;
  CenterY := Surface.ClientHeight div 2;

  AdDraw.Setup2DScene;
end;

procedure TMainFrm.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := false;
  DrawColorband;
  DrawAlphaPreview;
end;

procedure TMainFrm.ToolButton1Click(Sender: TObject);
begin
  if CheckSaved then
  begin
    New;
  end;
end;

procedure TMainFrm.ToolButton2Click(Sender: TObject);
begin
  Load;
end;

procedure TMainFrm.ToolButton3Click(Sender: TObject);
begin
  Save;
end;

procedure TMainFrm.trbr_rChange(Sender: TObject);
begin
  pnl_r.Color := RGB(trbr_r.Position, 0, 0);
  pnl_g.Color := RGB(0, trbr_g.Position, 0);
  pnl_b.Color := RGB(0, 0, trbr_b.Position);

  Timer1.Enabled := true;

  if not Updating then
  begin
    Saved := false;
    UpdateStatusbar;
    
    if SelectedColor <> nil then
    begin
      SelectedColor^ :=
        Ad_ARGB(trbr_a.Position, trbr_r.Position, trbr_g.Position, trbr_b.Position); 
      UpdateColorListbox;
    end;
  end;
end;

procedure TMainFrm.UpdateColorListbox;
var
  i: integer;
begin
  lb_colors.Items.BeginUpdate;
  lb_colors.Items.Clear;
  for i := 0 to AdPartSys.Colors.Count - 1 do
  begin
    lb_colors.Items.Add(
      'R: ' + IntToStr(AdPartSys.Colors[i].r) + ' ' +
      'G: ' + IntToStr(AdPartSys.Colors[i].g) + ' ' +
      'B: ' + IntToStr(AdPartSys.Colors[i].b) + ' ' +
      'A: ' + IntToStr(AdPartSys.Colors[i].a));
  end;
  lb_colors.Items.EndUpdate;
end;

procedure TMainFrm.UpdateSaved(Sender: TObject;  Item: TJvCustomInspectorItem);
begin
  Saved := false;
  UpdateStatusbar;
end;

procedure TMainFrm.UpdateStatusbar;
begin
  Statusbar1.SimpleText := CurrentFilename;
  if not Saved then
    Statusbar1.SimpleText := Statusbar1.SimpleText + ' [*]';
end;

function TMainFrm.Save: boolean;
begin
  if CurrentFilename <> '' then
  begin
    AdPartSys.SaveToFile(CurrentFilename);
    result := true;
    Saved := true;
  end else
  begin
    result := SaveAs;
  end;

  UpdateStatusbar;
end;

function TMainFrm.SaveAs: boolean;
begin
  result := false;
  if SaveDialog1.Execute then
  begin
    CurrentFilename := ChangeFileExt(SaveDialog1.FileName, '.axp');
    AdPartSys.SaveToFile(CurrentFilename);
    Saved := true;
    result := true;
  end;

  UpdateStatusbar;
end;

procedure TMainFrm.Saveas1Click(Sender: TObject);
begin
  SaveAs;
end;

function TMainFrm.CheckSaved: boolean;
var
  btn: integer;
begin
  if not Saved then
  begin
    result := false;

    btn := Application.MessageBox(
      'Do you want to save your current project?', 'Question', MB_YESNOCANCEL);

    if btn = ID_YES then
      result := Save
    else if btn = ID_NO then
      result := true;
  end else
    result := true;
end;

procedure TMainFrm.Close1Click(Sender: TObject);
begin
  if CheckSaved then
    Close;
end;

procedure TMainFrm.Load;
begin
  if CheckSaved and OpenDialog1.Execute then
  begin
    New;
    AdPartSys.LoadFromFile(OpenDialog1.FileName);
    CurrentFilename := OpenDialog1.FileName;

    UpdateColorListbox;
    DrawColorband;

    Updating := true;
    cmb_classes.ItemIndex := cmb_classes.Items.IndexOf(AdPartSys.DefaultParticle.ClassName);
    Updating := false;

    JvInspector.InspectObject := AdPartSys.DefaultParticle;

    Saved := true;
  end;
end;

{ TJvInspectorParticleParameter }

procedure TJvInspectorParticleParameter.Edit;
var
  edt: TParamEdt;
begin
  edt := TParamEdt.Create(nil);
  edt.Edit(TAdParticleParameter(
    GetObjectProp(TJvInspector(Inspector).InspectObject, Data.Name)));
  edt.Free;
end;

procedure TJvInspectorParticleParameter.SetFlags(const Value: TInspectorItemFlags);
var
  NewValue: TInspectorItemFlags;
begin
  NewValue := Value + [iifEditButton, iifEditFixed];
  inherited SetFlags(NewValue);
end;

initialization
  TJvCustomInspectorData.ItemRegister.Add(
    TJvInspectorTypeInfoRegItem.Create(
      TJvInspectorParticleParameter, TypeInfo(TAdParticleParameter)));


end.
