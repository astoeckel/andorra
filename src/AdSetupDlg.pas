{
* This program is licensed under the Common Public License (CPL) Version 1.0
* You should have recieved a copy of the license with this file.
* If not, see http://www.opensource.org/licenses/cpl1.0.txt for more informations.
* 
* Inspite of the incompatibility between the Common Public License (CPL) and the GNU General Public License (GPL) you're allowed to use this program * under the GPL. 
* You also should have recieved a copy of this license with this file. 
* If not, see http://www.gnu.org/licenses/gpl.txt for more informations.
*
* Project: Andorra 2D
* Author:  Andreas Stoeckel
* File: AdSetupDlg.pas
* Comment: A class which shows a setup dlg
}

unit AdSetupDlg;

interface

uses
  SysUtils, Forms, StdCtrls, ExtCtrls, Controls, Classes,
  AdDraws, AdClasses, AdDllExplorer{$IFDEF Win32}, Windows{$ENDIF},
  IniFiles, XPMan;


type
  TAbilitiesList = class(TList)
    private
    	function GetItem(AIndex:integer):TAd2DLibAbilities;
    	procedure SetItem(AIndex:integer;AItem:TAd2DLibAbilities);
    protected
      procedure Notify(ptr:Pointer; action:TListNotification);override;
    public
      procedure Add(Item:TAd2DLibAbilities);
    	property Items[AIndex:integer]:TAd2DLibAbilities read GetItem write SetItem;default;
  end;

  TAdSetupDlgSection = (dlgResolution, dlgAdvancedOptions, dlgPlugin);
  TAdSetupDlgSections = set of TAdSetupDlgSection;

  TAdSetupFrm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    imgContainer: TPanel;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    GroupBox3: TGroupBox;
    ComboBox1: TComboBox;
    GroupBox2: TGroupBox;
    CheckBox1: TCheckBox;
    ComboBox2: TComboBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    Image1: TImage;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    CheckBox2: TCheckBox;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel1: TPanel;
    XPManifest1: TXPManifest;
    procedure ComboBox1Change(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FPluginFiles:TStringList;
    FPluginAbilities:TAbilitiesList;
    
    function GetPlugins:boolean;
    procedure GetResolutions;
    procedure SetAbilities(AAbilities:TAd2DLibAbilities);
    procedure PluginCallBack(DllFileName:string;DllInfo:TAd2dLibInfo;DllAbilities:TAd2DLibAbilities);
  public
    function Execute(
      acaption:string; acopyright:string; aimage:string; aaddraw:TAdDraw;
      asections:TAdSetupDlgSections=[dlgResolution, dlgAdvancedOptions, dlgPlugin];
      aform:TForm=nil; aini:TIniFile=nil):boolean;overload;
  end;

  TAdSetup = class
  private
    FCaption:string;
    FCopyright:string;
    FImage:string;
    FAdDraw:TAdDraw;
    FForm:TForm;
    FSections:TAdSetupDlgSections;
    FOwner:TComponent;
    FIni:TIniFile;
    FIniFile: TIniFile;
    FOwnIniFile:boolean;
    procedure SetIni(AValue:TIniFile);
  public
    constructor Create(AOwner:TComponent);
    destructor Destroy;override;

    property Title:string read FCaption write FCaption;
    property Copyright:string read FCopyright write FCopyright;
    property Image:string read FImage write FImage;
    property AdDraw:TAdDraw read FAdDraw write FAdDraw;
    property Form:TForm read FForm write FForm;
    property Sections:TAdSetupDlgSections read FSections write FSections;
    property Ini:TIniFile read FIniFile write SetIni;

    function Execute(
      acaption:string; acopyright:string; aimage:string; aaddraw:TAdDraw;
      asections:TAdSetupDlgSections=[dlgResolution, dlgAdvancedOptions, dlgPlugin];
      aform:TForm=nil; aini:TIniFile=nil):boolean;overload;
    function Execute:boolean;overload;
  end;

const
  dlgAll = [dlgResolution, dlgAdvancedOptions, dlgPlugin];

implementation

{$R *.dfm}

type
  TStringArray = array of string;

procedure DevideString(AString:string;AChar:char;var AResult:TStringArray);
var
  s:string;
  p:integer;
begin
  SetLength(AResult,0);

  while length(AString) > 0 do
  begin
    //Search the next place where "AChar" apears in "AString"
    p := Pos(AChar,AString);

    if p = 0 then
    begin
      //If not found copy the whole string
      s := copy(AString,1,length(AString));
      AString := '';
    end
    else
    begin
      s := copy(AString,1,p-1);
      AString := copy(AString,p+1,length(AString)-p+1);
    end;

    //Add item
    SetLength(AResult,Length(AResult)+1);
    AResult[high(AResult)] := s;
  end;
end;

type
  PAbilities = ^TAd2DLibAbilities;

procedure TAbilitiesList.Add(Item: TAd2DLibAbilities);
var
  ptr:PAbilities;
begin
  New(ptr);
  ptr^ := Item;
  inherited Add(ptr);
end;

function TAbilitiesList.GetItem(AIndex:integer):TAd2DLibAbilities;
begin
  result := PAbilities(inherited Items[AIndex])^;
end;

procedure TAbilitiesList.Notify(ptr: Pointer; action: TListNotification);
begin
  if (action = lnDeleted) and (ptr <> nil) then
    Dispose(PAbilities(ptr));
end;

procedure TAbilitiesList.SetItem(AIndex:integer;AItem:TAd2DLibAbilities);
begin
  inherited Items[AIndex] := @AItem;
end;             

{ TTAdSetupDlg }

procedure TAdSetupFrm.CheckBox3Click(Sender: TObject);
begin
  Combobox2.Enabled := not CheckBox3.Checked;
end;

procedure TAdSetupFrm.ComboBox1Change(Sender: TObject);
begin
  SetAbilities(FPluginAbilities.Items[ComboBox1.ItemIndex]);
end;

function TAdSetupFrm.Execute(
   acaption:string; acopyright:string; aimage:string; aaddraw:TAdDraw;
   asections:TAdSetupDlgSections; aform:TForm; aini:TIniFile):boolean;
var
  scrwidth,scrheight,bitdepth:integer;
  strs:TStringArray;
  toppos,i,gb:integer;
begin
  //Create Lists
  FPluginFiles := TStringList.Create;
  FPluginAbilities := TAbilitiesList.Create;

  //Set captions and load image
  Caption := acaption;
  Label1.Caption := acopyright;
  if FileExists(aimage) then
    Image1.Picture.LoadFromFile(aimage);

  //Fill comboboxes
  if not GetPlugins then
  begin
    Application.MessageBox('No Andorra 2D Plugin found.','Fatal error');
    result := false;
    exit;
  end;
  GetResolutions;

  //Preset options
  if (aaddraw <> nil) and (aini = nil) then
  begin
    CheckBox1.Checked := doAntialias in aaddraw.Options;
    CheckBox2.Checked := doVSync in aaddraw.Options;
    CheckBox4.Checked := doFullscreen in aaddraw.Options;
    RadioButton1.Checked := not (doHardware in aaddraw.Options);
    RadioButton2.Checked := doHardware in aaddraw.Options;

    Combobox1.ItemIndex := FPluginFiles.IndexOf(aaddraw.DllName);
    if Combobox1.ItemIndex < 0 then
      Combobox1.ItemIndex := 0;
  end
  else
  begin
    //Load presets from ini file
    if aini <> nil then
    begin
      CheckBox1.Checked := aini.ReadBool('ad2dsetup','antialias',false);
      CheckBox2.Checked := aini.ReadBool('ad2dsetup','vsync',false);
      CheckBox3.Checked := aini.ReadBool('ad2dsetup','curres',false);
      CheckBox4.Checked := aini.ReadBool('ad2dsetup','fullscreen',false);
      RadioButton1.Checked := not aini.ReadBool('ad2dsetup','hardware',true);
      RadioButton2.Checked := aini.ReadBool('ad2dsetup','hardware',true);

      Combobox1.ItemIndex := FPluginFiles.IndexOf(aini.ReadString('ad2dsetup','plugin',''));
      if Combobox1.ItemIndex < 0 then
        Combobox1.ItemIndex := 0;

      Combobox2.ItemIndex := Combobox2.Items.IndexOf(aini.ReadString('ad2dsetup','resolution','1024x768x32'));
      if Combobox2.ItemIndex < 0 then
        Combobox2.ItemIndex := 0;

      Combobox1Change(nil);
    end;
  end;

  //Setup Setup-Dialog option group selections
  toppos := 0;
  gb := 0;
  for i := 0 to ComponentCount-1 do
  begin
    if Components[i] is TGroupbox then
      with Components[i] as TGroupbox do
      begin
        if TAdSetupDlgSection(gb) in asections then
        begin
          Top := toppos;
          toppos := toppos + Height + 5;
          Visible := true;
        end
        else
        begin
          Visible := false;
        end;

        gb := gb+1;
      end;
  end;
  Panel1.Height := toppos;
  ClientHeight := Panel2.Height + Panel3.Height + toppos;

  if FPluginFiles.Count > 0 then
  begin
    result := ShowModal = mrOk;
    if result then
    begin
      //Get Resolution
      DevideString(Combobox2.Items[Combobox2.ItemIndex],'x',strs);
      scrwidth := StrToInt(strs[0]);
      scrheight := StrToInt(strs[1]);
      bitdepth := StrToInt(strs[2]);

      if aaddraw <> nil then
      begin
        //Set Dll-File
        if (dlgPlugin in asections) then
          aaddraw.DllName := ExtractFilePath(ParamStr(0))+FPluginFiles[Combobox1.ItemIndex];

        if dlgPlugin in asections then
          aaddraw.Options := aaddraw.Options - [doHardware];
        if dlgResolution in asections then
          aaddraw.Options := aaddraw.Options - [doFullscreen];
        if dlgAdvancedOptions in asections then
        begin
          aaddraw.Options := aaddraw.Options - [doAntialias];
          aaddraw.Options := aaddraw.Options - [doVSync];
        end;

        //Set Options
        if RadioButton2.Checked and (dlgPlugin in asections) then
          aaddraw.Options := aaddraw.Options + [doHardware];
        if CheckBox4.Checked and (dlgResolution in asections) then
          aaddraw.Options := aaddraw.Options + [doFullscreen];
        if CheckBox1.Checked and (dlgAdvancedOptions in asections) then
          aaddraw.Options := aaddraw.Options + [doAntialias];
        if CheckBox2.Checked and (dlgAdvancedOptions in asections) then
          aaddraw.Options := aaddraw.Options + [doVSync];

        //Get current desktop resolution
        if CheckBox3.Checked then
        begin
          scrwidth := Screen.Width;
          scrheight := Screen.Height;
          bitdepth := 32;
        end;

        //Set AdDraw Display
        aaddraw.Display.Width := scrwidth;
        aaddraw.Display.Height := scrheight;
        aaddraw.Display.BitCount := bitdepth;
      end;

      //Setup form
      if (aform <> nil) and (dlgResolution in asections) then
      begin
        aform.Width := scrwidth;
        aform.Height := scrheight;

        aform.BorderIcons := [biSystemMenu, biMinimize];

        //Set form position. Place in Top-Left edge if fullscreen.
        if CheckBox4.Checked then
        begin
          Top := 0;
          Left := 0;
        end
        else
        begin
          aform.Position := poScreenCenter;
        end;

        //Set form border - if fullscreen or current screen resolution then use no border.
        if (CheckBox3.Checked) or (CheckBox4.Checked) then
        begin
          aform.BorderStyle := bsNone;
        end else
        begin
          aform.BorderStyle := bsSingle;
        end;
      end;

      //Save settings
      if aini <> nil then
      begin
        aini.WriteBool('ad2dsetup','antialias',CheckBox1.Checked);
        aini.WriteBool('ad2dsetup','vsync',CheckBox2.Checked);
        aini.WriteBool('ad2dsetup','curres',CheckBox3.Checked);
        aini.WriteBool('ad2dsetup','fullscreen',CheckBox4.Checked);
        aini.WriteBool('ad2dsetup','hardware',RadioButton2.Checked);
        aini.WriteString('ad2dsetup','plugin',FPluginFiles[Combobox1.ItemIndex]);
        aini.WriteString('ad2dsetup','resolution',Combobox2.Items[Combobox2.ItemIndex]);
      end;
    end;
  end
  else
  begin
    Application.MessageBox('No Andorra 2D Plugin DLLs were found!','Error',0);
    result := false;
  end;

  FPluginAbilities.Free;
  FPluginFiles.Free;
end;

procedure TAdSetupFrm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if ModalResult = mrNone then  
    ModalResult := mrCancel;
end;

function TAdSetupFrm.GetPlugins:boolean;
var
  explorer:TAdDLLExplorer;
begin
  FPluginFiles.Clear;
  FPluginAbilities.Clear;
  Combobox1.Items.Clear;

  explorer := TAdDllExplorer.Create;
  explorer.GetPlugins(PluginCallBack, ExtractFilePath(ParamStr(0)),'dll');
  explorer.Free;

  result := Combobox1.Items.Count > 0;
  if result then
  begin
    Combobox1.ItemIndex := 0;
    Combobox1Change(nil);
  end;
end;

procedure TAdSetupFrm.GetResolutions;
{$IFDEF win32}
var
  res:boolean;
  mode:TDevMode;
  i:integer;
  s:string;
{$ENDIF}
begin
  Combobox2.Items.Clear;
  {$IFDEF win32}
    i := 0;
    repeat
      mode.dmSize := SizeOf(TDevMode);
      res := EnumDisplaySettings(nil,i,mode);
      if res then
      begin
        if (mode.dmPelsWidth >= 640) and (mode.dmBitsPerPel >= 16) then
        begin
          s :=  inttostr(mode.dmPelsWidth)+'x'+inttostr(mode.dmPelsHeight)+'x'
               +inttostr(mode.dmBitsPerPel);
          if Combobox2.Items.IndexOf(s) = -1 then
            Combobox2.Items.Add(s);
        end;
        i := i + 1;
      end;
    until not res;
  {$ELSE}
    Combobox2.Items.Add('800x600x32');
    Combobox2.Items.Add('1024x768x32');
    Combobox2.Items.Add('1280x1024x32');
  {$ENDIF}
  Combobox2.ItemIndex := Combobox2.Items.Count-1;
end;

procedure TAdSetupFrm.PluginCallBack(DllFileName: string; DllInfo: TAd2dLibInfo;
  DllAbilities: TAd2DLibAbilities);
begin
  FPluginFiles.Add(DllFileName);
  FPluginAbilities.Add(DllAbilities);
  Combobox1.Items.Add(DllInfo.LibTitle);
end;

procedure TAdSetupFrm.SetAbilities(AAbilities: TAd2DLibAbilities);
begin
  CheckBox4.Enabled := AAbilities.LibFullscreen or (not AAbilities.LibWindowed);
  CheckBox4.Checked := (AAbilities.LibFullscreen and CheckBox4.Checked) or (not AAbilities.LibWindowed);
  CheckBox1.Enabled := AAbilities.LibAntialias;
  CheckBox1.Checked := AAbilities.LibAntialias and CheckBox1.Checked;
  CheckBox2.Enabled := AAbilities.LibVSync;
  CheckBox2.Checked := AAbilities.LibVSync and CheckBox2.Checked;
  RadioButton1.Enabled := AAbilities.LibSoftware;
  RadioButton2.Enabled := AAbilities.LibHardware;
  RadioButton1.Checked := (AAbilities.LibSoftware) or RadioButton1.Checked;
  RadioButton2.Checked := (AAbilities.LibHardware) or RadioButton2.Checked;
end;

{ TAdSetup }

constructor TAdSetup.Create(AOwner:TComponent);
begin
  FCaption := Application.Title;
  FCopyright := #169+' by Andreas Stöckel';
  FImage := '';
  FAdDraw := nil;
  FForm := nil;
  FSections := dlgAll;
  FOwner := AOwner;
  FIni := TIniFile.Create(ExtractFilePath(ParamStr(0))+'adsettings.ini');
  FOwnIniFile := true;
end;

function TAdSetup.Execute(acaption, acopyright, aimage: string;
  aaddraw: TAdDraw; asections: TAdSetupDlgSections; aform: TForm; aini:TIniFile): boolean;
var
  dlg:TAdSetupFrm;
begin
  FCaption := acaption;
  Copyright := acopyright;
  Image := aimage;
  AdDraw := aaddraw;
  Sections := asections;
  Form := aform;
  dlg := TAdSetupFrm.Create(fowner);
  result := dlg.Execute(acaption, acopyright, aimage, aaddraw, asections, aform, aini);
  dlg.Free;
end;

destructor TAdSetup.Destroy;
begin
  if FOwnIniFile then
  begin
    FreeAndNil(FIni);
  end;

  inherited;
end;

function TAdSetup.Execute: boolean;
begin
  result := Execute(FCaption, FCopyRight, FImage, FAdDraw, FSections, FForm, FIni);
end;

procedure TAdSetup.SetIni(AValue: TIniFile);
begin
  if FOwnIniFile then
  begin
    FreeAndNil(FIni);
  end;
  
  FOwnIniFile := false;
  FIni := AValue;
end;

end.
