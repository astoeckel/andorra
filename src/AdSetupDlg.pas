unit AdSetupDlg;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  {$IFDEF FPC}Interfaces,{$ENDIF}
  SysUtils, Forms, Classes, Controls, StdCtrls, ExtCtrls, Messages,
  IniFiles,
  {$IFNDEF FPC}XPMan, {$ELSE}lMessages, {$ENDIF}
  {$IFDEF WIN32}Windows,{$ENDIF}
  AdClasses, AdTypes, AdDraws, AdDLLExplorer, AdMessages;

const
{$IFNDEF FPC}
  WM_AD_REBUILDCONTROLS = WM_USER + 1;
{$ELSE}
  WM_AD_REBUILDCONTROLS = LM_USER + 1;
{$ENDIF}

type
  ESetupDlg = class(Exception);
  ESetupDlgNoPluginsFound = class(ESetupDlg);

  TAdSetupDlgSection = (
    dlgResolution,
    dlgAdvancedOptions,
    dlgPlugin
  );

  TAdSetupDlgSections = set of TAdSetupDlgSection;

  TAdSetupSection = class
    private
      FGroupbox: TGroupbox;
      FControls: TList;
      FSpacerPanel: TPanel;
      FCaption: string;
      function GetHeight: integer;
    public
      constructor Create(AParent: TWinControl; ACaption: string);
      destructor Destroy;override;

      procedure AddControl(AControl: TWinControl);

      property Caption: string read FCaption;
  end;

  TAdSetupForm = class(TForm)
    private
      FOnRebuildControls: TNotifyEvent;
      procedure MsgRebuildControls(var Message: TMessage); message WM_AD_REBUILDCONTROLS;
    public
      procedure DoRebuildControls;
      property OnRebuildControls: TNotifyEvent read FOnRebuildControls write FOnRebuildControls;
  end;

  TAdSetup = class
    private
      FDraw: TAdDraw;
      FDllExplorer: TAdDllExplorer;
      FForm: TAdSetupForm;

      FTopPanel: TPanel;
      FImageFrame: TPanel;
      FBottomPanel: TPanel;
      FCenterPanel: TPanel;
      FOkBtn: TButton;
      FCancelBtn: TButton;
      FPluginCombobox: TComboBox;
      FResolutionCombobox: TComboBox;
      FFullscreenCheckBox: TCheckBox;
      FDesktopResolutionCheckBox: TCheckBox;
      FHeaderImage: TImage;

      FPluginList: TStringList;
      FPluginIndex: integer;

      FSetupSections: TList;
      FUpdating: boolean;

      FSections: TAdSetupDlgSections;
      FImage: string;
      FTitle: string;
      FIni: TInifile;
      FOwnIni: boolean;

      procedure CreateDefaultControls;
      procedure CreateSectionControls;
      procedure CreateAdvancedControls;      
      procedure CreatePluginControls;
      procedure CreateResolutionControls;
      procedure AutoResize;
      
      procedure AddSectionSpacer;

      procedure FreeSections;
      procedure InitForm;
      procedure SetupForm;
      procedure FreeForm;
      function GetPlugins: boolean;
      procedure GetResolutions;

      procedure DesktopResolutionClick(Sender: TObject);
      procedure FullscreenClick(Sender: TObject);
      procedure PluginChange(Sender: TObject);
      procedure ResolutionChange(Sender: TObject);

      procedure BooleanPropChange(Sender: TObject);

      procedure DoRebuildControls(Sender: TObject);

      function SearchTagComponent(ATag: integer; AComp: TComponent): TComponent;

      procedure SetIni(AValue: TIniFile);
      procedure LoadSettings;
      procedure StoreSettings;
    public
      constructor Create(ADraw: TAdDraw);
      destructor Destroy;override;

      function Execute: boolean;

      property Sections: TAdSetupDlgSections read FSections write FSections;
      property Image: string read FImage write FImage;
      property Title: string read FTitle write FTitle;
      property Ini: TIniFile read FIni write SetIni;
  end;

const
  dlgAll = [dlgResolution, dlgAdvancedOptions, dlgPlugin];


implementation

{ TAdSetup }

constructor TAdSetup.Create(ADraw: TAdDraw);
begin
  inherited Create;

  FDraw := ADraw;

  FDllExplorer := TAdDllExplorer.Create;
  FSetupSections := TList.Create;
  FPluginList := TStringList.Create;

  FPluginIndex := -1;

  FSections := dlgAll;
  FTitle := Application.Title;
  FOwnIni := true;
end;

destructor TAdSetup.Destroy;
begin
  FDllExplorer.Free;
  FSetupSections.Free;
  FPluginList.Free;

  FreeForm;

  inherited;
end;

function TAdSetup.Execute: boolean;
begin
  result := false;

  //Initialize Form
  InitForm;

  //Create controls
  CreateDefaultControls;

  //Search for a plugin list
  if GetPlugins then
  begin
    //Create section controls
    CreateSectionControls;

    //Load old settings
    LoadSettings;

    if FPluginIndex = -1 then
    begin
      FPluginIndex := 0;
      PluginChange(nil);
    end;

    if FForm.ShowModal = mrOk then
    begin
      //Store new settings
      StoreSettings;
      result := true;
    end else
    begin
      if FIni <> nil then
      begin
        FIni.Free;
        FIni := nil;
      end;
    end;    
  end else
    raise ESetupDlgNoPluginsFound.Create(MsgSetupNoPluginsFound);

  //Free created dialog controls
  FreeSections;
  FreeForm;
end;

procedure TAdSetup.InitForm;
begin
  //Free old instance (if existing) first
  FreeForm;

  {$IFDEF FPC}
  Application.Initialize;
  FForm := TAdSetupForm.Create(nil);
  {$ELSE}
  FForm := TAdSetupForm.CreateNew(nil);
  {$ENDIF}
  FForm.OnRebuildControls := DoRebuildControls;

  //Set form properties
  SetupForm;
end;

procedure TAdSetup.SetupForm;
begin
  FForm.Width := 320;
  FForm.Height := 460;
  FForm.BorderStyle := bsSingle;
  FForm.BorderIcons := [biSystemMenu];
  FForm.Caption := FTitle;
  FForm.Position := poScreenCenter;
end;

procedure TAdSetup.FreeForm;
begin
  if FForm <> nil then
  begin
    FForm.Free;
    FForm := nil;
  end;

  FTopPanel := nil;
  FImageFrame := nil;
  FHeaderImage := nil;
  FBottomPanel := nil;
  FCenterPanel := nil;
  FCancelBtn := nil;
  FOkBtn := nil;
end;

procedure TAdSetup.CreateDefaultControls;
begin
  //Create top panel
  FTopPanel := TPanel.Create(FForm);
  FTopPanel.Parent := FForm;
  FTopPanel.BevelOuter := bvNone;
  FTopPanel.Align := alTop;
  FTopPanel.Width := FForm.Width;
  FTopPanel.Height := 0;

  if FileExists(FImage) then
  begin
    FImageFrame := TPanel.Create(FTopPanel);
    FImageFrame.Parent := FTopPanel;
    FImageFrame.BevelWidth := 2;
    FImageFrame.BevelOuter := bvLowered;

    //Create image
    FHeaderImage := TImage.Create(FImageFrame);
    FHeaderImage.Parent := FImageFrame;
    FHeaderImage.Align := alClient;
    FHeaderImage.Picture.LoadFromFile(FImage);

    FImageFrame.Width := FHeaderImage.Picture.Width + FImageFrame.BevelWidth * 2;
    FImageFrame.Height := FHeaderImage.Picture.Height + FImageFrame.BevelWidth * 2;
    FTopPanel.Height := FImageFrame.Height + 10;

    FImageFrame.Top := 5;
    FImageFrame.Left := (FTopPanel.ClientWidth - FImageFrame.Width) div 2;
  end;

  //Create bottom panel
  FBottomPanel := TPanel.Create(FForm);
  FBottomPanel.Parent := FForm;
  FBottomPanel.BevelOuter := bvNone;
  FBottomPanel.Align := alBottom;
  FBottomPanel.Height := 35;
  FBottomPanel.Width := FForm.Width;
  FBottomPanel.BorderWidth := 5;

  //Create center panel
  FCenterPanel := TPanel.Create(FForm);
  FCenterPanel.Parent := FForm;
  FCenterPanel.BevelOuter := bvNone;
  FCenterPanel.Left := 20;
  FCenterPanel.Width := FForm.ClientWidth - 40;
  FCenterPanel.Top := FTopPanel.Height;
  FCenterPanel.Height := FForm.ClientHeight - FBottomPanel.Height - FTopPanel.Height;

  FCenterPanel.Anchors := [akTop, akLeft, akRight, akBottom];
  FCenterPanel.BorderWidth := 5;

  //Create buttons for bottom panel

  //"Cancel" Button
  FCancelBtn := TButton.Create(FBottomPanel);
  FCancelBtn.Parent := FBottomPanel;
  FCancelBtn.Caption := MsgSetupBtnCancel;
  FCancelBtn.Width := 75;
  FCancelBtn.Align := alRight;
  FCancelBtn.Cancel := true;
  FCancelBtn.ModalResult := mrCancel;

  //Spacer
  with TPanel.Create(FBottomPanel) do
  begin
    Parent := FBottomPanel;
    BevelOuter := bvNone;
    Width := 10;
    Align := alRight;
    Left := FBottomPanel.ClientWidth - FCancelBtn.Width;
  end;

  //"Ok" Button
  FOkBtn := TButton.Create(FBottomPanel);
  FOkBtn.Parent := FBottomPanel;
  FOkBtn.Caption := MsgSetupBtnOk;
  FOkBtn.Width := 75;
  FOkBtn.Align := alRight;
  FOkBtn.Default := true;
  FOkBtn.ModalResult := mrOk;
  FOkBtn.TabOrder := 0;
end;

procedure TAdSetup.FreeSections;
var
  i: Integer;
begin
  for i := 0 to FSetupSections.Count - 1 do
  begin
    TAdSetupSection(FSetupSections[i]).Free;
  end;
  FSetupSections.Clear;

  for i := FCenterPanel.ComponentCount - 1 downto 0 do
  begin
    if FCenterPanel.Components[i] is TPanel then
      FCenterPanel.Components[i].Free;
  end;
end;

function TAdSetup.GetPlugins: boolean;
begin
  if dlgPlugin in FSections then
  begin
    FPluginIndex := -1;
    FPluginList.Clear;
    
    {$IFDEF WIN32}
      FDllExplorer.GetPlugins(FPluginList, ExtractFilePath(ParamStr(0)), '.dll');
    {$ELSE}
      FDllExplorer.GetPlugins(FPluginList, ExtractFilePath(ParamStr(0)), '.so');
    {$ENDIF}

    result := FPluginList.Count > 0;
  end else
  begin
    result := true;
  end;  
end;

procedure TAdSetup.AddSectionSpacer;
var
  spc: TPanel;
begin
  //Create spacer control
  spc := TPanel.Create(FCenterPanel);
  spc.Parent := FCenterPanel;
  spc.BevelOuter := bvNone;
  spc.Align := alTop;
  spc.Height := 5;
  spc.Top := FCenterPanel.ClientHeight + 1000;
end;

procedure TAdSetup.AutoResize;
var
  i: integer;
  h: integer;
begin
  h := 0;
  for i := 0 to FCenterPanel.ComponentCount - 1 do
  begin
    if FCenterPanel.Components[i] is TControl then
    begin
      h := h + TControl(FCenterPanel.Components[i]).Height;
    end;
  end;

  FForm.ClientHeight := h +
    FBottomPanel.Height +
    FTopPanel.Height +
    FCenterPanel.BorderWidth * 2;
end;

procedure TAdSetup.CreateSectionControls;
begin
  if dlgResolution in FSections then
    CreateResolutionControls;

  if dlgAdvancedOptions in FSections then
    CreateAdvancedControls;  

  if dlgPlugin in FSections then
    CreatePluginControls;
  
  AutoResize;
end;

procedure TAdSetup.CreatePluginControls;
var
  setupsection: TAdSetupSection;
  i: integer;
begin
  setupsection := TAdSetupSection.Create(FCenterPanel, MsgSetupPlugin);

  FPluginCombobox := TCombobox.Create(FForm);
  setupsection.AddControl(FPluginCombobox);

  FPluginCombobox.Style := csDropDownList;
  for i := 0 to FPluginList.Count - 1 do
    FPluginCombobox.Items.Add(FPluginList.Names[i]);

  if FPluginIndex = -1 then
    FPluginCombobox.ItemIndex := 0
  else
    FPluginCombobox.ItemIndex := FPluginIndex;

  FPluginCombobox.OnChange := PluginChange;

  AddSectionSpacer;
  FSetupSections.Add(setupsection);
end;

procedure TAdSetup.GetResolutions;
{$IFDEF WIN32}
var
  res:boolean;
  mode:TDeviceMode;
  i:integer;
  s:string;
  {$IFNDEF FPC}
  mon: TMonitor;
  {$ENDIF}
{$ENDIF}
begin
  FResolutionCombobox.Items.Clear;
  {$IFDEF WIN32}
    i := 0;
    repeat
      mode.dmSize := SizeOf(TDeviceMode);
      res := EnumDisplaySettings(nil,i,mode);
      if res then
      begin
        if (mode.dmPelsWidth >= 640) and (mode.dmBitsPerPel >= 16) then
        begin
          s := inttostr(mode.dmPelsWidth)+'x'+inttostr(mode.dmPelsHeight)+'x'
               +inttostr(mode.dmBitsPerPel);
          if FResolutionCombobox.Items.IndexOf(s) = -1 then
            FResolutionCombobox.Items.Add(s);
        end;
        i := i + 1;
      end;
    until not res;
  {$ELSE}
    FResolutionCombobox.Items.Add('800x600x32');
    FResolutionCombobox.Items.Add('1024x768x32');
    FResolutionCombobox.Items.Add('1280x1024x32');
  {$ENDIF}

  {$IFNDEF FPC}
  mon := Screen.MonitorFromWindow(FForm.Handle);
  s := IntToStr(mon.Width) + 'x' + IntToStr(mon.Height) + 'x32';
  if FResolutionCombobox.Items.IndexOf(s) > -1 then
   FResolutionCombobox.ItemIndex := FResolutionCombobox.Items.IndexOf(s)
  else
    FResolutionCombobox.ItemIndex := FResolutionCombobox.Items.Count - 1;
  {$ENDIF}
end;

procedure TAdSetup.CreateResolutionControls;
var
  setupsection: TAdSetupSection;
begin
  setupsection := TAdSetupSection.Create(FCenterPanel, MsgSetupResolution);

  //Add resolution drop down
  FResolutionCombobox := TCombobox.Create(FForm);
  setupsection.AddControl(FResolutionCombobox);

  FResolutionCombobox.Style := csDropDownList;

  //Load items
  GetResolutions;

  FResolutionCombobox.OnChange := ResolutionChange;

  //Add fullscreen checkbox
  FFullscreenCheckBox := TCheckBox.Create(FForm);
  setupsection.AddControl(FFullscreenCheckBox);

  FFullscreenCheckBox.Caption := MsgSetupFullscreen;
  FFullscreenCheckBox.OnClick := FullscreenClick;

  //Add current desktop resolution checkbox
  FDesktopResolutionCheckBox := TCheckBox.Create(FForm);
  setupsection.AddControl(FDesktopResolutionCheckBox);

  FDesktopResolutionCheckBox.Caption := MsgSetupCurrentDesktopResolution;
  FDesktopResolutionCheckBox.OnClick := DesktopResolutionClick;

  AddSectionSpacer;
  FSetupSections.Add(setupsection);
end;

procedure TAdSetup.CreateAdvancedControls;

  //Returns a existing section with the specified section. If the section
  //does not exist, a new one is created. 
  function GetSection(ACaption: string): TAdSetupSection;
  var
    i: integer;
  begin
    result := nil;
    for i := 0 to FSetupSections.Count - 1 do
    begin
      if (TAdSetupSection(FSetupSections[i]).Caption = ACaption) then
      begin
        result := TAdSetupSection(FSetupSections[i]);
        break;
      end;
    end;

    if result = nil then
    begin
      result := TAdSetupSection.Create(FCenterPanel, ACaption);
      FSetupSections.Add(result);
      AddSectionSpacer;
    end;
  end;

var
  i: Integer;
  sect: TAdSetupSection;
  chkcntr: TCheckBox;
begin
  for i := 0 to FDraw.Properties.Count - 1 do
  begin

    //The resolution group is a fixed part of the setup dialog
    if FDraw.Properties[i].PropGroup = 'Resolution' then
      continue;

    //Don't display the read only properties
    if FDraw.Properties[i].PropType = ptReadOnly then
      continue;
      
    sect := GetSection(FDraw.Properties[i].PropGroup);
    case FDraw.Properties[i].PropType of
      //Add a new checkbox control to the section
      ptBoolean:
      begin
        chkcntr := TCheckBox.Create(FForm);
        sect.AddControl(chkcntr);
        chkcntr.Caption := FDraw.Properties[i].PropViewName;
        chkcntr.Tag := (i + 1);
        chkcntr.OnClick := BooleanPropChange;
      end;
    end;      
  end;
end;

procedure TAdSetup.DoRebuildControls(Sender: TObject);
begin
  //Make Form invisible to hide the unaesthetic control rebuild process
  FForm.Hide;

  //Store current settings
  StoreSettings;

  FUpdating := true;

  try
    FreeSections;
    CreateSectionControls;

    //Relead current settings
    LoadSettings;
  finally
    FUpdating  := false;
    FForm.Show;
  end;         
end;

function TAdSetup.SearchTagComponent(ATag: integer;
  AComp: TComponent): TComponent;
var
  i: integer;
begin
  if AComp.Tag = ATag then
    result := AComp
  else
  begin
    result := nil;
    for i := 0 to AComp.ComponentCount - 1 do
    begin
      result := SearchTagComponent(ATag, AComp.Components[i]);
      if result <> nil then break;
    end;
  end;
end;

procedure TAdSetup.SetIni(AValue: TIniFile);
begin
  if (FIni <> nil) and (FOwnIni) then
  begin
    FIni.Free;
    FIni := nil;
  end;

  FOwnIni := false;
  FIni := AValue;
end;

procedure TAdSetup.LoadSettings;
var
  i: integer;
  comp: TComponent;
  ind: integer;
begin
  if FOwnIni then
  begin
    if FIni <> nil then
      FIni.Free;  
    FIni := TIniFile.Create(ExtractFilePath(Application.ExeName) + 'adsettings.ini');
  end;

  if FIni <> nil then
  begin
    //Load resolution settings
    if (dlgResolution in FSections) then
    begin
      FResolutionCombobox.ItemIndex :=
        FResolutionCombobox.Items.IndexOf(
          FIni.ReadString('ad2dsetup', 'resolution', '1024x768x32'));
      if FResolutionCombobox.ItemIndex < 0 then
        FResolutionCombobox.ItemIndex := 0;

      FDesktopResolutionCheckBox.Checked :=
        FIni.ReadBool('ad2dsetup', 'curres', false);
      FFullscreenCheckBox.Checked :=
        FIni.ReadBool('ad2dsetup', 'fullscreen', false);

      with FDraw.Display do
        DisplayMode := dmWindowed;

      ResolutionChange(nil);
      if FFullscreenCheckBox.Checked then
        FullscreenClick(nil);
      if FDesktopResolutionCheckBox.Checked then
        DesktopResolutionClick(nil);
    end;

    //Load plugin settings
    if (dlgPlugin in FSections) then
    begin
      if not FUpdating then
      begin
        ind := -1;
        for i := 0 to FPluginList.Count - 1 do
        begin
          if FPluginList.ValueFromIndex[i] = FIni.ReadString('ad2dsetup', 'plugin', '') then
          begin
            ind := i; break;
          end;
        end;
        if ind <> FPluginIndex then
        begin
          FPluginCombobox.ItemIndex := ind;
        end;
      end;
    end;

    //Load advanced settings
    if (dlgAdvancedOptions in FSections) then
    begin
      for i := 0 to FDraw.Properties.Count - 1 do
      begin
        comp := SearchTagComponent(i + 1, FForm);
        if (comp <> nil) then
        begin
          case FDraw.Properties[i].PropType of
            ptBoolean:
            begin
              if comp is TCheckBox then
              begin
                TCheckBox(comp).Checked :=
                  FIni.ReadBool('ad2dsetup', FDraw.Properties[i].PropName, false);
                //Store property
                BooleanPropChange(comp);
              end;
            end;
          end;
        end;
      end;
    end;    
  end;
end;

procedure TAdSetup.StoreSettings; 
var
  i: integer;
  comp: TComponent;
begin
  if (FIni <> nil) then
  begin
    //Store resolution settings
    if (dlgResolution in FSections) then
    begin
      FIni.WriteString('ad2dsetup', 'resolution',
        FResolutionCombobox.Items[FResolutionCombobox.ItemIndex]);
      FIni.WriteBool('ad2dsetup', 'curres', FDesktopResolutionCheckBox.Checked);
      FIni.WriteBool('ad2dsetup', 'fullscreen', FFullscreenCheckBox.Checked);
    end;

    //Store plugin settings
    if (dlgPlugin in FSections) then
    begin
      FIni.WriteString('ad2dsetup', 'plugin',
        FPluginList.ValueFromIndex[FPluginIndex]);
    end;

    //Store advanced options
    if (dlgAdvancedOptions in FSections) then
    begin
      for i := 0 to FDraw.Properties.Count - 1 do
      begin
        comp := SearchTagComponent(i + 1, FForm);
        if (comp <> nil) then
        begin
          case FDraw.Properties[i].PropType of
            ptBoolean:
            begin
              if comp is TCheckBox then
                FIni.WriteBool('ad2dsetup', FDraw.Properties[i].PropName,
                  TCheckBox(comp).Checked);
            end;
          end;
        end;
      end;
    end;

    //Free current ini and save settings to file
    if (FOwnIni) then
    begin
      FIni.UpdateFile;
      FIni.Free;
      FIni := nil;
    end;
  end;
end;

//Event handling

procedure TAdSetup.DesktopResolutionClick(Sender: TObject);
begin
  FResolutionCombobox.Enabled := not FDesktopResolutionCheckBox.Checked;
  FFullscreenCheckBox.Enabled := not FDesktopResolutionCheckBox.Checked;
  FFullscreenCheckBox.Checked := true;

  if FDesktopResolutionCheckBox.Checked then
  begin
    with FDraw.Display do
      DisplayMode := dmScreenRes;
  end
  else
    FullScreenClick(nil);
end;

procedure TAdSetup.FullscreenClick(Sender: TObject);
begin
  if FFullscreenCheckBox.Checked then
  begin
    with FDraw.Display do
      DisplayMode := dmFullscreen;
  end
  else
  begin
    with FDraw.Display do
      DisplayMode := dmWindowed;
  end
end;

procedure TAdSetup.PluginChange(Sender: TObject);
begin
  if (FPluginCombobox.ItemIndex > -1) and (not FUpdating) then
  begin
    FDraw.DllName := FPluginList.ValueFromIndex[FPluginCombobox.ItemIndex];
    FPluginIndex := FPluginCombobox.ItemIndex;

    FForm.DoRebuildControls;
  end;
end; 

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

procedure TAdSetup.ResolutionChange(Sender: TObject);
var
  strs: TStringArray;
begin
  //Get Resolution
  DevideString(FResolutionCombobox.Items[FResolutionCombobox.ItemIndex],'x',strs);
  with FDraw.Display do
  begin
    Width := StrToInt(strs[0]);
    Height := StrToInt(strs[1]);
    case StrToInt(strs[2]) of
      16: BitDepth := ad16Bit;
      32: BitDepth := ad32Bit;  
    end;
  end;
end;

procedure TAdSetup.BooleanPropChange(Sender: TObject);
begin
  if (Sender is TCheckBox) then
  begin
    FDraw.Properties.SetProp(
      FDraw.Properties[TCheckBox(Sender).Tag - 1].PropName,
      TCheckBox(Sender).Checked);
  end;
end;

{ TAdSetupSection }

constructor TAdSetupSection.Create(AParent: TWinControl; ACaption: string);
begin
  inherited Create;

  //Create groupbox
  FGroupbox := TGroupBox.Create(AParent);
  FGroupbox.Parent := AParent;
  FGroupbox.Align := alTop;
  FGroupbox.Width := AParent.ClientWidth;
  FGroupbox.Caption := ACaption;
  FGroupbox.Top := AParent.ClientHeight + 1000;
  FGroupbox.Height := 16;

  //Create inner spacer panel
  FSpacerPanel := TPanel.Create(FGroupbox);
  FSpacerPanel.Parent := FGroupbox;
  FSpacerPanel.Align := alClient;
  FSpacerPanel.Width := FGroupbox.ClientWidth;
  FSpacerPanel.BevelOuter := bvNone;
  FSpacerPanel.BorderWidth := 5;

  //Create controls list
  FControls := TList.Create;

  //Set caption property
  FCaption := ACaption;
end;

destructor TAdSetupSection.Destroy;
begin
  FGroupbox.Free;
  FControls.Free;
  
  inherited;
end;

function TAdSetupSection.GetHeight: integer;
var
  i: Integer;
begin
  result := 10;
  for i := 0 to FControls.Count - 1 do
  begin
    result := result + TControl(FControls[i]).Height;
  end;
end;

procedure TAdSetupSection.AddControl(AControl: TWinControl);
var
  h: integer;
begin
  h := GetHeight;
  AControl.Parent := FSpacerPanel;
  AControl.Left := 10;
  AControl.Top := h;
  {$IFDEF FPC}
  AControl.Width := FSpacerPanel.ClientWidth - 30;
  {$ELSE}
  AControl.Width := FSpacerPanel.ClientWidth - 20;
  {$ENDIF}

  FGroupbox.Height := h + AControl.Height + 30;
  FControls.Add(AControl);
end;

{ TAdSetupForm }

procedure TAdSetupForm.DoRebuildControls;
begin
  PostMessage(Handle, WM_AD_REBUILDCONTROLS, 0, 0);
end;

procedure TAdSetupForm.MsgRebuildControls(var Message: TMessage);
begin
  if Assigned(FOnRebuildControls) then
    FOnRebuildControls(self);
end;

end.
