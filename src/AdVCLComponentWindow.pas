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
* File: AdVCLComponentWindow.pas
* Comment: Contains a window framework which uses an existing VCL/LCL control for as surface. Only works under windows.
}

{Contains a window framework which uses an existing VCL/LCL control for as surface. Only works under windows.}
unit AdVCLComponentWindow;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  SysUtils, Classes, Types, Forms, Controls, TypInfo, Windows,
  AdTypes, AdEvents, AdWindowFramework,
  AdVCLComponentEventConnector;

type
  {@exclude}
  TAdVCLComponentWindow = class(TAdHandleWindowFramework)
    private
      FBinded:boolean;
      FInitialized:boolean;
      FControl: TWinControl;
      FConnector: TAdVCLComponentEventConnector;
    protected
      function GetClientWidth:integer;override;
      function GetClientHeight:integer;override;
      procedure SetTitle(AValue:string);override;

      function ChangeResolution(width, height, bitdepth: LongWord):boolean;
      procedure SetupDisplay(AProps: TAdDisplayProperties);

      procedure SetupEvents;
      procedure ResetEvents;

      procedure SetCursorVisible(AValue:Boolean);override;
    public
      constructor Create;override;
      destructor Destroy;override;
      
      function BindTo(AObj:Pointer):boolean;override;
      function InitDisplay(AProps:TAdDisplayProperties):boolean;override;

      procedure Run;override;
      procedure Terminate;override;
  end;

implementation

{ TAdVCLComponentWindow }

constructor TAdVCLComponentWindow.Create;
begin
  inherited;
end;

destructor TAdVCLComponentWindow.Destroy;
begin
  ResetEvents;
  inherited;
end;

function TAdVCLComponentWindow.BindTo(AObj: Pointer): boolean;
begin
  FBinded := TObject(AObj) is TWinControl;
  if FBinded then
  begin
    FControl := TWinControl(AObj);
    FHandle := FControl.Handle;
  end;
  result := FBinded;
end;

function TAdVCLComponentWindow.GetClientHeight: integer;
begin
  result := 0;
  if FInitialized then
    result := FControl.ClientHeight;
end;

function TAdVCLComponentWindow.GetClientWidth: integer;
begin
  result := 0;
  if FInitialized then
    result := FControl.ClientWidth;
end;

function TAdVCLComponentWindow.InitDisplay(AProps: TAdDisplayProperties): boolean;
begin
  result := false;
  if FBinded then
  begin
    FInitialized := true;
    SetupEvents;
    if AProps.Mode <> dmDefault then
      SetupDisplay(AProps);
    result := true;
  end;
end;

procedure TAdVCLComponentWindow.Run;
begin
  //Application is already running
end;

procedure TAdVCLComponentWindow.SetCursorVisible(AValue: Boolean);
begin
  inherited;
  if FInitialized then
  begin
    if AValue then
      FControl.Cursor := crDefault
    else
      FControl.Cursor := crNone;
  end;
end;

procedure TAdVCLComponentWindow.SetTitle(AValue: string);
begin
  Application.Title := AValue;
end;

procedure TAdVCLComponentWindow.SetupDisplay(AProps: TAdDisplayProperties);
var
  form: TForm;
begin
  if (FControl is TForm) then
    form := TForm(FControl)
  else
    form := nil;

  if form <> nil then
  begin
    form.Color := 0;
    form.BorderIcons := [biSystemMenu];
    form.Caption := Title;

    if AProps.Mode = dmWindowed then
    begin
      form.BorderStyle := bsSingle;
      form.ClientWidth := AProps.Width;
      form.ClientHeight := AProps.Height;
      form.Top := (Screen.Height - AProps.Height) div 2;
      form.Left := (Screen.Width - AProps.Width) div 2;
    end else
    if (AProps.Mode = dmScreenRes) or (AProps.Mode = dmFullscreen) then
    begin
      if AProps.Mode = dmFullScreen then
      begin
        ChangeResolution(AProps.Width, AProps.Height, ord(AProps.BitDepth));
      end;
      form.BorderStyle := bsNone;
      form.Width := Screen.Width;
      form.Height := Screen.Height;
      form.Top := 0;
      form.Left := 0;
    end;

    //Refresh handle - it may have changed because of the changes in form.BorderStyle
    //and form.BorderIcons.
    //This only applies to Delphi 2005 and earlier
    FHandle := form.Handle;
  end;
end;

function TAdVCLComponentWindow.ChangeResolution(width, height, bitdepth: LongWord):boolean;
var
  DeviceMode: TDevMode;
  i:integer;
begin
  result := false;
  i := 0;
  while EnumDisplaySettings(nil, i, DeviceMode) do
  begin
    with DeviceMode do
    begin
      if (dmPelsWidth = width) and (dmPelsHeight = height) and (dmBitsPerPel = bitdepth) then
      begin
        if ChangeDisplaySettings(DeviceMode, CDS_TEST) = DISP_CHANGE_SUCCESSFUL then
        begin
          result := true;
          ChangeDisplaySettings(DeviceMode, CDS_FULLSCREEN);
        end;
        exit;
     end;
    end;

    i := i + 1;
  end;
end;


procedure TAdVCLComponentWindow.SetupEvents;
begin
  ResetEvents;

  FConnector := TAdVCLComponentEventConnector.Create(FControl, self);
end;

procedure TAdVCLComponentWindow.ResetEvents;
begin
  if FConnector <> nil then
  begin
    FConnector.Free;
    FConnector := nil;
  end;
end;

procedure TAdVCLComponentWindow.Terminate;
begin
  Application.Terminate;
end;

initialization
  RegisterWindowFramework(TAdVCLComponentWindow);

end.
