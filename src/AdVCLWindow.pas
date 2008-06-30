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
* File: AdVCLWindow.pas
* Comment: Contains a window framework which creates a window using the VCL/LCL.
}

{Contains a window framework which creates a window using the VCL/LCL.}
unit AdVCLWindow;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  {$IFDEF FPC}Interfaces, {$ENDIF}
  Classes, Controls, Windows, Forms,
  AdEvents, AdTypes, AdWindowFramework, AdVCLComponentEventConnector;

type
  {@exclude}
  TAdVCLWindow = class(TAdHandleWindowFrameWork)
    private
      FForm:TForm;
      FBinded:boolean;
      FInitialized:boolean;
      FConnector: TAdVCLComponentEventConnector;

      FIdle : TIdleEvent;

      function ChangeResolution(width, height, bitdepth : LongWord):boolean;

      procedure Idle(Sender: TObject; var Done: boolean);
    protected
      procedure SetTitle(AValue:string);override;

      function GetClientWidth:integer;override;
      function GetClientHeight:integer;override;

      procedure SetCursorVisible(AValue:Boolean);override;

      procedure SetupEvents;
      procedure ResetEvents;
    public
      constructor Create;override;
      destructor Destroy;override;
      
      function BindTo(AObj:Pointer):boolean;override;
      function InitDisplay(AProps:TAdDisplayProperties):boolean;override;

      procedure Run;override;
      procedure Terminate;override;
  end;

implementation

{ TAdVCLWindow }

constructor TAdVCLWindow.Create;
begin
  inherited;
end;

destructor TAdVCLWindow.Destroy;
begin
  ResetEvents;
  
  if FForm <> nil then
    FForm.Free;
  inherited;
end;

function TAdVCLWindow.GetClientHeight: integer;
begin
  result := 0;
  if FForm <> nil then
    result := FForm.ClientHeight;
end;

function TAdVCLWindow.GetClientWidth: integer;
begin
  result := 0;
  if FForm <> nil then
    result := FForm.ClientWidth;
end;

function TAdVCLWindow.BindTo(AObj: Pointer): boolean;
begin
  FBinded := AObj = nil;
  result := FBinded;
end;

function TAdVCLWindow.ChangeResolution(width, height, bitdepth: LongWord):boolean;
var
  DeviceMode: TDeviceModeA;
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

function TAdVCLWindow.InitDisplay(AProps:TAdDisplayProperties): boolean;
begin
  result := false;
  if (FBinded) and not (FInitialized) then
  begin
    Application.Initialize;

    Application.CreateForm(TForm, FForm);

    FForm.Color := 0;
    FForm.BorderIcons := [biSystemMenu];
    FForm.Caption := Title;

    if (AProps.Mode = dmWindowed) or (AProps.Mode = dmDefault) then
    begin
      FForm.BorderStyle := bsSingle;
      FForm.ClientWidth := AProps.Width;
      FForm.ClientHeight := AProps.Height;
      FForm.Position := poScreenCenter;
    end else
    if (AProps.Mode = dmScreenRes) or (AProps.Mode = dmFullscreen) then
    begin
      FForm.BorderStyle := bsNone;
      FForm.Width := Screen.Width;
      FForm.Height := Screen.Height;
      FForm.Top := 0;
      FForm.Left := 0;

      if AProps.Mode = dmFullScreen then
      begin
        FForm.Width := AProps.Width;
        FForm.Height := AProps.Height;
        if not ChangeResolution(AProps.Width, AProps.Height, ord(AProps.BitDepth)) then
        begin
          result := false;
          exit;
        end;
      end;
    end else exit;

    result := true;
    FInitialized := true;
    FHandle := FForm.Handle;

    SetCursorVisible(true);

    SetupEvents;
  end;
end;

procedure TAdVCLWindow.Run;
begin
  if FInitialized then
  begin
    Application.Run;
  end;
end;

procedure TAdVCLWindow.Terminate;
begin
  if FForm <> nil then
  begin
    FForm.Close;
  end;
end;

procedure TAdVCLWindow.SetCursorVisible(AValue: Boolean);
begin
  inherited;
  if FInitialized then
  begin
    if AValue then
      FForm.Cursor := crDefault
    else
      FForm.Cursor := crNone; 
  end;
end;

procedure TAdVCLWindow.SetTitle(AValue: string);
begin
  inherited;
  FForm.Caption := AValue;
  Application.Title := AValue;
end;

procedure TAdVCLWindow.Idle(Sender: TObject; var Done: boolean);
begin
  if Assigned(Events.OnIdle) then
    Events.OnIdle(self, done);
end;          

procedure TAdVCLWindow.ResetEvents;
begin
  if FConnector <> nil then
  begin
    FConnector.Free;
    FConnector := nil;
  end;
end;

procedure TAdVCLWindow.SetupEvents;
begin
  ResetEvents;

  FConnector := TAdVCLComponentEventConnector.Create(FForm, self);

  //Store old event handler
  FIdle := Application.OnIdle;

  //Set new event handler
  Application.OnIdle := Idle;
end;

initialization
  RegisterWindowFramework(TAdVCLWindow);

end.
