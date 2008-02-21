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
  AdEvents, AdTypes, AdWindowFramework;

type
  {@exclude}
  TAdVCLWindow = class(TAdHandleWindowFrameWork)
    private
      FForm:TForm;
      FBinded:boolean;
      FInitialized:boolean;

      FIdle : TIdleEvent;

      function ChangeResolution(width, height, bitdepth : LongWord):boolean;
      function ConvertShift(Shift: TShiftState):TAdShiftState;
      function ConvertButton(Button : TMouseButton):TAdMouseButton;

      procedure SetupEvents;

      {Event handle methods}

      procedure Idle(Sender:TObject; var Done:boolean);
      procedure Paint(Sender:TObject);
      procedure Resize(Sender:TObject);
      procedure Activate(Sender:TObject);
      procedure Deactivate(Sender:TObject);
      procedure Close(Sender: TObject; var Action: TCloseAction);

      procedure Click(Sender: TObject);
      procedure DblClick(Sender: TObject);
      procedure MouseDown(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; X, Y: integer);
      procedure MouseUp(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; X, Y: integer);
      procedure MouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
      procedure MouseWheel(Sender: TObject; Shift: TShiftState;
        WheelDelta: integer; MousePos:TPoint; var Handled:boolean);

      procedure KeyDown(Sender: TObject; var Key: Word; Shift:TShiftState);
      procedure KeyUp(Sender: TObject; var Key: Word; Shift:TShiftState);
      procedure KeyPress(Sender: TObject; var Key: Char);
      
    protected
      procedure SetTitle(AValue:string);override;

      function GetClientWidth:integer;override;
      function GetClientHeight:integer;override;

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

{ TAdVCLWindow }

constructor TAdVCLWindow.Create;
begin
  inherited;
end;

destructor TAdVCLWindow.Destroy;
begin
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
        end
        else
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

    if AProps.Mode = dmWindowed then
    begin
      FForm.BorderStyle := bsSingle;
      FForm.Width := AProps.Width;
      FForm.Height := AProps.Height;
      FForm.Position := poScreenCenter;
    end else
    if (AProps.Mode = dmScreenRes) or (AProps.Mode = dmFullscreen) then
    begin
      if AProps.Mode = dmFullScreen then
      begin
        if not ChangeResolution(AProps.Width, AProps.Height, AProps.BitDepth) then
        begin
          result := false;
          exit;
        end;
      end;
      FForm.BorderStyle := bsNone;
      FForm.Width := Screen.Width;
      FForm.Height := Screen.Height;
      FForm.Top := 0;
      FForm.Left := 0;
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

procedure TAdVCLWindow.SetupEvents;
begin
  //Store old event handlers
  FIdle := Application.OnIdle;

  //Set new event handlers
  Application.OnIdle := Idle;

  FForm.OnPaint := Paint;
  FForm.OnResize := Resize;
  FForm.OnDeactivate := Deactivate;
  FForm.OnActivate := Activate;

  FForm.OnClick := Click;
  FForm.OnDblClick := DblClick;
  FForm.OnClose := Close;
  FForm.OnMouseMove := MouseMove;
  FForm.OnMouseDown := MouseDown;
  FForm.OnMouseUp := MouseUp;
  FForm.OnMouseWheel := MouseWheel;

  FForm.OnKeyDown := KeyDown;
  FForm.OnKeyPress := KeyPress;
  FForm.OnKeyUp := KeyUp;
end;

function TAdVCLWindow.ConvertButton(Button: TMouseButton): TAdMouseButton;
begin
  result := TAdMouseButton(ord(Button));
end;

function TAdVCLWindow.ConvertShift(Shift: TShiftState): TAdShiftState;
begin
  result := [];
  if ssShift in Shift then result := result + [asShift];
  if ssCtrl in Shift then result := result + [asCtrl];
  if ssAlt in Shift then result := result + [asAlt];
  if ssDouble in Shift then result := result + [asDouble];
  if ssLeft in Shift then result := result + [asLeft];
  if ssRight in Shift then result := result + [asRight];
  if ssMiddle in Shift then result := result + [asMiddle];
end;

{TAdVCLWindow Event handlers}

procedure TAdVCLWindow.Idle(Sender: TObject; var Done: boolean);
begin
  if Assigned(Events.OnIdle) then
    Events.OnIdle(Self, Done);
  if Assigned(FIdle) then
    FIdle(Self, Done);
end;

procedure TAdVCLWindow.Deactivate(Sender: TObject);
begin
  if Assigned(Events.OnDeactivate) then
    Events.OnDeactivate(self);
end;

procedure TAdVCLWindow.Activate(Sender: TObject);
begin
  if Assigned(Events.OnActivate) then
    Events.OnActivate(self);
end;

procedure TAdVCLWindow.Click(Sender: TObject);
var
  p:TPoint;
begin
  GetCursorPos(p);
  p := FForm.ScreenToClient(p);
  if Assigned(Events.OnClick) then
    Events.OnClick(self, p.X, p.Y);
end;

procedure TAdVCLWindow.DblClick(Sender: TObject);
var
  p:TPoint;
begin
  GetCursorPos(p);
  p := FForm.ScreenToClient(p);
  if Assigned(Events.OnClick) then
    Events.OnDblClick(self, p.X, p.Y);
end;

procedure TAdVCLWindow.Close(Sender: TObject; var Action: TCloseAction);
var
  canclose:boolean;
begin
  if Assigned(Events.OnClose) then
  begin
    canclose := false;
    Events.OnClose(self, canclose);
    if canclose then
      Action := caFree
    else
      Action := caNone;
  end;
end;

procedure TAdVCLWindow.KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Assigned(Events.OnKeyDown) then
    Events.OnKeyDown(self, Key, ConvertShift(Shift));
end;

procedure TAdVCLWindow.KeyPress(Sender: TObject; var Key: Char);
begin
  if Assigned(Events.OnKeyPress) then
    Events.OnKeyPress(self, Key);
end;

procedure TAdVCLWindow.KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Assigned(Events.OnKeyUp) then
    Events.OnKeyUp(self, Key, ConvertShift(Shift));
end;

procedure TAdVCLWindow.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if Assigned(Events.OnMouseDown) then
    Events.OnMouseDown(self, ConvertButton(Button), ConvertShift(Shift), X, Y);
end;

procedure TAdVCLWindow.MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: integer);
begin
  if Assigned(Events.OnMouseMove) then
    Events.OnMouseMove(self, ConvertShift(Shift), X, Y);
end;

procedure TAdVCLWindow.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if Assigned(Events.OnMouseUp) then
    Events.OnMouseUp(self, ConvertButton(Button), ConvertShift(Shift), X, Y);
end;

procedure TAdVCLWindow.MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: integer; MousePos:TPoint; var Handled:boolean);
begin
  Handled := false;
  if Assigned(Events.OnMouseWheel) then
  begin
    Handled := true;
    Events.OnMouseWheel(self, ConvertShift(Shift), WheelDelta, MousePos.X,
      MousePos.Y);
  end;
end;

procedure TAdVCLWindow.Paint(Sender: TObject);
begin
  if Assigned(Events.OnPaint) then
    Events.OnPaint(self);
end;

procedure TAdVCLWindow.Resize(Sender: TObject);
begin
  if Assigned(Events.OnResize) then
    Events.OnResize(self);
end;

initialization
  RegisterWindowFramework(TAdVCLWindow);

end.
