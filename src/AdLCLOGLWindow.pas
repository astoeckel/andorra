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
* File: AdLCLOGLComponentWindow.pas
* Comment: Contains a window framework class for Lazarus which uses TOpenGLControl to create an OpenGL context on a new, LCL created window
}

{Contains a window framework class for Lazarus which uses TOpenGLControl to create an OpenGL context on a new, LCL created window}
unit AdLCLOGLWindow;

{$MODE DELPHI}

interface

uses
  Interfaces, Classes, Controls, Forms, OpenGLContext,
  AdEvents, AdTypes, AdWindowFramework;

type

  { TAdLCLOGLWindow }

  TAdLCLOGLWindow = class(TAdGLContextGeneratingWindowFramework)
    private
      FForm:TForm;
      FContext:TOpenGLControl;
      FBinded:boolean;
      FInitialized:boolean;
      
      FMouseX, FMouseY : integer;

      FIdle : TIdleEvent;

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
      
      procedure Swap;override;
  end;

implementation

{ TAdLCLOGLWindow }

constructor TAdLCLOGLWindow.Create;
begin
  inherited;
end;

destructor TAdLCLOGLWindow.Destroy;
begin
  if FContext <> nil then
    FContext.Free;
  if FForm <> nil then
    FForm.Free;
  inherited;
end;

function TAdLCLOGLWindow.GetClientHeight: integer;
begin
  result := 0;
  if FForm <> nil then
    result := FForm.ClientHeight;
end;

function TAdLCLOGLWindow.GetClientWidth: integer;
begin
  result := 0;
  if FForm <> nil then
    result := FForm.ClientWidth;
end;

function TAdLCLOGLWindow.BindTo(AObj: Pointer): boolean;
begin
  FBinded := AObj = nil;
  result := FBinded;
end;

function TAdLCLOGLWindow.InitDisplay(AProps:TAdDisplayProperties): boolean;
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
        //
      end;
      FForm.BorderStyle := bsNone;
      FForm.Width := Screen.Width;
      FForm.Height := Screen.Height;
      FForm.Top := 0;
      FForm.Left := 0;
    end else exit;
    
    FContext := TOpenGLControl.Create(FForm);
    FContext.Parent := FForm;
    FContext.Align := alClient;
    FContext.MakeCurrent;

    result := true;
    FInitialized := true;

    SetCursorVisible(true);

    SetupEvents;
  end;
end;

procedure TAdLCLOGLWindow.Run;
begin
  if FInitialized then
  begin
    Application.Run;
  end;
end;

procedure TAdLCLOGLWindow.Terminate;
begin
  if FForm <> nil then
  begin
    FForm.Close;
  end;
end;

procedure TAdLCLOGLWindow.Swap;
begin
  if FInitialized then
  begin
    FContext.SwapBuffers;
  end;
end;

procedure TAdLCLOGLWindow.SetCursorVisible(AValue: Boolean);
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

procedure TAdLCLOGLWindow.SetTitle(AValue: string);
begin
  inherited;
  FForm.Caption := AValue;
  Application.Title := AValue;
end;

procedure TAdLCLOGLWindow.SetupEvents;
begin
  //Store old event handlers
  FIdle := Application.OnIdle;

  //Set new event handlers
  Application.OnIdle := Idle;

  FContext.OnPaint := Paint;
  FContext.OnResize := Resize;
  FForm.OnDeactivate := Deactivate;
  FForm.OnActivate := Activate;
  FForm.OnClose := Close;

  FContext.OnClick := Click;
  FContext.OnDblClick := DblClick;
  FContext.OnMouseMove := MouseMove;
  FContext.OnMouseDown := MouseDown;
  FContext.OnMouseUp := MouseUp;
  FContext.OnMouseWheel := MouseWheel;

  FContext.OnKeyDown := KeyDown;
  FContext.OnKeyPress := KeyPress;
  FContext.OnKeyUp := KeyUp;
end;

function TAdLCLOGLWindow.ConvertButton(Button: TMouseButton): TAdMouseButton;
begin
  result := TAdMouseButton(ord(Button));
end;

function TAdLCLOGLWindow.ConvertShift(Shift: TShiftState): TAdShiftState;
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

{TAdLCLOGLWindow Event handlers}

procedure TAdLCLOGLWindow.Idle(Sender: TObject; var Done: boolean);
begin
  if Assigned(Events.OnIdle) then
    Events.OnIdle(Self, Done);
  if Assigned(FIdle) then
    FIdle(Self, Done);
end;

procedure TAdLCLOGLWindow.Deactivate(Sender: TObject);
begin
  if Assigned(Events.OnDeactivate) then
    Events.OnDeactivate(self);
end;

procedure TAdLCLOGLWindow.Activate(Sender: TObject);
begin
  if Assigned(Events.OnActivate) then
    Events.OnActivate(self);
end;

procedure TAdLCLOGLWindow.Click(Sender: TObject);
begin
  if Assigned(Events.OnClick) then
    Events.OnClick(self, FMouseX, FMouseY);
end;

procedure TAdLCLOGLWindow.DblClick(Sender: TObject);
begin
  if Assigned(Events.OnClick) then
    Events.OnDblClick(self, FMouseX, FMouseY);
end;

procedure TAdLCLOGLWindow.Close(Sender: TObject; var Action: TCloseAction);
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

procedure TAdLCLOGLWindow.KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Assigned(Events.OnKeyDown) then
    Events.OnKeyDown(self, Key, ConvertShift(Shift));
end;

procedure TAdLCLOGLWindow.KeyPress(Sender: TObject; var Key: Char);
begin
  if Assigned(Events.OnKeyPress) then
    Events.OnKeyPress(self, Key);
end;

procedure TAdLCLOGLWindow.KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Assigned(Events.OnKeyUp) then
    Events.OnKeyUp(self, Key, ConvertShift(Shift));
end;

procedure TAdLCLOGLWindow.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if Assigned(Events.OnMouseDown) then
    Events.OnMouseDown(self, ConvertButton(Button), ConvertShift(Shift), X, Y);
end;

procedure TAdLCLOGLWindow.MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: integer);
begin
  FMouseX := X; FMouseY := Y;
  if Assigned(Events.OnMouseMove) then
    Events.OnMouseMove(self, ConvertShift(Shift), X, Y);
end;

procedure TAdLCLOGLWindow.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if Assigned(Events.OnMouseUp) then
    Events.OnMouseUp(self, ConvertButton(Button), ConvertShift(Shift), X, Y);
end;

procedure TAdLCLOGLWindow.MouseWheel(Sender: TObject; Shift: TShiftState;
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

procedure TAdLCLOGLWindow.Paint(Sender: TObject);
begin
  if Assigned(Events.OnPaint) then
    Events.OnPaint(self);
end;

procedure TAdLCLOGLWindow.Resize(Sender: TObject);
begin
  if Assigned(Events.OnResize) then
    Events.OnResize(self);
end;

initialization
  RegisterWindowFramework(TAdLCLOGLWindow);

end.

