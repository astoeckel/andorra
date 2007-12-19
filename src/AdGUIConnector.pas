{
* This program is licensed under the Common Public License (CPL) Version 1.0
* You should have recieved a copy of the license with this file.
* If not, see http://www.opensource.org/licenses/cpl1.0.txt for more
* informations.
*
* Inspite of the incompatibility between the Common Public License (CPL) and
* the GNU General Public License (GPL) you're allowed to use this program
* under the GPL.
* You also should have recieved a copy of this license with this file.
* If not, see http://www.gnu.org/licenses/gpl.txt for more informations.
*
* Project: Andorra 2D
* Author:  Andreas Stoeckel
* File: AdGUIConnector.pas
* Comment: Connects the event handlers of a TAdComponent to the event-handlers of a TForm.
}

unit AdGUIConnector;

interface

uses Forms, Controls, Classes, AdGUI, AdTypes, Types;

type
  TAdGUIConnector = class
    private
      FParent:TAdComponent;
      FForm:TForm;
      FConnected:boolean;
      FOldClick:TNotifyEvent;
      FOldDblClick:TNotifyEvent;
      FOldMouseDown:TMouseEvent;
      FOldMouseUp:TMouseEvent;
      FOldMouseMove:TMouseMoveEvent;
      FOldKeyPress:TKeyPressEvent;
      FOldKeyDown:TKeyEvent;
      FOldKeyUp:TKeyEvent;
      FOldMouseWheel:TMouseWheelEvent;
    protected
      procedure Click(Sender: TObject);
      procedure DblClick(Sender: TObject);
      procedure MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      procedure MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      procedure MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
      procedure MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
      procedure KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
      procedure KeyPress(Sender: TObject; var Key: Char);
      procedure KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    public
      constructor Create(AParent:TAdComponent);
      destructor Destroy;override;
      procedure ConnectEventHandlers(Form:TForm);
      procedure RestoreEventHandlers;
      property Parent:TAdComponent read FParent;
      property Form:TForm read FForm;
      property Connected:boolean read FConnected;
  end;

implementation

{ TAdGUIConnector }

constructor TAdGUIConnector.Create(AParent: TAdComponent);
begin
  inherited Create;
  FParent := AParent;
  FConnected := false;
end;

destructor TAdGUIConnector.Destroy;
begin
  RestoreEventHandlers;
  inherited;
end;

procedure TAdGUIConnector.ConnectEventHandlers(Form: TForm);
begin
  if FConnected then
  begin
    RestoreEventHandlers;
  end;

  FForm := Form;

  FOldDblClick := FForm.OnDblClick;
  FOldClick := FForm.OnClick;
  FOldKeyDown := FForm.OnKeyDown;
  FOldKeyUp := FForm.OnKeyUp;
  FOldKeyPress := FForm.OnKeyPress;
  FOldMouseMove := FForm.OnMouseMove;
  FOldMouseDown := FForm.OnMouseDown;
  FOldMouseUp := FForm.OnMouseUp;
  FOldMouseWheel := FForm.OnMouseWheel;

  FForm.OnDblClick := DblClick;
  FForm.OnClick := Click;
  FForm.OnKeyDown := KeyDown;
  FForm.OnKeyUp := KeyUp;
  FForm.OnKeyPress := KeyPress;
  FForm.OnMouseMove := MouseMove;
  FForm.OnMouseDown := MouseDown;
  FForm.OnMouseUp := MouseUp;
  FForm.OnMouseWheel := MouseWheel;

  FConnected := true;
end;

procedure TAdGUIConnector.RestoreEventHandlers;
begin
  if FConnected then
  begin
    FConnected := false;

    FForm.OnDblClick := FOldDblClick;
    FForm.OnClick := FOldClick;
    FForm.OnKeyDown := FOldKeyDown;
    FForm.OnKeyUp := FOldKeyUp;
    FForm.OnKeyPress := FOldKeyPress;
    FForm.OnMouseMove := FOldMouseMove;
    FForm.OnMouseDown := FOldMouseDown;
    FForm.OnMouseUp := FOldMouseUp;
    FForm.OnMouseWheel := FOldMouseWheel;
  end;
end;

procedure TAdGUIConnector.DblClick(Sender: TObject);
var p:TPoint;
begin
  p := Mouse.CursorPos;
  p := Form.ScreenToClient(p);
  FParent.DblClick(p.X,p.Y);
  if Assigned(FOldDblClick) then
  begin
    FOldDblClick(Sender);
  end;
end;

procedure TAdGUIConnector.Click(Sender: TObject);
var p:TPoint;
begin
  p := Mouse.CursorPos;
  p := Form.ScreenToClient(p);
  FParent.Click(p.X,p.Y);
  if Assigned(FOldClick) then
  begin
    FOldClick(Sender);
  end;
end;

procedure TAdGUIConnector.KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  FParent.KeyDown(Key, Shift);
  if Assigned(FOldKeyDown) then
  begin
    FOldKeyDown(Sender, Key, Shift);
  end;
end;

procedure TAdGUIConnector.KeyPress(Sender: TObject; var Key: Char);
begin
  FParent.KeyPress(Key);
  if Assigned(FOldKeyPress) then
  begin
    FOldKeyPress(Sender, Key);
  end;
end;

procedure TAdGUIConnector.KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  FParent.KeyUp(Key, Shift);
  if Assigned(FOldKeyUp) then
  begin
    FOldKeyUp(Sender, Key, Shift);
  end;
end;

procedure TAdGUIConnector.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FParent.MouseDown(Button, Shift, X, Y);
  if Assigned(FOldMouseDown) then
  begin
    FOldMouseDown(Sender, Button, Shift, X, Y);
  end;
end;

procedure TAdGUIConnector.MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  FParent.MouseMove(Shift, X, Y);
  if Assigned(FOldMouseMove) then
  begin
    FOldMouseMove(Sender, Shift, X, Y);
  end;  
end;

procedure TAdGUIConnector.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FParent.MouseUp(Button, Shift, X, Y);
  if Assigned(FOldMouseUp) then
  begin
    FOldMouseUp(Sender, Button, Shift, X, Y);
  end;
end;

procedure TAdGUIConnector.MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  FParent.MouseWheel(Shift, WheelDelta, AdPoint(MousePos.X, MousePos.Y), Handled);
  if Assigned(FOldMouseWheel) then
  begin
    FOldMouseWheel(Sender, Shift, WheelDelta, MousePos, Handled);
  end;
end;

end.
