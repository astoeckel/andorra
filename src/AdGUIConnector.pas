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

{Contains a helper class which connects a TAdWindowFramework to a TAdComponent.}
unit AdGUIConnector;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  AdWindowFramework, AdEvents, AdGUI;

type
  {Connects a TAdWindowFramework to a TAdComponent.}
  TAdGUIConnector = class
    private
      FParent:TAdComponent;
      FWindow:TAdWindowFramework;
      FConnected:boolean;
      FOldClick:TAdClickEvent;
      FOldDblClick:TAdClickEvent;
      FOldMouseDown:TAdMouseEvent;
      FOldMouseUp:TAdMouseEvent;
      FOldMouseMove:TAdMouseMoveEvent;
      FOldKeyPress:TAdKeyPressEvent;
      FOldKeyDown:TAdKeyEvent;
      FOldKeyUp:TAdKeyEvent;
      FOldMouseWheel:TAdMouseWheelEvent;
    protected
      procedure Click(Sender: TObject; X, Y: Integer);
      procedure DblClick(Sender: TObject; X, Y: Integer);
      procedure MouseDown(Sender: TObject; Button: TAdMouseButton;
        Shift: TAdShiftState; X, Y: Integer);
      procedure MouseUp(Sender: TObject; Button: TAdMouseButton;
        Shift: TAdShiftState; X, Y: Integer);
      procedure MouseMove(Sender: TObject; Shift: TAdShiftState; X, Y: Integer);
      procedure MouseWheel(Sender: TObject; Shift: TAdShiftState;
        WheelDelta: integer; X, Y: integer);
      procedure KeyDown(Sender: TObject; Key: Word; Shift: TAdShiftState);
      procedure KeyPress(Sender: TObject; Key: Char);
      procedure KeyUp(Sender: TObject; Key: Word; Shift: TAdShiftState);
    public
      {Creates a new instance of TAdGUIConnector. AParent specifies the target
      component. Remember to free TAdGUIConnector befor destroying the component.}
      constructor Create(AParent:TAdComponent);
      {Destroys the instance of TAdGUIConnector and restores the event handlers.}
      destructor Destroy;override;
      {Connects the event handlers to the given window framework. Commonly
      AdDraw.Window is used here. TAdGUIConnector will call the events which
      were earlier connected to the window framework. It is not recommended
      to override the event handlers while TAdGUIConnector is connected to
      the window.}
      procedure ConnectEventHandlers(Window:TAdWindowFramework);
      {Restores the event connections made.}
      procedure RestoreEventHandlers;
      {The parent gui component.}
      property Parent:TAdComponent read FParent;
      {The window framework the events are taken from.}
      property Window:TAdWindowFramework read FWindow;
      {Specifies whether TAdGUIConnector is currently connected to a window framework.}
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

procedure TAdGUIConnector.ConnectEventHandlers(Window: TAdWindowFramework);
begin
  if FConnected then
  begin
    RestoreEventHandlers;
  end;

  FWindow := Window;

  FOldDblClick := FWindow.Events.OnDblClick;
  FOldClick := FWindow.Events.OnClick;
  FOldKeyDown := FWindow.Events.OnKeyDown;
  FOldKeyUp := FWindow.Events.OnKeyUp;
  FOldKeyPress := FWindow.Events.OnKeyPress;
  FOldMouseMove := FWindow.Events.OnMouseMove;
  FOldMouseDown := FWindow.Events.OnMouseDown;
  FOldMouseUp := FWindow.Events.OnMouseUp;
  FOldMouseWheel := FWindow.Events.OnMouseWheel;

  FWindow.Events.OnDblClick := DblClick;
  FWindow.Events.OnClick := Click;
  FWindow.Events.OnKeyDown := KeyDown;
  FWindow.Events.OnKeyUp := KeyUp;
  FWindow.Events.OnKeyPress := KeyPress;
  FWindow.Events.OnMouseMove := MouseMove;
  FWindow.Events.OnMouseDown := MouseDown;
  FWindow.Events.OnMouseUp := MouseUp;
  FWindow.Events.OnMouseWheel := MouseWheel;

  FConnected := true;
end;

procedure TAdGUIConnector.RestoreEventHandlers;
begin
  if FConnected then
  begin
    FConnected := false;

    FWindow.Events.OnDblClick := FOldDblClick;
    FWindow.Events.OnClick := FOldClick;
    FWindow.Events.OnKeyDown := FOldKeyDown;
    FWindow.Events.OnKeyUp := FOldKeyUp;
    FWindow.Events.OnKeyPress := FOldKeyPress;
    FWindow.Events.OnMouseMove := FOldMouseMove;
    FWindow.Events.OnMouseDown := FOldMouseDown;
    FWindow.Events.OnMouseUp := FOldMouseUp;
    FWindow.Events.OnMouseWheel := FOldMouseWheel;
  end;
end;

procedure TAdGUIConnector.DblClick(Sender: TObject; X, Y: Integer);
begin
  FParent.DblClick(X, Y);
  if Assigned(FOldDblClick) then
    FOldDblClick(Sender, X, Y);
end;

procedure TAdGUIConnector.Click(Sender: TObject; X, Y: Integer);
begin
  FParent.Click(X, Y);
  if Assigned(FOldClick) then
    FOldClick(Sender, X, Y);
end;

procedure TAdGUIConnector.KeyDown(Sender: TObject; Key: Word;
  Shift: TAdShiftState);
begin
  FParent.KeyDown(Key, Shift);
  if Assigned(FOldKeyDown) then
    FOldKeyDown(Sender, Key, Shift);
end;

procedure TAdGUIConnector.KeyPress(Sender: TObject; Key: Char);
begin
  FParent.KeyPress(Key);
  if Assigned(FOldKeyPress) then
    FOldKeyPress(Sender, Key);
end;

procedure TAdGUIConnector.KeyUp(Sender: TObject; Key: Word;
  Shift: TAdShiftState);
begin
  FParent.KeyUp(Key, Shift);
  if Assigned(FOldKeyUp) then
    FOldKeyUp(Sender, Key, Shift);
end;

procedure TAdGUIConnector.MouseDown(Sender: TObject; Button: TAdMouseButton;
  Shift: TAdShiftState; X, Y: Integer);
begin
  FParent.MouseDown(Button, Shift, X, Y);
  if Assigned(FOldMouseDown) then
    FOldMouseDown(Sender, Button, Shift, X, Y);
end;

procedure TAdGUIConnector.MouseMove(Sender: TObject; Shift: TAdShiftState; X,
  Y: Integer);
begin
  FParent.MouseMove(Shift, X, Y);
  if Assigned(FOldMouseMove) then
    FOldMouseMove(Sender, Shift, X, Y);
end;

procedure TAdGUIConnector.MouseUp(Sender: TObject; Button: TAdMouseButton;
  Shift: TAdShiftState; X, Y: Integer);
begin
  FParent.MouseUp(Button, Shift, X, Y);
  if Assigned(FOldMouseUp) then
    FOldMouseUp(Sender, Button, Shift, X, Y);
end;

procedure TAdGUIConnector.MouseWheel(Sender: TObject; Shift: TAdShiftState;
  WheelDelta: Integer; X, Y: Integer);
begin
  FParent.MouseWheel(Shift, WheelDelta, X, Y);
  if Assigned(FOldMouseWheel) then
    FOldMouseWheel(Sender, Shift, WheelDelta, X, Y);
end;

end.
