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
* File: AdVCLComponentEventConnector.pas
* Comment: Contains a class that connects window framework events to a vcl component.
}

{Contains a class that connects window framework events to a vcl/lcl component.}
unit AdVCLComponentEventConnector;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  SysUtils, Classes, Types, Forms, Controls, TypInfo,
  AdEvents, AdWindowFramework;

type
  {Connects the events of a vcl/lcl component to a window framework. This class
   is internally used by a variety of windowframeworks.}
  TAdVCLComponentEventConnector = class
    private
      FControl: TControl;
      FFramework: TAdWindowframework;

      FOldClick: TNotifyEvent;
      FOldDblClick: TNotifyEvent;
      FOldMouseMove: TMouseMoveEvent;
      FOldMouseUp: TMouseEvent;
      FOldMouseDown: TMouseEvent;
      FOldMouseWheel: TMouseWheelEvent;

      FOldKeyDown: TKeyEvent;
      FOldKeyPress: TKeyPressEvent;
      FOldKeyUp: TKeyEvent;

      FOldResize: TNotifyEvent;
      FOldPaint: TNotifyEvent;
      FOldActivate: TNotifyEvent;
      FOldDeactivate: TNotifyEvent;
      FOldClose: TCloseEvent;
      FOldIdle: TIdleEvent;

      FClickComp: TControl;
      FDblClickComp: TControl;
      FMouseMoveComp: TControl;
      FMouseDownComp: TControl;
      FMouseUpComp: TControl;
      FMouseWheelComp: TControl;

      FKeyDownComp: TControl;
      FKeyUpComp: TControl;
      FKeyPressComp: TControl;

      FResizeComp: TControl;
      FPaintComp: TControl;
      FActivateComp: TControl;
      FDeactivateComp: TControl;
      FCloseComp: TControl;

      FMouseX, FMouseY : integer;

      procedure Click(Sender:TObject);
      procedure DblClick(Sender:TObject);
      procedure MouseMove(Sender:TObject; Shift:TShiftState; X, Y:integer);
      procedure MouseDown(Sender:TObject; Button:TMouseButton;
        Shift : TShiftState; X, Y:integer);
      procedure MouseUp(Sender:TObject; Button:TMouseButton;
        Shift : TShiftState; X, Y:integer);
      procedure MouseWheel(Sender:TObject; Shift:TShiftState;
        WheelDelta: integer; MousePos: TPoint; var Handled: Boolean);

      procedure KeyDown(Sender:TObject; var Key:Word; Shift : TShiftState);
      procedure KeyUp(Sender:TObject; var Key:Word; Shift : TShiftState);
      procedure KeyPress(Sender:TObject; var Key:Char);

      procedure Resize(Sender: TObject);
      procedure Paint(Sender: TObject);
      procedure Activate(Sender: TObject);
      procedure Deactivate(Sender: TObject);
      procedure Close(Sender: TObject; var Action: TCloseAction);
      procedure Idle(Sender: TObject; var Done: boolean);
    protected      
      function ConvertButton(Button: TMouseButton): TAdMouseButton;
      function ConvertShift(Shift: TShiftState): TAdShiftState;

      procedure ConnectEvents;
      procedure DisconnectEvents;
    public
      {Connects a control to the window framework.}
      constructor Create(AControl: TControl; AFramework: TAdWindowframework);
      {Disconnects all eventhandlers and destroys this instance of 
       TAdVCLComponentEventConnector.}
      destructor Destroy;override;
  end;

implementation

{ TAdVCLComponentEventConnector }

constructor TAdVCLComponentEventConnector.Create(AControl: TControl;
  AFramework: TAdWindowframework);
begin
  inherited Create;

  FControl := AControl;
  FFramework := AFramework;

  ConnectEvents;
end;

destructor TAdVCLComponentEventConnector.Destroy;
begin
  DisconnectEvents;

  inherited Destroy;
end;

type
  PNotifyEvent = ^TNotifyEvent;
  PMouseMoveEvent = ^TMouseMoveEvent;
  PMouseEvent = ^TMouseEvent;
  PMouseWheelEvent = ^TMouseWheelEvent;
  PKeyEvent = ^TKeyEvent;
  PKeyPressEvent = ^TKeyPressEvent;
  PCloseEvent = ^TCloseEvent;

procedure TAdVCLComponentEventConnector.ConnectEvents;
var
  Control : TControl;
  PropInfo: PPropInfo;
  Method : TMethod;
begin
  Control := FControl;
  while Control <> nil do
  begin
    //Check for click event
    PropInfo := GetPropInfo(Control, 'OnClick');
    if (FClickComp = nil) and (PropInfo <> nil) then
    begin
      Method := GetMethodProp(Control, PropInfo);
      FOldClick := TNotifyEvent(Method);
      PNotifyEvent(@Method.Code)^ := Click; Method.Data := self;
      SetMethodProp(Control, PropInfo, Method);
      FClickComp := Control;
    end;

    //Check for double click event
    PropInfo := GetPropInfo(Control, 'OnDblClick');
    if (FDblClickComp = nil) and (PropInfo <> nil) then
    begin
      Method := GetMethodProp(Control, PropInfo);
      FOldDblClick := TNotifyEvent(Method);
      PNotifyEvent(@Method.Code)^ := DblClick; Method.Data := self;
      SetMethodProp(Control, PropInfo, Method);
      FDblClickComp := Control;
    end;

    //Check for on mouse move event
    PropInfo := GetPropInfo(Control, 'OnMouseMove');
    if (FMouseMoveComp = nil) and (PropInfo <> nil) then
    begin
      Method := GetMethodProp(Control, PropInfo);
      FOldMouseMove := TMouseMoveEvent(Method);
      PMouseMoveEvent(@Method.Code)^ := MouseMove; Method.Data := self;
      SetMethodProp(Control, PropInfo, Method);
      FMouseMoveComp := Control;
    end;

    //Check for on mouse up event
    PropInfo := GetPropInfo(Control, 'OnMouseUp');
    if (FMouseUpComp = nil) and (PropInfo <> nil) then
    begin
      Method := GetMethodProp(Control, PropInfo);
      FOldMouseUp := TMouseEvent(Method);
      PMouseEvent(@Method.Code)^ := MouseUp; Method.Data := self;
      SetMethodProp(Control, PropInfo, Method);
      FMouseUpComp := Control;
    end;

    //Check for on mouse down event
    PropInfo := GetPropInfo(Control, 'OnMouseDown');
    if (FMouseDownComp = nil) and (PropInfo <> nil) then
    begin
      Method := GetMethodProp(Control, PropInfo);
      FOldMouseDown := TMouseEvent(Method);
      PMouseEvent(@Method.Code)^ := MouseDown; Method.Data := self;
      SetMethodProp(Control, PropInfo, Method);
      FMouseDownComp := Control;
    end;

    //Check for on mouse wheel event
    PropInfo := GetPropInfo(Control, 'OnMouseWheel');
    if (FMouseWheelComp = nil) and (PropInfo <> nil) then
    begin
      Method := GetMethodProp(Control, PropInfo);
      FOldMouseWheel := TMouseWheelEvent(Method);
      PMouseWheelEvent(@Method.Code)^ := MouseWheel; Method.Data := self;
      SetMethodProp(Control, PropInfo, Method);
      FMouseWheelComp := Control;
    end;

    //Check for on key down event
    PropInfo := GetPropInfo(Control, 'OnKeyDown');
    if (FKeyDownComp = nil) and (PropInfo <> nil) then
    begin
      Method := GetMethodProp(Control, PropInfo);
      FOldKeyDown := TKeyEvent(Method);
      PKeyEvent(@Method.Code)^ := KeyDown; Method.Data := self;
      SetMethodProp(Control, PropInfo, Method);
      FKeyDownComp := Control;
    end;

    //Check for on key up event
    PropInfo := GetPropInfo(Control, 'OnKeyUp');
    if (FKeyUpComp = nil) and (PropInfo <> nil) then
    begin
      Method := GetMethodProp(Control, PropInfo);
      FOldKeyUp := TKeyEvent(Method);
      PKeyEvent(@Method.Code)^ := KeyUp; Method.Data := self;
      SetMethodProp(Control, PropInfo, Method);
      FKeyUpComp := Control;
    end;

    //Check for on key press event
    PropInfo := GetPropInfo(Control, 'OnKeyPress');
    if (FKeyPressComp = nil) and (PropInfo <> nil) then
    begin
      Method := GetMethodProp(Control, PropInfo);
      FOldKeyPress := TKeyPressEvent(Method);
      PKeyPressEvent(@Method.Code)^ := KeyPress; Method.Data := self;
      SetMethodProp(Control, PropInfo, Method);
      FKeyPressComp := Control;
    end;

    //Check for on resize event
    PropInfo := GetPropInfo(Control, 'OnResize');
    if (FResizeComp = nil) and (PropInfo <> nil) then
    begin
      Method := GetMethodProp(Control, PropInfo);
      FOldResize := TNotifyEvent(Method);
      PNotifyEvent(@Method.Code)^ := Resize; Method.Data := self;
      SetMethodProp(Control, PropInfo, Method);
      FResizeComp := Control;
    end;

    //Check for on paint event
    PropInfo := GetPropInfo(Control, 'OnPaint');
    if (FPaintComp = nil) and (PropInfo <> nil) then
    begin
      Method := GetMethodProp(Control, PropInfo);
      FOldPaint := TNotifyEvent(Method);
      PNotifyEvent(@Method.Code)^ := Paint; Method.Data := self;
      SetMethodProp(Control, PropInfo, Method);
      FPaintComp := Control;
    end;

    //Check for on activate event
    PropInfo := GetPropInfo(Control, 'OnActivate');
    if (FActivateComp = nil) and (PropInfo <> nil) then
    begin
      Method := GetMethodProp(Control, PropInfo);
      FOldActivate := TNotifyEvent(Method);
      PNotifyEvent(@Method.Code)^ := Activate; Method.Data := self;
      SetMethodProp(Control, PropInfo, Method);
      FActivateComp := Control;
    end;

    //Check for on deactivate event
    PropInfo := GetPropInfo(Control, 'OnDeactivate');
    if (FDeactivateComp = nil) and (PropInfo <> nil) then
    begin
      Method := GetMethodProp(Control, PropInfo);
      FOldDeactivate := TNotifyEvent(Method);
      PNotifyEvent(@Method.Code)^ := Deactivate; Method.Data := self;
      SetMethodProp(Control, PropInfo, Method);
      FDeactivateComp := Control;
    end;

    //Check for on close event
    PropInfo := GetPropInfo(Control, 'OnClose');
    if (FDeactivateComp = nil) and (PropInfo <> nil) then
    begin
      Method := GetMethodProp(Control, PropInfo);
      FOldClose := TCloseEvent(Method);
      PCloseEvent(@Method.Code)^ := Close; Method.Data := self;
      SetMethodProp(Control, PropInfo, Method);
      FCloseComp := Control;
    end;
    
    if Control.Parent is TControl then
      Control := Control.Parent
    else
      Control := nil;
  end;

  //Connect idle event
  FOldIdle := Application.OnIdle;
  Application.OnIdle := Idle;
end;

procedure TAdVCLComponentEventConnector.DisconnectEvents;
begin
  if FClickComp <> nil then
    SetMethodProp(FClickComp, 'OnClick', TMethod(FOldClick));
  if FDblClickComp <> nil then
    SetMethodProp(FDblClickComp, 'OnDblClick', TMethod(FOldDblClick));

  if FMouseDownComp <> nil then
    SetMethodProp(FMouseDownComp, 'OnMouseDown', TMethod(FOldMouseDown));
  if FMouseUpComp <> nil then
    SetMethodProp(FMouseUpComp, 'OnMouseUp', TMethod(FOldMouseUp));
  if FMouseMoveComp <> nil then
    SetMethodProp(FMouseMoveComp, 'OnMouseMove', TMethod(FOldMouseMove));
  if FMouseWheelComp <> nil then
    SetMethodProp(FMouseWheelComp, 'OnMouseWheel', TMethod(FOldMouseWheel));

  if FKeyDownComp <> nil then
    SetMethodProp(FKeyDownComp, 'OnKeyDown', TMethod(FOldKeyDown));
  if FKeyUpComp <> nil then
    SetMethodProp(FKeyUpComp, 'OnKeyUp', TMethod(FOldKeyUp));
  if FKeyPressComp <> nil then
    SetMethodProp(FKeyPressComp, 'OnKeyPress', TMethod(FOldKeyPress));

  if FResizeComp <> nil then
    SetMethodProp(FResizeComp, 'OnResize', TMethod(FOldResize));
  if FPaintComp <> nil then
    SetMethodProp(FPaintComp, 'OnPaint', TMethod(FOldPaint));
  if FActivateComp <> nil then
    SetMethodProp(FActivateComp, 'OnActivate', TMethod(FOldActivate));
  if FDeactivateComp <> nil then
    SetMethodProp(FDeactivateComp, 'OnDeactivate', TMethod(FOldDeactivate));
  if FCloseComp <> nil then
    SetMethodProp(FCloseComp, 'OnClose', TMethod(FOldClose));

  //Reset Idle Event
  Application.OnIdle := FOldIdle;
end;

function TAdVCLComponentEventConnector.ConvertButton(Button: TMouseButton): TAdMouseButton;
begin
  result := TAdMouseButton(ord(Button));
end;

function TAdVCLComponentEventConnector.ConvertShift(Shift: TShiftState): TAdShiftState;
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

// Event handlers

procedure TAdVCLComponentEventConnector.Click(Sender: TObject);
begin
  if Assigned(FFramework.Events.OnClick) then
    FFramework.Events.OnClick(FFramework, FMouseX, FMouseY);

  if Assigned(FOldClick) then
    FOldClick(Sender);
end;


procedure TAdVCLComponentEventConnector.DblClick(Sender: TObject);
begin
  if Assigned(FFramework.Events.OnDblClick) then
    FFramework.Events.OnDblClick(FFramework, FMouseX, FMouseY);  

  if Assigned(FOldDblClick) then
    FOldDblClick(Sender);
end;

procedure TAdVCLComponentEventConnector.KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(FFramework.Events.OnKeyDown) then
    FFramework.Events.OnKeyDown(FFramework, Key, ConvertShift(Shift));

  if Assigned(FOldKeyDown) then
    FOldKeyDown(Sender, Key, Shift);
end;

procedure TAdVCLComponentEventConnector.KeyPress(Sender: TObject; var Key: Char);
begin
  if Assigned(FFramework.Events.OnKeyPress) then
    FFramework.Events.OnKeyPress(FFramework, Key);

  if Assigned(FOldKeyPress) then
    FOldKeyPress(Sender, Key);
end;

procedure TAdVCLComponentEventConnector.KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(FFramework.Events.OnKeyUp) then
    FFramework.Events.OnKeyUp(FFramework, Key, ConvertShift(Shift));

  if Assigned(FOldKeyUp) then
    FOldKeyUp(Sender, Key, Shift);
end;

procedure TAdVCLComponentEventConnector.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  FMouseX := X; FMouseY := Y;

  if Assigned(FFramework.Events.OnMouseDown) then
    FFramework.Events.OnMouseDown(FFramework, ConvertButton(Button), ConvertShift(Shift), X, Y);

  if Assigned(FOldMouseDown) then
    FOldMouseDown(Sender, Button, Shift, X, Y);
end;

procedure TAdVCLComponentEventConnector.MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
begin
  FMouseX := X; FMouseY := Y;

  if Assigned(FFramework.Events.OnMouseMove) then
    FFramework.Events.OnMouseMove(FFramework, ConvertShift(Shift), X, Y);

  if Assigned(FOldMouseMove) then
    FOldMouseMove(Sender, Shift, X, Y);
end;

procedure TAdVCLComponentEventConnector.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  FMouseX := X; FMouseY := Y;

  if Assigned(FFramework.Events.OnMouseUp) then
    FFramework.Events.OnMouseUp(FFramework, ConvertButton(Button), ConvertShift(Shift), X, Y);

  if Assigned(FOldMouseUp) then
    FOldMouseUp(Sender, Button, Shift, X, Y);
end;

procedure TAdVCLComponentEventConnector.MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: integer; MousePos: TPoint; var Handled: Boolean);
begin
  if Assigned(FFramework.Events.OnMouseWheel) then
    FFramework.Events.OnMouseWheel(FFramework, ConvertShift(Shift), WheelDelta, MousePos.X,
      MousePos.Y);

  if Assigned(FOldMouseWheel) then
    FOldMouseWheel(Sender, Shift, WheelDelta, MousePos, Handled);
end;

procedure TAdVCLComponentEventConnector.Paint(Sender: TObject);
begin
  if Assigned(FFramework.Events.OnPaint) then
    FFramework.Events.OnPaint(FFramework);

  if Assigned(FOldPaint) then
    FOldPaint(Sender);
end;

procedure TAdVCLComponentEventConnector.Resize(Sender: TObject);
begin
  if Assigned(FFramework.Events.OnResize) then
    FFramework.Events.OnResize(FFramework);

  if Assigned(FOldResize) then
    FOldResize(Sender);
end;

procedure TAdVCLComponentEventConnector.Deactivate(Sender: TObject);
begin
  if Assigned(FFramework.Events.OnDeactivate) then
    FFramework.Events.OnDeactivate(FFramework);

  if Assigned(FOldDeactivate) then
    FOldDeactivate(Sender);
end;

procedure TAdVCLComponentEventConnector.Activate(Sender: TObject);
begin
  if Assigned(FFramework.Events.OnActivate) then
    FFramework.Events.OnActivate(FFramework);

  if Assigned(FOldActivate) then
    FOldActivate(Sender);
end;

procedure TAdVCLComponentEventConnector.Close(Sender: TObject;
  var Action: TCloseAction);
var
  CanClose: boolean;
begin
  if Assigned(FFramework.Events.OnClose) then
  begin
    CanClose := true;
    FFramework.Events.OnClose(FFramework, CanClose);
    if not CanClose then
      Action := caNone;
  end;

  if Assigned(FOldClose) then
    FOldClose(Sender, Action);
end;

procedure TAdVCLComponentEventConnector.Idle(Sender: TObject;
  var Done: boolean);
begin
  if Assigned(FFramework.Events.OnIdle) then
    FFramework.Events.OnIdle(FFramework, Done);

  if Assigned(FOldIdle) then
    FOldIdle(Sender, Done);
end;

end.
