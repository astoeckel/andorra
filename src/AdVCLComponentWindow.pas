unit AdVCLComponentWindow;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  SysUtils, Classes, Types, Forms, Controls, TypInfo,
  AdEvents, AdWindowFramework;

type
  TAdVCLComponentWindow = class(TAdHandleWindowFrameWork)
    private
      FBinded:boolean;
      FInitialized:boolean;
      FControl : TWinControl;

      FOldClick : TNotifyEvent;
      FOldDblClick : TNotifyEvent;
      FOldMouseMove : TMouseMoveEvent;
      FOldMouseUp : TMouseEvent;
      FOldMouseDown : TMouseEvent;
      FOldMouseWheel : TMouseWheelEvent;

      FOldKeyDown : TKeyEvent;
      FOldKeyPress : TKeyPressEvent;
      FOldKeyUp : TKeyEvent;

      FSetClick : boolean;
      FSetDblClick : boolean;
      FSetMouseMove : boolean;
      FSetMouseDown : boolean;
      FSetMouseUp : boolean;
      FSetMouseWheel : boolean;

      FSetKeyDown : boolean;
      FSetKeyUp : boolean;
      FSetKeyPress : boolean;

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
    protected
      function GetClientWidth:integer;override;
      function GetClientHeight:integer;override;
      procedure SetTitle(AValue:string);override;

      procedure SetupEvents;

      procedure SetCursorVisible(AValue:Boolean);override;

      function ConvertButton(Button: TMouseButton): TAdMouseButton;
      function ConvertShift(Shift: TShiftState): TAdShiftState;
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

type
  PNotifyEvent = ^TNotifyEvent;
  PMouseMoveEvent = ^TMouseMoveEvent;
  PMouseEvent = ^TMouseEvent;
  PMouseWheelEvent = ^TMouseWheelEvent;
  PKeyEvent = ^TKeyEvent;
  PKeyPressEvent = ^TKeyPressEvent;

procedure TAdVCLComponentWindow.SetupEvents;
var
  count : integer;
  Properties:TPropList;
  i : integer;
  Control : TControl;
  method : TMethod;
begin
  if FInitialized then
  begin
    Control := FControl;
    while Control <> nil do
    begin
      count := GetPropList(Control.ClassInfo, [tkMethod], @Properties);

      for i := 0 to count - 1 do
      begin
        method := GetMethodProp(Control, Properties[i]^.Name);
        if (Properties[i]^.Name = 'OnClick') and (not FSetClick) then
        begin
          FOldClick := TNotifyEvent(method);
          PNotifyEvent(@method.Code)^ := Click; method.Data := self;
          SetMethodProp(Control, Properties[i]^.Name, method);
          FSetClick := true;
        end else
        if (Properties[i]^.Name = 'OnDblClick') and (not FSetDblClick) then
        begin
          FOldDblClick := TNotifyEvent(method);
          PNotifyEvent(@method.Code)^ := DblClick; method.Data := self;
          SetMethodProp(Control, Properties[i]^.Name, method);
          FSetDblClick := true;
        end else
        if (Properties[i]^.Name = 'OnMouseMove') and (not FSetMouseMove) then
        begin
          FOldMouseMove := TMouseMoveEvent(method);
          PMouseMoveEvent(@method.Code)^ := MouseMove; method.Data := self;
          SetMethodProp(Control, Properties[i]^.Name, method);
          FSetMouseMove := true;
        end else
        if (Properties[i]^.Name = 'OnMouseUp') and (not FSetMouseUp) then
        begin
          FOldMouseUp := TMouseEvent(method);
          PMouseEvent(@method.Code)^ := MouseUp; method.Data := self;
          SetMethodProp(Control, Properties[i]^.Name, method);
          FSetMouseUp := true;
        end else
        if (Properties[i]^.Name = 'OnMouseDown') and (not FSetMouseDown) then
        begin
          FOldMouseDown := TMouseEvent(method);
          PMouseEvent(@method.Code)^ := MouseDown; method.Data := self;
          SetMethodProp(Control, Properties[i]^.Name, method);
          FSetMouseDown := true;
        end else
        if (Properties[i]^.Name = 'OnMouseWheel') and (not FSetMouseWheel) then
        begin
          FOldMouseWheel := TMouseWheelEvent(method);
          PMouseWheelEvent(@method.Code)^ := MouseWheel; method.Data := self;
          SetMethodProp(Control, Properties[i]^.Name, method);
          FSetMouseWheel := true;
        end else
        if (Properties[i]^.Name = 'OnKeyDown') and (not FSetKeyDown) then
        begin
          FOldKeyDown := TKeyEvent(method);
          PKeyEvent(@method.Code)^ := KeyDown; method.Data := self;
          SetMethodProp(Control, Properties[i]^.Name, method);
          FSetKeyDown := true;
        end else
        if (Properties[i]^.Name = 'OnKeyUp') and (not FSetKeyUp) then
        begin
          FOldKeyUp := TKeyEvent(method);
          PKeyEvent(@method.Code)^ := KeyUp; method.Data := self;
          SetMethodProp(Control, Properties[i]^.Name, method);
          FSetKeyUp := true;
        end else
        if (Properties[i]^.Name = 'OnKeyPress') and (not FSetKeyPress) then
        begin
          FOldKeyPress := TKeyPressEvent(method);
          PKeyPressEvent(@method.Code)^ := KeyPress; method.Data := self;
          SetMethodProp(Control, Properties[i]^.Name, method);
          FSetKeyPress := true;
        end;
      end;
      
      if Control.Parent is TControl then
        Control := Control.Parent
      else
        Control := nil;
    end;
  end;   
end;

procedure TAdVCLComponentWindow.Terminate;
begin
  Application.Terminate;
end;

function TAdVCLComponentWindow.ConvertButton(Button: TMouseButton): TAdMouseButton;
begin
  result := TAdMouseButton(ord(Button));
end;

function TAdVCLComponentWindow.ConvertShift(Shift: TShiftState): TAdShiftState;
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

{ TAdVCLComponent Event Handlers }

procedure TAdVCLComponentWindow.Click(Sender: TObject);
begin
  if Assigned(Events.OnClick) then
    Events.OnClick(self, FMouseX, FMouseY);  

  if Assigned(FOldClick) then
    FOldClick(Sender);
end;

procedure TAdVCLComponentWindow.DblClick(Sender: TObject);
begin
  if Assigned(Events.OnDblClick) then
    Events.OnDblClick(self, FMouseX, FMouseY);  

  if Assigned(FOldDblClick) then
    FOldDblClick(Sender);
end;

procedure TAdVCLComponentWindow.KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(Events.OnKeyDown) then
    Events.OnKeyDown(self, Key, ConvertShift(Shift));

  if Assigned(FOldKeyDown) then
    FOldKeyDown(Sender, Key, Shift);
end;

procedure TAdVCLComponentWindow.KeyPress(Sender: TObject; var Key: Char);
begin
  if Assigned(Events.OnKeyPress) then
    Events.OnKeyPress(self, Key);

  if Assigned(FOldKeyPress) then
    FOldKeyPress(Sender, Key);
end;

procedure TAdVCLComponentWindow.KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(Events.OnKeyUp) then
    Events.OnKeyUp(self, Key, ConvertShift(Shift));

  if Assigned(FOldKeyUp) then
    FOldKeyUp(Sender, Key, Shift);
end;

procedure TAdVCLComponentWindow.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  FMouseX := X; FMouseY := Y;

  if Assigned(Events.OnMouseDown) then
    Events.OnMouseDown(self, ConvertButton(Button), ConvertShift(Shift), X, Y);

  if Assigned(FOldMouseDown) then
    FOldMouseDown(Sender, Button, Shift, X, Y);
end;

procedure TAdVCLComponentWindow.MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
begin
  FMouseX := X; FMouseY := Y;
  
  if Assigned(Events.OnMouseMove) then
    Events.OnMouseMove(self, ConvertShift(Shift), X, Y);

  if Assigned(FOldMouseMove) then
    FOldMouseMove(Sender, Shift, X, Y);
end;

procedure TAdVCLComponentWindow.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  FMouseX := X; FMouseY := Y;

  if Assigned(Events.OnMouseUp) then
    Events.OnMouseUp(self, ConvertButton(Button), ConvertShift(Shift), X, Y);

  if Assigned(FOldMouseUp) then
    FOldMouseUp(Sender, Button, Shift, X, Y);
end;

procedure TAdVCLComponentWindow.MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: integer; MousePos: TPoint; var Handled: Boolean);
begin
  if Assigned(Events.OnMouseWheel) then
    Events.OnMouseWheel(self, ConvertShift(Shift), WheelDelta, MousePos.X,
      MousePos.Y);

  if Assigned(FOldMouseWheel) then
    FOldMouseWheel(Sender, Shift, WheelDelta, MousePos, Handled);
end;

initialization
  RegisterWindowFramework(TAdVCLComponentWindow);

end.
