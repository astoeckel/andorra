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
* Comment: Contains a window framework class for Lazarus which uses TOpenGLControl to create an OpenGL context on an existsing control.
}

{Contains a window framework class for Lazarus which uses TOpenGLControl to create an OpenGL context on an existsing control.}
unit AdLCLOGLComponentWindow;

{$MODE DELPHI}

interface

uses
  Interfaces, Classes, Controls, Forms, OpenGLContext,
  AdEvents, AdTypes, AdWindowFramework;
  
type

  { TAdLCLOGLWindow }

  TAdLCLOGLComponentWindow = class(TAdGLContextGeneratingWindowFramework)
    private
      FControl:TWinControl;
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

{ TAdLCLOGLComponentWindow }

constructor TAdLCLOGLComponentWindow.Create;
begin
  inherited;
end;

destructor TAdLCLOGLComponentWindow.Destroy;
begin
  if FContext <> nil then
    FContext.Free;
  inherited;
end;

function TAdLCLOGLComponentWindow.GetClientHeight: integer;
begin
  result := 0;
  if FContext <> nil then
    result := FContext.ClientHeight;
end;

function TAdLCLOGLComponentWindow.GetClientWidth: integer;
begin
  result := 0;
  if FContext <> nil then
    result := FContext.ClientWidth;
end;

function TAdLCLOGLComponentWindow.BindTo(AObj: Pointer): boolean;
begin
  result := false;
  if (TObject(AObj) is TWinControl) and (not FInitialized) then
  begin
    FControl := TWinControl(AObj);
    FBinded := true;
    result := FBinded;
  end;
end;

function TAdLCLOGLComponentWindow.InitDisplay(AProps:TAdDisplayProperties): boolean;
begin
  result := false;
  if (FBinded) and not (FInitialized) then
  begin
    FContext := TOpenGLControl.Create(FControl.Owner);
    FContext.Parent := FControl;
    FContext.Align := alClient;
    FContext.MakeCurrent;

    result := true;
    FInitialized := true;

    SetCursorVisible(true);

    SetupEvents;
  end;
end;

procedure TAdLCLOGLComponentWindow.Run;
begin
  if FInitialized then
  begin
    Application.Run;
  end;
end;

procedure TAdLCLOGLComponentWindow.Terminate;
begin
  Application.Terminate;
end;

procedure TAdLCLOGLComponentWindow.Swap;
begin
  if FInitialized then
  begin
    FContext.SwapBuffers;
  end;
end;

procedure TAdLCLOGLComponentWindow.SetCursorVisible(AValue: Boolean);
begin
  inherited;
  if FInitialized then
  begin
    if AValue then
      FContext.Cursor := crDefault
    else
      FContext.Cursor := crNone;
  end;
end;

procedure TAdLCLOGLComponentWindow.SetTitle(AValue: string);
begin
  inherited;
  Application.Title := AValue;
end;

procedure TAdLCLOGLComponentWindow.SetupEvents;
var
  FForm : TForm;
begin
  FForm := nil;
  if FControl.Owner is TForm then
    FForm := TForm(FControl.Owner)
  else
    exit;

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

function TAdLCLOGLComponentWindow.ConvertButton(Button: TMouseButton): TAdMouseButton;
begin
  result := TAdMouseButton(ord(Button));
end;

function TAdLCLOGLComponentWindow.ConvertShift(Shift: TShiftState): TAdShiftState;
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

{TAdLCLOGLComponentWindow Event handlers}

procedure TAdLCLOGLComponentWindow.Idle(Sender: TObject; var Done: boolean);
begin
  if Assigned(Events.OnIdle) then
    Events.OnIdle(Self, Done);
  if Assigned(FIdle) then
    FIdle(Self, Done);
end;

procedure TAdLCLOGLComponentWindow.Deactivate(Sender: TObject);
begin
  if Assigned(Events.OnDeactivate) then
    Events.OnDeactivate(self);
end;

procedure TAdLCLOGLComponentWindow.Activate(Sender: TObject);
begin
  if Assigned(Events.OnActivate) then
    Events.OnActivate(self);
end;

procedure TAdLCLOGLComponentWindow.Click(Sender: TObject);
begin
  if Assigned(Events.OnClick) then
    Events.OnClick(self, FMouseX, FMouseY);
end;

procedure TAdLCLOGLComponentWindow.DblClick(Sender: TObject);
begin
  if Assigned(Events.OnClick) then
    Events.OnDblClick(self, FMouseX, FMouseY);
end;

procedure TAdLCLOGLComponentWindow.Close(Sender: TObject; var Action: TCloseAction);
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

procedure TAdLCLOGLComponentWindow.KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Assigned(Events.OnKeyDown) then
    Events.OnKeyDown(self, Key, ConvertShift(Shift));
end;

procedure TAdLCLOGLComponentWindow.KeyPress(Sender: TObject; var Key: Char);
begin
  if Assigned(Events.OnKeyPress) then
    Events.OnKeyPress(self, Key);
end;

procedure TAdLCLOGLComponentWindow.KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Assigned(Events.OnKeyUp) then
    Events.OnKeyUp(self, Key, ConvertShift(Shift));
end;

procedure TAdLCLOGLComponentWindow.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if Assigned(Events.OnMouseDown) then
    Events.OnMouseDown(self, ConvertButton(Button), ConvertShift(Shift), X, Y);
end;

procedure TAdLCLOGLComponentWindow.MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: integer);
begin
  FMouseX := X; FMouseY := Y;
  if Assigned(Events.OnMouseMove) then
    Events.OnMouseMove(self, ConvertShift(Shift), X, Y);
end;

procedure TAdLCLOGLComponentWindow.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if Assigned(Events.OnMouseUp) then
    Events.OnMouseUp(self, ConvertButton(Button), ConvertShift(Shift), X, Y);
end;

procedure TAdLCLOGLComponentWindow.MouseWheel(Sender: TObject; Shift: TShiftState;
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

procedure TAdLCLOGLComponentWindow.Paint(Sender: TObject);
begin
  if Assigned(Events.OnPaint) then
    Events.OnPaint(self);
end;

procedure TAdLCLOGLComponentWindow.Resize(Sender: TObject);
begin
  if Assigned(Events.OnResize) then
    Events.OnResize(self);
end;

initialization
  RegisterWindowFramework(TAdLCLOGLComponentWindow);
  
end.
