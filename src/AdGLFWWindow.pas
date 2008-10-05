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
* File: AdGLFW.pas
* Comment: Adds an Andorra 2D window framework which uses glfw
}

{Adds an Andorra 2D window framework which uses glfw}
unit AdGLFWWindow;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  AdTypes, AdEvents, AdWindowFramework, glfw;

type
  {@exclude}
  TAdGLFWWindow = class(TAdGLContextGeneratingWindowFrameWork)
    private
      FBinded:boolean;
      FInitialized:boolean;
      FClosed:boolean;
    protected
      function GetClientWidth:integer;override;
      function GetClientHeight:integer;override;
      procedure SetTitle(AValue:string);override;
      procedure SetCursorVisible(AValue:boolean);override;
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

var
  WindowInstance : TAdGLFWWindow;

  MouseX, MouseY : integer;
  LastWheelPos : integer;
  MouseDown : boolean;
  LastClick : double;

//Converts a GLFW keycode to an Andorra 2D keycode
function ConvertKey(AKey: Integer):Integer;
begin
  result := AKey;
  if AKey > 256 then
  begin
    case AKey of
      GLFW_KEY_SPACE       : result := AVK_SPACE;
      GLFW_KEY_ESC         : result := AVK_ESCAPE;
      GLFW_KEY_F1          : result := AVK_F1;
      GLFW_KEY_F2          : result := AVK_F2;
      GLFW_KEY_F3          : result := AVK_F3;
      GLFW_KEY_F4          : result := AVK_F4;
      GLFW_KEY_F5          : result := AVK_F5;
      GLFW_KEY_F6          : result := AVK_F6;
      GLFW_KEY_F7          : result := AVK_F7;
      GLFW_KEY_F8          : result := AVK_F8;
      GLFW_KEY_F9          : result := AVK_F9;
      GLFW_KEY_F10         : result := AVK_F10;
      GLFW_KEY_F11         : result := AVK_F11;
      GLFW_KEY_F12         : result := AVK_F12;
      GLFW_KEY_F13         : result := AVK_F13;
      GLFW_KEY_F14         : result := AVK_F14;
      GLFW_KEY_F15         : result := AVK_F15;
      GLFW_KEY_F16         : result := AVK_F16;
      GLFW_KEY_F17         : result := AVK_F17;
      GLFW_KEY_F18         : result := AVK_F18;
      GLFW_KEY_F19         : result := AVK_F19;
      GLFW_KEY_F20         : result := AVK_F20;
      GLFW_KEY_F21         : result := AVK_F21;
      GLFW_KEY_F22         : result := AVK_F22;
      GLFW_KEY_F23         : result := AVK_F23;
      GLFW_KEY_F24         : result := AVK_F24;
      GLFW_KEY_UP          : result := AVK_UP;
      GLFW_KEY_DOWN        : result := AVK_DOWN;
      GLFW_KEY_LEFT        : result := AVK_LEFT;
      GLFW_KEY_RIGHT       : result := AVK_RIGHT;
      GLFW_KEY_LSHIFT      : result := AVK_LSHIFT;
      GLFW_KEY_RSHIFT      : result := AVK_RSHIFT;
      GLFW_KEY_LCTRL       : result := AVK_LCTRL;
      GLFW_KEY_RCTRL       : result := AVK_RCTRL;
      GLFW_KEY_LALT        : result := AVK_LALT;
      GLFW_KEY_RALT        : result := AVK_RALT;
      GLFW_KEY_TAB         : result := AVK_TAB;
      GLFW_KEY_ENTER       : result := AVK_ENTER;
      GLFW_KEY_BACKSPACE   : result := AVK_BACK;
      GLFW_KEY_INSERT      : result := AVK_INSERT;
      GLFW_KEY_DEL         : result := AVK_DELETE;
      GLFW_KEY_PAGEUP      : result := AVK_PAGEUP;
      GLFW_KEY_PAGEDOWN    : result := AVK_PAGEDOWN;
      GLFW_KEY_HOME        : result := AVK_HOME;
      GLFW_KEY_END         : result := AVK_END;
      GLFW_KEY_KP_0        : result := AVK_NUMPAD_0;
      GLFW_KEY_KP_1        : result := AVK_NUMPAD_1;
      GLFW_KEY_KP_2        : result := AVK_NUMPAD_2;
      GLFW_KEY_KP_3        : result := AVK_NUMPAD_3;
      GLFW_KEY_KP_4        : result := AVK_NUMPAD_4;
      GLFW_KEY_KP_5        : result := AVK_NUMPAD_5;
      GLFW_KEY_KP_6        : result := AVK_NUMPAD_6;
      GLFW_KEY_KP_7        : result := AVK_NUMPAD_7;
      GLFW_KEY_KP_8        : result := AVK_NUMPAD_8;
      GLFW_KEY_KP_9        : result := AVK_NUMPAD_9;
      GLFW_KEY_KP_DIVIDE   : result := Ord('/'); //!Andorra 2D doesn't know this keycode
      GLFW_KEY_KP_MULTIPLY : result := Ord('*'); //!Andorra 2D doesn't know this keycode
      GLFW_KEY_KP_SUBTRACT : result := Ord('-'); //!Andorra 2D doesn't know this keycode
      GLFW_KEY_KP_ADD      : result := Ord('+'); //!Andorra 2D doesn't know this keycode
      GLFW_KEY_KP_DECIMAL  : result := Ord(','); //!Andorra 2D doesn't know this keycode
      GLFW_KEY_KP_EQUAL    : result := Ord('='); //!Andorra 2D doesn't know this keycode
      GLFW_KEY_KP_ENTER    : result := AVK_ENTER; //!Andorra 2D doesn't know this keycode
    end;
  end;
end;

//Returns the current shift state of the window
function GetShiftState:TAdShiftState;
begin
  result := [];
  
  //Check whether a "SHIFT" key is pressed
  if (glfwGetKey(GLFW_KEY_LSHIFT) = GLFW_PRESS) or
     (glfwGetKey(GLFW_KEY_RSHIFT) = GLFW_PRESS) then
    result := result + [asShift];
    
   //Check whether a "CTRL" key is pressed
  if (glfwGetKey(GLFW_KEY_LCTRL) = GLFW_PRESS) or
     (glfwGetKey(GLFW_KEY_RCTRL) = GLFW_PRESS) then
    result := result + [asCtrl];
    
  //Check whether a "ALT" key is pressed
  if (glfwGetKey(GLFW_KEY_LALT) = GLFW_PRESS) or
     (glfwGetKey(GLFW_KEY_RALT) = GLFW_PRESS) then
    result := result + [asAlt];
    
  //Check whether the left mouse button is pressed
  if (glfwGetMouseButton(GLFW_MOUSE_BUTTON_LEFT) = GLFW_PRESS) then
    result := result + [asLeft];

  //Check whether the right mouse button is pressed
  if (glfwGetMouseButton(GLFW_MOUSE_BUTTON_RIGHT) = GLFW_PRESS) then
    result := result + [asRight];

  //Check whether the center mouse button is pressed
  if (glfwGetMouseButton(GLFW_MOUSE_BUTTON_MIDDLE) = GLFW_PRESS) then
    result := result + [asMiddle];
end;

{ GLWF Input Callbacks }

procedure MouseButtonCallback(Button, Action: Integer); stdcall;
var
  btn : TAdMouseButton;
  shift : TAdShiftState;
begin
  //Determine which mouse button this callback call alludes to
  case Button of
    GLFW_MOUSE_BUTTON_LEFT : btn := abLeft;
    GLFW_MOUSE_BUTTON_RIGHT : btn := abRight;
    GLFW_MOUSE_BUTTON_MIDDLE : btn := abMiddle;
    else
      btn := TAdMouseButton(Button);
  end;

  //Get the current shift state
  shift := GetShiftState;

  if Action = GLFW_PRESS then
  begin
    //Remember that the left mouse button has been pressed, to call the "OnClick" event lateron
    if btn = abLeft then
      MouseDown := true;
     
    //Call the mouse down event
    if Assigned(WindowInstance.Events.OnMouseDown) then
      WindowInstance.Events.OnMouseDown(WindowInstance, btn, shift, MouseX, MouseY);
  end else
  begin
    //The left mousebutton had already been pressed.
    if MouseDown then
    begin
      //This is a double click
      if (glfwGetTime - LastClick) < 0.3 then
      begin
        if Assigned(WindowInstance.Events.OnDblClick) then
          WindowInstance.Events.OnDblClick(WindowInstance, MouseX, MouseY);
        shift := shift + [asDouble]; //Add the "double" state to the shift state
      end else
      begin
        //This is a single click
        if Assigned(WindowInstance.Events.OnClick) then
          WindowInstance.Events.OnClick(WindowInstance, MouseX, MouseY);
        
        //Get the current time to be able to call the "double click" event
        LastClick := glfwGetTime;
      end;
      
      MouseDown := false;
    end;

    //Call the "OnMouseUp" event
    if Assigned(WindowInstance.Events.OnMouseUp) then
      WindowInstance.Events.OnMouseUp(WindowInstance, btn, shift, MouseX, MouseY);
  end;
    
end;

procedure MousePosCallback(X, Y: Integer); stdcall;
begin
  //Store the current mouse position
  MouseX := X;
  MouseY := Y;

  //Call the mouse move event
  if Assigned(WindowInstance.Events.OnMouseMove) then
    WindowInstance.Events.OnMouseMove(WindowInstance, GetShiftState, MouseX, MouseY);
end;

procedure KeyCallback(Key, Action: Integer); stdcall;
begin
  if Action = GLFW_PRESS then
  begin
    //Call the key down event
    if Assigned(WindowInstance.Events.OnKeyDown) then
      WindowInstance.Events.OnKeyDown(WindowInstance, ConvertKey(Key), GetShiftState);
  end else
  begin
    //Call the key up event
    if Assigned(WindowInstance.Events.OnKeyUp) then
      WindowInstance.Events.OnKeyUp(WindowInstance, ConvertKey(Key), GetShiftState);
  end;
end;

const
  WheelMultFact = 120;

procedure MouseWheelCallback(Pos: Integer); stdcall;
begin
  //Call the mouse wheel event
  if Assigned(WindowInstance.Events.OnMouseWheel) then
    WindowInstance.Events.OnMouseWheel(
      WindowInstance, [], (Pos - LastWheelPos) * WheelMultFact, MouseX, MouseY);

  //Store the current position to be able to calculate a "delta" value
  LastWheelPos := Pos;
end;

procedure KeyPressCallback(Character, Action: Integer); stdcall;
begin
  //Call the key press event
  if (Action = GLFW_PRESS) and Assigned(WindowInstance.Events.OnKeyPress) then
    WindowInstance.Events.OnKeyPress(WindowInstance, Chr(Character));
end;

procedure PaintCallback; stdcall;
begin
  //Call the on paint event
  if Assigned(WindowInstance.Events.OnPaint) then
    WindowInstance.Events.OnPaint(WindowInstance);
end;

function CloseCallback:integer; stdcall;
var
  b:boolean;
begin
  //Call the close callback. Allow the GLFW window to be closed
  result := Integer(true);
  if Assigned(WindowInstance.Events.OnClose) then
  begin
    b := true;
    WindowInstance.Events.OnClose(WindowInstance, b);
    result := Integer(not b); //Return whether the window has to be closed
  end;
end;

var
  Deactivated:boolean = true;

procedure ResizeCallback(width, height:integer); stdcall;
begin
  //Call the on resize event
  if Assigned(WindowInstance.Events.OnResize) then
    WindowInstance.Events.OnResize(WindowInstance);
    
  //If the width and the height of the window is 0, the window has been minimized.    
  if (width = 0) and (height = 0) then
  begin
    //Call the "OnDeactivate" callback
    if not Deactivated then
      if Assigned(WindowInstance.Events.OnDeactivate) then
        WindowInstance.Events.OnDeactivate(WindowInstance);
    
    Deactivated := true;
  end else
  begin
    if Deactivated then
    begin
      //The window has been restored, call the "OnActivate" callback
      if Assigned(WindowInstance.Events.OnActivate) then
        WindowInstance.Events.OnActivate(WindowInstance);
      Deactivated := false;
    end;
  end;
end;

{ TAdGLFWWindow }

constructor TAdGLFWWindow.Create;
begin
  inherited;

  //Only one GLFW Window Framework instance can run at once
  //! Probably raise an exception here
  if WindowInstance <> nil then
    WindowInstance.Free;

  //Initialize GLFW
  glfwInit;

  //Set the window instance variable to the instance of this object
  WindowInstance := self;
end;

destructor TAdGLFWWindow.Destroy;
begin
  //Terminate the glfwInstance
  glfwTerminate;

  //Set the local window instance variable to nil
  WindowInstance := nil;

  inherited;
end;

function TAdGLFWWindow.GetClientHeight: integer;
var
  w, h:integer;
begin
  //Return the height of the current window
  result := 0;
  if FInitialized then
  begin
    glfwGetWindowSize(w, h);
    result := h;
  end;
end;

function TAdGLFWWindow.GetClientWidth: integer;
var
  w, h:integer;
begin
  //Return the width of the current window
  result := 0;
  if FInitialized then
  begin
    glfwGetWindowSize(w, h);
    result := w;
  end;
end;

function TAdGLFWWindow.BindTo(AObj: Pointer): boolean;
begin
  //GLFW creates is own window, thus we only allow binding to the "nil" object.
  FBinded := AObj = nil;
  result := FBinded;
end;

function TAdGLFWWindow.InitDisplay(AProps: TAdDisplayProperties): boolean;
var
  DesktopMode:GLFWvidmode;
  rBits, gBits, bBits, aBits:byte;
  w, h:integer;
  mode:integer;
begin
  result := false;
  if FBinded and (not FInitialized) then
  begin
    //Set the desired bit depth of the new OpenGL context
    if AProps.BitDepth = ad16Bit then
    begin
      //R5G6B5A0
      rBits := 5; gBits := 6; bBits := 5; aBits := 0;
    end
    else
    begin
      //R8G8B8A0
      rBits := 8; gBits := 8; bBits := 8; aBits := 8;
    end;

    //Get information about the current desktop
    glfwGetDesktopMode(@DesktopMode);

    if (AProps.Mode = dmWindowed) or (AProps.Mode = dmDefault) then
    begin
      //Windowed mode
      w := AProps.Width;
      h := AProps.Height;
      mode := GLFW_WINDOW;
    end else
    if AProps.Mode = dmScreenRes then
    begin
      //Fullscreen mode - don't change the current resolution
      w := DesktopMode.Width;
      h := DesktopMode.Height;
      rBits := DesktopMode.RedBits;
      gBits := DesktopMode.GreenBits;
      bBits := DesktopMode.BlueBits;
      mode := GLFW_FULLSCREEN;
    end else
    begin
      //Fullscreen mode - set the resolution the the desired value
      w := AProps.Width;
      h := AProps.Height;
      mode := GLFW_FULLSCREEN;
    end;

    //Open a GLFW window
    FInitialized := glfwOpenWindow(w, h, rBits, gBits, bBits, aBits, 24, 8, mode) = 1;

    //Center the window if we are in the window mode
    if (AProps.Mode = dmWindowed) or (AProps.Mode = dmDefault) then
      glfwSetWindowPos((DesktopMode.Width - w) div 2, (DesktopMode.Height - h) div 2);

    if FInitialized then
    begin
      //Set the window title
      glfwSetWindowTitle(PChar(Title));

      //Set all event callbacks
      glfwSetMouseButtonCallback(MouseButtonCallback);
      glfwSetMousePosCallback(MousePosCallback);
      glfwSetMouseWheelCallback(MouseWheelCallback);
      glfwSetKeyCallback(KeyCallback);
      glfwSetCharCallback(KeyPressCallback);
      glfwSetWindowRefreshCallback(PaintCallback);
      glfwSetWindowCloseCallback(CloseCallback);
      glfwSetWindowSizeCallback(ResizeCallback);

      //Enable repeating the "Char" event if a key is being hold for a longer time
      glfwEnable(GLFW_KEY_REPEAT);
    end;

    result := FInitialized;
  end;
end;

procedure TAdGLFWWindow.Run;
var
  Done:boolean;
begin
  while (not FClosed) do
  begin
    //Wait for a event to come
    glfwWaitEvents;

    //Call the on idle event
    done := true;
    if Assigned(Events.OnIdle) then
      Events.OnIdle(Self, Done);

    //If the OnIdle event set the "Done" variable to "false", switch to the
    //permanent imidiate mode.
    while (not Done) and (not FClosed) do
    begin
      //Check whether there are new events
      glfwPollEvents;

      //Call the OnIdle event
      Done := true;
      if Assigned(Events.OnIdle) then
        Events.OnIdle(Self, Done);

      //Check whether the window has been closed
      FClosed := not (glfwGetWindowParam(GLFW_OPENED) = 1);
    end;

    //Check whether the window has been closed
    FClosed := not (glfwGetWindowParam(GLFW_OPENED) = 1);
  end;  
end;

procedure TAdGLFWWindow.Terminate;
begin
  //Close the window. The "run" method recognizes whether the window is closed and
  //quits
  if FInitialized then
    glfwCloseWindow;
end;

procedure TAdGLFWWindow.SetCursorVisible(AValue: Boolean);
begin
  inherited;
  
  //Set the mousecursor visible or not
  if AValue then
    glfwEnable(GLFW_MOUSE_CURSOR)
  else
    glfwDisable(GLFW_MOUSE_CURSOR);
end;

procedure TAdGLFWWindow.SetTitle(AValue: string);
begin
  inherited;

  //Set the title of the window to the specified value
  if FInitialized then
    glfwSetWindowTitle(PChar(AValue));
end;

procedure TAdGLFWWindow.Swap;
begin
  //Swap the GLFW's OpenGL context back- and frontbuffer
  if FInitialized then
    glfwSwapBuffers;
end;

initialization
   RegisterWindowFramework(TAdGLFWWindow);

end.
