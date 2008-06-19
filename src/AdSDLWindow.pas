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
* File: AdSDLWindow.pas
* Comment: Contains a windowframework class, that uses the SDL library.
}

{Contains a windowframework class, that uses the SDL library. To use this 
 window framework, the base SDL library has to be available.}
unit AdSDLWindow;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  SDL, AdTypes, AdEvents, AdWindowFramework;

type
  {@exclude}
  TAdSDLWindow = class(TAdGLContextGeneratingWindowFramework)
    private
      FBinded:boolean;
      FInitialized:boolean;
      FClosed:boolean;

      FMouseDown: boolean;
      FLastClick: Cardinal;

      FSDLSurface: PSDL_Surface;

      procedure HandleEvent(AEvent: TSDL_Event);
      function GetButton(Button: Byte): TAdMouseButton;
      function GetShiftState: TAdShiftState;
      function GetKey(Key: TSDL_KeyboardEvent): Word;
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

{ TAdSDLWindow }

constructor TAdSDLWindow.Create;
begin
  inherited;

end;

destructor TAdSDLWindow.Destroy;
begin
  if FInitialized then
  begin
    SDL_Quit;
  end;

  inherited;
end;

function TAdSDLWindow.BindTo(AObj: Pointer): boolean;
begin
  FBinded := AObj = nil;
  result := FBinded;
end;

function TAdSDLWindow.GetClientHeight: integer;
begin
  result := FSDLSurface.h;
end;

function TAdSDLWindow.GetClientWidth: integer;
begin
  result := FSDLSurface.w;
end;

function TAdSDLWindow.InitDisplay(AProps: TAdDisplayProperties): boolean;
var
  videoflags: Cardinal;
  videoinfo: PSDL_VideoInfo;
  w, h: integer;
begin
  result := false;
  if SDL_Init(SDL_INIT_VIDEO) >= 0 then
  begin
    videoinfo := SDL_GetVideoInfo;
    if (videoinfo <> nil) then
    begin
      //Set video flags
      videoflags := SDL_OPENGL;

      w := AProps.Width;
      h := AProps.Height;

      if AProps.Mode = dmScreenRes then
      begin
        w := videoinfo.current_w;
        h := videoinfo.current_h;
        videoflags := videoflags or SDL_NOFRAME;
      end else
      if AProps.Mode = dmFullscreen then
      begin
        videoflags := videoflags or SDL_FULLSCREEN;
      end;

      //Use doublebuffering
      SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);

      FSDLSurface := SDL_SetVideoMode(
        w, h, Ord(AProps.BitDepth), videoflags);
        
      if FSDLSurface <> nil then
      begin
        result := true;
        FInitialized := true;
      end;
    end;
  end;
end;

procedure TAdSDLWindow.Run;
var
  Done:boolean;
  event: TSDL_Event;
begin
  while (not FClosed) do
  begin
    SDL_WaitEvent(@event);

    done := true;

    if Assigned(Events.OnIdle) then
      Events.OnIdle(Self, Done);

    HandleEvent(event);

    while (not Done) and (not FClosed) do
    begin
      SDL_PollEvent(@event);

      Done := true;
      if Assigned(Events.OnIdle) then
        Events.OnIdle(Self, Done);

      HandleEvent(event);
    end;
  end;
end;

function TAdSDLWindow.GetButton(Button: Byte): TAdMouseButton;
begin
  result := abLeft;
  case Button of
    SDL_BUTTON_LEFT: result := abLeft;
    SDL_BUTTON_MIDDLE: result := abMiddle;
    SDL_BUTTON_RIGHT: result := abRight;
  end;
end;

function TAdSDLWindow.GetShiftState: TAdShiftState;
var
  keystate: PChar;
  x, y: integer;
begin
  x := 0;
  y := 0;
  result := [];
  keystate := PChar(SDL_GetKeyState(nil));
  if (keystate[SDLK_RSHIFT] > #0) or
     (keystate[SDLK_LSHIFT] > #0) then
    result := result + [asShift];

  if (keystate[SDLK_RCTRL] > #0) or
     (keystate[SDLK_LCTRL] > #0) then
    result := result + [asCtrl];

  if (keystate[SDLK_RALT] > #0) or
     (keystate[SDLK_LALT] > #0) then
    result := result + [asAlt];
  
  if (SDL_GetMouseState(x, y) and SDL_BUTTON(1) > 0) then
    result := result + [asLeft];
  if (SDL_GetMouseState(x, y) and SDL_BUTTON(3) > 0) then
    result := result + [asRight];
  if (SDL_GetMouseState(x, y) and SDL_BUTTON(2) > 0) then
    result := result + [asMiddle];
end;

function TAdSDLWindow.GetKey(Key: TSDL_KeyboardEvent): Word;
begin
  case Key.keysym.sym of
    SDLK_SPACE       : result := AVK_SPACE;
    SDLK_ESCAPE      : result := AVK_ESCAPE;
    SDLK_F1          : result := AVK_F1;
    SDLK_F2          : result := AVK_F2;
    SDLK_F3          : result := AVK_F3;
    SDLK_F4          : result := AVK_F4;
    SDLK_F5          : result := AVK_F5;
    SDLK_F6          : result := AVK_F6;
    SDLK_F7          : result := AVK_F7;
    SDLK_F8          : result := AVK_F8;
    SDLK_F9          : result := AVK_F9;
    SDLK_F10         : result := AVK_F10;
    SDLK_F11         : result := AVK_F11;
    SDLK_F12         : result := AVK_F12;
    SDLK_F13         : result := AVK_F13;
    SDLK_F14         : result := AVK_F14;
    SDLK_F15         : result := AVK_F15;
    SDLK_UP          : result := AVK_UP;
    SDLK_DOWN        : result := AVK_DOWN;
    SDLK_LEFT        : result := AVK_LEFT;
    SDLK_RIGHT       : result := AVK_RIGHT;
    SDLK_LSHIFT      : result := AVK_LSHIFT;
    SDLK_RSHIFT      : result := AVK_RSHIFT;
    SDLK_LCTRL       : result := AVK_LCTRL;
    SDLK_RCTRL       : result := AVK_RCTRL;
    SDLK_LALT        : result := AVK_LALT;
    SDLK_RALT        : result := AVK_RALT;
    SDLK_TAB         : result := AVK_TAB;
    SDLK_RETURN      : result := AVK_ENTER;
    SDLK_BACKSPACE   : result := AVK_BACK;
    SDLK_INSERT      : result := AVK_INSERT;
    SDLK_DELETE      : result := AVK_DELETE;
    SDLK_PAGEUP      : result := AVK_PAGEUP;
    SDLK_PAGEDOWN    : result := AVK_PAGEDOWN;
    SDLK_HOME        : result := AVK_HOME;
    SDLK_END         : result := AVK_END;
    SDLK_KP0         : result := AVK_NUMPAD_0;
    SDLK_KP1         : result := AVK_NUMPAD_1;
    SDLK_KP2         : result := AVK_NUMPAD_2;
    SDLK_KP3         : result := AVK_NUMPAD_3;
    SDLK_KP4         : result := AVK_NUMPAD_4;
    SDLK_KP5         : result := AVK_NUMPAD_5;
    SDLK_KP6         : result := AVK_NUMPAD_6;
    SDLK_KP7         : result := AVK_NUMPAD_7;
    SDLK_KP8         : result := AVK_NUMPAD_8;
    SDLK_KP9         : result := AVK_NUMPAD_9;
    SDLK_KP_DIVIDE   : result := Ord('/');
    SDLK_KP_MULTIPLY : result := Ord('*');
    SDLK_KP_MINUS    : result := Ord('-');
    SDLK_KP_PLUS     : result := Ord('+');
    SDLK_KP_ENTER    : result := AVK_ENTER;
    else
      result := Key.keysym.unicode;
  end;
end;

procedure TAdSDLWindow.HandleEvent(AEvent: TSDL_Event);
var
  shift: TAdShiftState;
begin
  case AEvent.type_ of
    SDL_ACTIVEEVENT:
    begin
      if AEvent.active.gain = 0 then
      begin
        if Assigned(Events.OnDeactivate) then
          Events.OnDeactivate(self);
      end else
      begin
        if Assigned(Events.OnActivate) then
          Events.OnActivate(self);
      end;
    end;

    SDL_MOUSEMOTION:
      if Assigned(Events.OnMouseMove) then
        Events.OnMouseMove(
          self, GetShiftState, AEvent.motion.x, AEvent.motion.y);

    SDL_MOUSEBUTTONDOWN:
    begin
      if Assigned(Events.OnMouseDown) and not FMouseDown then
        Events.OnMouseDown(
          self, GetButton(AEvent.button.button), GetShiftState,
          AEvent.button.x, AEvent.button.y);
          
      FMouseDown := true;
    end;

    SDL_MOUSEBUTTONUP:
    begin
      shift := GetShiftState;
      
      //Check for doublelick
      if (SDL_GetTicks - FLastClick) < 300 then
      begin
        if Assigned(Events.OnDblClick) and (FMouseDown) then
          Events.OnDblClick(self, AEvent.button.x, AEvent.button.y);
      end
      else begin
        if Assigned(Events.OnClick) and (FMouseDown) then
          Events.OnClick(self, AEvent.button.x, AEvent.button.y);

        shift := shift + [asDouble];

        FLastClick := SDL_GetTicks;
      end;

      if Assigned(Events.OnMouseUp) then
        Events.OnMouseUp(
          self, GetButton(AEvent.button.button), shift,
          AEvent.button.x, AEvent.button.y);

      FMouseDown := false;
    end;

    SDL_KEYDOWN:
    begin
      if Assigned(Events.OnKeyDown) then
        Events.OnKeyDown(
          Self, GetKey(AEvent.key), GetShiftState);
      if Assigned(Events.OnKeyPress) then
        Events.OnKeyPress(
          Self, Char(AEvent.key.keysym.Unicode));
    end;

    SDL_KEYUP:
      if Assigned(Events.OnKeyUp) then
        Events.OnKeyUp(
          Self, GetKey(AEvent.key), GetShiftState);

    SDL_VIDEORESIZE:
      if Assigned(Events.OnResize) then
        Events.OnResize(self);

    SDL_QUITEV:
    begin
      FClosed := true;
    end;
  end;
end;

procedure TAdSDLWindow.SetCursorVisible(AValue: boolean);
begin
  inherited;
  case AValue of
    false: SDL_ShowCursor(SDL_DISABLE);
    true: SDL_ShowCursor(SDL_ENABLE);
  end;
end;

procedure TAdSDLWindow.SetTitle(AValue: string);
begin
  inherited;
  SDL_WM_SetCaption(PChar(AValue), '');
end;

procedure TAdSDLWindow.Swap;
begin
  SDL_GL_SwapBuffers;
end;

procedure TAdSDLWindow.Terminate;
begin
  if FInitialized then
  begin
    FClosed := true;
  end;
end;

initialization
  RegisterWindowFramework(TAdSDLWindow);

end.
