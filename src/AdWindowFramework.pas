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
* File: AdWindowFramework.pas
* Comment: Contains the abstract base windowframework class.
}

{Contains the abstract base windowframework class.}
unit AdWindowFramework;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  AdTypes, AdPersistent, AdEvents, AdContainers;

type
  {A simple class which contains all events that may be caught by a TAdWindowFramework.}
  TAdEventHandler = class(TAdPersistent)
    private
      FPaint : TAdNotifyEvent;
      FResize : TAdNotifyEvent;
      FActivate : TAdNotifyEvent;
      FDeactivate : TAdNotifyEvent;
      FClose : TAdCloseEvent;

      FClick : TAdClickEvent;
      FDblClick : TAdClickEvent;
      FMouseMove : TAdMouseMoveEvent;
      FMouseUp : TAdMouseEvent;
      FMouseDown : TAdMouseEvent;
      FMouseWheel : TAdMouseWheelEvent;

      FKeyDown : TAdKeyEvent;
      FKeyPress : TAdKeyPressEvent;
      FKeyUp : TAdKeyEvent;

      FIdle : TAdIdleEvent;
    published
      {Called when the operating systems wants the window to be redrawn.}
      property OnPaint : TAdNotifyEvent read FPaint write FPaint;
      {Called when the window resizes.}
      property OnResize : TAdNotifyEvent read FResize write FResize;
      {Called when the window gets the focus.}
      property OnActivate : TAdNotifyEvent read FActivate write FActivate;
      {Called when the window looses the focus.}
      property OnDeactivate : TAdNotifyEvent read FDeactivate write FDeactivate;
      {Called when the user wants to close the window. @seealso(TAdCloseEvent)}
      property OnClose : TAdCloseEvent read FClose write FClose;

      {Called when a click on the window surface is cast.}
      property OnClick : TAdClickEvent read FClick write FClick;
      {Called when a double-click on the window surface is cast.}
      property OnDblClick : TAdClickEvent read FDblClick write FDblClick;
      {Called when a mouse button is pressed on the window surface. @seealso(TAdMouseEvent)}
      property OnMouseDown : TAdMouseEvent read FMouseDown write FMouseDown;
      {Called when the mouse moves on the window surface. @seealso(TAdMouseMoveEvent)}
      property OnMouseMove : TAdMouseMoveEvent read FMouseMove write FMouseMove;
      {Called when a moouse button is released. @seealso(TAdMouseEvent)}
      property OnMouseUp : TAdMouseEvent read FMouseUp write FMouseUp;
      {Called when the mousewheel is moved up or down. @seealso(TAdMouseWheelEvent)}
      property OnMouseWheel : TAdMouseWheelEvent read FMouseWheel write FMouseWheel;

      {Called when a key on the keyboard , which produces a readable character, is pressed. Depending on OS settings, KeyPress may also be called when a key is hold down for a specific time. @seealso(TAdKeyPressEvent)}
      property OnKeyPress : TAdKeyPressEvent read FKeyPress write FKeyPress;
      {Called when a key on the keyboard  is pressed. @seealso(TAdKeyEvent)} 
      property OnKeyDown : TAdKeyEvent read FKeyDown write FKeyDown;
      {Called when a key on the keyboard is released. @seealso(TAdKeyEvent)} 
      property OnKeyUp : TAdKeyEvent read FKeyUp write FKeyUp;

      {Use OnIdle to inject your application main loop here. @seealso(TAdIdleEvent)}
      property OnIdle : TAdIdleEvent read FIdle write FIdle;
  end;

  {An abstract class which represents a window or the drawing surface. TAdWindowFramework is passed to the plugin dll, which uses the data given by the frame work to produce its own device context or uses an already existing one.}
  TAdWindowFramework = class(TAdPersistent)
    private
      FTitle:string;
      FEvents:TAdEventHandler;
      FCursorVisible:Boolean;
    protected
      function GetClientWidth:integer;virtual;abstract;
      function GetClientHeight:integer;virtual;abstract;
      procedure SetTitle(AValue:string);virtual;
      procedure SetCursorVisible(AValue:Boolean);virtual;
    public
      {Creates an instance of TAdWindowFramework.}
      constructor Create;virtual;
      {Destroys the instance of TAdWindowFramework.}
      destructor Destroy;override;

      {The window framework will try to "bind" itsself to the object (which in most cases is a component) given by the "AObj"-Parameter. For Windowframeworks which create a window theirselfes, "AObj" must be @nil. "BindTo" will return @true when the binding is valid.  @seealso(InitDisplay)}
      function BindTo(AObj:Pointer):boolean;virtual;abstract;
      {Inits the display surface on the object passed by BindTo. If the window-framework does not create the window itself, the properties given in "AProps" may be ignored. @seealso(BindTo)}
      function InitDisplay(AProps:TAdDisplayProperties):boolean;virtual;abstract;

      {If the windowframework creates the window itself, "Run" causes the window system to perform its event loop. "Run" will be left wenn the main window is closed or "Terminate" is called. }
      procedure Run;virtual;abstract;
      {The main event loop will be left and the application will close}
      procedure Terminate;virtual;abstract;

      {Returns an identification string which consists of the inheritance hierarchy of the class.}
      function IdentStr:ShortString;

      {Property which is used to set the window title. Has no effect on a window framework which doesn't create a window by itself.}
      property Title:string read FTitle write SetTitle;
      {Contains a list of events the window may receive. @seealso(TAdEventHandler)}
      property Events:TAdEventHandler read FEvents;
      {Returns the width of the client area of the window.}
      property ClientWidth:integer read GetClientWidth;
      {Retrurns the height of the client area of the window}
      property ClientHeight:integer read GetClientHeight;
      {Use this property to show or hide the mouse cursor.}
      property CursorVisible:Boolean read FCursorVisible write SetCursorVisible;
  end;
  {@exclude}
  TAdWindowFrameworkClass = class of TAdWindowFramework;

  {A child class of TAdWindowFramework which may preserve the handle to the control on windows.}
  TAdHandleWindowFramework = class(TAdWindowFramework)
    protected
      FHandle:LongInt;
    public
      {Handle to the parent control.}
      property Handle:LongInt read FHandle write FHandle;
  end;
  
  {A child class of TAdWindowFramework which creates a own OpenGL context to be used by the Andorra 2D OpenGL plugin.}
  TAdGLContextGeneratingWindowFramework = class(TAdWindowFramework)
    public
      procedure Swap;virtual;abstract;
  end;

{Returns a TAdDisplayProperties record containing the specified parameters. @seealso(TAdDisplayProperties)}
function AdDisplayProperties(AWidth, AHeight:integer; AMode:TAdWindowDisplayMode;
  ABitDepth: TAdBitDepth):TAdDisplayProperties;

{Procedure which registers a window framework class so that it may be automaticly used by TAdDraw lateron.}
procedure RegisterWindowFramework(AWndFrmWrk:TAdWindowFrameworkClass);

var
  {@exclude}
  RegisteredWindowFrameworks:TAdLinkedList;

implementation

procedure RegisterWindowFramework(AWndFrmWrk:TAdWindowFrameworkClass);
var
  PSS : PShortString;
begin 
  New(PSS);
  PSS^ := AWndFrmWrk.ClassName;
  RegisteredWindowFrameworks.Add(PSS);

  AdRegisterClass(AWndFrmWrk);
end;

function AdDisplayProperties(AWidth, AHeight:integer; AMode:TAdWindowDisplayMode;
  ABitDepth: TAdBitDepth): TAdDisplayProperties;
begin
  with result do
  begin
    Width := AWidth;
    Height := AHeight;
    Mode := AMode;
    BitDepth := ABitDepth;
  end;
end;


{ TAdWindowFramework }

constructor TAdWindowFramework.Create;
begin
  inherited Create;

  FEvents := TAdEventHandler.Create;
end;

destructor TAdWindowFramework.Destroy;
begin
  FEvents.Free;

  inherited Destroy;
end;

function TAdWindowFramework.IdentStr: ShortString;
var
  aclass:TClass;
begin
  aclass := self.ClassType;
  result := '';
  while aclass <> TObject do
  begin
    //We have to use a short string here, because the string is exchanged between
    //plugin and host.
    result := result + '.' + aclass.ClassName;
    aclass := aclass.ClassParent;
  end;
end;

procedure TAdWindowFramework.SetCursorVisible(AValue: Boolean);
begin
  FCursorVisible := AValue;
end;

procedure TAdWindowFramework.SetTitle(AValue: string);
begin
  FTitle := AValue;
end;

initialization
  RegisteredWindowFrameworks := TAdLinkedList.Create;

finalization
  RegisteredWindowFrameworks.StartIteration;
  while not RegisteredWindowFrameworks.ReachedEnd do
  begin
    Dispose(PShortString(RegisteredWindowFrameworks.GetCurrent));
  end;
  RegisteredWindowFrameworks.Free;

end.
