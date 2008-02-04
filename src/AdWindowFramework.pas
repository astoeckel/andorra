unit AdWindowFramework;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  AdPersistent, AdEvents, AdContainers;

type
  {Defines how the window is displayed.}
  TAdWindowDisplayMode = (
    dmWindowed,{< The window is displayed in the window mode.}
    dmScreenRes,{< The window is resized to the current screen resolution.}
    dmFullscreen{< The screen resolution will be changed and the window is displayed int the fullscreen mode.});

  {Defines the properties of the window}
  TAdDisplayProperties = record
    Width : integer;{< The width of the window.}
    Height : integer;{< The height of the window.}
    Mode : TAdWindowDisplayMode;{< The displaymode. @seealso(TAdWindowDisplayMode)}
    BitDepth : Byte;{< The window bit depth. May be 16- or 32-Bit.}
  end;

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
      property OnPaint : TAdNotifyEvent read FPaint write FPaint;
      property OnResize : TAdNotifyEvent read FResize write FResize;
      property OnActivate : TAdNotifyEvent read FActivate write FActivate;
      property OnDeactivate : TAdNotifyEvent read FDeactivate write FDeactivate;
      property OnClose : TAdCloseEvent read FClose write FClose;

      property OnClick : TAdClickEvent read FClick write FClick;
      property OnDblClick : TAdClickEvent read FDblClick write FDblClick;
      property OnMouseDown : TAdMouseEvent read FMouseDown write FMouseDown;
      property OnMouseMove : TAdMouseMoveEvent read FMouseMove write FMouseMove;
      property OnMouseUp : TAdMouseEvent read FMouseUp write FMouseUp;
      property OnMouseWheel : TAdMouseWheelEvent read FMouseWheel write FMouseWheel;

      property OnKeyPress : TAdKeyPressEvent read FKeyPress write FKeyPress;
      property OnKeyDown : TAdKeyEvent read FKeyDown write FKeyDown;
      property OnKeyUp : TAdKeyEvent read FKeyUp write FKeyUp;

      property OnIdle : TAdIdleEvent read FIdle write FIdle;
  end;

  TAdWindowFramework = class(TAdPersistent)
    private
      FTitle:string;
      FEvents:TAdEventHandler;
    protected
      function GetClientWidth:integer;virtual;abstract;
      function GetClientHeight:integer;virtual;abstract;
      procedure SetTitle(AValue:string);virtual;
    public
      constructor Create;virtual;
      destructor Destroy;override;

      function BindTo(AObj:Pointer):boolean;virtual;abstract;
      function InitDisplay(AProps:TAdDisplayProperties):boolean;virtual;abstract;

      procedure Run;virtual;abstract;
      procedure Terminate;virtual;abstract;

      function IdentStr:ShortString;

      property Title:string read FTitle write SetTitle;
      property Events:TAdEventHandler read FEvents;
      property ClientWidth:integer read GetClientWidth;
      property ClientHeight:integer read GetClientHeight;
  end;
  TAdWindowFrameworkClass = class of TAdWindowFramework;

  TAdHandleWindowFramework = class(TAdWindowFramework)
    protected
      FHandle:LongInt;
    public
      property Handle:LongInt read FHandle write FHandle;
  end;

  TAdGLContextGeneratingWindowFramework = class(TAdWindowFramework)
    public
      procedure Swap;virtual;abstract;
  end;

{Returns a TAdDisplayProperties record containing the specified parameters. @seealso(TAdDisplayProperties)}
function AdDisplayProperties(AWidth, AHeight:integer; AMode:TAdWindowDisplayMode;
  ABitDepth:byte):TAdDisplayProperties;

procedure RegisterWindowFramework(AWndFrmWrk:TAdWindowFrameworkClass);

var
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
  ABitDepth:byte):TAdDisplayProperties;
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
    result := result + '.' + aclass.ClassName;
    aclass := aclass.ClassParent;
  end;
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
