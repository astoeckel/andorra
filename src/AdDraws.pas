{
* This program is licensed under the GNU Lesser General Public License Version 2
* You should have recieved a copy of the license with this file.
* If not, see http://www.gnu.org/licenses/lgpl.html for more informations
*
* Project: Andorra 2D
* Author:  Andreas Stoeckel
* File: AdDraws.pas
* Comment: This unit contais the main Andorra 2D Component (TAdDraw) comparable to TDXDraw 
}

unit AdDraws;

interface

uses Windows, Classes, AndorraUtils, Andorra, Graphics;

type

  TAdDrawBitCount = byte;

  TAdDrawDisplay = record
    Width:integer;
    Height:integer;
    BitCount:TAdDrawBitCount;
  end;

  TAdDrawMode = (doFullscreen, doWaitVBlank, doStretch, doHardware, doZBuffer, doAntialias, doSystemMemory);
  TAdDrawModes = set of TAdDrawMode;

  TAdDraw = class
  private

    FParent:HWND;
    FOptions:TAdDrawModes;
    FDllName:string;
    FFinalize:TNotifyEvent;
    FInitialize:TNotifyEvent;
    FInitialized:boolean;

    procedure SetDllName(val : string);

    procedure SetupThings;

  protected
    AdAppl:TAndorraApplication;
    AdDllLoader : TAndorraDllLoader;

  public
    Display:TAdDrawDisplay;

    constructor Create(AParent : HWND);
    destructor Destroy; override;

    property Parent : HWND read FParent;

    function Initialize: boolean;
    procedure Finalize;
    function Restore: boolean;

    procedure ClearSurface(Color:TColor);
    procedure BeginScene;
    procedure EndScene;
    procedure Flip;

    function CanDraw:boolean;
  published
    property Options : TAdDrawModes read FOptions write FOptions;
    property DllName : string read FDllName write SetDllName;
    property Initialized : boolean read FInitialized;

    property OnFinalize : TNotifyEvent read FFinalize write FFinalize;
    property OnInitialize : TNotifyEvent read FInitialize write FInitialize;
  end;

implementation

{ TAdDraw }

constructor TAdDraw.Create(AParent : HWND);
begin
	inherited Create;
  FParent := AParent;
  AdDllLoader := TAndorraDllLoader.Create;

  SetupThings;
end;

procedure TAdDraw.SetupThings;
begin
  //Initialize all Parameters
  with Display do
  begin
    Width := 800;
    Height := 600;
    BitCount := 32;
  end;

  FOptions := [doHardware];
end;

destructor TAdDraw.Destroy;
begin
  //Free all loaded objects
  if AdAppl <> nil then
  begin
    AdDllLoader.DestroyApplication(AdAppl);
  end;
  
  AdDllLoader.Destroy;
	inherited Destroy;
end;

procedure TAdDraw.SetDllName(val : string);
begin
  if val <> FDllName then
  begin
    //If the Library is changed, the system will shut down
    Finalize;

    FDllName := val;

    //Free old Application
    if AdAppl <> nil then
    begin
      AdDllLoader.DestroyApplication(AdAppl);
    end;

    //Load the new Library
    AdDllLoader.LoadLibrary(val);

    //Create the new Application
    AdAppl := AdDllLoader.CreateApplication;
  end;
end;

function TAdDraw.Initialize: boolean;
var ARect:TRect;
begin

  result := false;

  if Assigned(FInitialize) then
  begin
    //OnInitialize
    FInitialize(Self);
  end;

  if (AdAppl <> nil) and (FParent <> 0) and (AdDllLoader.LibraryLoaded) then
  begin
    //Initialize Andorra 2D
    if doFullscreen in FOptions then
    begin
      //Set a new window position and change the borderstyle to WS_POPUP = bsNone
      SetWindowPos(FParent,HWND_TOPMOST,0,0,Display.Width,Display.Height,SWP_SHOWWINDOW);
      SetWindowLong(FParent,GWL_STYLE,WS_POPUP);

      result := AdDllLoader.InitDisplay(AdAppl,FParent, doHardware in FOptions,
            doFullscreen in FOptions, Display.BitCount, Display.Width, Display.Height);
    end
    else
    begin
      //Get the rect of the window
      GetWindowRect(FParent,ARect);

      result := AdDllLoader.InitDisplay(AdAppl,FParent, doHardware in FOptions,
            doFullscreen in FOptions, Display.BitCount, ARect.Right-ARect.Left, ARect.Bottom-ARect.Top);
    end;
  end;
end;

procedure TAdDraw.Finalize;
begin
  if Assigned(FFinalize) then
  begin
    FFinalize(Self);
  end;
end;

function TAdDraw.Restore: boolean;
begin
  result := true;
  //Nothing, has to be implemented in the DLL
end;

procedure TAdDraw.ClearSurface(Color:TColor);
begin
  AdDllLoader.ClearScene(AdAppl,Ad_RGB(GetRValue(Color),GetGValue(Color),GetBValue(Color)));
end;

procedure TAdDraw.BeginScene;
begin
  AdDllLoader.BeginScene(AdAppl);
end;

procedure TAdDraw.EndScene;
begin
  AdDllLoader.EndScene(AdAppl);
end;

procedure TAdDraw.Flip;
begin
  AdDllLoader.Flip(AdAppl);
end;

function TAdDraw.CanDraw:boolean;
begin
  //Only for compatibility
  result := true;
end;

end.
