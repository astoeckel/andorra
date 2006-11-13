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

uses Windows, Classes, AndorraUtils, Andorra;

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
    FDisplay:TAdDrawDisplay;
    FOptions:TAdDrawModes;
    FDllName:string;
    FFinalize:TNotifyEvent;
    FInitialize:TNotifyEvent;
    FInitialized:boolean;

    procedure SetDllName(val : string);

    procedure SetUpThings;

  protected
    AdAppl:TAndorraApplication;
    AdDllLoader : TAndorraDllLoader;

  public
    constructor Create(AParent : HWND);
    destructor Destroy; override;

    property Parent : HWND read FParent;

    function Initialize: boolean;
    procedure Finalize;
    function Restore: boolean;

  published
    property Display : TAdDrawDisplay read FDisplay write FDisplay;
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
  AdDllLoader.Create;

  AdAppl := AdDllLoader.CreateApplication;

  SetUpThings;
end;

procedure TAdDraw.SetUpThings;
begin
  //Initialize all Parameters
  with FDisplay do
  begin
    Width := 800;
    Height := 600;
    BitCount := 32;
  end;

  FOptions := [doHardware];
end;

destructor TAdDraw.Destroy;
begin

  //Free all memory
  AdDllLoader.DestroyApplication(AdAppl);

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

    //Load the new Library
    AdDllLoader.LoadLibrary(val);
  end;
end;

function TAdDraw.Initialize: boolean;
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
    result := AdDllLoader.InitDisplay(AdAppl,FParent, doHardware in FOptions,
          doFullscreen in FOptions, FDisplay.BitCount, FDisplay.Width, FDisplay.Height);
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
  //Nothing
end;

end.
