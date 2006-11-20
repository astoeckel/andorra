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

{ Contains the main Andorra Classes for graphic output }
unit AdDraws;

interface

uses Windows, Classes, AndorraUtils, Andorra, Graphics;

type


  TAdDrawBitCount = byte;

  {Specifies the dimensions of the display.

  However, remember that this settings will be ignored, when fullscreen isn't turned on.
  To use fullscreen turn the "doFullscreen" property in the "Options" on.}
  TAdDrawDisplay = record
    //The Width of the Display
    Width:integer;
    //The Height of the Display
    Height:integer;
    //The Bitcount of the Display (May be 16 or 32 (and normaly 24, but this is, whyever, very buggy...) )
    BitCount:TAdDrawBitCount;
  end;

  {Specifies the options the application is created with.

  If you change these settings while running, simply call the "restore" function of TAdDraw.}
  TAdDrawMode = (
    doFullscreen,  //< Specifies weather the application should run in the fullscreen mode or not
    doWaitVBlank, //< If turned on, the frame rate is equal to the vertical frequenzy of the screen
    doStretch, //< Should the picture be stretched when the window resizes?
    doHardware,//< Run in hardware mode? (WARNING: Should be set!)
    doZBuffer, //< The ZBuffer has to be used if you are using 3D Objects in your scene
    doAntialias,//< should Antialiasing be used
    doSystemMemory//< use system memory instead of video memory for textures?
  );
  {Declares a set of TAdDrawMode. See above to learn what all these settings mean.}
  TAdDrawModes = set of TAdDrawMode;

  {This is the main class for using Andorra 2D. It is comparable to DelphiX's TDXDraw.}
  TAdDraw = class
  private

    FParent:HWND;
    FOptions:TAdDrawModes;
    FDllName:string;
    FFinalize:TNotifyEvent;
    FInitialize:TNotifyEvent;
    FInitialized:boolean;
    FDisplay:TAdDrawDisplay;
    FDisplayRect:TRect;

    procedure SetDllName(val : string);

    procedure SetupThings;

  protected
    DisplayWidth,DisplayHeight:integer;
  public
    {The Andorra Dll Loader. You can use this class to get direct control over
    the engine.}
    AdDllLoader : TAndorraDllLoader;
    {The Andorra Reference for the DllLoader}
    AdAppl:TAndorraApplication;

    //Create the class. AParent is the handle of the control, where displaying should take place.
    constructor Create(AParent : HWND);
    //This is a destroctor.
    destructor Destroy; override;

    //Here you can read the parent value, you've set in the constructor.
    property Parent : HWND read FParent;

    //The Rect the Displaing takes place
    property DisplayRect:TRect read FDisplayRect;

    //Initialize the application with all parameters set in "options". Returns false if the operation failed.
    function Initialize: boolean;
    //Finalize the application
    procedure Finalize;

    //Fills the Surface with a specific color.
    procedure ClearSurface(Color:TColor);
    //Starts the output. All graphic commands have to come after this command.
    procedure BeginScene;
    //Ends the output. All graphic commands have to come before this command.
    procedure EndScene;
    //Set the projection matrix and the camera to a 2D perspective.
    procedure Setup2DScene;
    //Flip the backbuffer and the frontbuffer and display the current picture.
    procedure Flip;

    //Returns weather Andorra is ready to draw
    function CanDraw:boolean;
  published
    //This property contains the diplay settings (width, height and bitcount)
    property Display : TAdDrawDisplay read FDisplay write FDisplay;
    //This property contains the options (see TAdDrawMode)
    property Options : TAdDrawModes read FOptions write FOptions;
    //Set this value to load a library
    property DllName : string read FDllName write SetDllName;
    //Returns weather the application is initialized
    property Initialized : boolean read FInitialized;

    //Event is called before the application is finalized
    property OnFinalize : TNotifyEvent read FFinalize write FFinalize;
    //Event is called after the application is initialized
    property OnInitialize : TNotifyEvent read FInitialize write FInitialize;
  end;

  {TAdTexture basicly loads a texture from a bitmap or a file into the video memory}
  TAdTexture = class
    private
      FParent:TAdDraw;
      FWidth:integer;
      FHeight:integer;
      FBaseRect:TRect;
      function GetLoaded:boolean;
    protected
    public
      {Link to the Andorra Texture.}
      AdTexture:TAndorraTexture;
      {This is a constructor. AADraw defines the parent andorra application.}
      constructor Create(AAdDraw:TAdDraw);
      {A destructor. What did you think?}
      destructor Destroy;override;
      {Creates a new andorra texture from a file. The old one will be flushed.}
      procedure LoadFromFile(afile:string;ATransparent:boolean;ATransparentColor:TColor);
      {Creates a new andorra texture from a bitmap. The old one will be flushed.}
      procedure LoadFromBitmap(ABitmap:TBitmap);
      {Add an alphachannel to the current texture. The bitmap has to have the same size as the loaded one.}
      procedure AddAlphaChannel(ABitmap:TBitmap);
      {Overide the alpha channel. 255 to make the texture completly opac.}
      procedure SetAlphaValue(AValue:byte);
      {Flush the texture.}
      procedure FreeTexture;

      {The parent TAdDraw.}
      property Parent:TAdDraw read FParent;
      {Returns weather a texture is loaded.}
      property Loaded:boolean read GetLoaded;
      {Returns the width of the texture. Use BaseRect.Right instead.}
      property Width:integer read FWidth;
      {Returns the height of the texture. Use BaseRect.Bottom instead.}
      property Height:integer read FHeight;
      {If a loaded texture has a size which sizes are not power of two, it will be resized.
      To keep the original image size it will be stored into BaseRect.}
      property BaseRect:TRect read FBaseRect;
  end;

  {A list which is able to contain TRects}
  type TRectList = class(TList)
    private
     	function GetItem(AIndex:integer):TRect;
     	procedure SetItem(AIndex:integer;AItem:TRect);
      protected
    public
      {Read/Write acess to the rectangles.}
     	property Items[AIndex:integer]:TRect read GetItem write SetItem;default;
      {Add a rectangle.}
      procedure Add(ARect:TRect);
      {Clear the list and all used memory.}
      procedure Clear;override;
    published
  end;

  TPictureCollectionItem = class
    private
      FParent:TAdDraw;
      FWidth,FHeight:integer;
      FPatternWidth,FPatternHeight:integer;
      FSkipWidth,FSkipHeight:integer;
      FTexture:TAdTexture;
      FColor:TColor;
      FLastColor:TAndorraColor;
      FName:string;
      FAddedByList:boolean;
      procedure SetPatternWidth(AValue:integer);
      procedure SetPatternHeight(AValue:integer);
      procedure SetSkipWidth(AValue:integer);
      procedure SetSkipHeight(AValue:integer);
      function GetPatternCount:integer;
      function GetWidth:integer;
      function GetHeight:integer;
      procedure SetCurrentColor(Alpha:byte);
    protected
      Rects:TRectList;
      procedure CreatePatternRects;
    public
      AdImage:TAndorraImage;
      constructor Create(AAdDraw:TAdDraw);
      destructor Destroy;override;
      procedure Draw(Dest:TAdDraw;X,Y,PatternIndex:integer);
      procedure StretchDraw(Dest:TAdDraw;const DestRect:TRect;PatternIndex:integer);
      procedure DrawAdd(Dest: TAdDraw; const DestRect: TRect; PatternIndex: Integer;
        Alpha: Integer);
      procedure DrawAlpha(Dest: TAdDraw; const DestRect: TRect; PatternIndex: Integer;
        Alpha: Integer);
      procedure DrawRotate(Dest: TAdDraw; X, Y, Width, Height: Integer; PatternIndex: Integer;
        CenterX, CenterY: Double; Angle: Integer);
      procedure DrawRotateAdd(Dest: TAdDraw; X, Y, Width, Height: Integer; PatternIndex: Integer;
        CenterX, CenterY: Double; Angle: Integer;
        Alpha: Integer);
      procedure DrawRotateAlpha(Dest: TAdDraw; X, Y, Width, Height: Integer; PatternIndex: Integer;
        CenterX, CenterY: Double; Angle: Integer;
        Alpha: Integer);
      procedure Restore;
      function GetPatternRect(ANr:integer):TRect;
      property Parent:TAdDraw read FParent write FParent;
      property Width:integer read GetWidth;
      property Height:integer read GetHeight;
      property PatternWidth:integer read FPatternWidth write SetPatternWidth;
      property PatternHeight:integer read FPatternHeight write SetPatternHeight;
      property SkipWidth:integer read FSkipWidth write SetSkipWidth;
      property SkipHeight:integer read FSkipHeight write SetSkipHeight;
      property Texture:TAdTexture read FTexture;
      property PatternCount:integer read GetPatternCount;
      property Color:TColor read FColor write FColor;
      property Name:string read FName write FName;
  end;

  TPictureCollection = class(TList)
    private
      FParent:TAdDraw;
     	function GetItem(AIndex:integer):TPictureCollectionItem;
     	procedure SetItem(AIndex:integer;AItem:TPictureCollectionItem);
    protected
      procedure Notify(Ptr: Pointer; Action: TListNotification); overload;
    public
     	property Items[AIndex:integer]:TPictureCollectionItem read GetItem write SetItem;default;
      function Add(AName:string):TPictureCollectionItem;
      function Find(AName:string):TPictureCollectionItem;
      procedure Restore;
      constructor Create(AAdDraw:TAdDraw);
      destructor Destroy;
      property Parent:TAdDraw read FParent;
    published
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

    //Load the new Library
    AdDllLoader.LoadLibrary(val);
  end;
end;

function TAdDraw.Initialize: boolean;
var ARect:TRect;
begin

  result := false;

  if not Initialized then
  begin
    //Create the new Application
    AdAppl := AdDllLoader.CreateApplication;

    if (AdAppl <> nil) and (FParent <> 0) and (AdDllLoader.LibraryLoaded) then
    begin
      //Initialize Andorra 2D
      if doFullscreen in FOptions then
      begin
        //Set a new window position and change the borderstyle to WS_POPUP = bsNone
        SetWindowPos(FParent,HWND_TOPMOST,0,0,FDisplay.Width,FDisplay.Height,SWP_SHOWWINDOW);
        SetWindowLong(FParent,GWL_STYLE,WS_POPUP);

        DisplayWidth := FDisplay.Width;
        DisplayHeight := FDisplay.Height;

        result := AdDllLoader.InitDisplay(AdAppl,FParent, doHardware in FOptions,
              doFullscreen in FOptions, FDisplay.BitCount, FDisplay.Width, Display.Height);
      end
      else
      begin
        //Get the rect of the window
        GetClientRect(FParent,ARect);

        DisplayWidth := ARect.Right-ARect.Left;
        DisplayHeight := ARect.Bottom-ARect.Top;

        result := AdDllLoader.InitDisplay(AdAppl,FParent, doHardware in FOptions,
              doFullscreen in FOptions, FDisplay.BitCount, DisplayWidth, DisplayHeight);
      end;
      FDisplayRect := Rect(0,0,DisplayWidth,DisplayHeight);
      AdDllLoader.SetTextureQuality(AdAppl,tqNone);
      Setup2DScene;
    end;

    if Assigned(FInitialize) then
    begin
      //OnInitialize
      FInitialize(Self);
    end;

    FInitialized := result;
  end;
end;

procedure TAdDraw.Finalize;
begin
  if Assigned(FFinalize) then
  begin
    FFinalize(Self);
  end;

  if AdAppl <> nil then
  begin
    AdDllLoader.DestroyApplication(AdAppl);
    AdAppl := nil;
    FInitialized := false;
  end;
end;

procedure TAdDraw.ClearSurface(Color:TColor);
begin
  AdDllLoader.ClearScene(AdAppl,Ad_RGB(GetRValue(Color),GetGValue(Color),GetBValue(Color)));
end;

procedure TAdDraw.BeginScene;
begin
  if AdAppl <> nil then
  begin
    AdDllLoader.BeginScene(AdAppl);
  end;
end;

procedure TAdDraw.EndScene;
begin
  if AdAppl <> nil then
  begin
    AdDllLoader.EndScene(AdAppl);
  end;
end;

procedure TAdDraw.Setup2DScene;
begin
  if AdAppl <> nil then
  begin
    AdDllLoader.SetupScene(AdAppl,DisplayWidth,DisplayHeight);
  end;
end;

procedure TAdDraw.Flip;
begin
  if AdAppl <> nil then
  begin
    AdDllLoader.Flip(AdAppl);  
  end;
end;

function TAdDraw.CanDraw:boolean;
begin
  result := (AdAppl <> nil) and (Initialized);
end;

{TAdTexture}

constructor TAdTexture.Create(AAdDraw:TAdDraw);
begin
  inherited Create;
  AdTexture := nil;
  FParent := AAdDraw;
end;

destructor TAdTexture.Destroy;
begin
  FreeTexture;
  inherited Destroy;
end;

procedure TAdTexture.LoadFromFile(afile:string;ATransparent:boolean;ATransparentColor:TColor);
var FTransparentColor:TAndorraColor;
    Info:TImageInfo;
begin
  FreeTexture;
  FTransparentColor := Ad_ARGB(0,0,0,0);
  if ATransparent then
  begin
    FTransparentColor := Ad_ARGB(255,GetRValue(ATransparentColor),GetGValue(ATransparentColor),GetBValue(ATransparentColor));
  end;
  AdTexture := FParent.AdDllLoader.LoadTextureFromFile(FParent.AdAppl,PChar(Afile),FTransparentColor);
  Info := FParent.AdDllLoader.GetTextureInfo(AdTexture);
  FWidth := Info.Width;
  FHeight := Info.Height;
  FBaseRect := Info.BaseRect;
end;

procedure TAdTexture.LoadFromBitmap(ABitmap:TBitmap);
var FColorDepth:byte;
    Info:TImageInfo;
begin
  FreeTexture;
  case ABitmap.PixelFormat of
    pf16bit: FColorDepth := 16;
    pf32bit: FColorDepth := 32;
  else
    FColorDepth := 32;
  end;
  AdTexture := FParent.AdDllLoader.LoadTextureFromBitmap(Fparent.AdAppl,ABitmap,FColorDepth);
  Info := FParent.AdDllLoader.GetTextureInfo(AdTexture);
  FWidth := Info.Width;
  FHeight := Info.Height;
  FBaseRect := Info.BaseRect;
end;

procedure TAdTexture.AddAlphaChannel(ABitmap:TBitmap);
begin
  if AdTexture <> nil then
  begin
    FParent.AdDllLoader.AddTextureAlphaChannel(AdTexture,ABitmap);
  end;
end;

procedure TAdTexture.SetAlphaValue(AValue:byte);
begin
  if AdTexture <> nil then
  begin
    FParent.AdDllLoader.SetTextureAlpha(AdTexture,AValue);
  end;
end;

procedure TAdTexture.FreeTexture;
begin
  if AdTexture <> nil then
  begin
    FParent.AdDllLoader.FreeTexture(AdTexture);
  end;
end;

function TAdTexture.GetLoaded:boolean;
begin
  result := AdTexture <> nil;
end;

{TRectList}

procedure TRectList.Add(ARect: TRect);
var ar:PRect;
begin
  new(ar);
  ar^ := ARect;
  inherited Add(ar);
end;

procedure TRectList.Clear;
begin
  while Count > 0 do
  begin
    Dispose(inherited Items[0]);
    Delete(0);
  end;
end;

function TRectList.GetItem(AIndex:integer):TRect;
begin
  result := PRect(inherited Items[AIndex])^;
end;

procedure TRectList.SetItem(AIndex:integer;AItem:TRect);
begin
  PRect(inherited Items[AIndex])^ := AItem;
end;


{TPictureCollectionItem}

constructor TPictureCollectionItem.Create(AAdDraw:TAdDraw);
begin
  inherited Create;
  FTexture := TAdTexture.Create(AAdDraw);
  FParent := AAdDraw;
  AdImage := FParent.AdDllLoader.CreateImage(AAdDraw.AdAppl);
  Rects := TRectList.Create;
  FColor := clWhite;
end;

destructor TPictureCollectionItem.Destroy;
begin
  FTexture.Free;
  FParent.AdDllLoader.DestroyImage(AdImage);
  Rects.Free;
  inherited Destroy;
end;

procedure TPictureCollectionItem.CreatePatternRects;
var ax,ay:integer;
begin
  Rects.Clear;
  with Texture.BaseRect do
  begin
    if (FPatternWidth <> 0) and (FPatternHeight <> 0) then
    begin
      for ay := 0 to ((Bottom+FSkipHeight) div (PatternHeight+FSkipHeight)) - 1 do
      begin
        for ax := 0 to ((Right+FSkipWidth) div (PatternWidth+FSkipWidth)) - 1 do
        begin
          Rects.Add(Bounds(
            ax*(PatternWidth+FSkipWidth),ay*(PatternHeight+FSkipHeight),
            Width,Height));
        end;
      end;
    end
    else
    begin
      Rects.Add(Rect(0,0,Right,Bottom));
    end;
  end;
end;

procedure TPictureCollectionItem.Draw(Dest:TAdDraw;X,Y,PatternIndex:integer);
begin
  if Texture.Loaded then
  begin
    SetCurrentColor(255);
    FParent.AdDllLoader.DrawImage(
      FParent.AdAppl,AdImage,Rect(X,Y,X+Width,Y+Height),Rects[PatternIndex],
      0,0,0,bmAlpha);
  end;
end;

procedure TPictureCollectionItem.DrawAdd(Dest: TAdDraw; const DestRect: TRect;
  PatternIndex, Alpha: Integer);
var CurCol:TAndorraColor;
begin
  if Texture.Loaded then
  begin
    SetCurrentColor(Alpha);
    FParent.AdDllLoader.DrawImage(
      FParent.AdAppl,AdImage,DestRect,Rects[PatternIndex],
      0,0,0,bmAdd);    
  end;
end;

procedure TPictureCollectionItem.DrawAlpha(Dest: TAdDraw; const DestRect: TRect;
  PatternIndex, Alpha: Integer);
var CurCol:TAndorraColor;
begin
  if Texture.Loaded then
  begin
    SetCurrentColor(Alpha);
    FParent.AdDllLoader.DrawImage(
      FParent.AdAppl,AdImage,DestRect,Rects[PatternIndex],
      0,0,0,bmAlpha);
  end;
end;

procedure TPictureCollectionItem.DrawRotate(Dest: TAdDraw; X, Y, Width, Height,
  PatternIndex: Integer; CenterX, CenterY: Double; Angle: Integer);
begin
  if Texture.Loaded then
  begin
    SetCurrentColor(255);
    FParent.AdDllLoader.DrawImage(
      FParent.AdAppl,AdImage,Rect(X,Y,X+Width,Y+Height),Rects[PatternIndex],
      Angle,CenterX,CenterY,bmAlpha);
  end;
end;

procedure TPictureCollectionItem.DrawRotateAdd(Dest: TAdDraw; X, Y, Width,
  Height, PatternIndex: Integer; CenterX, CenterY: Double; Angle,
  Alpha: Integer);
var CurCol:TAndorraColor;
begin
  if Texture.Loaded then
  begin
    SetCurrentColor(Alpha);
    FParent.AdDllLoader.DrawImage(
      FParent.AdAppl,AdImage,Rect(X,Y,X+Width,Y+Height),Rects[PatternIndex],
      Angle,CenterX,CenterY,bmAdd);
  end;
end;

procedure TPictureCollectionItem.DrawRotateAlpha(Dest: TAdDraw; X, Y, Width,
  Height, PatternIndex: Integer; CenterX, CenterY: Double; Angle,
  Alpha: Integer);
begin
  if Texture.Loaded then
  begin
    SetCurrentColor(Alpha);
    FParent.AdDllLoader.DrawImage(
      FParent.AdAppl,AdImage,Rect(X,Y,X+Width,Y+Height),Rects[PatternIndex],
      Angle,CenterX,CenterY,bmAlpha);
  end;
end;

procedure TPictureCollectionItem.StretchDraw(Dest: TAdDraw; const DestRect: TRect; PatternIndex: integer);
begin
  if Texture.Loaded then
  begin
    SetCurrentColor(255);
    FParent.AdDllLoader.DrawImage(
      FParent.AdAppl,AdImage,DestRect,Rects[PatternIndex],
      0,0,0,bmAlpha);
  end;
end;

procedure TPictureCollectionItem.Restore;
begin
  with FParent.AdDllLoader do
  begin
    if FTexture.Loaded then
    begin
      ImageLoadTexture(AdImage,FTexture.AdTexture);
      FWidth := Texture.BaseRect.Right;
      FHeight := Texture.BaseRect.Bottom;
      CreatePatternRects;
    end;
  end;
end;

procedure TPictureCollectionItem.SetPatternWidth(AValue: Integer);
begin
  FPatternWidth := AValue;
  CreatePatternRects;
end;

procedure TPictureCollectionItem.SetSkipHeight(AValue: integer);
begin
  FSkipHeight := AValue;
  CreatePatternRects;
end;

procedure TPictureCollectionItem.SetSkipWidth(AValue: integer);
begin
  FSkipWidth := AValue;
  CreatePatternRects;
end;

procedure TPictureCollectionItem.SetCurrentColor(Alpha: byte);
var CurCol:TAndorraColor;
begin
  if Texture.Loaded then
  begin
    CurCol := Ad_ARGB(Alpha,GetRValue(FColor),GetGValue(FColor),GetBValue(FColor));
    if not CompareColors(CurCol,FLastColor) then
    begin
      FParent.AdDllLoader.SetImageColor(AdImage,CurCol);
      FLastColor := CurCol;
    end;
  end;
end;

procedure TPictureCollectionItem.SetPatternHeight(AValue: Integer);
begin
  FPatternHeight := AValue;
  CreatePatternRects;
end;

function TPictureCollectionItem.GetHeight: integer;
begin
  Result := FPatternHeight;
  if (Result<=0) then
    Result := FHeight;
end;

function TPictureCollectionItem.GetWidth: integer;
begin
  Result := FPatternWidth;
  if (Result<=0) then
    Result := FWidth;
end;

function TPictureCollectionItem.GetPatternCount: integer;
begin
  result := Rects.Count;
end;

function TPictureCollectionItem.GetPatternRect(ANr: Integer):TRect;
begin
  result := Rects[ANr];
end;


{TPictureCollection}

function TPictureCollection.Add(AName: string): TPictureCollectionItem;
begin
  result := TPictureCollectionItem.Create(FParent);
  result.Name := AName;
  result.FAddedByList := true;
  inherited Add(result);
end;

constructor TPictureCollection.Create(AAdDraw: TAdDraw);
begin
  inherited Create;
  FParent := AAdDraw;
end;

destructor TPictureCollection.Destroy;
begin
  inherited Destroy;
end;

function TPictureCollection.Find(AName: string): TPictureCollectionItem;
var i:integer;
begin
  for i := 0 to Count - 1 do
  begin
    if Items[i].Name = AName then
    begin
      result := Items[i];
      break;
    end;
  end;
end;

function TPictureCollection.GetItem(AIndex:integer):TPictureCollectionItem;
begin
 result := inherited Items[AIndex];
end;

procedure TPictureCollection.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then
  begin
    with TPictureCollectionItem(Ptr) do
    begin
      if FAddedByList then
      begin
        Free;
      end;
    end;
  end;
end;

procedure TPictureCollection.Restore;
var i:integer;
begin
  for i := 0 to Count - 1 do
  begin
    Items[i].Restore;
  end;
end;

procedure TPictureCollection.SetItem(AIndex:integer;AItem:TPictureCollectionItem);
begin
 inherited Items[AIndex] := AItem;
end;




end.
