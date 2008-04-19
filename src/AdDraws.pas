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
* File: AdDraws.pas
* Comment: This unit contais the main Andorra 2D Component (TAdDraw) comparable to TDXDraw
}

{ Contains the main Andorra Classes for graphic output }
unit AdDraws;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

{'$DEFINDE DO_NOT_INCLUDE_STD_FORMATS}
{'$DEFINDE DO_NOT_INCLUDE_STD_WINDOWMGR}              

uses

  SysUtils, Classes,
  AdClasses, AdTypes, AdList, AdPersistent,
  AdWindowFramework, AdDLLLoader,
  AdCanvas, AdBitmap, AdFontFactory, AdFont, AdEvents
  {$IFNDEF DO_NOT_INCLUDE_STD_FORMATS}
  ,AdStandardFontGenerator, AdSimpleCompressors, AdFormats
  {$ENDIF}
  {$IFNDEF DO_NOT_INCLUDE_STD_WINDOWMGR}
  ,AdComponentWindow
  {$ENDIF};

type

  ELoad = class(Exception);

  {This is the main class for using Andorra 2D.}
  TAdDraw = class;

  TAdCustomImage = class;
  TAdImage = class;
  TAdCustomTexture = class;
  TAdRenderTargetTexture = class;
  TAdTexture = class;


  {Specifies the event which called the procedure}
  TSurfaceEventState = (seInitialize,seFinalize,seInitialized,seBeginScene,seEndScene);
  {The declaration of the surface event handler}
  TSurfaceEvent = procedure(Sender:TObject;AEvent:TSurfaceEventState) of object;
  {A pointer on TSurfaceEvent}
  PSurfaceEvent = ^TSurfaceEvent;
  {A list which contains the surface events}
  TSurfaceEventList = class(TAdList)
    private
      function GetItem(AIndex:integer):TSurfaceEvent;
      procedure SetItem(AIndex:integer;AItem:TSurfaceEvent);
    protected
      procedure Notify(Ptr: Pointer; Action: TListNotification);override;
    public
      property Items[AIndex:integer]:TSurfaceEvent read GetItem write SetItem;default;
      procedure Add(Item:TSurfaceEvent);
      procedure Remove(Item:TSurfaceEvent);
  end;


  //A record for adding a new log entry into the log system
  TAdLogMessage = record
    Text:string;//< the text of the message
    Sender:string;//< the sender of the message. (May be the plugin or something else.)
    Typ:string;//< the typ of the message
  end;

  //The log system class
  TAdLog = class
    private
      FItems:TStringList;
    public
      FileName:string;

      constructor Create;
      destructor Destroy;override;
      procedure LoadFromFile(AFile:string);
      procedure SaveToFile(AFile:string);
      procedure AddMessage(AMessage:TAdLogMessage);virtual;

      property Items:TStringList read FItems;
  end;

  TAdSurface = class
    private
      FDraw : TAdDraw;
      FActivated: boolean;
      procedure SetDraw(AValue:TAdDraw);
    protected
      property AdDraw:TAdDraw read FDraw write SetDraw;

      procedure DoActivation;virtual;
      procedure DoDeactivation;virtual;      
      procedure Notify(ASender:TObject;AEvent:TSurfaceEventState);virtual;
    public
      constructor Create(ADraw:TAdDraw);virtual;

      procedure Activate;
      procedure Deactivate;
      property Activated: boolean read FActivated write FActivated;

      procedure ClearSurface(AColor: Cardinal);virtual;

      function CanDraw:boolean;virtual;abstract;

      property Parent: TAdDraw read FDraw;
  end;

  TAdRenderingSurface = class(TAdSurface)
    private
      FWidth, FHeight: integer;
      FSceneWidth, FSceneHeight: integer;
      FCanvas: TAdCanvas;
      FViewPort: TAdRect;
      FViewMatrix: TAdMatrix;
      FProjectionMatrix: TAdMatrix;
      procedure SetViewPort(AValue:TAdRect);
      procedure SetViewMatrix(AValue:TAdMatrix);
      procedure SetProjectionMatrix(AValue:TAdMatrix);
      procedure UpdateMatrix;
      function GetDisplayRect: TAdRect;
    protected
      procedure Notify(ASender:TObject;AEvent:TSurfaceEventState);override;
      procedure DoInitialize;virtual;
      procedure DoFinalize;virtual;
      procedure DoBeginScene;virtual;
      procedure DoEndScene;virtual;
      procedure DoActivation;override;
      procedure CanvasRelease(Sender:TObject);
      property Width: integer read FWidth write FWidth;
      property Height: integer read FHeight write FHeight;
      property SceneWidth: integer read FSceneWidth;
      property SceneHeight: integer read FSceneHeight;
    public
      procedure Setup2DScene(AWidth, AHeight: integer);overload;
      procedure Setup2DScene;overload;
      procedure Setup3DScene(AWidth, AHeight: integer; APos, ADir, AUp: TAdVector3);

      property ViewMatrix:TAdMatrix read FViewMatrix write SetViewMatrix;
      property ProjectionMatrix:TAdMatrix read FProjectionMatrix write SetProjectionMatrix;
      property Viewport:TAdRect read FViewPort write SetViewPort;

      property DisplayRect: TAdRect read GetDisplayRect;

      property Canvas:TAdCanvas read FCanvas;
  end;

  TAdTextureSurface = class(TAdRenderingSurface)
    private
      FImage : TAdCustomImage;
      FTexture: TAdRenderTargetTexture;
    protected
      procedure DoActivation;override;
    public
      constructor Create(ADraw:TAdDraw);override;
      destructor Destroy;override;

      procedure ClearSurface(AColor: Cardinal);override;
      procedure SetSize(AWidth, AHeight: integer);

      function CanDraw:boolean;override;

      property Image:TAdCustomImage read FImage;
      property Texture:TAdRenderTargetTexture read FTexture;
  end;

  {This is the main class for using Andorra 2D. It builds the main surface.}
  TAdDraw = class(TAdRenderingSurface)
  private

    FParent:Pointer;
    FOptions:TAdOptions;
    FDllName:string;
    FFinalize:TNotifyEvent;
    FInitialize:TNotifyEvent;
    FInitialized:boolean;
    FDisplayRect:TAdRect;

    FAmbientColor:LongInt;

    FSurfaceEventList:TSurfaceEventList;
    FFonts:TAdFontFactory;

    FTextureFilter:TAd2dTextureFilter;
    FWnd:TAdWindowFramework;

    FActiveSurface:TAdSurface;

    procedure SetDllName(val : string);
    procedure SetupThings;
    procedure SetOptions(AValue:TAdOptions);
    function GetDisplayRect:TAdRect;
    function SearchWindowFramework:boolean;
    procedure SetActiveSurface(ASurface:TAdSurface);
  protected
    procedure DoActivation;override;
    procedure CallNotifyEvent(AEventState:TSurfaceEventState);
  public
    AdDllLoader : TAdDllLoader;
    AdAppl:TAd2DApplication;
    Display : TAdDisplay;

    constructor Create(AParent: Pointer);reintroduce;
    destructor Destroy; override;

    function Initialize: boolean;
    procedure Finalize;

    procedure Flip;
    procedure BeginScene;
    procedure EndScene;

    property Parent : Pointer read FParent;
    property DisplayRect:TAdRect read FDisplayRect;

    procedure LogProc(LogItem:TAdLogItem);

    procedure RegisterNotifyEvent(AProc:TSurfaceEvent);
    procedure UnRegisterNotifyEvent(AProc:TSurfaceEvent);

    procedure Run;

    procedure ClearSurface(AColor: Cardinal);override;

    function GetTextureParams(ABitDepth:byte):TAd2dBitmapTextureParameters;

    function CanDraw:boolean;override;

    property Options : TAdOptions read FOptions write SetOptions;
    property TextureFilter : TAd2dTextureFilter read FTextureFilter write FTextureFilter;
    property DllName : string read FDllName write SetDllName;
    property Initialized : boolean read FInitialized;

    property Fonts:TAdFontFactory read FFonts;
    property Window:TAdWindowFramework read FWnd;
    property ActiveSurface:TAdSurface read FActiveSurface write SetActiveSurface;

    property OnFinalize : TNotifyEvent read FFinalize write FFinalize;
    property OnInitialize : TNotifyEvent read FInitialize write FInitialize;
  end;

  TAdLight = class
    private
      FParent:TAdDraw;
      FX,FY,FZ:double;
      FRange:double;
      FFalloff:double;
      FColor:TAndorraColor;
      procedure SetX(AValue:double);
      procedure SetY(AValue:double);
      procedure SetZ(AValue:double);
      procedure SetRange(AValue:double);
      procedure SetColor(AValue:TAndorraColor);
      procedure SetFalloff(AValue:double);
    protected
      procedure Notify(ASender:TObject;AEvent:TSurfaceEventState);
    public
      AdLight:TAd2DLight;
      constructor Create(AParent:TAdDraw);
      destructor Destroy;override;

      procedure Restore;

      procedure Enable;
      procedure Disable;

      procedure Initialize;
      procedure Finalize;

      property X:double read FX write SetX;
      property Y:double read FY write SetY;
      property Z:double read FZ write SetZ;
      property Range:double read FRange write SetRange;
      property Color:TAndorraColor read FColor write SetColor;
      property Falloff:double read FFalloff write SetFalloff;
  end;

  TAdCustomTexture = class
    private
      FParent:TAdDraw;
      FBitDepth: Byte;

      function GetBitDepth:byte;
      function GetInitialized:boolean;
      procedure SetBitDepth(AValue:byte);
      procedure SetAd2DTexture(AValue: TAd2dTexture);
    protected
      FOwnTexture: Boolean;
      FAd2dTexture:TAd2dTexture;
    public
      constructor Create(AParent:TAdDraw);virtual;
      destructor Destroy;override;

      procedure Initialize;virtual;abstract;
      procedure Finalize;virtual;abstract;

      procedure Clear;virtual;abstract;

      property Texture: TAd2dTexture read FAd2DTexture write SetAd2DTexture;
      property Initialized: boolean read GetInitialized;
      property BitDepth: Byte read GetBitDepth write SetBitDepth;
      property Parent: TAdDraw read FParent;
  end;

  TAdRenderTargetTexture = class(TAdCustomTexture)
    private
      FWidth : integer;
      FHeight : integer;
      function GetTexture:TAd2dRenderTargetTexture;
      procedure SetTexture(AValue:TAd2dRenderTargetTexture);

      procedure SetWidth(AValue: integer);
      procedure SetHeight(AValue: integer);
      procedure UpdateSize;
    protected
      procedure Notify(ASender:TObject;AEvent:TSurfaceEventState);
    public
      constructor Create(AParent:TAdDraw);override;
      destructor Destroy;override;

      procedure Clear;override;
      procedure Initialize;override;
      procedure Finalize;override;

      property Texture:TAd2dRenderTargetTexture read GetTexture write SetTexture;
      property Width: integer read FWidth write SetWidth;
      property Height: integer read FHeight write SetHeight;    
  end;

  TAdTexture = class(TAdCustomTexture)
    private
      FCache: TMemoryStream;
      FCompressorClass:TAdGraphicCompressorClass;
      function GetTexture:TAd2dBitmapTexture;
      procedure SetTexture(AValue:TAd2dBitmapTexture);
    protected
      procedure Notify(ASender:TObject;AEvent:TSurfaceEventState);
    public
      constructor Create(AParent:TAdDraw);override;
      destructor Destroy;override;

      procedure Clear;override;
      procedure Initialize;override;
      procedure Finalize;override;

      procedure LoadFromStream(AStream:TStream);
      procedure SaveToStream(AStream:TStream);
      procedure LoadFromFile(AFile:string);
      procedure SaveToFile(AFile:string);

      procedure LoadFromGraphic(AGraphic:TObject);
      procedure LoadGraphicFromFile(AFile: string; Transparent: boolean = true;
        TransparentColor: Longint = $1FFFFFFF);
      procedure SaveToGraphic(AGraphic:TObject);

      property Compressor:TAdGraphicCompressorClass read FCompressorClass write FCompressorClass;
      property Texture: TAd2dBitmapTexture read GetTexture write SetTexture;
  end;

  TRectList = class(TAdList)
    private
     	function GetItem(AIndex:integer):TAdRect;
     	procedure SetItem(AIndex:integer;AItem:TAdRect);
    protected
      procedure Notify(Ptr: Pointer; Action: TListNotification);override;
    public
      {Read/Write acess to the rectangles.}
     	property Items[AIndex:integer]:TAdRect read GetItem write SetItem;default;
      {Add a rectangle.}
      procedure Add(ARect:TAdRect);
  end;

  //This represents one image in an ImageList.
  TAdCustomImage = class
    private
      FParent:TAdDraw;
      FWidth,FHeight:integer;
      FPatternWidth,FPatternHeight:integer;
      FPatternStop:integer;
      FSkipWidth,FSkipHeight:integer;
      FColor:LongInt;
      FLastColor:TAndorraColor;
      FAlpha:byte;
      FDetails:integer;
      FSrcRect:TAdRect;
      FUseIndexBuffer:boolean;
      procedure SetPatternWidth(AValue:integer);
      procedure SetPatternHeight(AValue:integer);
      procedure SetSkipWidth(AValue:integer);
      procedure SetSkipHeight(AValue:integer);
      function GetPatternCount:integer;
      function GetWidth:integer;
      function GetHeight:integer;
      function GetColor:TAndorraColor;
      procedure SetCurrentColor(Alpha:byte);
      procedure SetDetails(AValue:integer);
      procedure SetTexture(AValue:TAdCustomTexture);
    protected
      FOwnTexture:boolean;
      FTexture:TAdCustomTexture;

      Rects:TRectList;
      procedure DrawMesh(DestApp:TAdSurface;DestRect,SourceRect:TAdRect;Rotation:integer;
        RotCenterX,RotCenterY:single;BlendMode:TAd2DBlendMode);
      procedure BuildVertices;
      procedure CreatePatternRects;
      procedure Notify(ASender:TObject;AEvent:TSurfaceEventState);    
    public
      //Contains the link to Andorras Image
      AdMesh:TAd2DMesh;

      //A Constructor
      constructor Create(AAdDraw:TAdDraw);virtual;
      //A Destructor
      destructor Destroy;override;

      //Draws the image at a specified position. If you've set "PatternWidth" and "PatternHeight", this will draw the pattern you've specified in PatternIndex.
      procedure Draw(Dest:TAdSurface;X,Y,PatternIndex:integer);
      //The same as Draw, but you can stretch the Image.
      procedure StretchDraw(Dest:TAdSurface;const DestRect:TAdRect;PatternIndex:integer);
      //Draw a sprite with additive blending.
      procedure DrawAdd(Dest: TAdSurface; const DestRect: TAdRect; PatternIndex: Integer;
        Alpha: Integer);
      //Draw a sprite with alpha blending.
      procedure DrawAlpha(Dest: TAdSurface; const DestRect: TAdRect; PatternIndex: Integer;
        Alpha: Integer);
      //Draw only the mask.
      procedure DrawMask(Dest: TAdSurface; const DestRect: TAdRect; PatternIndex: Integer;
        Alpha: Integer);
      //Draw a sprite rotated. CenterX and CenterY specify the center of the rotation - May be a value between 0 and 1. Rotation is a value between 0 and 360.
      procedure DrawRotate(Dest: TAdSurface; X, Y, Width, Height: Integer; PatternIndex: Integer;
        CenterX, CenterY: Double; Angle: Integer);
      //The same as DrawRotate, just with additive blending.
      procedure DrawRotateAdd(Dest: TAdSurface; X, Y, Width, Height: Integer; PatternIndex: Integer;
        CenterX, CenterY: Double; Angle: Integer;
        Alpha: Integer);
      //The same as DrawRotate, just with alpha blending.
      procedure DrawRotateAlpha(Dest: TAdSurface; X, Y, Width, Height: Integer; PatternIndex: Integer;
        CenterX, CenterY: Double; Angle: Integer;
        Alpha: Integer);
      //The same as DrawRotate, just drawing a the mask.
      procedure DrawRotateMask(Dest: TAdSurface; X, Y, Width, Height: Integer; PatternIndex: Integer;
        CenterX, CenterY: Double; Angle: Integer;
        Alpha: Integer);
      //Draw only specified part from the image. Alpha blending.
      procedure StretchBltAlpha(Dest:TAdSurface; SourceRect,DestRect:TAdRect;CenterX,CenterY:double;Angle:Integer;Alpha:Integer);
      //Draw only specified part from the image. Additive blending.
      procedure StretchBltAdd(Dest:TAdSurface; SourceRect,DestRect:TAdRect;CenterX,CenterY:double;Angle:Integer;Alpha:Integer);

      //If you've set the color or a new texture you have to call this function to see your changes.
      procedure Restore;

      //Frees all data
      procedure Finalize;
      //Restores all freed date
      procedure Initialize;

      //Returns the rect of one pattern.
      function GetPatternRect(ANr:integer):TAdRect;
      //Returns the parent you've set in the constructor
      property Parent:TAdDraw read FParent write FParent;

      //Returns the width of the image.
      property Width:integer read GetWidth;
      //Returns the height of the image.
      property Height:integer read GetHeight;
      //Set the width of one pattern.
      property PatternWidth:integer read FPatternWidth write SetPatternWidth;
      //Set the height of one pattern.
      property PatternHeight:integer read FPatternHeight write SetPatternHeight;
      //The horizontal space between the patterns.
      property SkipWidth:integer read FSkipWidth write SetSkipWidth;
      //The vertical space between the patterns.
      property SkipHeight:integer read FSkipHeight write SetSkipHeight;

      //The texture which will be painted.
      property Texture:TAdCustomTexture read FTexture write SetTexture;

      //Returns the count of the patterns.
      property PatternCount:integer read GetPatternCount;
      //If you have empty patterns, you may set PatternStop. PatternCount will be decrased by PatternStop.
      property PatternStop:integer read FPatternStop write FPatternStop;

      //Defines the color the image is drawn in.
      property Color:Longint read FColor write FColor;
      //Important for using lights: How many vertices does the image have.
      property Details:integer read FDetails write SetDetails;
  end;

  TAdImageList = class;

  TAdImage = class(TAdCustomImage)
    private
      FName:string;
      function GetTexture:TAdTexture;
      procedure SetTexture(AValue:TAdTexture);
    protected

    public
      //Contains a pointer to the image list which created the image. Set to nil if you don't want the image to be freed by the ImageList
      FreeByList:TAdImageList;

      //A Constructor
      constructor Create(AAdDraw:TAdDraw);override;

      //Assings the settings of another item
      procedure Assign(AItem:TAdImage);

      //Saves the image to a stream
      procedure SaveToStream(AStream:TStream);
      //Loads the image from a stream
      procedure LoadFromStream(AStream:TStream);
      //Saves the image to a file
      procedure SaveToFile(AFile:string);
      //Loads the image from a file
      procedure LoadFromFile(AFile:string);        

      //Name of the image in the imagelist.
      property Name:string read FName write FName;

      //Access to the texture abstraction layer
      property Texture:TAdTexture read GetTexture write SetTexture;
  end;

  //Administrates the images
  TAdImageList = class(TAdList)
    private
      FParent:TAdDraw;
      FCompressor:TAdGraphicCompressorClass;
     	function GetItem(AIndex:integer):TAdImage;
     	procedure SetItem(AIndex:integer;AItem:TAdImage);
      procedure SetCompressor(ACompressor:TAdGraphicCompressorClass);
    protected
      procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    public
      //Returns you an item
     	property Items[AIndex:integer]:TAdImage read GetItem write SetItem;default;
      //Add a new image to the list.
      function Add(AName:string):TAdImage;overload;
      //Returns the index of a item
      function IndexOf(AName:string):integer;
      //Finds an image in the list.
      function Find(AName:string):TAdImage;
      //Returns a new imagelist which contains all images which name contains the given substring
      function FindEx(ASubStr:string):TAdImageList;
      //Call the restore function of every item in the list.
      procedure Restore;
      //A constructor
      constructor Create(AAdDraw:TAdDraw);
      //A destructor
      destructor Destroy;override;
      //Save the whole list to a stream
      procedure SaveToStream(AStream:TStream);
      //Load a whole list from a stream
      procedure LoadFromStream(AStream:TStream);
      //Saves the whole list to a file
      procedure SaveToFile(AFile:string);
      //Loads a whole list from a file
      procedure LoadFromFile(AFile:string);
      //The parent you've specified in the constructor.
      property Parent:TAdDraw read FParent;
      //Apply the same compressor on every item
      property Compressor:TAdGraphicCompressorClass read FCompressor write SetCompressor;
    published
  end;

implementation

{ TAdDraw }

constructor TAdDraw.Create(AParent : Pointer);
begin
	inherited Create(nil);
  FParent := AParent;
  FAmbientColor := RGB(255, 255, 255);
  AdDllLoader := TAdDllLoader.Create;
  SetupThings;

  FTextureFilter := atPoint;
  FSurfaceEventList := TSurfaceEventList.Create;

  AdDraw := self;
end;

procedure TAdDraw.SetupThings;
begin
  //Initialize all Parameters
  with Display do
  begin
    Width := 800;
    Height := 600;
    BitCount := 32;
    Freq := 0;
  end;

  FOptions := [doHardware];
end;

destructor TAdDraw.Destroy;
begin
  //Free all loaded objects
  if AdAppl <> nil then
  begin
    Finalize;
  end;
  
  AdDllLoader.Destroy;

  FSurfaceEventList.Free;

	inherited Destroy;
end;

procedure TAdDraw.DoActivation;
begin 
  if Initialized then
  begin
    AdAppl.SetRenderTarget(nil);
    inherited DoActivation;
  end;
end;

procedure TAdDraw.UnRegisterNotifyEvent(AProc: TSurfaceEvent);
begin
  FSurfaceEventList.Remove(AProc)
end;

procedure TAdDraw.RegisterNotifyEvent(AProc: TSurfaceEvent);
begin
  FSurfaceEventList.Add(AProc);
end;    

procedure TAdDraw.Run;
begin
  if FWnd <> nil then
    FWnd.Run;
end;

procedure TAdDraw.SetActiveSurface(ASurface: TAdSurface);
begin
  if FActiveSurface <> nil then
    FActiveSurface.Deactivate;

  FActiveSurface := ASurface;
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

procedure TAdDraw.SetOptions(AValue:TAdOptions);
begin
  FOptions := AValue;
  if Initialized then
  begin
    AdAppl.Options := AValue;
  end;
end;

function TAdDraw.SearchWindowFramework: boolean;
var
  PSS:^ShortString;
  cref:TAdWindowFrameworkClass;
begin
  result := false;
  RegisteredWindowFrameworks.StartIteration;
  while not RegisteredWindowFrameworks.ReachedEnd do
  begin
    PSS := RegisteredWindowFrameworks.GetCurrent;
    cref := TAdWindowFrameworkClass(AdGetClass(PSS^));
    FWnd := cref.Create;
    result := FWnd.BindTo(FParent) and AdAppl.SupportsWindowFramework(FWnd.IdentStr);
    if not result then
    begin
      FWnd.Free; FWnd := nil;
    end else
    begin
      exit;
    end;
  end;
end;

function TAdDraw.Initialize: boolean;
var
  rect:TAdRect;
  props:TAdDisplayProperties;
begin

  result := false;
  if AdDllLoader.LibraryLoaded then
  begin

    //Create the new Application
    AdAppl := AdDllLoader.CreateApplication;
    
    if AdAppl <> nil then
    begin
      //Give the Plugin the possibility to send logs
      AdAppl.SetLogProc(LogProc);

      if SearchWindowFramework then
      begin
        props.Width := Display.Width;
        props.Height := Display.Height;
        props.BitDepth := Display.BitCount;
        if doFullscreen in Options then
          props.Mode := dmFullscreen
        else
          props.Mode := dmWindowed;
          
        FWnd.InitDisplay(props);

        FDisplayRect := GetDisplayRect;
        Display.Width := FDisplayRect.Right;
        Display.Height := FDisplayRect.Bottom;

        result := AdAppl.Initialize(FWnd,Options,Display);

        rect.Left := 0;
        rect.Top := 0;
        rect.Right := FWnd.ClientWidth;
        rect.Bottom := FWnd.ClientHeight;
        AdAppl.Viewport := rect;

        Width := FWnd.ClientWidth;
        Height := FWnd.ClientHeight;
      end else
      begin
        AdAppl.Free; AdAppl := nil;
      end;
    end
    else
    begin
    end;

    if Assigned(FInitialize) then
    begin
      //OnInitialize
      FInitialize(Self);
    end;

    FInitialized := result;

    if CanDraw then
    begin
      FFonts := TAdFontFactory.Create(AdAppl);

      CallNotifyEvent(seInitialize);
      CallNotifyEvent(seInitialized);

      ViewPort := AdRect(0,0,Width,Height);
      Setup2DScene(Width, Height);
    end;
  end;
end;

procedure TAdDraw.Finalize;
begin
  if Assigned(FFinalize) then
  begin
    FFinalize(Self);
  end;

  if FWnd <> nil then
    FWnd.Free;

  if AdAppl <> nil then
  begin
    if FFonts <> nil then
    begin
      FFonts.Free;
    end;
    
    FInitialized := false;
    CallNotifyEvent(seFinalize);
    
    AdAppl.Finalize;
    if AdAppl <> nil then FreeAndNil(AdAppl);
  end;
end;

procedure TAdDraw.LogProc(LogItem: TAdLogItem);
var Temp:TAdLogMessage;
begin
  Temp.Sender := self.ClassName;
  case LogItem.Typ of
    ltInfo: Temp.Typ := 'Info';
    ltWarning: Temp.Typ := 'Warning';
    ltError: Temp.Typ := 'Error';
    ltFatalError: Temp.Typ := 'Fatal Error';
    ltNone: Temp.Typ := 'Info';
  end;
  Temp.Text := PChar(LogItem.Text);
//  Log.AddMessage(Temp);
end;

procedure TAdDraw.ClearSurface(AColor: Cardinal);
begin
  inherited;
  
  AdAppl.ClearSurface(Ad_ARGB(255,
    GetRValue(AColor),
    GetGValue(AColor),
    GetBValue(AColor)));
end;

procedure TAdDraw.BeginScene;
begin
  if AdAppl <> nil then
  begin
    AdAppl.BeginScene;
    CallNotifyEvent(seBeginScene);
  end;
end;

procedure TAdDraw.EndScene;
begin
  if AdAppl <> nil then
  begin
    CallNotifyEvent(seEndScene);
    AdAppl.EndScene;
  end;
end;

procedure TAdDraw.Flip;
begin
  if AdAppl <> nil then
  begin
    AdAppl.Flip;
  end;
end;

function TAdDraw.GetTextureParams(
  ABitDepth: byte): TAd2dBitmapTextureParameters;
begin
  with result do
  begin
    if BitDepth <> 0 then    
      BitDepth := ABitDepth
    else
      BitDepth := Display.BitCount;  

    UseMipMaps := false;//doMipmaps in FOptions;
    MinFilter := FTextureFilter;
    MagFilter := FTextureFilter;
    MipFilter := FTextureFilter;
  end;
end;

function TAdDraw.GetDisplayRect: TAdRect;
begin
  if dofullscreen in Options then
  begin
    result := AdBounds(0,0,Display.Width,Display.Height);
  end
  else
  begin
    result := AdBounds(0,0,FWnd.ClientWidth,FWnd.ClientHeight);
  end;
end;

procedure TAdDraw.CallNotifyEvent(AEventState: TSurfaceEventState);
var i:integer;
begin
  i := 0;
  while i < FSurfaceEventList.Count do
  begin
    FSurfaceEventList.Items[i](self,AEventState);
    i := i + 1;
  end;
end;

function TAdDraw.CanDraw:boolean;
begin
  result := (AdAppl <> nil) and (Initialized);
end;

{TRectList}

procedure TRectList.Add(ARect: TAdRect);
var ar:PAdRect;
begin
  new(ar);
  ar^ := ARect;
  inherited Add(ar);
end;

function TRectList.GetItem(AIndex:integer):TAdRect;
begin
  result := PAdRect(inherited Items[AIndex])^;
end;

procedure TRectList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then
  begin
    Dispose(PAdRect(Ptr));
  end;
  inherited;
end;

procedure TRectList.SetItem(AIndex:integer;AItem:TAdRect);
begin
  PAdRect(inherited Items[AIndex])^ := AItem;
end;


{TPictureCollectionItem}

constructor TAdCustomImage.Create(AAdDraw:TAdDraw);
begin
  inherited Create;

  FTexture := nil;

  FParent := AAdDraw;
  FParent.RegisterNotifyEvent(Notify);
  Rects := TRectList.Create;
  FColor := RGB(255, 255, 255);
  FAlpha := 255;
  FPatternStop := 0;
  FDetails := 1;
  Initialize;
end;

destructor TAdCustomImage.Destroy;
begin
  if (FOwnTexture) and (FTexture <> nil) then
  begin
    FTexture.Free;
  end;
  Rects.Free;
  FParent.UnRegisterNotifyEvent(Notify);
  Finalize;
  inherited Destroy;
end;

procedure TAdCustomImage.DrawMesh(DestApp: TAdSurface; DestRect,
  SourceRect: TAdRect; Rotation: integer; RotCenterX, RotCenterY: single;
  BlendMode: TAd2DBlendMode);
var
  mat1,mat2:TAdMatrix;
  curx,cury:single;
  Mode:TAd2DDrawMode;
begin
  if DestApp <> nil then
    DestApp.Activate;

  if (not CompareRects(SourceRect,FSrcRect)) then
  begin
    FSrcRect := SourceRect;
    BuildVertices;
  end;

  //Initialize the matrix
  mat1 := AdMatrix_Identity;
  mat2 := AdMatrix_Identity;

  //Set the scale matrix
  mat1 := AdMatrix_Scale((DestRect.Right-DestRect.Left)/FWidth,(DestRect.Bottom-DestRect.Top)/FHeight,1);
  mat2 := AdMatrix_Multiply(mat1,mat2);

  if (Rotation <> 0) then
  begin
    CurX := (DestRect.Right-DestRect.Left)*RotCenterX;
    CurY := (DestRect.Bottom-DestRect.Top)*RotCenterY;

    mat1 := AdMatrix_Translate(-CurX,-CurY,0);
    mat2 := AdMatrix_Multiply(mat2,mat1);

    mat1 := AdMatrix_RotationZ(Rotation/360*2*PI);
    mat2 := AdMatrix_Multiply(mat2,mat1);

    mat1 := AdMatrix_Translate(CurX,CurY,0);
    mat2 := AdMatrix_Multiply(mat2,mat1);
  end;

  //Translate the image
  mat1 := AdMatrix_Translate(DestRect.Left,DestRect.Top,0);
  mat2 := AdMatrix_Multiply(mat2,mat1);

  AdMesh.SetMatrix(mat2); 

  if AdMesh.UseIndexBuffer then
  begin
    Mode := adTriangles;
  end
  else
  begin
    Mode := adTriangleStrips;
  end;
  
  AdMesh.Draw(BlendMode,Mode);
end;

procedure TAdCustomImage.BuildVertices;
var
  Vertices:TAdVertexArray;
  Indices:TAdIndexArray;
  i,x,y:integer;
  ax,ay:double;
  w,h:integer;
  vc,ic:integer;
  c:TAndorraColor;
begin
  if AdMesh <> nil then
  begin
    vc := (FDetails+1)*(FDetails+1);
    ic := FDetails*FDetails*6;

    SetLength(Vertices,vc);
    SetLength(Indices,ic);

    FUseIndexBuffer := FDetails > 1;

    w := FSrcRect.Right - FSrcRect.Left;
    h := FSrcRect.Bottom - FSrcRect.Top;
    c := GetColor;

    i := 0;
    for y := 0 to FDetails do
    begin
      for x := 0 to FDetails do
      begin
        ay := y*fheight/FDetails;
        ax := x*fwidth/FDetails;
        Vertices[i].Position := AdVector3(ax,ay,0);
        Vertices[i].Color := c;
        Vertices[i].Texture :=
          AdVector2(
            (FSrcRect.Left + w/FDetails*x) / Texture.Texture.Width,
            (FSrcRect.Top  + h/FDetails*y) / Texture.Texture.Height);
        Vertices[i].Normal := AdVector3(0,0,-1);
        i := i + 1;
      end;
    end;
    AdMesh.Vertices := Vertices;

    if FUseIndexBuffer then
    begin
      i := 0;
      for y := 0 to FDetails - 1 do
      begin
        for x := 0 to FDetails - 1 do
        begin
          Indices[i] :=   y     * (FDetails+1) + x + 1;
          Indices[i+1] := (y+1) * (FDetails+1) + x;
          Indices[i+2] := y     * (FDetails+1) + x;
          Indices[i+3] := y     * (FDetails+1) + x + 1;
          Indices[i+4] := (y+1) * (FDetails+1) + x + 1;
          Indices[i+5] := (y+1) * (FDetails+1) + x;
          i := i + 6;
        end;
      end;
      AdMesh.IndexBuffer := Indices;
    end
    else
    begin
      AdMesh.IndexBuffer := nil;
    end;

    AdMesh.PrimitiveCount := FDetails*FDetails*2;
    AdMesh.Update;
  end;
end;

procedure TAdCustomImage.CreatePatternRects;
var ax,ay:integer;
begin
  Rects.Clear;
  if (FPatternWidth <> 0) and (FPatternHeight <> 0) then
  begin
    for ay := 0 to ((FHeight+FSkipHeight) div (PatternHeight+FSkipHeight)) - 1 do
    begin
      for ax := 0 to ((FWidth+FSkipWidth) div (PatternWidth+FSkipWidth)) - 1 do
      begin
        Rects.Add(AdBounds(
          ax*(PatternWidth+FSkipWidth),ay*(PatternHeight+FSkipHeight),
          FPatternWidth,FPatternHeight));
      end;
    end;
  end
  else
  begin
    Rects.Add(AdRect(0,0,FWidth,FHeight));
  end;
end;

procedure TAdCustomImage.Draw(Dest:TAdSurface;X,Y,PatternIndex:integer);
begin
  if (Texture.Texture.Loaded) and (Dest.CanDraw) and (AdMesh <> nil) then
  begin
    SetCurrentColor(255);
    if (PatternIndex < 0) then PatternIndex := 0;
    if (PatternIndex > PatternCount-1) then PatternIndex := PatternCount-1;
    DrawMesh(Dest, AdRect(X,Y,X+Width,Y+Height), GetPatternRect(PatternIndex),
      0, 0, 0, bmAlpha);
  end;
end;

procedure TAdCustomImage.DrawAdd(Dest: TAdSurface; const DestRect: TAdRect;
  PatternIndex, Alpha: Integer);
begin
  if (Texture.Texture.Loaded) and (Dest.CanDraw) and (AdMesh <> nil) then
  begin
    SetCurrentColor(Alpha);
    if (PatternIndex < 0) then PatternIndex := 0;
    if (PatternIndex > PatternCount-1) then PatternIndex := PatternCount-1;
    DrawMesh(Dest,DestRect,GetPatternRect(PatternIndex),0,0,0,bmAdd);
  end;
end;

procedure TAdCustomImage.DrawAlpha(Dest: TAdSurface; const DestRect: TAdRect;
  PatternIndex, Alpha: Integer);
begin
  if (Texture.Texture.Loaded) and (Dest.CanDraw) and (AdMesh <> nil) then
  begin
    SetCurrentColor(Alpha);
    if (PatternIndex < 0) then PatternIndex := 0;
    if (PatternIndex > PatternCount-1) then PatternIndex := PatternCount-1;
    DrawMesh(Dest,DestRect,GetPatternRect(PatternIndex),0,0,0,bmAlpha);
  end;
end;

procedure TAdCustomImage.DrawMask(Dest: TAdSurface; const DestRect: TAdRect;
  PatternIndex, Alpha: Integer);
begin
  if (Texture.Texture.Loaded) and (Dest.CanDraw) and (AdMesh <> nil) then
  begin
    SetCurrentColor(Alpha);
    if (PatternIndex < 0) then PatternIndex := 0;
    if (PatternIndex > PatternCount-1) then PatternIndex := PatternCount-1;
    DrawMesh(Dest,DestRect,GetPatternRect(PatternIndex),0,0,0,bmMask);
  end;
end;

procedure TAdCustomImage.DrawRotate(Dest: TAdSurface; X, Y, Width, Height,
  PatternIndex: Integer; CenterX, CenterY: Double; Angle: Integer);
begin
  if (Texture.Texture.Loaded) and (Dest.CanDraw) and (AdMesh <> nil) then
  begin
    SetCurrentColor(255);
    if (PatternIndex < 0) then PatternIndex := 0;
    if (PatternIndex > PatternCount-1) then PatternIndex := PatternCount-1;
    DrawMesh(Dest, AdRect(X,Y,X+Width,Y+Height), GetPatternRect(PatternIndex), Angle,
     CenterX, CenterY, bmAlpha);
  end;
end;

procedure TAdCustomImage.DrawRotateAdd(Dest: TAdSurface; X, Y, Width,
  Height, PatternIndex: Integer; CenterX, CenterY: Double; Angle,
  Alpha: Integer);
begin
  if (Texture.Texture.Loaded) and (Dest.CanDraw) and (AdMesh <> nil) then
  begin
    SetCurrentColor(Alpha);
    if (PatternIndex < 0) then PatternIndex := 0;
    if (PatternIndex > PatternCount-1) then PatternIndex := PatternCount-1;
      DrawMesh(Dest, AdRect(X,Y,X+Width,Y+Height), GetPatternRect(PatternIndex), Angle,
        CenterX, CenterY, bmAdd);
  end;
end;

procedure TAdCustomImage.DrawRotateAlpha(Dest: TAdSurface; X, Y, Width,
  Height, PatternIndex: Integer; CenterX, CenterY: Double; Angle,
  Alpha: Integer);
begin
  if (Texture.Texture.Loaded) and (Dest.CanDraw) and (AdMesh <> nil) then
  begin
    SetCurrentColor(Alpha);
    if (PatternIndex < 0) then PatternIndex := 0;
    if (PatternIndex > PatternCount-1) then PatternIndex := PatternCount-1;
    DrawMesh(Dest, AdRect(X,Y,X+Width,Y+Height), GetPatternRect(PatternIndex), Angle,
      CenterX,CenterY,bmAlpha);
  end;
end;

procedure TAdCustomImage.DrawRotateMask(Dest: TAdSurface; X, Y, Width,
  Height, PatternIndex: Integer; CenterX, CenterY: Double; Angle,
  Alpha: Integer);
begin
  if (Texture.Texture.Loaded) and (Dest.CanDraw) and (AdMesh <> nil) then
  begin
    SetCurrentColor(Alpha);
    if (PatternIndex < 0) then PatternIndex := 0;
    if (PatternIndex > PatternCount-1) then PatternIndex := PatternCount-1;
    DrawMesh(Dest, AdRect(X,Y,X+Width,Y+Height),GetPatternRect(PatternIndex),Angle,CenterX,CenterY,bmMask);
  end;
end;

procedure TAdCustomImage.StretchBltAdd(Dest: TAdSurface; SourceRect,
  DestRect: TAdRect; CenterX, CenterY:double; Angle, Alpha: Integer);
begin
  if (Texture.Texture.Loaded) and (Dest.CanDraw) and (AdMesh <> nil) then
  begin
    SetCurrentColor(Alpha);
    DrawMesh(Dest,DestRect,SourceRect,Angle,CenterX,CenterY,bmAdd);
  end;
end;

procedure TAdCustomImage.StretchBltAlpha(Dest: TAdSurface; SourceRect,
  DestRect: TAdRect; CenterX, CenterY:double; Angle, Alpha: Integer);
begin
  if (Texture.Texture.Loaded) and (Dest.CanDraw) and (AdMesh <> nil) then
  begin
    SetCurrentColor(Alpha);
    DrawMesh(Dest,DestRect,SourceRect,Angle,CenterX,CenterY,bmAlpha);
  end;
end;

procedure TAdCustomImage.StretchDraw(Dest: TAdSurface; const DestRect: TAdRect; PatternIndex: integer);
begin
  if (Texture.Texture.Loaded) and (Dest.CanDraw) and (AdMesh <> nil) then
  begin
    SetCurrentColor(255);
    if (PatternIndex < 0) then PatternIndex := 0;
    if (PatternIndex > PatternCount-1) then PatternIndex := PatternCount-1;
    DrawMesh(Dest,DestRect,GetPatternRect(PatternIndex),0,0,0,bmAlpha);
  end;
end;

procedure TAdCustomImage.Restore;
begin
  FWidth := Texture.Texture.BaseWidth;
  FHeight := Texture.Texture.BaseHeight;
  AdMesh.Texture := Texture.Texture;
  CreatePatternRects;
  FSrcRect := GetPatternRect(0);
  FLastColor := GetColor;
  BuildVertices;
end;

procedure TAdCustomImage.SetPatternWidth(AValue: Integer);
begin
  if AValue >= 0 then
  begin
    FPatternWidth := AValue;
  end
  else
  begin
    FPatternWidth := 0;
  end;
end;

procedure TAdCustomImage.SetSkipHeight(AValue: integer);
begin
  if AValue >= 0 then
  begin
    FSkipHeight := AValue;
  end
  else
  begin
    FSkipHeight := 0;
  end;
end;

procedure TAdCustomImage.SetSkipWidth(AValue: integer);
begin
  if AValue >= 0 then
  begin
    FSkipWidth := AValue;
  end
  else
  begin
    FSkipWidth := 0;
  end;
end;

procedure TAdCustomImage.SetTexture(AValue: TAdCustomTexture);
begin
  if FOwnTexture then
  begin
    FTexture.Free;
  end;
  FOwnTexture := false;
  FTexture := AValue;
end;

procedure TAdCustomImage.SetCurrentColor(Alpha: byte);
var CurCol:TAndorraColor;
begin
  if Texture.Texture.Loaded then
  begin
    FAlpha := Alpha;
    CurCol := GetColor;
    if not CompareColors(CurCol,FLastColor) then
    begin
      FLastColor := CurCol;
      BuildVertices;
    end;    
  end;
end;

procedure TAdCustomImage.SetDetails(AValue: integer);
begin
  if (AValue > 0) and (AValue <> FDetails) then
  begin
    FDetails := AValue;
    BuildVertices;
  end;
end;

procedure TAdCustomImage.SetPatternHeight(AValue: Integer);
begin
  if AValue >= 0 then
  begin
    FPatternHeight := AValue;
  end
  else
  begin
    FPatternHeight := 0;
  end;
end;

function TAdCustomImage.GetColor: TAndorraColor;
begin
  result := Ad_ARGB(FAlpha,GetRValue(FColor),GetGValue(FColor),GetBValue(FColor));
end;

function TAdCustomImage.GetHeight: integer;
begin
  Result := FPatternHeight;
  if (Result<=0) then
    Result := FHeight;
end;

function TAdCustomImage.GetWidth: integer;
begin
  Result := FPatternWidth;
  if (Result<=0) then
    Result := FWidth;
end;

procedure TAdCustomImage.Initialize;
begin
  if AdMesh <> nil then
  begin
    Finalize;
  end;
  AdMesh := FParent.AdAppl.CreateMesh;
end;

procedure TAdCustomImage.Finalize;
begin
  if AdMesh <> nil then
  begin
    FreeAndNil(AdMesh);
  end;
end;

procedure TAdCustomImage.Notify(ASender: TObject;AEvent: TSurfaceEventState);
begin
  if AEvent = seFinalize then
  begin
    Finalize;
  end;
  if AEvent = seInitialize then
  begin
    Initialize;
  end;
  if AEvent = seInitialized then
  begin
    Restore;
  end;
end;

function TAdCustomImage.GetPatternCount: integer;
begin
  result := Rects.Count - PatternStop;
end;

function TAdCustomImage.GetPatternRect(ANr: Integer):TAdRect;
begin
  if (ANr >= 0) and (ANr < Rects.Count) then
    result := Rects[ANr]
  else
    result := AdRect(0,0,0,0);
end;

{ TAdImage }

constructor TAdImage.Create(AAdDraw: TAdDraw);
begin
  inherited;

  FTexture := TAdTexture.Create(AAdDraw);
  FOwnTexture := true;
end;

procedure TAdImage.Assign(AItem: TAdImage);
var
  ms:TMemoryStream;
begin
  //Create a temporary stream
  ms := TMemoryStream.Create;

  //Save the data of the item which should be assigned
  AItem.SaveToStream(ms);

  //Load the saved data
  ms.Position := 0;
  LoadFromStream(ms);

  ms.Free;
end;

procedure TAdImage.LoadFromFile(AFile: string);
var
  ms:TMemoryStream;
begin
  ms := TMemoryStream.Create;

  ms.LoadFromFile(AFile);
  ms.Position := 0;

  LoadFromStream(ms);
  ms.Free;
end;

procedure TAdImage.SaveToFile(AFile: string);
var
  ms:TMemoryStream;
begin
  ms := TMemoryStream.Create;

  SaveToStream(ms);

  ms.SaveToFile(AFile);
  ms.Free;
end;

procedure TAdImage.LoadFromStream(AStream: TStream);
var
  s:string;
  c:char;
  l:integer;
begin
  s := '';
  AStream.Read(c,1); s := s + c;
  AStream.Read(c,1); s := s + c;

  if (s = 'PI') or (s = 'P2') then
  begin
    //Ver. 1 Data
    Texture.LoadFromStream(AStream);
    AStream.Read(l,SizeOf(l));
    SetLength(FName,l);
    AStream.Read(FName[1],l);
    AStream.Read(FDetails,SizeOf(FDetails));
    AStream.Read(FPatternWidth,SizeOf(FPatternWidth));
    AStream.Read(FPatternHeight,SizeOf(FPatternHeight));
    AStream.Read(FSkipWidth,SizeOf(FSkipWidth));
    AStream.Read(FSkipHeight,SizeOf(FSkipHeight));

    //Ver. 2 Data
    if (s = 'P2') then
    begin
      AStream.Read(FPatternStop,SizeOf(FPatternStop));
    end
    else
    begin
      FPatternStop := 0;
    end;
    
    Restore;
  end
  else
  begin
    raise ELoad.Create('This is not a vaild picture collection item.');
  end;
end;

procedure TAdImage.SaveToStream(AStream: TStream);
var
  c:char;
  l:integer;
begin
  c := 'P'; AStream.Write(c,1);
  c := '2'; AStream.Write(c,1);

  Texture.SaveToStream(AStream);

  l := length(FName);
  AStream.Write(l,SizeOf(l));
  AStream.Write(FName[1],l);

  AStream.Write(FDetails,SizeOf(FDetails));
  AStream.Write(FPatternWidth,SizeOf(FPatternWidth));
  AStream.Write(FPatternHeight,SizeOf(FPatternHeight));
  AStream.Write(FSkipWidth,SizeOf(FSkipWidth));
  AStream.Write(FSkipHeight,SizeOf(FSkipHeight));
  AStream.Write(FPatternStop,SizeOf(FPatternStop));
end;

function TAdImage.GetTexture: TAdTexture;
begin
  result := TAdTexture(inherited Texture);
end;

procedure TAdImage.SetTexture(AValue: TAdTexture);
begin
  inherited Texture := AValue;
end;

{ TAdImageList }

function TAdImageList.Add(AName: string): TAdImage;
begin
  result := TAdImage.Create(FParent);
  result.Name := AName;
  result.FreeByList := self;
  inherited Add(result);
end;

constructor TAdImageList.Create(AAdDraw: TAdDraw);
begin
  inherited Create;
  FParent := AAdDraw;
end;

destructor TAdImageList.Destroy;
begin
  inherited Destroy;
end;

function TAdImageList.Find(AName: string): TAdImage;
var i:integer;
begin
  result := nil;
  i := IndexOf(AName);
  if i > -1 then
  begin
    result := Items[i];
  end;
end;

function TAdImageList.FindEx(ASubStr: string): TAdImageList;
var
  i:integer;
begin
  result := TAdImageList.Create(FParent);
  for i := 0 to Count - 1 do
  begin
    if Pos(ASubStr,Items[i].Name) > 0 then
    begin
      result.Add(Items[i]);
    end;
  end;
end;

function TAdImageList.IndexOf(AName: string): integer;
var i:integer;
begin
  result := -1;
  for i := 0 to Count - 1 do
  begin
    if Items[i].Name = AName then
    begin
      result := i;
      break;
    end;
  end;
end;

function TAdImageList.GetItem(AIndex:integer):TAdImage;
begin
 result := inherited Items[AIndex];
end;

procedure TAdImageList.SaveToFile(AFile: string);
var ms:TMemoryStream;
begin
  ms := TMemoryStream.Create;
  SaveToStream(ms);
  ms.SaveToFile(AFile);
  ms.Free;
end;

procedure TAdImageList.LoadFromFile(AFile: string);
var ms:TMemoryStream;
begin
  ms := TMemoryStream.Create;
  ms.LoadFromFile(AFile);
  ms.Position := 0;
  LoadFromStream(ms);
  ms.Free;
end;

procedure TAdImageList.SaveToStream(AStream: TStream);
var i:integer;
    s:string;
    ms:TMemoryStream;
    size:int64;
begin
  s := 'TADPictCol';
  AStream.Write(s[1],10);
  i := Count;
  AStream.Write(i,SizeOf(i));
  for i := 0 to Count - 1 do
  begin
    ms := TMemoryStream.Create;
    Items[i].SaveToStream(ms);
    size := ms.Size;
    AStream.Write(size,SizeOf(size));
    ms.Position := 0;
    ms.SaveToStream(AStream);
    ms.Free;
  end;
end;

procedure TAdImageList.LoadFromStream(AStream: TStream);
var i,c:integer;
    s:string;
    ms:TMemoryStream;
    size:int64;
    temp:TAdImage;
begin
  SetLength(s,10);
  AStream.Read(s[1],10);
  if s = 'TADPictCol' then
  begin
    Clear;
    AStream.Read(c,SizeOf(c));
    for i := 0 to c - 1 do
    begin
      AStream.Read(size,SizeOf(Size));
      ms := TMemoryStream.Create;
      ms.CopyFrom(AStream,size);
      ms.Position := 0;
      temp := TAdImage.Create(FParent);
      with temp do
      begin
        FreeByList := self;
        LoadFromStream(ms);
      end;
      Add(temp);
      ms.Free;
    end;
  end
  else
  begin
    raise ELoad.Create('This is not a vaild Andorra Picture Library!');
  end;
end;

procedure TAdImageList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then
  begin
    with TAdImage(Ptr) do
    begin
      if FreeByList = self then
      begin
        Free;
      end;
    end;
  end;
end;

procedure TAdImageList.Restore;
var i:integer;
begin
  for i := 0 to Count - 1 do
  begin
    Items[i].Restore;
  end;
end;

procedure TAdImageList.SetCompressor(ACompressor: TAdGraphicCompressorClass);
var
  i:integer;
begin
  FCompressor := ACompressor;
  for i := 0 to Count - 1 do
  begin
    Items[i].Texture.Compressor := FCompressor;
  end;
end;

procedure TAdImageList.SetItem(AIndex:integer;AItem:TAdImage);
begin
 inherited Items[AIndex] := AItem;
end;

{ TAdLight }

constructor TAdLight.Create(AParent: TAdDraw);
begin
  inherited Create;
  FParent := AParent;
  FParent.RegisterNotifyEvent(Notify);
  FRange := 50;
  FFalloff := 1;
  FColor := Ad_RGB(255,255,255);
  Initialize;
end;

destructor TAdLight.Destroy;
begin
  FParent.UnRegisterNotifyEvent(Notify);
  Finalize;
  inherited Destroy;
end;

procedure TAdLight.Disable;
begin
  AdLight.Disable;
end;

procedure TAdLight.Enable;
begin
  AdLight.Enable;
end;

procedure TAdLight.Finalize;
begin
  if AdLight <> nil then
  begin
    FreeAndNil(AdLight);
  end;
end;

procedure TAdLight.Initialize;
begin
  Finalize;
  AdLight := FParent.AdAppl.CreateLight;
  AdLight.X := FX;
  AdLight.Y := FY;
  AdLight.Z := FZ;
  AdLight.Range := FRange;
  AdLight.Color := FColor;
  AdLight.Falloff := FFalloff;
  AdLight.Restore;
end;

procedure TAdLight.Notify(ASender: TObject; AEvent: TSurfaceEventState);
begin
  if AEvent = seInitialize then
  begin
    Initialize;
  end;

  if AEvent = seFinalize then
  begin
    Finalize;
  end;
end;

procedure TAdLight.Restore;
begin
  AdLight.Restore;
end;

procedure TAdLight.SetColor(AValue: TAndorraColor);
begin
  if not CompareColors(AValue,FColor) then
  begin
    FColor := AValue;
    AdLight.Color := FColor;
  end;
end;

procedure TAdLight.SetFalloff(AValue: double);
begin
  if not (AValue = FFalloff) then
  begin
    FFalloff := AValue;
    AdLight.Falloff := FFalloff;
  end;
end;

procedure TAdLight.SetRange(AValue: double);
begin
  if not (AValue = FRange) then
  begin
    FRange := AValue;
    AdLight.Range := FRange;
  end;
end;

procedure TAdLight.SetX(AValue: double);
begin
  if not (AValue = FX) then
  begin
    FX := AValue;
    AdLight.X := FX;
  end;
end;

procedure TAdLight.SetY(AValue: double);
begin
  if not (AValue = FY) then
  begin
    FY := AValue;
    AdLight.Y := FY;
  end;
end;

procedure TAdLight.SetZ(AValue: double);
begin
  if not (AValue = FZ) then
  begin
    FZ := AValue;
    AdLight.Z := FZ;
  end;
end;

{ TAdLog }

procedure TAdLog.AddMessage(AMessage: TAdLogMessage);
const
  cTabulator = 20;
var
  Space:integer;
  LMessage:string;
  i:integer;
begin
  LMessage := '[' + AMessage.Typ + ']';

  Space := cTabulator - length(AMessage.Typ) - 2;
  if Space < 1 then Space := 1;
  for i := 0 to Space do
    LMessage := LMessage + ' ';


  LMessage := LMessage + AMessage.Sender + ':';

  Space := cTabulator - length(AMessage.Sender) - 1;
  if Space < 1 then Space := 1;
  for i := 0 to Space do
    LMessage := LMessage + ' ';


  LMessage := LMessage + AMessage.Text;

  self.Items.Add(LMessage);
  if FileName <> '' then
  begin
    SaveToFile(FileName);
  end;
end;

constructor TAdLog.Create;
begin
  inherited Create;
  FItems := TStringList.Create;
end;

destructor TAdLog.Destroy;
begin
  Items.Free;
  inherited Destroy;
end;

procedure TAdLog.LoadFromFile(AFile: string);
begin
  Items.LoadFromFile(AFile);
  Items.Add('');
end;

procedure TAdLog.SaveToFile(AFile: string);
begin
  Items.SaveToFile(AFile);
end;

{ TSurfaceEventList }

procedure TSurfaceEventList.Add(Item: TSurfaceEvent);
var Event:PSurfaceEvent;
begin
  New(Event);
  Event^ := Item;
  inherited Add(Event);
end;

function TSurfaceEventList.GetItem(AIndex:integer):TSurfaceEvent;
begin
  result := PSurfaceEvent(inherited Items[AIndex])^;
end;

procedure TSurfaceEventList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then
  begin
    Dispose(PSurfaceEvent(Ptr));
  end;
  inherited;
end;

procedure TSurfaceEventList.Remove(Item: TSurfaceEvent);
var i:integer;
begin
  i := 0;
  while i < Count do
  begin
    if (TMethod(Items[i]).Code = TMethod(Item).Code) and
       (TMethod(Items[i]).Data = TMethod(Item).Data) then
    begin
      Delete(i);
    end;
    i := i + 1;
  end;
end;

procedure TSurfaceEventList.SetItem(AIndex:integer;AItem:TSurfaceEvent);
begin
  inherited Items[AIndex] := @AItem;
end;  

{ TAdCustomTexture }

constructor TAdCustomTexture.Create(AParent: TAdDraw);
begin
  inherited Create;
  
  FParent := AParent;
  FBitDepth := 32;
  
  Initialize;
end;

destructor TAdCustomTexture.Destroy;
begin
  Finalize;
  inherited;
end;

function TAdCustomTexture.GetBitDepth: byte;
begin
  if Initialized then
  begin
    result := Texture.BitCount;
  end
  else
  begin
    result := FBitDepth;
  end;
end;

function TAdCustomTexture.GetInitialized: boolean;
begin
  result := (FAd2DTexture <> nil) and (FAd2dTexture.Width > 0) and (FAd2dTexture.Height > 0);
end;

procedure TAdCustomTexture.SetAd2DTexture(AValue: TAd2dTexture);
begin
  if (FOwnTexture) and (FAd2dTexture <> nil) then
    FAd2dTexture.Free;

  FOwnTexture := false;
  FAd2dTexture := AValue;

  FBitDepth := AValue.BitCount;
end;

procedure TAdCustomTexture.SetBitDepth(AValue: byte);
begin
  FBitDepth := AValue;
end;   

{ TAdTexture }

constructor TAdTexture.Create(AParent:TAdDraw);
begin
  inherited Create(AParent);

  FCompressorClass := nil;
  Parent.RegisterNotifyEvent(Notify);
end;

destructor TAdTexture.Destroy;
begin
  Parent.UnRegisterNotifyEvent(Notify);
  inherited Destroy;
end;

procedure TAdTexture.Initialize;
begin
  Finalize;
  FAd2dTexture := Parent.AdAppl.CreateBitmapTexture;  
end;

procedure TAdTexture.Finalize;
begin
  Clear;
  if (FOwnTexture) and (FAd2dTexture <> nil) then
  begin
    FAd2dTexture.Free;
    FAd2dTexture := nil;
  end;
end;

procedure TAdTexture.Clear;
begin
  if FCache <> nil then
  begin
    FreeAndNil(FCache);
  end;
  if Texture <> nil then
  begin
    Texture.FlushTexture;
  end;
end;

procedure TAdTexture.SaveToFile(AFile: string);
var
  ms:TMemoryStream;
begin
  ms := TMemoryStream.Create;
  SaveToStream(ms);
  ms.SaveToFile(AFile);
end;

procedure TAdTexture.LoadFromFile(AFile: string);
var
  ms:TMemoryStream;
begin
  ms := TMemoryStream.Create;
  ms.LoadFromFile(AFile);
  ms.Position := 0;
  LoadFromStream(ms);
  ms.Free;
end;

procedure TAdTexture.LoadFromGraphic(AGraphic: TObject);
var
  bmp:TAdBitmap;
begin
  bmp := TAdBitmap.Create;
  try
    bmp.Assign(AGraphic);
    Texture.LoadFromBitmap(bmp, FParent.GetTextureParams(BitDepth));
  finally
    bmp.Free;
  end;
end;

procedure TAdTexture.SaveToGraphic(AGraphic: TObject);
var
  bmp:TAdBitmap;
begin
  bmp := TAdBitmap.Create;
  try
    bmp.ReserveMemory(Texture.BaseWidth, Texture.BaseHeight);
    Texture.SaveToBitmap(bmp);
    bmp.AssignTo(AGraphic);
  finally
    bmp.Free;
  end;
end;

procedure TAdTexture.LoadGraphicFromFile(AFile: string; Transparent: boolean;
  TransparentColor: LongInt);
var
  bmp:TAdBitmap;
begin
  bmp := TAdBitmap.Create;
  try
    bmp.LoadGraphicFromFile(AFile, Transparent, TransparentColor);
    Texture.LoadFromBitmap(bmp, FParent.GetTextureParams(BitDepth));
  finally
    bmp.Free;
  end;
end;

procedure TAdTexture.LoadFromStream(AStream: TStream);
var c:char;
    bmp:TAdBitmap;
begin
  AStream.Read(c,1);
  if c = 'T' then
  begin
    bmp := TAdBitmap.Create;
    try
      bmp.LoadFromStream(AStream);
      AStream.Read(FBitDepth,1);
      Texture.LoadFromBitmap(bmp, FParent.GetTextureParams(BitDepth));
      bmp.Free;
    except
      bmp.Free;
      raise;
    end;
  end;
end;

procedure TAdTexture.SaveToStream(AStream: TStream);
var c:char;
    bmp:TAdBitmap;
    bits:byte;
begin
  if (Texture.Loaded) then
  begin
    c := 'T'; AStream.Write(c,1);

    bmp := TAdBitmap.Create;
    bmp.Compressor := FCompressorClass;
    try
      bmp.ReserveMemory(Texture.BaseWidth,Texture.BaseHeight);
      Texture.SaveToBitmap(bmp);
      bmp.SaveToStream(AStream);
    finally
      bmp.Free;
    end;

    bits := Texture.BitCount;
    AStream.Write(bits,1);

  end
  else
  begin
    c := #0; AStream.Write(c,1);
  end;
end;

procedure TAdTexture.Notify(ASender: TObject; AEvent: TSurfaceEventState);
begin
  if AEvent = seFinalize then
  begin
    if FCache <> nil then
    begin
      FreeAndNil(FCache);
    end;
    if FAd2DTexture.Loaded then
    begin
      FCache := TMemoryStream.Create;
      SaveToStream(FCache);
      FCache.Position := 0;
    end;
    Finalize;
  end else
  if AEvent = seInitialize then
  begin
    Initialize;
    if FCache <> nil then
    begin
      FCache.Position := 0;
      LoadFromStream(FCache);
      FreeAndNil(FCache);
    end;
  end;
end;

function TAdTexture.GetTexture: TAd2dBitmapTexture;
begin
  result := TAd2dBitmapTexture(inherited Texture);
end;

procedure TAdTexture.SetTexture(AValue: TAd2dBitmapTexture);
begin
  inherited Texture := AValue;
end;

{ TAdSurface }

constructor TAdSurface.Create(ADraw: TAdDraw);
begin
  inherited Create;
  AdDraw := ADraw;
end;

procedure TAdSurface.Activate;
begin
  if not FActivated then
  begin
    FActivated := true;
    DoActivation;
    FDraw.ActiveSurface := self;
  end;
end;

procedure TAdSurface.Deactivate;
begin
  if FActivated then
  begin
    FActivated := false;
    DoDeactivation;
  end;
end;

procedure TAdSurface.DoActivation;
begin
  //
end;

procedure TAdSurface.DoDeactivation;
begin
  //
end;

procedure TAdSurface.Notify(ASender: TObject; AEvent: TSurfaceEventState);
begin
  //
end;

procedure TAdSurface.ClearSurface(AColor: Cardinal);
begin
  Activate;
end;

procedure TAdSurface.SetDraw(AValue: TAdDraw);
begin
  if FDraw <> nil then
    FDraw.UnRegisterNotifyEvent(Notify);

  if AValue <> nil then
  begin
    FDraw := AValue;
    FDraw.RegisterNotifyEvent(Notify);
  end;
end;

{ TAdRenderingSurface }

procedure TAdRenderingSurface.CanvasRelease(Sender: TObject);
begin
  if not Activated then
    Activate;
end;

procedure TAdRenderingSurface.DoFinalize;
begin
  if FCanvas <> nil then
  begin
    FCanvas.Free;
    FCanvas := nil;
  end;
end;

procedure TAdRenderingSurface.DoInitialize;
begin
  DoFinalize;
  FCanvas := TAdCanvas.Create(AdDraw.AdAppl);
  FCanvas.OnRelease := CanvasRelease;
  FCanvas.Font := AdDraw.Fonts.GenerateFont('Tahoma', 10, []);
end;

procedure TAdRenderingSurface.DoActivation;
begin
  AdDraw.AdAppl.Viewport := FViewPort;
  UpdateMatrix;
end;

procedure TAdRenderingSurface.DoBeginScene;
begin
  FCanvas.StartFrame;
end;

procedure TAdRenderingSurface.DoEndScene;
begin
  FCanvas.EndFrame;
end;

procedure TAdRenderingSurface.Notify(ASender: TObject;
  AEvent: TSurfaceEventState);
begin
  case AEvent of
    seBeginScene: DoBeginScene;
    seEndScene: DoEndScene;
    seFinalize: DoFinalize;
    seInitialized: DoInitialize;
  end;
end;

procedure TAdRenderingSurface.SetProjectionMatrix(AValue: TAdMatrix);
begin
  FProjectionMatrix := AValue;
  UpdateMatrix;
end;

procedure TAdRenderingSurface.SetViewMatrix(AValue: TAdMatrix);
begin
  FViewMatrix := AValue;
  UpdateMatrix;
end;

procedure TAdRenderingSurface.SetViewPort(AValue: TAdRect);
begin
  FViewPort := AValue;
  if Activated and AdDraw.CanDraw then
    AdDraw.AdAppl.Viewport := AValue;
end;

function TAdRenderingSurface.GetDisplayRect: TAdRect;
begin
  result := AdRect(0, 0, FSceneWidth, FSceneHeight);
end;

procedure TAdRenderingSurface.UpdateMatrix;
begin
  if Activated and AdDraw.CanDraw then
    AdDraw.AdAppl.SetupManualScene(FViewMatrix, FProjectionMatrix);
end;

procedure TAdRenderingSurface.Setup2DScene(AWidth, AHeight: integer);
var
  curscene:TAdSurface;
begin
  if AdDraw.CanDraw then
  begin
    curscene := nil;
    if not Activated then
      curscene := AdDraw.ActiveSurface;

    Activate;
    AdDraw.AdAppl.Setup2DScene(AWidth, AHeight);
    AdDraw.AdAppl.GetScene(FViewMatrix, FProjectionMatrix);

    FSceneWidth := AWidth;
    FSceneHeight := AHeight;

    if curscene <> nil then
      curscene.Activate;
  end;
end;

procedure TAdRenderingSurface.Setup2DScene;
begin
  Setup2DScene(FWidth, FHeight);
end;

procedure TAdRenderingSurface.Setup3DScene(AWidth, AHeight: integer; APos, ADir,
  AUp: TAdVector3);
var
  curscene:TAdSurface;
begin
  if AdDraw.CanDraw then
  begin
    curscene := nil;
    if not Activated then
      curscene := AdDraw.ActiveSurface;

    Activate;
    AdDraw.AdAppl.Setup3DScene(AWidth, AHeight, APos, ADir, AUp);
    AdDraw.AdAppl.GetScene(FViewMatrix, FProjectionMatrix);

    FSceneWidth := AWidth;
    FSceneHeight := AHeight;

    if curscene <> nil then
      curscene.Activate;
  end;
end;

{ TAdTextureSurface }

constructor TAdTextureSurface.Create(ADraw: TAdDraw);
begin
  inherited Create(ADraw);

  FImage := TAdCustomImage.Create(ADraw);
  FTexture := TAdRenderTargetTexture.Create(ADraw);
  FImage.Texture := FTexture;
  Width := 128;
  Height := 128;
  if ADraw.Initialized then
    DoInitialize;
end;

destructor TAdTextureSurface.Destroy;
begin
  FTexture.Free;
  FImage.Free;
  inherited;
end;

procedure TAdTextureSurface.SetSize(AWidth, AHeight: integer);
begin
  Width := AWidth;
  Height := AHeight;
  Viewport := AdRect(0,0,AWidth,AHeight);
  if CanDraw then
  begin
    FTexture.Width := AWidth;
    FTexture.Height := AHeight;
    FImage.Restore;
    Setup2DScene(Width, Height);
  end;
end;

procedure TAdTextureSurface.DoActivation;
begin
  if CanDraw then
  begin
    AdDraw.AdAppl.SetRenderTarget(FTexture.Texture);
    inherited DoActivation;
  end;
end;

function TAdTextureSurface.CanDraw: boolean;
begin
  result := FTexture.Initialized and AdDraw.CanDraw;
end;

procedure TAdTextureSurface.ClearSurface(AColor: Cardinal);
begin
  inherited;

  if CanDraw then
    AdDraw.AdAppl.ClearSurface(
      Ad_ARGB(
        (AColor and $FF000000) shr 24,
        (AColor and $000000FF),
        (AColor and $0000FF00) shr 8,
        (AColor and $00FF0000) shr 16));
end;

{ TAdRenderTargetTexture }

constructor TAdRenderTargetTexture.Create(AParent: TAdDraw);
begin
  FWidth := 128;
  FHeight := 128;

  inherited Create(AParent);

  Parent.RegisterNotifyEvent(Notify);
end;

destructor TAdRenderTargetTexture.Destroy;
begin
  Parent.UnRegisterNotifyEvent(Notify);
  inherited;
end;

procedure TAdRenderTargetTexture.Clear;
begin
  if Texture <> nil then
  begin
    Texture.FlushMemory;
  end;
end;

procedure TAdRenderTargetTexture.Initialize;
begin
  Finalize;
  FAd2dTexture := Parent.AdAppl.CreateRenderTargetTexture;
  UpdateSize;
end;

procedure TAdRenderTargetTexture.Finalize;
begin
  if FAd2dTexture <> nil then
  begin
    FAd2dTexture.Free;
    FAd2dTexture := nil;
  end;
end;

procedure TAdRenderTargetTexture.Notify(ASender: TObject;
  AEvent: TSurfaceEventState);
begin
  case AEvent of
    seInitialize: Initialize;
    seFinalize: Finalize;
  end;
end;

function TAdRenderTargetTexture.GetTexture: TAd2dRenderTargetTexture;
begin
  result := TAd2dRenderTargetTexture(inherited Texture);
end;

procedure TAdRenderTargetTexture.SetTexture(AValue: TAd2dRenderTargetTexture);
begin
  inherited Texture := AValue;
end;

procedure TAdRenderTargetTexture.SetHeight(AValue: integer);
begin
  if AValue <> FHeight then
  begin
    FHeight := AValue;
    UpdateSize;
  end;
end;

procedure TAdRenderTargetTexture.SetWidth(AValue: integer);
begin
  if AValue <> FWidth then
  begin
    FWidth := AValue;
    UpdateSize;
  end;
end;

procedure TAdRenderTargetTexture.UpdateSize;
begin
  if FAd2dTexture <> nil then
  begin
    Texture.SetSize(FWidth, FHeight, FBitDepth);
  end;
end;

end.
