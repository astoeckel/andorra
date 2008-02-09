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

uses

  SysUtils, Classes,
  AdClasses, AdTypes, AdList, AdPersistent,
  AdWindowFramework, AdDLLLoader,
  AdCanvas, AdBitmap, AdFontFactory, AdFont
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
  //This represents one image in an ImageList.
  TAdImage = class;

  {Specifies the event which called the procedure}
  TSurfaceEventState = (seInitialize,seFinalize,seInitialized);
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

  {TAdSurface = class
    private
      FParent : TAdDraw;
    protected
      FWidth, FHeight : integer;
    public
      constructor Create(AParent:TAdDraw);
      
      procedure Activate;abstract;virtual;
      procedure Render(ATarget:TAdSurface);abstract;virtual;      
      procedure SetSize(AWidth, AHeight:integer);virtual;

      property Width : integer read FWidth;
      property Height : integer read FHeight;
  end;                }

  {This is the main class for using Andorra 2D. It is comparable to DelphiX's TDXDraw.}
  TAdDraw = class
  private

    FParent:Pointer;
    FOptions:TAdOptions;
    FDllName:string;
    FFinalize:TNotifyEvent;
    FInitialize:TNotifyEvent;
    FInitialized:boolean;
    FDisplayRect:TAdRect;

    FAmbientColor:LongInt;

    FLog:TAdLog;
    FLogFileName:string;
    FAutoLoadLog:boolean;

    FSurfaceEventList:TSurfaceEventList;
    FCanvas:TAdCanvas;
    FFonts:TAdFontFactory;

    FTextureFilter:TAd2dTextureFilter;
    FWnd:TAdWindowFramework;

    procedure SetDllName(val : string);
    procedure SetupThings;
    procedure SetOptions(AValue:TAdOptions);
    procedure SetAmbientColor(AValue:LongInt);
    procedure SetAutoLoadLog(AValue:boolean);
    function GetDisplayRect:TAdRect;
    function SearchWindowFramework:boolean;
  protected
    procedure CallNotifyEvent(AEventState:TSurfaceEventState);
  public
    AdDllLoader : TAdDllLoader;
    AdAppl:TAd2DApplication;
    Display : TAdDisplay;

    constructor Create(AParent : Pointer);
    destructor Destroy; override;

    function Initialize: boolean;
    procedure Finalize;

    procedure ClearSurface(Color:LongInt);
    procedure Flip;
    function CanDraw:boolean;
    procedure BeginScene;
    procedure EndScene;

    procedure Setup2DScene;

    property Parent : Pointer read FParent;
    property DisplayRect:TAdRect read FDisplayRect;

    procedure LogProc(LogItem:TAdLogItem);

    procedure RegisterNotifyEvent(AProc:TSurfaceEvent);
    procedure UnRegisterNotifyEvent(AProc:TSurfaceEvent);

    procedure Run;

    function GetTextureParams(ABitDepth:byte):TAd2dBitmapTextureParameters;

    property Options : TAdOptions read FOptions write SetOptions;
    property TextureFilter : TAd2dTextureFilter read FTextureFilter write FTextureFilter;
    property DllName : string read FDllName write SetDllName;
    property Initialized : boolean read FInitialized;
    property AmbientColor:LongInt read FAmbientColor write SetAmbientColor;

    property Log : TAdLog read FLog;
    property AutoLoadLog: boolean read FAutoLoadLog write SetAutoLoadLog;
    property LogFileName:string read FLogFileName write FLogFileName;
    property Canvas:TAdCanvas read FCanvas;
    property Fonts:TAdFontFactory read FFonts;
    property Window:TAdWindowFramework read FWnd;

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

  TAdTexture = class
    private
      FParent:TAdDraw;
      FCache:TMemoryStream;
      FAd2DTexture:TAd2DBitmapTexture;
      FCompressorClass:TAdGraphicCompressorClass;
      FBitDepth:byte;
      function GetInitialized:boolean;
      function GetBitDepth:byte;
      procedure SetBitDepth(AValue:byte);
    protected
      procedure Notify(ASender:TObject;AEvent:TSurfaceEventState);
    public
      constructor Create(AParent:TAdDraw);
      destructor Destroy;override;
      procedure Initialize;
      procedure Finalize;
      procedure Clear;

      procedure LoadFromStream(AStream:TStream);
      procedure SaveToStream(AStream:TStream);
      procedure LoadFromFile(AFile:string);
      procedure SaveToFile(AFile:string);

      procedure LoadFromGraphic(AGraphic:TObject);
      procedure LoadGraphicFromFile(AFile: string; Transparent: boolean = true;
        TransparentColor: Longint = $1FFFFFFF);
      procedure SaveToGraphic(AGraphic:TObject);

      property Texture:TAd2DBitmapTexture read FAd2DTexture;
      property Initialized:boolean read GetInitialized;
      property Compressor:TAdGraphicCompressorClass read FCompressorClass write FCompressorClass;
      property BitDepth:byte read GetBitDepth write SetBitDepth;
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

  TAdImageList = class;

  //This represents one image in an ImageList.
  TAdImage = class
    private
      FParent:TAdDraw;
      FWidth,FHeight:integer;
      FPatternWidth,FPatternHeight:integer;
      FPatternStop:integer;
      FSkipWidth,FSkipHeight:integer;
      FTexture:TAdTexture;
      FColor:LongInt;
      FLastColor:TAndorraColor;
      FAlpha:byte;
      FName:string;
      FDetails:integer;
      FOwnTexture:boolean;
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
      procedure SetTexture(AValue:TAdTexture);
    protected
      Rects:TRectList;
      procedure DrawMesh(DestApp:TAdDraw;DestRect,SourceRect:TAdRect;Rotation:integer;
        RotCenterX,RotCenterY:single;BlendMode:TAd2DBlendMode);
      procedure BuildVertices;
      procedure CreatePatternRects;
      procedure Notify(ASender:TObject;AEvent:TSurfaceEventState);
    public
      //True if this item can be freed by the image list
      FreeByList:TAdImageList;
      //Contains the link to Andorras Image
      AdMesh:TAd2DMesh;
      //A Constructor
      constructor Create(AAdDraw:TAdDraw);
      //A Destructor
      destructor Destroy;override;
      //Draws the image at a specified position. If you've set "PatternWidth" and "PatternHeight", this will draw the pattern you've specified in PatternIndex.
      procedure Draw(Dest:TAdDraw;X,Y,PatternIndex:integer);
      //The same as Draw, but you can stretch the Image.
      procedure StretchDraw(Dest:TAdDraw;const DestRect:TAdRect;PatternIndex:integer);
      //Draw a sprite with additive blending.
      procedure DrawAdd(Dest: TAdDraw; const DestRect: TAdRect; PatternIndex: Integer;
        Alpha: Integer);
      //Draw a sprite with alpha blending.
      procedure DrawAlpha(Dest: TAdDraw; const DestRect: TAdRect; PatternIndex: Integer;
        Alpha: Integer);
      //Draw only the mask.
      procedure DrawMask(Dest: TAdDraw; const DestRect: TAdRect; PatternIndex: Integer;
        Alpha: Integer);
      //Draw a sprite rotated. CenterX and CenterY specify the center of the rotation - May be a value between 0 and 1. Rotation is a value between 0 and 360.
      procedure DrawRotate(Dest: TAdDraw; X, Y, Width, Height: Integer; PatternIndex: Integer;
        CenterX, CenterY: Double; Angle: Integer);
      //The same as DrawRotate, just with additive blending.
      procedure DrawRotateAdd(Dest: TAdDraw; X, Y, Width, Height: Integer; PatternIndex: Integer;
        CenterX, CenterY: Double; Angle: Integer;
        Alpha: Integer);
      //The same as DrawRotate, just with alpha blending.
      procedure DrawRotateAlpha(Dest: TAdDraw; X, Y, Width, Height: Integer; PatternIndex: Integer;
        CenterX, CenterY: Double; Angle: Integer;
        Alpha: Integer);
      //The same as DrawRotate, just drawing a the mask.
      procedure DrawRotateMask(Dest: TAdDraw; X, Y, Width, Height: Integer; PatternIndex: Integer;
        CenterX, CenterY: Double; Angle: Integer;
        Alpha: Integer);
      //Draw only specified part from the image. Alpha blending.
      procedure StretchBltAlpha(Dest:TAdDraw; SourceRect,DestRect:TAdRect;CenterX,CenterY:double;Angle:Integer;Alpha:Integer);
      //Draw only specified part from the image. Additive blending.
      procedure StretchBltAdd(Dest:TAdDraw; SourceRect,DestRect:TAdRect;CenterX,CenterY:double;Angle:Integer;Alpha:Integer);
      //If you've set the color or a new texture you have to call this function to see your changes.
      procedure Restore;
      //Frees all data
      procedure Finalize;
      //Restores all freed date
      procedure Initialize;
      //Returns the rect of one pattern.
      function GetPatternRect(ANr:integer):TAdRect;
      //Saves the image to a stream
      procedure SaveToStream(AStream:TStream);
      //Loads the image from a stream
      procedure LoadFromStream(AStream:TStream);
      //Saves the image to a file
      procedure SaveToFile(AFile:string);
      //Loads the image from a file
      procedure LoadFromFile(AFile:string);
      //Assings the settings of another item
      procedure Assign(AItem:TAdImage);
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
      property Texture:TAdTexture read FTexture write SetTexture;
      //Returns the count of the patterns.
      property PatternCount:integer read GetPatternCount;
      //If you have empty patterns, you may set PatternStop. PatternCount will be decrased by PatternStop.
      property PatternStop:integer read FPatternStop write FPatternStop;
      //Here you can set the fonts color
      property Color:Longint read FColor write FColor;
      //Name of the image in the imagelist.
      property Name:string read FName write FName;
      //Important for using lights: How many vertices does the image have.
      property Details:integer read FDetails write SetDetails;
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
var amsg:TAdLogMessage;
begin
	inherited Create;
  FParent := AParent;
  FAmbientColor := RGB(255, 255, 255);
  AdDllLoader := TAdDllLoader.Create;
  SetupThings;

  FLog := TAdLog.Create;
  FLogFileName := 'adlog.txt';
  FTextureFilter := atPoint; 
  FSurfaceEventList := TSurfaceEventList.Create;

  AutoLoadLog := true;

  amsg.Text := 'AdDraw was created: '+TimeToStr(Time);
  amsg.Sender := self.ClassName;
  amsg.Typ := 'Info';
  FLog.AddMessage(amsg);
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

  if FAutoLoadLog then
  begin
    Log.SaveToFile(FLogFileName);
  end;
  FLog.Free;

  FSurfaceEventList.Free;

	inherited Destroy;
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

procedure TAdDraw.SetAmbientColor(AValue: Longint);
begin
  FAmbientColor := AValue;
  if Initialized then
  begin
    AdAppl.AmbientLightColor := AD_RGB(GetRValue(AValue),GetGValue(AValue),
      GetBValue(AValue));
  end;
end;

procedure TAdDraw.SetAutoLoadLog(AValue: boolean);
begin
  FAutoLoadLog := AValue;
  if FAutoLoadLog then
  begin
    if FileExists(FLogFileName) then
    begin
      FLog.LoadFromFile(FLogFileName);
      FLog.FileName := FLogFileName;
    end;
  end
  else
  begin
    FLog.FileName := '';
  end;
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
  amsg:TAdLogMessage;
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

        AdAppl.AmbientLightColor := AD_RGB(GetRValue(FAmbientColor),GetGValue(FAmbientColor),
          GetBValue(FAmbientColor));

        Setup2DScene;
      end else
      begin
        AdAppl.Free; AdAppl := nil;

        amsg.Text := 'Unable to find a supported Window Framework. Try to use another Plugin or bind another window framework class.';
        amsg.Sender := 'TAdDraw';
        amsg.Typ := 'Fatal Error';
        Log.AddMessage(amsg);
      end;
    end
    else
    begin
      amsg.Text := 'Unable to initialize Andorra 2D. Check weather you have installed the newest driver.';
      amsg.Sender := 'TAdDraw';
      amsg.Typ := 'Fatal Error';
      Log.AddMessage(amsg);
    end;

    if Assigned(FInitialize) then
    begin
      //OnInitialize
      FInitialize(Self);
    end;

    FInitialized := result;

    if CanDraw then
    begin
      CallNotifyEvent(seInitialize);
      CallNotifyEvent(seInitialized);

      FFonts := TAdFontFactory.Create(AdAppl);

      FCanvas := TAdCanvas.Create(AdAppl);
      FCanvas.Font := Fonts.GenerateFont('Tahoma', 10, []);
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
    if FCanvas <> nil then
    begin
      FCanvas.Free;
    end;
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
  Log.AddMessage(Temp);
end;

procedure TAdDraw.ClearSurface(Color:Longint);
begin
  AdAppl.ClearSurface(Ad_ARGB(255,GetRValue(Color),GetGValue(Color),GetBValue(Color)));
end;

procedure TAdDraw.BeginScene;
begin
  if AdAppl <> nil then
  begin
    AdAppl.BeginScene;
    FCanvas.StartFrame;
  end;
end;

procedure TAdDraw.EndScene;
begin
  if AdAppl <> nil then
  begin
    FCanvas.EndFrame;
    AdAppl.EndScene;
  end;
end;

procedure TAdDraw.Setup2DScene;
begin
  if AdAppl <> nil then
  begin
    FDisplayRect := GetDisplayRect;
    AdAppl.Viewport := FDisplayRect;
    AdAppl.Setup2DScene(FDisplayRect.Right,FDisplayRect.Bottom);
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

    UseMipMaps := doMipmaps in FOptions;
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

constructor TAdImage.Create(AAdDraw:TAdDraw);
begin
  inherited Create;
  FTexture := TAdTexture.Create(AAdDraw);
  FParent := AAdDraw;
  FParent.RegisterNotifyEvent(Notify);
  Rects := TRectList.Create;
  FColor := RGB(255, 255, 255);
  FAlpha := 255;
  FOwnTexture := true;
  FPatternStop := 0;
  FDetails := 1;
  Initialize;
end;

destructor TAdImage.Destroy;
begin
  if FOwnTexture then
  begin
    FTexture.Free;
  end;
  Rects.Free;
  FParent.UnRegisterNotifyEvent(Notify);
  Finalize;
  inherited Destroy;
end;

procedure TAdImage.DrawMesh(DestApp: TAdDraw; DestRect,
  SourceRect: TAdRect; Rotation: integer; RotCenterX, RotCenterY: single;
  BlendMode: TAd2DBlendMode);
var
  mat1,mat2:TAdMatrix;
  curx,cury:single;
  Mode:TAd2DDrawMode;
begin
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

procedure TAdImage.Assign(AItem: TAdImage);
var ms:TMemoryStream;
begin
  ms := TMemoryStream.Create;
  AItem.SaveToStream(ms);
  ms.Position := 0;
  LoadFromStream(ms);
  ms.Free;
end;

procedure TAdImage.BuildVertices;
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

procedure TAdImage.CreatePatternRects;
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

procedure TAdImage.Draw(Dest:TAdDraw;X,Y,PatternIndex:integer);
begin
  if (Texture.Texture.Loaded) and (Dest.CanDraw) and (AdMesh <> nil) then
  begin
    SetCurrentColor(255);
    if (PatternIndex < 0) then PatternIndex := 0;
    if (PatternIndex > PatternCount-1) then PatternIndex := PatternCount-1;
    DrawMesh(Dest, AdRect(X,Y,X+Width,Y+Height), Rects[PatternIndex],
      0, 0, 0, bmAlpha);
  end;
end;

procedure TAdImage.DrawAdd(Dest: TAdDraw; const DestRect: TAdRect;
  PatternIndex, Alpha: Integer);
begin
  if (Texture.Texture.Loaded) and (Dest.CanDraw) and (AdMesh <> nil) then
  begin
    SetCurrentColor(Alpha);
    if (PatternIndex < 0) then PatternIndex := 0;
    if (PatternIndex > PatternCount-1) then PatternIndex := PatternCount-1;
    DrawMesh(Dest,DestRect,Rects[PatternIndex],0,0,0,bmAdd);
  end;
end;

procedure TAdImage.DrawAlpha(Dest: TAdDraw; const DestRect: TAdRect;
  PatternIndex, Alpha: Integer);
begin
  if (Texture.Texture.Loaded) and (Dest.CanDraw) and (AdMesh <> nil) then
  begin
    SetCurrentColor(Alpha);
    if (PatternIndex < 0) then PatternIndex := 0;
    if (PatternIndex > PatternCount-1) then PatternIndex := PatternCount-1;
    DrawMesh(Dest,DestRect,Rects[PatternIndex],0,0,0,bmAlpha);
  end;
end;

procedure TAdImage.DrawMask(Dest: TAdDraw; const DestRect: TAdRect;
  PatternIndex, Alpha: Integer);
begin
  if (Texture.Texture.Loaded) and (Dest.CanDraw) and (AdMesh <> nil) then
  begin
    SetCurrentColor(Alpha);
    if (PatternIndex < 0) then PatternIndex := 0;
    if (PatternIndex > PatternCount-1) then PatternIndex := PatternCount-1;
    DrawMesh(Dest,DestRect,Rects[PatternIndex],0,0,0,bmMask);
  end;
end;

procedure TAdImage.DrawRotate(Dest: TAdDraw; X, Y, Width, Height,
  PatternIndex: Integer; CenterX, CenterY: Double; Angle: Integer);
begin
  if (Texture.Texture.Loaded) and (Dest.CanDraw) and (AdMesh <> nil) then
  begin
    SetCurrentColor(255);
    if (PatternIndex < 0) then PatternIndex := 0;
    if (PatternIndex > PatternCount-1) then PatternIndex := PatternCount-1;
    DrawMesh(Dest, AdRect(X,Y,X+Width,Y+Height), Rects[PatternIndex], Angle,
     CenterX, CenterY, bmAlpha);
  end;
end;

procedure TAdImage.DrawRotateAdd(Dest: TAdDraw; X, Y, Width,
  Height, PatternIndex: Integer; CenterX, CenterY: Double; Angle,
  Alpha: Integer);
begin
  if (Texture.Texture.Loaded) and (Dest.CanDraw) and (AdMesh <> nil) then
  begin
    SetCurrentColor(Alpha);
    if (PatternIndex < 0) then PatternIndex := 0;
    if (PatternIndex > PatternCount-1) then PatternIndex := PatternCount-1;
      DrawMesh(Dest, AdRect(X,Y,X+Width,Y+Height), Rects[PatternIndex], Angle,
        CenterX, CenterY, bmAdd);
  end;
end;

procedure TAdImage.DrawRotateAlpha(Dest: TAdDraw; X, Y, Width,
  Height, PatternIndex: Integer; CenterX, CenterY: Double; Angle,
  Alpha: Integer);
begin
  if (Texture.Texture.Loaded) and (Dest.CanDraw) and (AdMesh <> nil) then
  begin
    SetCurrentColor(Alpha);
    if (PatternIndex < 0) then PatternIndex := 0;
    if (PatternIndex > PatternCount-1) then PatternIndex := PatternCount-1;
    DrawMesh(Dest, AdRect(X,Y,X+Width,Y+Height), Rects[PatternIndex], Angle,
      CenterX,CenterY,bmAlpha);
  end;
end;

procedure TAdImage.DrawRotateMask(Dest: TAdDraw; X, Y, Width,
  Height, PatternIndex: Integer; CenterX, CenterY: Double; Angle,
  Alpha: Integer);
begin
  if (Texture.Texture.Loaded) and (Dest.CanDraw) and (AdMesh <> nil) then
  begin
    SetCurrentColor(Alpha);
    if (PatternIndex < 0) then PatternIndex := 0;
    if (PatternIndex > PatternCount-1) then PatternIndex := PatternCount-1;
    DrawMesh(Dest, AdRect(X,Y,X+Width,Y+Height),Rects[PatternIndex],Angle,CenterX,CenterY,bmMask);
  end;
end;

procedure TAdImage.StretchBltAdd(Dest: TAdDraw; SourceRect,
  DestRect: TAdRect; CenterX, CenterY:double; Angle, Alpha: Integer);
begin
  if (Texture.Texture.Loaded) and (Dest.CanDraw) and (AdMesh <> nil) then
  begin
    SetCurrentColor(Alpha);
    DrawMesh(Dest,DestRect,SourceRect,Angle,CenterX,CenterY,bmAdd);
  end;
end;

procedure TAdImage.StretchBltAlpha(Dest: TAdDraw; SourceRect,
  DestRect: TAdRect; CenterX, CenterY:double; Angle, Alpha: Integer);
begin
  if (Texture.Texture.Loaded) and (Dest.CanDraw) and (AdMesh <> nil) then
  begin
    SetCurrentColor(Alpha);
    DrawMesh(Dest,DestRect,SourceRect,Angle,CenterX,CenterY,bmAlpha);
  end;
end;

procedure TAdImage.StretchDraw(Dest: TAdDraw; const DestRect: TAdRect; PatternIndex: integer);
begin
  if (Texture.Texture.Loaded) and (Dest.CanDraw) and (AdMesh <> nil) then
  begin
    SetCurrentColor(255);
    if (PatternIndex < 0) then PatternIndex := 0;
    if (PatternIndex > PatternCount-1) then PatternIndex := PatternCount-1;
    DrawMesh(Dest,DestRect,Rects[PatternIndex],0,0,0,bmAlpha);
  end;
end;

procedure TAdImage.Restore;
begin
  FWidth := Texture.Texture.BaseWidth;
  FHeight := Texture.Texture.BaseHeight;
  AdMesh.Texture := Texture.Texture;
  CreatePatternRects;
  FSrcRect := GetPatternRect(0);
  FLastColor := GetColor;
  BuildVertices;
end;

procedure TAdImage.SetPatternWidth(AValue: Integer);
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

procedure TAdImage.SetSkipHeight(AValue: integer);
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

procedure TAdImage.SetSkipWidth(AValue: integer);
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

procedure TAdImage.SetTexture(AValue: TAdTexture);
begin
  if FOwnTexture then
  begin
    FTexture.Free;
  end;
  FOwnTexture := false;
  FTexture := AValue;
end;

procedure TAdImage.SetCurrentColor(Alpha: byte);
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

procedure TAdImage.SetDetails(AValue: integer);
begin
  if (AValue > 0) and (AValue <> FDetails) then
  begin
    FDetails := AValue;
    BuildVertices;
  end;
end;

procedure TAdImage.SetPatternHeight(AValue: Integer);
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

function TAdImage.GetColor: TAndorraColor;
begin
  result := Ad_ARGB(FAlpha,GetRValue(FColor),GetGValue(FColor),GetBValue(FColor));
end;

function TAdImage.GetHeight: integer;
begin
  Result := FPatternHeight;
  if (Result<=0) then
    Result := FHeight;
end;

function TAdImage.GetWidth: integer;
begin
  Result := FPatternWidth;
  if (Result<=0) then
    Result := FWidth;
end;

procedure TAdImage.Initialize;
begin
  if AdMesh <> nil then
  begin
    Finalize;
  end;
  AdMesh := FParent.AdAppl.CreateMesh;
end;

procedure TAdImage.LoadFromFile(AFile: string);
var ms:TMemoryStream;
begin
  ms := TMemoryStream.Create;
  ms.LoadFromFile(AFile);
  ms.Position := 0;
  LoadFromStream(ms);
  ms.Free;
end;

procedure TAdImage.SaveToFile(AFile: string);
var ms:TMemoryStream;
begin
  ms := TMemoryStream.Create;
  SaveToStream(ms);
  ms.SaveToFile(AFile);
  ms.Free;
end;

procedure TAdImage.LoadFromStream(AStream: TStream);
var s:string;
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
var c:char;
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

procedure TAdImage.Finalize;
begin
  if AdMesh <> nil then
  begin
    FreeAndNil(AdMesh);
  end;
end;

procedure TAdImage.Notify(ASender: TObject;AEvent: TSurfaceEventState);
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

function TAdImage.GetPatternCount: integer;
begin
  result := Rects.Count - PatternStop;
end;

function TAdImage.GetPatternRect(ANr: Integer):TAdRect;
begin
  result := Rects[ANr];
end;


{TPictureCollection}

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

{ TAdTexture }

procedure TAdTexture.Clear;
begin
  if FCache <> nil then
  begin
    FreeAndNil(FCache);
  end;
  Texture.FlushTexture;
end;

constructor TAdTexture.Create(AParent:TAdDraw);
begin
  inherited Create;
  FParent := AParent;
  Initialize;
  FParent.RegisterNotifyEvent(Notify);
  FCompressorClass := nil;
  FBitDepth := 32;
end;

destructor TAdTexture.Destroy;
begin
  FParent.UnRegisterNotifyEvent(Notify);
  Finalize;
  Inherited Destroy;
end;

procedure TAdTexture.Finalize;
begin
  if Initialized then
  begin
    FreeAndNil(FAd2DTexture);
  end;
end;

function TAdTexture.GetBitDepth: byte;
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

function TAdTexture.GetInitialized: boolean;
begin
  result := (FAd2DTexture <> nil) and (FAd2dTexture.Width > 0) and (FAd2dTexture.Height > 0);
end;

procedure TAdTexture.Initialize;
begin
  Finalize;
  FAd2DTexture := FParent.AdAppl.CreateBitmapTexture;
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
  end;

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

procedure TAdTexture.SetBitDepth(AValue: byte);
begin
  FBitDepth := AValue;
end;

end.
