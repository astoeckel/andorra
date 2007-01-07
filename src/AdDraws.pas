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

uses Windows, Controls, Math, Types, SysUtils, Classes, AdClasses, AdDllLoader,
     Graphics, Dialogs, Huffman;

type

  {This is the main class for using Andorra 2D. It is comparable to DelphiX's TDXDraw.}
  TAdDraw = class;
  //This represents one image in an ImageList.
  TPictureCollectionItem = class;

  {Specifies a textures texture state.}
  TTextureMode = (tmWrap,tmMirror,tmClamp);
  {Specifies the event which called the procedure}
  TSurfaceEventState = (seInitialize,seFinalize,seInitialized);
  {The declaration of the surface event handler}
  TSurfaceEvent = procedure(Sender:TObject;AEvent:TSurfaceEventState) of object;
  {A pointer on TSurfaceEvent}
  PSurfaceEvent = ^TSurfaceEvent;
  {A list which contains the surface events}
  TSurfaceEventList = class(TList)
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
    Text:string;
    Sender:string;
    Typ:string;
  end;

  //The log system class
  TAdLog = class
    private
      Items:TStringList;
    public
      //This specifies an file the log is automaticly saved to. May be '' for no auto saving.
      FileName:string;
      //A constructor.
      constructor Create;
      //Wow. A destructor.
      destructor Destroy;override;
      //Load an old logfile
      procedure LoadFromFile(AFile:string);
      //Save the current logfile. Happens automaticly if FileName is set.
      procedure SaveToFile(AFile:string);
      //Adds an TAdLog Message Record to the log system.
      procedure AddMessage(AMessage:TAdLogMessage);
  end;

  {This is the main class for using Andorra 2D. It is comparable to DelphiX's TDXDraw.}
  TAdDraw = class
  private

    FParent:TWinControl;
    FOptions:TAdOptions;
    FDllName:string;
    FFinalize:TNotifyEvent;
    FInitialize:TNotifyEvent;
    FInitialized:boolean;
    FDisplayRect:TRect;

    FAmbientColor:TColor;

    FLog:TAdLog;
    FLogFileName:string;
    FAutoLoadLog:boolean;

    FSurfaceEventList:TSurfaceEventList;
    //FCanvas:TSurfaceCanvas;

    procedure SetDllName(val : string);

    procedure SetupThings;

    procedure SetOptions(AValue:TAdOptions);
    procedure SetAmbientColor(AValue:TColor);

    procedure SetAutoLoadLog(AValue:boolean);

    function GetDisplayRect:TRect;

  protected
    procedure CallNotifyEvent(AEventState:TSurfaceEventState);
  public
    {The Andorra Dll Loader. You can use this class to get direct control over
    the engine.}
    AdDllLoader : TAndorraDllLoader;
    {The Andorra Reference for the DllLoader}
    AdAppl:TAdApplication;
    //This property contains the diplay settings (width, height and bitcount)
    Display : TAdDisplay;

    //Create the class. AParent is the handle of the control, where displaying should take place.
    constructor Create(AParent : TWinControl);
    //This is a destroctor.
    destructor Destroy; override;

    //Here you can read the parent value, you've set in the constructor.
    property Parent : TWinControl read FParent;

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

    //Recives log events
    procedure LogProc(LogItem:TAdLogItem);

    //Register an event that will be called if the surface is finalized or initialized.
    procedure RegisterNotifyEvent(AProc:TSurfaceEvent);
    //UnRegister a registered event.
    procedure UnRegisterNotifyEvent(AProc:TSurfaceEvent);

    //This property contains the options (see TAdDrawMode)
    property Options : TAdOptions read FOptions write SetOptions;
    //Set this value to load a library
    property DllName : string read FDllName write SetDllName;
    //Returns weather the application is initialized
    property Initialized : boolean read FInitialized;
    //Set the ambient light color here
    property AmbientColor:TColor read FAmbientColor write SetAmbientColor;

    //Event is called before the application is finalized
    property OnFinalize : TNotifyEvent read FFinalize write FFinalize;
    //Event is called after the application is initialized
    property OnInitialize : TNotifyEvent read FInitialize write FInitialize;

    //The log system
    property Log : TAdLog read FLog;
    //Specifies weather the log should automaticly be loaded at startup and saved at shutdown.
    property AutoLoadLog: boolean read FAutoLoadLog write SetAutoLoadLog;
    //The name of the logfile.
    property LogFileName:string read FLogFileName write FLogFileName;
    //A canvas for drawing directly on the surface. (Draw through the canvas shouldn't be used very often because it is slow.)
    //property Canvas:TSurfaceCanvas read FCanvas;
  end;

  {TAdLight is the representation of a light in your game. Before using lights
  be sure that you've turned on "doLights" in the options. You can only use 8 Lights in a scene by one time.}
  {TAdLight = class
    private
      FParent:TAdDraw;
    protected
    public
      //Link to Andorras Light
      AdLight:TAndorraLight;
      //Contains information about the light.
      Data:TLight;
      //A constructor
      constructor Create(AParent:TAdDraw);
      //A destructor
      destructor Destroy;override;
      //Push the settings you've made in "data" into the engine.
      procedure Restore;
      {Enables the light. Note that most graphic boards can only display 8 Lights a time.

      All lights are automaticly disabled in the "EndScene" routine.}
      {procedure Enable;
      //Disable a light manually.
      procedure Disable;
  end;

  {TAdTexture basicly loads a texture from a bitmap or a file into the video memory}
  TAdTexture = class
    private
      FParent:TAdDraw;
      FWidth:integer;
      FHeight:integer;
      FBaseRect:TRect;
      FPicture:TBitmap;
      FPictureAlpha:TBitmap;
      FPixelFormat:byte;
      function GetLoaded:boolean;
    protected
      procedure Notify(Sender:TObject;AEvent:TSurfaceEventState);
    public
      {Link to the Andorra Texture.}
      AdTexture:TAdTexture;
      {This is a constructor. AADraw defines the parent andorra application.}
      constructor Create(AAdDraw:TAdDraw);
      {A destructor. What did you think?}
      destructor Destroy;override;
      {Creates a new andorra texture from a file. The old one will be flushed.}
      procedure LoadFromFile(afile:string;ATransparent:boolean;ATransparentColor:TColor);
      {Creates a new andorra texture from a bitmap. The old one will only be flushed if the height, width or pixelformat are different}
      procedure LoadFromBitmap(ABitmap:TBitmap;ABitDepth:byte);overload;
      {Creates a new andorra texture from a bitmap. The old one will only be flushed if the height, width or pixelformat are different. Selects the PixelFormat automaticly from the bitmap.}
      procedure LoadFromBitmap(ABitmap:TBitmap);overload;
      {Add an alphachannel to the current texture. The bitmap has to have the same size as the loaded one.}
      procedure AddAlphaChannel(ABitmap:TBitmap);
      {Overide the alpha channel. 255 to make the texture completly opac.}
      procedure SetAlphaValue(AValue:byte);
      {Writes the current texture into a bitmap.}
      procedure SaveToBitmap(ABitmap:TBitmap);
      {Writes the current texture into a bitmap.}
      procedure SaveAlphaChannelToBitmap(ABitmap:TBitmap);
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
      {Returns the Pixelformat of the current texture. May be 16- or 32- Bit.}
      property PixelFormat:byte read FPixelFormat;
  end;

  {A list which is able to contain TRects}
  TRectList = class(TList)
    private
     	function GetItem(AIndex:integer):TRect;
     	procedure SetItem(AIndex:integer;AItem:TRect);
    protected
      procedure Notify(Ptr: Pointer; Action: TListNotification);override;
    public
      {Read/Write acess to the rectangles.}
     	property Items[AIndex:integer]:TRect read GetItem write SetItem;default;
      {Add a rectangle.}
      procedure Add(ARect:TRect);
    published
  end;

  TInitialLetters = string[4];

  {The abstract picture compressor class}
  TCompressor = class(TPersistent)
    public
      //Returns the initial letters of this compressor. Will be calles without creating the object!!!
      function GetInitial:TInitialLetters;virtual;abstract;
      //Writes the bitmap into a stream
      procedure Write(AStream:TStream;ABmp:TAdBitmap);virtual;abstract;
      //Reads the bitmap from the stream
      procedure Read(AStream:TStream;ABmp:TAdBitmap);virtual;abstract;
  end;

  //An error raised if there is an error due loading the picture
  ELoad = class(Exception);
  //An error raised if the compressor used isn't found.
  ENoCompressor = class(ELoad);

  {The standard compressor. The data will be compressed with the "Huffman"
  Algorithm. Uses an Huffman-Algorithm written by Marc Schmitz. Available on http://www.delphipraxis.net/topic51522_huffman+algorithmus.html&highlight=huffman}
  THAICompressor = class(TCompressor)
    public
      //Returns the initial letters of this compressor. Will be calles without creating the object!!!
      function GetInitial:TInitialLetters;override;
      //Writes the bitmap into a stream
      procedure Write(AStream:TStream;ABmp:TAdBitmap);virtual;abstract;
      //Reads the bitmap from the stream
      procedure Read(AStream:TStream;ABmp:TAdBitmap);virtual;abstract;
  end;

  {A class of the compressor for easy registering}
  TCompressorClass = class of TCompressor;

  //This represents one image in an ImageList.
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
      FTextureXMode:TTextureMode;
      FTextureYMode:TTextureMode;
      FDetail:integer;
      FOwnTexture:boolean;
      FCompressor:TCompressor;
      FCompressorClass:TCompressorClass;
      procedure SetPatternWidth(AValue:integer);
      procedure SetPatternHeight(AValue:integer);
      procedure SetSkipWidth(AValue:integer);
      procedure SetSkipHeight(AValue:integer);
      function GetPatternCount:integer;
      function GetWidth:integer;
      function GetHeight:integer;
      procedure SetCurrentColor(Alpha:byte);
      procedure SetTextureXMode(AValue:TTextureMode);
      procedure SetTextureYMode(AValue:TTextureMode);
      procedure SetDetail(AValue:integer);
      procedure SetTexture(AValue:TAdTexture);
      procedure SetCompressor(AClass:TCompressorClass);
    protected
      Rects:TRectList;
      procedure CreatePatternRects;
      procedure Notify(ASender:TObject;AEvent:TSurfaceEventState);
    public
      //True if this item can be freed by the image list
      FreeByList:boolean;
      //Contains the link to Andorras Image
      AdMesh:TAdMesh;
      //A Constructor
      constructor Create(AAdDraw:TAdDraw);
      //A Destructor
      destructor Destroy;override;
      //Draws the image at a specified position. If you've set "PatternWidth" and "PatternHeight", this will draw the pattern you've specified in PatternIndex.
      procedure Draw(Dest:TAdDraw;X,Y,PatternIndex:integer);
      //The same as Draw, but you can stretch the Image.
      procedure StretchDraw(Dest:TAdDraw;const DestRect:TRect;PatternIndex:integer);
      //Draw a sprite with additive blending.
      procedure DrawAdd(Dest: TAdDraw; const DestRect: TRect; PatternIndex: Integer;
        Alpha: Integer);
      //Draw a sprite with alpha blending.
      procedure DrawAlpha(Dest: TAdDraw; const DestRect: TRect; PatternIndex: Integer;
        Alpha: Integer);
      //Draw only the mask.
      procedure DrawMask(Dest: TAdDraw; const DestRect: TRect; PatternIndex: Integer;
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
      procedure StretchBltAlpha(Dest:TAdDraw; SourceRect,DestRect:TRect;CenterX,CenterY:integer;Angle:Integer;Alpha:Integer);
      //Draw only specified part from the image. Additive blending.
      procedure StretchBltAdd(Dest:TAdDraw; SourceRect,DestRect:TRect;CenterX,CenterY:integer;Angle:Integer;Alpha:Integer);
      //If you've set the color or a new texture you have to call this function to see your changes.
      procedure Restore;
      //Frees all data
      procedure Finalize;
      //Restores all freed date
      procedure Initialize;
      //Returns the rect of one pattern.
      function GetPatternRect(ANr:integer):TRect;
      //Saves the image to a stream
      procedure SaveToStream(AStream:TStream);
      //Loads the image from a stream
      procedure LoadFromStream(AStream:TStream);
      //Saves the image to a file
      procedure SaveToFile(AFile:string);
      //Loads the image from a file
      procedure LoadFromFile(AFile:string);
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
      //Here you can dye an image.
      property Color:TColor read FColor write FColor;
      //Name of the image in the imagelist.
      property Name:string read FName write FName;
      //Set the mode of the texture.
      property TextureXMode:TTextureMode read FTextureXMode write SetTextureXMode;
      //Set the mode of the texture.
      property TextureYMode:TTextureMode read FTextureYMode write SetTextureYMode;
      //Important for using lights: How many vertices does the image have.
      property Detail:integer read FDetail write SetDetail;
      //Set a compressor class. Default: TBMPCompressor;
      property Compressor:TCompressorClass read FCompressorClass write SetCompressor;
  end;

  //Administrates the images
  TPictureCollection = class(TList)
    private
      FParent:TAdDraw;
      FCompressor:TCompressorClass;
     	function GetItem(AIndex:integer):TPictureCollectionItem;
     	procedure SetItem(AIndex:integer;AItem:TPictureCollectionItem);
      procedure SetCompressor(ACompressor:TCompressorClass);
    protected
      procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    public
      //Returns you an item
     	property Items[AIndex:integer]:TPictureCollectionItem read GetItem write SetItem;default;
      //Add a new image to the list.
      function Add(AName:string):TPictureCollectionItem;overload;
      //Find an image in the list.
      function Find(AName:string):TPictureCollectionItem;
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
      //Set this to the same compressor to every item
      property Compressor:TCompressorClass read FCompressor write SetCompressor; 
    published
  end;

  //Class for calculating the FPS
  TPerformanceCounter = class
    private
      lt,th,ffps:integer;
    public
      //Time between the frames in ms
      TimeGap:integer;
      //The current FPS
      FPS:integer;
      //Calculates the new values
      procedure Calculate;
      //Creates a new instance of the performance counter
      constructor Create;
  end;

const
  CanvasPatternSize = 512;

var
  //Contains all registered compressors. You must not change the contents.
  RegisteredCompressors:TStringList;

//Is called for registering a compressor class. If you register a compressor it will be automaticly used for decompressing.
procedure RegisterCompressor(AClass:TClass);

implementation

type TRGBRec = packed record
  r,g,b:byte;
end;

type PRGBRec = ^TRGBRec;

procedure RegisterCompressor(AClass:TClass);
begin
  RegisterClass(TPersistentClass(AClass));
  RegisteredCompressors.Add(AClass.ClassName);
end;

{ TAdDraw }

constructor TAdDraw.Create(AParent : TWinControl);
var amsg:TAdLogMessage;
begin
	inherited Create;
  FParent := AParent;
  FAmbientColor := clWhite;
  AdDllLoader := TAndorraDllLoader.Create;
  SetupThings;

  FLog := TAdLog.Create;
  FLogFileName := 'adlog.txt';

  //FSurfaceEventList := TSurfaceEventList.Create;
  //FCanvas := TSurfaceCanvas.Create(self);

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

  //FSurfaceEventList.Free;
  //FCanvas.Free;
  
	inherited Destroy;
end;

procedure TAdDraw.UnRegisterNotifyEvent(AProc: TSurfaceEvent);
begin
  //FSurfaceEventList.Remove(AProc)
end;

procedure TAdDraw.RegisterNotifyEvent(AProc: TSurfaceEvent);
begin
  //FSurfaceEventList.Add(AProc);
end;


procedure TAdDraw.SetAmbientColor(AValue: TColor);
begin
  {if Initialized then
  begin
    FAmbientColor := AValue;
    AdDllLoader.SetAmbientLight(AdAppl,AD_RGB(GetRValue(AValue),GetGValue(AValue),
      GetBValue(AValue)));
  end;}
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
    //AdDllLoader.SetOptions(AdAppl,Options);
  end;
end;

function TAdDraw.Initialize: boolean;
var amsg:TAdLogMessage;
begin

  result := false;

  if not Initialized then
  begin
    //Create the new Application
    AdAppl := AdDllLoader.CreateApplication;
    if (AdAppl <> nil) and (FParent <> nil) and (AdDllLoader.LibraryLoaded) then
    begin
      //Give the Plugin the possibility to send logs
      AdAppl.SetLogProc(LogProc);

      FDisplayRect := GetDisplayRect;
      Display.Width := FDisplayRect.Right;
      Display.Height := FDisplayRect.Bottom;

      result := AdAppl.Initialize(FParent.Handle,Options,Display);

      //AdDllLoader.SetTextureQuality(AdAppl,tqNone);
      Setup2DScene;
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
    end;
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
    FInitialized := false;
    CallNotifyEvent(seFinalize);
    AdAppl.Finalize;
    FreeAndNil(AdAppl)
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

procedure TAdDraw.ClearSurface(Color:TColor);
begin
  AdAppl.ClearSurface(Ad_RGB(GetRValue(Color),GetGValue(Color),GetBValue(Color)));
end;

procedure TAdDraw.BeginScene;
begin
  if AdAppl <> nil then
  begin
    AdAppl.BeginScene;
  end;
end;

procedure TAdDraw.EndScene;
begin
  if AdAppl <> nil then
  begin
    AdAppl.EndScene;
  end;
end;

procedure TAdDraw.Setup2DScene;
begin
  if AdAppl <> nil then
  begin
    FDisplayRect := GetDisplayRect;
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

function TAdDraw.GetDisplayRect: TRect;
begin
  if dofullscreen in Options then
  begin
    result := Bounds(0,0,Display.Width,Display.Height);
  end
  else
  begin
    result := Bounds(0,0,FParent.ClientWidth,FParent.ClientHeight);
  end;
end;

procedure TAdDraw.CallNotifyEvent(AEventState: TSurfaceEventState);
//var i:integer;
begin
  {i := 0;
  while i < FSurfaceEventList.Count do
  begin
    //FSurfaceEventList.Items[i](self,AEventState);
    i := i + 1;
  end;     }
end;

function TAdDraw.CanDraw:boolean;
begin
  result := (AdAppl <> nil) and (Initialized);
end;

{TAdTexture}

{constructor TAdTexture.Create(AAdDraw:TAdDraw);
begin
  inherited Create;
  AdTexture := nil;
  FParent := AAdDraw;
  FParent.RegisterNotifyEvent(Notify);
end;

destructor TAdTexture.Destroy;
begin
  FParent.UnRegisterNotifyEvent(Notify);
  FreeTexture;
  if Assigned(FPicture) then
  begin
    FreeAndNil(FPicture);
  end;
  if Assigned(FPictureAlpha) then
  begin
    FreeAndNil(FPictureAlpha);
  end;
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
  FPixelFormat := 32;
end;

procedure TAdTexture.LoadFromBitmap(ABitmap:TBitmap;ABitDepth:byte);
var FColorDepth:byte;
    Info:TImageInfo;
begin
  if (ABitmap.Width <> FBaseRect.Right) or (ABitmap.Height <> FBaseRect.Bottom) or
     (FPixelFormat <> ABitDepth) or (AdTexture = nil) then
  begin
    FreeTexture;
    case ABitDepth of
      16: FColorDepth := 16;
    else
      FColorDepth := 32;
    end;
    AdTexture := FParent.AdDllLoader.LoadTextureFromBitmap(Fparent.AdAppl,ABitmap,FColorDepth);
    Info := FParent.AdDllLoader.GetTextureInfo(AdTexture);
    FWidth := Info.Width;
    FHeight := Info.Height;
    FBaseRect := Info.BaseRect;
    FPixelFormat := FColorDepth;
  end
  else
  begin
    FParent.AdDllLoader.RefreshTextureWithBitmap(AdTexture,ABitmap);
  end;
end;


procedure TAdTexture.LoadFromBitmap(ABitmap: TBitmap);
var ABitDepth:byte;
begin
  case ABitmap.PixelFormat of
    pf16Bit: ABitDepth := 16;
  else
    ABitDepth := 32;
  end;
  LoadFromBitmap(ABitmap,ABitDepth);
end;

procedure TAdTexture.AddAlphaChannel(ABitmap:TBitmap);
begin
  if AdTexture <> nil then
  begin
    FParent.AdDllLoader.AddTextureAlphaChannel(AdTexture,ABitmap);
  end;
end;

procedure TAdTexture.SaveAlphaChannelToBitmap(ABitmap: TBitmap);
begin
  if AdTexture <> nil then
  begin
    FParent.AdDllLoader.GetTextureAlphaChannelAsBitmap(AdTexture,ABitmap);
  end;
end;

procedure TAdTexture.SaveToBitmap(ABitmap: TBitmap);
begin
  if AdTexture <> nil then
  begin
    FParent.AdDllLoader.GetTextureAsBitmap(AdTexture,ABitmap);
  end;
end;

procedure TAdTexture.Notify(Sender: TObject; AEvent: TSurfaceEventState);
begin
  if AEvent = seFinalize then
  begin
    if Loaded then
    begin
      if Assigned(FPicture) then
      begin
        FreeAndNil(FPicture);
      end;
      if Assigned(FPictureAlpha) then
      begin
        FreeAndNil(FPictureAlpha);
      end;
      FPicture := TBitmap.Create;
      FPictureAlpha := TBitmap.Create;
      SaveToBitmap(FPicture);
      SaveAlphaChannelToBitmap(FPictureAlpha);
    end;
    FreeTexture;
  end;
  if AEvent = seInitialize then
  begin
    if not Loaded then
    begin
      if Assigned(FPicture) then
      begin
        LoadFromBitmap(FPicture);
        AddAlphaChannel(FPictureAlpha);
        FreeAndNil(FPicture);
        FreeAndNil(FPictureAlpha);
      end;
    end;
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
    AdTexture := nil;
  end;
end;

function TAdTexture.GetLoaded:boolean;
begin
  result := AdTexture <> nil;
end;

{TRectList}

{procedure TRectList.Add(ARect: TRect);
var ar:PRect;
begin
  new(ar);
  ar^ := ARect;
  inherited Add(ar);
end;

function TRectList.GetItem(AIndex:integer):TRect;
begin
  result := PRect(inherited Items[AIndex])^;
end;

procedure TRectList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then
  begin
    Dispose(Ptr);
  end;
  inherited;
end;

procedure TRectList.SetItem(AIndex:integer;AItem:TRect);
begin
  PRect(inherited Items[AIndex])^ := AItem;
end; }


{TPictureCollectionItem}

{constructor TPictureCollectionItem.Create(AAdDraw:TAdDraw);
begin
  inherited Create;
  FTexture := TAdTexture.Create(AAdDraw);
  FParent := AAdDraw;
  FParent.RegisterNotifyEvent(Notify);
  Rects := TRectList.Create;
  FColor := clWhite;
  FOwnTexture := true;
  Compressor := THAICompressor;
  Initialize;
end;

destructor TPictureCollectionItem.Destroy;
begin
  if FOwnTexture then
  begin
    FTexture.Free;
  end;
  if FCompressor <> nil then
  begin
    FCompressor.Free;
  end;
  Rects.Free;
  FParent.UnRegisterNotifyEvent(Notify);
  Finalize;
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
  if (Texture.Loaded) and (Dest.CanDraw) and (AdImage <> nil) then
  begin
    SetCurrentColor(255);
    if (PatternIndex < 0) then PatternIndex := 0;
    if (PatternIndex > PatternCount-1) then PatternIndex := PatternCount-1;
    FParent.AdDllLoader.DrawImage(
      FParent.AdAppl,AdImage,Rect(X,Y,X+Width,Y+Height),Rects[PatternIndex],
      0,0,0,bmAlpha);
  end;
end;

procedure TPictureCollectionItem.DrawAdd(Dest: TAdDraw; const DestRect: TRect;
  PatternIndex, Alpha: Integer);
begin
  if (Texture.Loaded) and (Dest.CanDraw) and (AdImage <> nil) then
  begin
    SetCurrentColor(Alpha);
    if (PatternIndex < 0) then PatternIndex := 0;
    if (PatternIndex > PatternCount-1) then PatternIndex := PatternCount-1;
    FParent.AdDllLoader.DrawImage(
      FParent.AdAppl,AdImage,DestRect,Rects[PatternIndex],
      0,0,0,bmAdd);
  end;
end;

procedure TPictureCollectionItem.DrawAlpha(Dest: TAdDraw; const DestRect: TRect;
  PatternIndex, Alpha: Integer);
begin
  if (Texture.Loaded) and (Dest.CanDraw) and (AdImage <> nil) then
  begin
    SetCurrentColor(Alpha);
    if (PatternIndex < 0) then PatternIndex := 0;
    if (PatternIndex > PatternCount-1) then PatternIndex := PatternCount-1;
    FParent.AdDllLoader.DrawImage(
      FParent.AdAppl,AdImage,DestRect,Rects[PatternIndex],
      0,0,0,bmAlpha);
  end;
end;

procedure TPictureCollectionItem.DrawMask(Dest: TAdDraw; const DestRect: TRect;
  PatternIndex, Alpha: Integer);
begin
  if (Texture.Loaded) and (Dest.CanDraw) and (AdImage <> nil) then
  begin
    SetCurrentColor(Alpha);
    if (PatternIndex < 0) then PatternIndex := 0;
    if (PatternIndex > PatternCount-1) then PatternIndex := PatternCount-1;
    FParent.AdDllLoader.DrawImage(
      FParent.AdAppl,AdImage,DestRect,Rects[PatternIndex],
      0,0,0,bmMask);
  end;
end;

procedure TPictureCollectionItem.DrawRotate(Dest: TAdDraw; X, Y, Width, Height,
  PatternIndex: Integer; CenterX, CenterY: Double; Angle: Integer);
begin
  if (Texture.Loaded) and (Dest.CanDraw) and (AdImage <> nil) then
  begin
    SetCurrentColor(255);
    if (PatternIndex < 0) then PatternIndex := 0;
    if (PatternIndex > PatternCount-1) then PatternIndex := PatternCount-1;
    FParent.AdDllLoader.DrawImage(
      FParent.AdAppl,AdImage,Rect(X,Y,X+Width,Y+Height),Rects[PatternIndex],
      Angle,CenterX,CenterY,bmAlpha);
  end;
end;

procedure TPictureCollectionItem.DrawRotateAdd(Dest: TAdDraw; X, Y, Width,
  Height, PatternIndex: Integer; CenterX, CenterY: Double; Angle,
  Alpha: Integer);
begin
  if (Texture.Loaded) and (Dest.CanDraw) and (AdImage <> nil) then
  begin
    SetCurrentColor(Alpha);
    if (PatternIndex < 0) then PatternIndex := 0;
    if (PatternIndex > PatternCount-1) then PatternIndex := PatternCount-1;
    FParent.AdDllLoader.DrawImage(
      FParent.AdAppl,AdImage,Rect(X,Y,X+Width,Y+Height),Rects[PatternIndex],
      Angle,CenterX,CenterY,bmAdd);
  end;
end;

procedure TPictureCollectionItem.DrawRotateAlpha(Dest: TAdDraw; X, Y, Width,
  Height, PatternIndex: Integer; CenterX, CenterY: Double; Angle,
  Alpha: Integer);
begin
  if (Texture.Loaded) and (Dest.CanDraw) and (AdImage <> nil) then
  begin
    SetCurrentColor(Alpha);
    if (PatternIndex < 0) then PatternIndex := 0;
    if (PatternIndex > PatternCount-1) then PatternIndex := PatternCount-1;
    FParent.AdDllLoader.DrawImage(
      FParent.AdAppl,AdImage,Rect(X,Y,X+Width,Y+Height),Rects[PatternIndex],
      Angle,CenterX,CenterY,bmAlpha);
  end;
end;

procedure TPictureCollectionItem.DrawRotateMask(Dest: TAdDraw; X, Y, Width,
  Height, PatternIndex: Integer; CenterX, CenterY: Double; Angle,
  Alpha: Integer);
begin
  if (Texture.Loaded) and (Dest.CanDraw) and (AdImage <> nil) then
  begin
    SetCurrentColor(Alpha);
    if (PatternIndex < 0) then PatternIndex := 0;
    if (PatternIndex > PatternCount-1) then PatternIndex := PatternCount-1;
    FParent.AdDllLoader.DrawImage(
      FParent.AdAppl,AdImage,Rect(X,Y,X+Width,Y+Height),Rects[PatternIndex],
      Angle,CenterX,CenterY,bmMask);
  end;
end;

procedure TPictureCollectionItem.StretchBltAdd(Dest: TAdDraw; SourceRect,
  DestRect: TRect; CenterX, CenterY, Angle, Alpha: Integer);
begin
  if (Texture.Loaded) and (Dest.CanDraw) and (AdImage <> nil) then
  begin
    SetCurrentColor(Alpha);
    FParent.AdDllLoader.DrawImage(
      FParent.AdAppl,AdImage,DestRect,SourceRect,
      Angle,CenterX,CenterY,bmAdd);
  end;
end;

procedure TPictureCollectionItem.StretchBltAlpha(Dest: TAdDraw; SourceRect,
  DestRect: TRect; CenterX, CenterY, Angle, Alpha: Integer);
begin
  if (Texture.Loaded) and (Dest.CanDraw) and (AdImage <> nil) then
  begin
    SetCurrentColor(Alpha);
    FParent.AdDllLoader.DrawImage(
      FParent.AdAppl,AdImage,DestRect,SourceRect,
      Angle,CenterX,CenterY,bmAlpha);
  end;
end;

procedure TPictureCollectionItem.StretchDraw(Dest: TAdDraw; const DestRect: TRect; PatternIndex: integer);
begin
  if (Texture.Loaded) and (Dest.CanDraw) and (AdImage <> nil) then
  begin
    SetCurrentColor(255);
    if (PatternIndex < 0) then PatternIndex := 0;
    if (PatternIndex > PatternCount-1) then PatternIndex := PatternCount-1;
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

procedure TPictureCollectionItem.SetTexture(AValue: TAdTexture);
begin
  if FOwnTexture then
  begin
    FTexture.Free;
  end;
  FOwnTexture := false;
  FTexture := AValue;
end;

procedure TPictureCollectionItem.SetTextureXMode(AValue: TTextureMode);
begin
  if AValue <> FTextureXMode then
  begin
    case AValue of
      tmWrap: FParent.AdDllLoader.SetTextureXMode(AdImage,amWrap);
      tmMirror: FParent.AdDllLoader.SetTextureXMode(AdImage,amMirror);
      tmClamp: FParent.AdDllLoader.SetTextureXMode(AdImage,amClamp);
    end;
    FTextureXMode := AValue;
  end;
end;

procedure TPictureCollectionItem.SetTextureYMode(AValue: TTextureMode);
begin
  if AValue <> FTextureYMode then
  begin
    case AValue of
      tmWrap: FParent.AdDllLoader.SetTextureYMode(AdImage,amWrap);
      tmMirror: FParent.AdDllLoader.SetTextureYMode(AdImage,amMirror);
      tmClamp: FParent.AdDllLoader.SetTextureYMode(AdImage,amClamp);
    end;
    FTextureYMode := AValue;
  end;
end;

procedure TPictureCollectionItem.SetCompressor(AClass: TCompressorClass);
begin
  if FCompressor <> nil then
  begin
    FreeAndNil(FCompressor);
  end;
  FCompressor := AClass.Create;
  FCompressorClass := AClass;
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

procedure TPictureCollectionItem.SetDetail(AValue: integer);
begin
  if AValue > 0 then
  begin
    FDetail := AValue;
    FParent.AdDllLoader.SetImageDetail(AdImage,AValue);
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

procedure TPictureCollectionItem.Initialize;
begin
  if AdImage <> nil then
  begin
    Finalize;
  end;
  AdImage := FParent.AdDllLoader.CreateImage(FParent.AdAppl);
  FParent.AdDllLoader.SetImageColor(AdImage,FLastColor);
end;

procedure TPictureCollectionItem.LoadFromFile(AFile: string);
var ms:TMemoryStream;
begin
  ms := TMemoryStream.Create;
  ms.LoadFromFile(AFile);
  ms.Position := 0;
  LoadFromStream(ms);
  ms.Free;
end;

procedure TPictureCollectionItem.SaveToFile(AFile: string);
var ms:TMemoryStream;
begin
  ms := TMemoryStream.Create;
  SaveToStream(ms);
  ms.SaveToFile(AFile);
  ms.Free;
end;

procedure TPictureCollectionItem.LoadFromStream(AStream: TStream);
var bmp1,bmp2:TBitmap;
    s:string;
    c:char;
    i,l:integer;
    atemp:TCompressor;
    cref:TPersistentClass;
begin
  s := '';
  AStream.Read(c,1); s := s + c;
  AStream.Read(c,1); s := s + c;
  if s = 'PI' then
  begin
    AStream.Read(c,1);
    if c = 'T' then
    begin
      bmp1 := TBitmap.Create;
      bmp2 := TBitmap.Create;
      //Select a compressor
      SetLength(s,4);
      AStream.Read(s[1],4);
      for i := 0 to RegisteredCompressors.Count - 1 do
      begin
        cref := GetClass(RegisteredCompressors[i]);
        if cref <> nil then
        begin
          atemp := TCompressor(TCompressorClass(cref).Create);
          if atemp.GetInitial <> s then
          begin
            FreeAndNil(atemp);
          end
          else
          begin
            break;
          end;
        end;
      end;
      if ATemp <> nil then
      begin
        ATemp.Read(AStream,bmp1,bmp2);
        Texture.LoadFromBitmap(bmp1);
        Texture.AddAlphaChannel(bmp2);
        ATemp.Free;
        bmp1.Free;
        bmp2.Free;
      end
      else
      begin
        raise ENoCompressor.Create('The compressor '+s+' is not registered!');
      end;
    end;
    AStream.Read(l,SizeOf(l));
    SetLength(FName,l);
    AStream.Read(FName[1],l);
    AStream.Read(FDetail,SizeOf(FDetail));
    AStream.Read(FPatternWidth,SizeOf(FPatternWidth));
    AStream.Read(FPatternHeight,SizeOf(FPatternHeight));
    AStream.Read(FSkipWidth,SizeOf(FSkipWidth));
    AStream.Read(FSkipHeight,SizeOf(FSkipHeight));
    AStream.Read(FTextureXMode,SizeOf(FTextureXMode));
    AStream.Read(FTextureYMode,SizeOf(FTextureYMode));
    Restore;
  end
  else
  begin
    raise ELoad.Create('This is not a vaild picture collection item.');
  end;
end;

procedure TPictureCollectionItem.SaveToStream(AStream: TStream);
var bmp1,bmp2:TBitmap;
    c:char;
    s:TInitialLetters;
    l:integer;
begin
  c := 'P'; AStream.Write(c,1);
  c := 'I'; AStream.Write(c,1);
  if (Texture.Loaded) and (FCompressor <> nil) then
  begin
    c := 'T'; AStream.Write(c,1);
    bmp1 := TBitmap.Create;
    bmp2 := TBitmap.Create;
    Texture.SaveToBitmap(bmp1);
    Texture.SaveAlphaChannelToBitmap(bmp2);
    s := FCompressor.GetInitial;
    AStream.Write(s[1],4);
    FCompressor.Write(AStream,bmp1,bmp2);
    bmp1.Free;
    bmp2.Free;
  end
  else
  begin
    c := #0; AStream.Write(c,1);
  end;
  l := length(FName);
  AStream.Write(l,SizeOf(l));
  AStream.Write(FName[1],l);
  AStream.Write(FDetail,SizeOf(FDetail));
  AStream.Write(FPatternWidth,SizeOf(FPatternWidth));
  AStream.Write(FPatternHeight,SizeOf(FPatternHeight));
  AStream.Write(FSkipWidth,SizeOf(FSkipWidth));
  AStream.Write(FSkipHeight,SizeOf(FSkipHeight));
  AStream.Write(FTextureXMode,SizeOf(FTextureXMode));
  AStream.Write(FTextureYMode,SizeOf(FTextureYMode));
end;

procedure TPictureCollectionItem.Finalize;
begin
  if AdImage <> nil then
  begin
    FParent.AdDllLoader.DestroyImage(AdImage);
    AdImage := nil;
  end;
end;

procedure TPictureCollectionItem.Notify(ASender: TObject;AEvent: TSurfaceEventState);
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

function TPictureCollectionItem.GetPatternCount: integer;
begin
  result := Rects.Count;
end;

function TPictureCollectionItem.GetPatternRect(ANr: Integer):TRect;
begin
  result := Rects[ANr];
end;   }


{TPictureCollection}

{function TPictureCollection.Add(AName: string): TPictureCollectionItem;
begin
  result := TPictureCollectionItem.Create(FParent);
  result.Name := AName;
  result.FreeByList := true;
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
  result := nil;
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

procedure TPictureCollection.SaveToFile(AFile: string);
var ms:TMemoryStream;
begin
  ms := TMemoryStream.Create;
  SaveToStream(ms);
  ms.SaveToFile(AFile);
  ms.Free;
end;

procedure TPictureCollection.LoadFromFile(AFile: string);
var ms:TMemoryStream;
begin
  ms := TMemoryStream.Create;
  ms.LoadFromFile(AFile);
  ms.Position := 0;
  LoadFromStream(ms);
  ms.Free;
end;

procedure TPictureCollection.SaveToStream(AStream: TStream);
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

procedure TPictureCollection.LoadFromStream(AStream: TStream);
var i,c:integer;
    s:string;
    ms:TMemoryStream;
    size:int64;
    temp:TPictureCollectionItem;
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
      temp := TPictureCollectionItem.Create(FParent);
      with temp do
      begin
        FreeByList := true;
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

procedure TPictureCollection.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then
  begin
    with TPictureCollectionItem(Ptr) do
    begin
      if FreeByList then
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

procedure TPictureCollection.SetCompressor(ACompressor: TCompressorClass);
var i:integer;
begin
  FCompressor := ACompressor;
  for i := 0 to Count - 1 do
  begin
    Items[i].Compressor := FCompressor;
  end;
end;

procedure TPictureCollection.SetItem(AIndex:integer;AItem:TPictureCollectionItem);
begin
 inherited Items[AIndex] := AItem;
end;   }

{ TAdLight }

{constructor TAdLight.Create(AParent: TAdDraw);
begin
  inherited Create;
  FParent := AParent;
  AdLight := AParent.AdDllLoader.CreateLight(AParent.AdAppl);
end;

destructor TAdLight.Destroy;
begin
  FParent.AdDllLoader.DestroyLight(AdLight);
  inherited Destroy;
end;

procedure TAdLight.Disable;
begin
  FParent.AdDllLoader.DisableLight(AdLight);
end;

procedure TAdLight.Enable;
begin
  FParent.AdDllLoader.EnableLight(AdLight);
end;

procedure TAdLight.Restore;
begin
  FParent.AdDllLoader.RestoreLight(AdLight,Data);
end; }

{ TAdLog }

procedure TAdLog.AddMessage(AMessage: TAdLogMessage);
begin
  Items.Add('['+AMessage.Sender+']'+' ['+AMessage.Typ+'] '+AMessage.Text);
  if FileName <> '' then
  begin
    SaveToFile(FileName);
  end;
end;

constructor TAdLog.Create;
begin
  inherited Create;
  Items := TStringList.Create;
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

{ TPerformanceCounter }

procedure TPerformanceCounter.Calculate;
var t:integer;
begin
  t := GetTickCount;
  timegap := t-lt;
  th := th + timegap;
  lt := t;
  fFPS := fFPS + 1;
  if th >= 1000 then
  begin
    th := 0;
    FPS := fFPS;
    fFPS := 0;
  end;
end;

constructor TPerformanceCounter.Create;
begin
  inherited Create;
  lt := GetTickCount;
  th := 0;
  timegap := 0;
  fps := 0;
end;

{ TSurfaceEventList }

{procedure TSurfaceEventList.Add(Item: TSurfaceEvent);
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
    Dispose(Ptr);
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
end;       }

{ THAICompressor }

function THAICompressor.GetInitial: TInitialLetters;
begin
  result := #3+'HAI'
end;

procedure THAICompressor.Read(AStream: TStream; AdBmp:TAdBitmap);
var
  input:TMemoryStream;
  output:TMemoryStream;
  dec:THuffmanDecoder;
  s:int64;
begin
  input := TMemoryStream.Create;
  AStream.Read(s,SizeOf(s));
  input.CopyFrom(AStream,s);

  output := TMemoryStream.Create;
  input.Position := 0;
  dec := THuffmanDecoder.Create;
  dec.Input := input;
  dec.Output := output;
  dec.Decode;
  input.Free;
  dec.Free;
  output.Position := 0;

  ABmp.LoadFromStream(output);
  
  output.Free;
end;

procedure THAICompressor.Write(AStream: TStream; ABmp:TAdBitmap);
var
    input:TMemoryStream;
    output:TMemoryStream;
    enc:THuffmanEncoder;
    s:int64;
begin
  input := TMemoryStream.Create;
  ABmp.SaveToStream(input);

  output := TMemoryStream.Create;

  Input.Position := 0;
  enc := THuffmanEncoder.Create;
  enc.Input := input;
  enc.Output := output;
  enc.Encode;
  enc.Free;
  input.Free;

  s := Output.Size;
  AStream.Write(s,SizeOf(s));
  Output.SaveToStream(AStream);
  Output.Free;
end; 

initialization
  RegisteredCompressors := TStringList.Create;
  RegisterCompressor(THAICompressor);

finalization
  RegisteredCompressors.Free;

end.
