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

uses {$IFDEF WIN32}Windows,{$ELSE}Libc,{$ENDIF}Controls, Math, {$INCLUDE AdTypes.inc},
     SysUtils, Classes, AdClasses, AdDLLLoader, Graphics, Huffman, AdBitmapEffects, AdList,
     AdCanvas;

type

  {This is the main class for using Andorra 2D. It is comparable to DelphiX's TDXDraw.}
  TAdDraw = class;
  //This represents one image in an ImageList.
  TAdImage = class;

  {Specifies a textures texture state.}
  TTextureMode = (tmWrap,tmMirror,tmClamp);
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
      //The lists items property
      property Items[AIndex:integer]:TSurfaceEvent read GetItem write SetItem;default;
      //Adds an event to the list
      procedure Add(Item:TSurfaceEvent);
      //Removes an event from the list.
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
    FCanvas:TAdCanvas;


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
    AdDllLoader : TAdDllLoader;
    {The Andorra Reference for the DllLoader}
    AdAppl:TAd2DApplication;
    //This property contains the diplay settings for fullscreen mode (width, height and bitcount)
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
    //A simple canvas to draw on
    property Canvas:TAdCanvas read FCanvas;
  end;

  {TAdLight is the representation of a light in your game. Before using lights
  be sure that you've turned on "doLights" in the options. You can only use 8 Lights in a scene by one time.}
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
      //Link to Andorras Light
      AdLight:TAd2DLight;
      //A constructor
      constructor Create(AParent:TAdDraw);
      //A destructor
      destructor Destroy;override;

      //Pushs the data set into the engine. Has to be called if you want to see the changes you made to the light.
      procedure Restore;

      {Enables the light. Note that most graphic boards can only display 8 Lights a time.

      All lights are automaticly disabled in the "EndScene" routine.}
      procedure Enable;
      //Disable a light manually.
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

  {A compressor which doesn't compress}
  TBMPCompressor = class(TCompressor)
    public
      //Returns the initial letters of this compressor. Will be calles without creating the object!!!
      function GetInitial:TInitialLetters;override;
      //Writes the bitmap into a stream
      procedure Write(AStream:TStream;ABmp:TAdBitmap);override;
      //Reads the bitmap from the stream
      procedure Read(AStream:TStream;ABmp:TAdBitmap);override;
  end;

  {The standard compressor. The data will be compressed with the "Huffman"
  Algorithm. Uses an Huffman-Algorithm written by Marc Schmitz. Available on http://www.delphipraxis.net/topic51522_huffman+algorithmus.html&highlight=huffman}
  THAICompressor = class(TCompressor)
    public
      //Returns the initial letters of this compressor. Will be calles without creating the object!!!
      function GetInitial:TInitialLetters;override;
      //Writes the bitmap into a stream
      procedure Write(AStream:TStream;ABmp:TAdBitmap);override;
      //Reads the bitmap from the stream
      procedure Read(AStream:TStream;ABmp:TAdBitmap);override;
  end;

  {A class of the compressor for easy registering}
  TCompressorClass = class of TCompressor;

  //An exception class
  EFormatNotSupportet = class(Exception);

  //A format is a construct which enables the posibility of loading different graphic formats to Andorra. This is only an abstract class.
  TPictFormat = class(TPersistent)
    public
      //Fills a list with its supported graphic extension.
      procedure FileExts(strs:TStringList);virtual;abstract;
      //Loads the graphic from a file and stros it in a TAdBitmap.
      function LoadFromFile(AFile:string;ABmp:TAdBitmap;Transparent:boolean;TransparentColor:TColor):boolean;virtual;abstract;
      //Assigns an TGraphic and  stores it in a TAdBitmap
      procedure AssignGraphic(AGraphic:TGraphic;ABmp:TAdBitmap);virtual;abstract;
      //Returns true if this format supports the graphicclass defined in AGraphicClass
      function SupportsGraphicClass(AGraphicClass:TGraphicClass):boolean;virtual;abstract;
  end;

  //A simple format which is able  to load bmps, dibs, wmfs and emfs.
  TSimpleFormat = class(TPictFormat)
    public
      //Fills a list with its supported graphic extension.
      procedure FileExts(strs:TStringList);override;
      //Loads the graphic from a file and stros it in a TAdBitmap.
      function LoadFromFile(AFile:string;ABmp:TAdBitmap;Transparent:boolean;TransparentColor:TColor):boolean;override;
      //Assigns an TGraphic and  stores it in a TAdBitmap
      procedure AssignGraphic(AGraphic:TGraphic;ABmp:TAdBitmap);override;
      //Returns true if this format supports the graphicclass defined in AGraphicClass
      function SupportsGraphicClass(AGraphicClass:TGraphicClass):boolean;override;
  end;

  //A class of the picture format
  TPictFormatClass = class of TPictFormat;

  //Represents a bitmap texture
  TAdTexture = class
    private
      FParent:TAdDraw;
      FCache:TMemoryStream;
      FAd2DTexture:TAd2DBitmapTexture;
      FCompressor:TCompressor;
      FCompressorClass:TCompressorClass;
      FBitDepth:byte;
      function GetInitialized:boolean;
      procedure SetCompressor(AClass:TCompressorClass);
      function GetBitDepth:byte;
      procedure SetBitDepth(AValue:byte);
    protected
      procedure Notify(ASender:TObject;AEvent:TSurfaceEventState);
    public
      //Creates an instance of TAdTexture
      constructor Create(AParent:TAdDraw);
      //Destroys the instance of TAdTexture
      destructor Destroy;override;

      //Loads the texture from a stream.
      procedure LoadFromStream(AStream:TStream);
      //Saves the texture to a stream.
      procedure SaveToStream(AStream:TStream);

      //Saves the texture's data to a file.
      procedure SaveToFile(AFile:string);
      //Loads the texture's data from a file. If you want to load the texture's graphic, call LoadGraphicFromFile or LoadFromGraphic
      procedure LoadFromFile(AFile:string);
  
      //Loads the graphic of the texture from a file
      procedure LoadGraphicFromFile(AFile:string;Transparent:boolean;TransparentColor:TColor);
      //Loads the graphic of the texture from a TGraphic
      procedure LoadFromGraphic(AGraphic:TGraphic);

      //Initializes the texture. Creates a TAd2DBitmapTexture object. 
      procedure Initialize;
      //Finalizes the texture. Frees the TAd2DBitmapTexture object. 
      procedure Finalize;
      //Clears the textures graphic
      procedure Clear;
      //A link to Andorras TAd2DBitmapTexture
      property Texture:TAd2DBitmapTexture read FAd2DTexture;
      //Returns whether the texture is initialized
      property Initialized:boolean read GetInitialized;
      //Set a compressor class. Default: TBMPCompressor;
      property Compressor:TCompressorClass read FCompressorClass write SetCompressor;
      //BitDepth
      property BitDepth:byte read GetBitDepth write SetBitDepth;
  end;

  {A list which is able to contain TRects}
  TRectList = class(TAdList)
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
      FColor:TColor;
      FLastColor:TAndorraColor;
      FAlpha:byte;
      FName:string;
      FDetails:integer;
      FOwnTexture:boolean;
      FSrcRect:TRect;
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
      procedure DrawMesh(DestApp:TAdDraw;DestRect,SourceRect:TRect;Rotation:integer;
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
      procedure StretchBltAlpha(Dest:TAdDraw; SourceRect,DestRect:TRect;CenterX,CenterY:double;Angle:Integer;Alpha:Integer);
      //Draw only specified part from the image. Additive blending.
      procedure StretchBltAdd(Dest:TAdDraw; SourceRect,DestRect:TRect;CenterX,CenterY:double;Angle:Integer;Alpha:Integer);
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
      property Color:TColor read FColor write FColor;
      //Name of the image in the imagelist.
      property Name:string read FName write FName;
      //Important for using lights: How many vertices does the image have.
      property Details:integer read FDetails write SetDetails;
  end;

  //Administrates the images
  TAdImageList = class(TAdList)
    private
      FParent:TAdDraw;
      FCompressor:TCompressorClass;
     	function GetItem(AIndex:integer):TAdImage;
     	procedure SetItem(AIndex:integer;AItem:TAdImage);
      procedure SetCompressor(ACompressor:TCompressorClass);
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
      property Compressor:TCompressorClass read FCompressor write SetCompressor; 
    published
  end;

  //Defines how the output of the TextOut-Ex function will be
  TAdFontDrawType = (
    dtBottom,
    dtTop,
    dtMiddle,
    dtLeft,
    dtRight,
    dtCenter,
    dtClip,
    dtWordWrap,
    dtDoLineFeeds
  );

  //A set of the draw type, used by TAdFont.FontOutEx
  TAdFontDrawTypes = set of TAdFontDrawType;

  //An array class to store the size of each letter of a font
  TPointArray = array[0..255] of TPoint;

  TFontMetadata = record
    Name:string[50];
    Size:integer;
    Style:TFontStyles;
    Shadow:boolean;
    ShadowDepth:integer;
    ShadowBlur:integer;
    ShadowAlpha:byte;
  end;

  //A text output class
  TAdFont = class
    private
      FTexture:TAdTexture;
      FMeshList:TAdImageList;
      FParent:TAdDraw;
      FLetterSize:TPointArray;
      FLetterCount:integer;
      FColor:TColor;
      FAlpha:byte;
      FPatternWidth,FPatternHeight:integer;
      FCompressor:TCompressorClass;
      FName:string;
      FOnlyMetadata:boolean;
      FMetadata:TFontMetadata;
      procedure SetColor(AValue:TColor);
      procedure SetAlpha(AValue:byte);
      function GetLoaded:boolean;
      procedure SetCompressor(AValue:TCompressorClass);
    protected
      property MeshList:TAdImageList read FMeshList;
      property LetterSize:TPointArray read FLetterSize;
      property PatternWidth:integer read FPatternWidth;
      property PatternHeight:integer read FPatternHeight;
      property Texture:TAdTexture read FTexture write FTexture;
    public
      //If true the TAdFontCollection will automaticly free this class
      CreatedByList:boolean;

      //Creates an instance of TAdFont
      constructor Create(AParent:TAdDraw);
      //Destroys the instance of TAdFont
      destructor Destroy;override;

      //Creates a font
      procedure CreateFont(AFont:string;AStyle:TFontStyles;ASize:integer;
        AShadow:boolean=false;AShadowDepth:integer=0;AShadowBlur:integer=0;
        AShadowAlpha:byte=64);
      //Removes the font from memory
      procedure ClearFont;

      //Writes the text
      procedure TextOut(AX,AY:integer;AText:string);
      //An extended Textout function.
      procedure TextOutEx(ARect:TRect;AText:string;ADrawType:TAdFontDrawTypes);
      //Returns the width of a specific text
      function TextWidth(AText:string):integer;
      //Returns the height of a specific text
      function TextHeight(AText:string):integer;

      //Saves the font to stream
      procedure SaveToStream(AStream:TStream);
      //Loads the font from a stream
      procedure LoadFromStream(AStream:TStream);
      //Saves the font to a file
      procedure SaveToFile(AFile:string);
      //Loads the font from a file
      procedure LoadFromFile(AFile:string);

      //Returns wether a font is loaded.
      property Loaded:boolean read GetLoaded;
      //The color of the font
      property Color:TColor read FColor write SetColor;
      //The transparency of the font
      property Alpha:byte read FAlpha write SetAlpha;
      //The compressor used to save the font
      property Compressor:TCompressorClass read FCompressor write SetCompressor;
      //The name of the font in the list
      property Name:string read FName write FName;
      //Defines wether only metadata to reconstruct the font is saved in files or streams
      property SaveOnlyMetadata:boolean read FOnlyMetadata write FOnlyMetadata;
      //Contains informations about the current font
      property Metadata:TFontMetadata read FMetadata; 
  end;

  //Contains the fonts
  TAdFontCollection = class(TAdList)
    private
      FParent:TAdDraw;
      FOnlyMetadata:boolean;
      procedure SetItem(Index:integer; Value:TAdFont);
      procedure SetOnlyMetadata(Value:boolean);
      function GetItem(Index:integer):TAdFont;
      function GetItemByName(Index:string):TAdFont;
    protected
      procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    public
      //Creates an instance of TAdFontCollection
      constructor Create(AParent:TAdDraw);
      //Destroys the instance
      destructor Destroy;override;
      //Adds a font to the list. AName specifies the name you can find the font by.
      function Add(AName,AFont:String;AStyle:TFontStyles;ASize:integer;
        AShadow:boolean=false;AShadowDepth:integer=0;AShadowBlur:integer=0;
        AShadowAlpha:byte=64):TAdFont;overload;
      //Searches for a specific font
      property Font[Index:string]:TAdFont read GetItemByName;default;
      //Searches for a specific item
      property Items[Index:integer]:TAdFont read GetItem write SetItem;
      //Defines weather only metadata to reconstruct the fonts is saved in streams or files
      property SaveOnlyMetadata:boolean read FOnlyMetadata write SetOnlyMetadata;
      
      //Saves the whole collection to a stream
      procedure SaveToStream(AStream:TStream);
      //Loads the whole collection from a stream
      procedure LoadFromStream(AStream:TStream);
      //Saves the whole collection to a file
      procedure SaveToFile(AFile:string);
      //Loads the whole collection from file
      procedure LoadFromFile(AFile:string);
  end;

  TAdPerformanceCounterState = (psPaused, psResumed, psRunning);

  //Class for calculating the FPS and the TimeGap
  TAdPerformanceCounter = class
    private
      FTimeGap:Double;
      FFPS:integer;
      FInterpolate:boolean;
      FState:TAdPerformanceCounterState;
      FLastTickCount:LongInt;
      FTempTime:LongInt;
      FTempFPS:integer;
      FInterpolationFactor:integer;
    public
      property State:TAdPerformanceCounterState read FState;
      property FPS:integer read FFPS;
      property TimeGap:double read FTimeGap;
      property Interpolate:Boolean read FInterpolate write FInterpolate;
      property InterpolationFactor:integer read FInterpolationFactor write FInterpolationFactor;

      constructor Create(ACreatePaused:boolean=false);

      procedure Resume;
      procedure Pause;
      procedure Calculate;    
  end;

var
  //Contains all registered compressors. You must not change the contents.
  RegisteredCompressors:TStringList;
  //Contains all registered pictureformats. You must not change the contents.
  RegisteredFormats:TStringList;

//Is called for registering a compressor class. If you register a compressor it will be automaticly used for decompressing.
procedure RegisterCompressor(AClass:TClass);

//Is called for registering a format class. The format classes are used to give the ability to load files of serveral formats.
procedure RegisterFormat(AClass:TClass);


implementation

procedure RegisterCompressor(AClass:TClass);
begin
  RegisterClass(TPersistentClass(AClass));
  RegisteredCompressors.Add(AClass.ClassName);
end;

procedure RegisterFormat(AClass:TClass);
begin
  RegisterClass(TPersistentClass(AClass));
  RegisteredFormats.Add(AClass.ClassName);
end;

{ TAdDraw }

constructor TAdDraw.Create(AParent : TWinControl);
var amsg:TAdLogMessage;
begin
	inherited Create;
  FParent := AParent;
  FAmbientColor := clWhite;
  AdDllLoader := TAdDllLoader.Create;
  SetupThings;

  FLog := TAdLog.Create;
  FLogFileName := 'adlog.txt';

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


procedure TAdDraw.SetAmbientColor(AValue: TColor);
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

function TAdDraw.Initialize: boolean;
var amsg:TAdLogMessage;
begin

  result := false;
  if AdDllLoader.LibraryLoaded then
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

      AdAppl.Viewport := FParent.ClientRect;

      AdAppl.AmbientLightColor := AD_RGB(GetRValue(FAmbientColor),GetGValue(FAmbientColor),
        GetBValue(FAmbientColor));

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

      FCanvas := TAdCanvas.Create(AdAppl);
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
    if FCanvas <> nil then FCanvas.Free;
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

procedure TAdDraw.ClearSurface(Color:TColor);
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
    AdAppl.EndScene;
    FCanvas.EndFrame;
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

procedure TRectList.Add(ARect: TRect);
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
end;


{TPictureCollectionItem}

constructor TAdImage.Create(AAdDraw:TAdDraw);
begin
  inherited Create;
  FTexture := TAdTexture.Create(AAdDraw);
  FParent := AAdDraw;
  FParent.RegisterNotifyEvent(Notify);
  Rects := TRectList.Create;
  FColor := clWhite;
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
  SourceRect: TRect; Rotation: integer; RotCenterX, RotCenterY: single;
  BlendMode: TAd2DBlendMode);
var
  mat1,mat2:TAdMatrix;
  curx,cury:single;
  Mode:TAd2DDrawMode;
begin
  if (not CompRects(SourceRect,FSrcRect)) then
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
        Rects.Add(Bounds(
          ax*(PatternWidth+FSkipWidth),ay*(PatternHeight+FSkipHeight),
          FPatternWidth,FPatternHeight));
      end;
    end;
  end
  else
  begin
    Rects.Add(Rect(0,0,FWidth,FHeight));
  end;
end;

procedure TAdImage.Draw(Dest:TAdDraw;X,Y,PatternIndex:integer);
begin
  if (Texture.Texture.Loaded) and (Dest.CanDraw) and (AdMesh <> nil) then
  begin
    SetCurrentColor(255);
    if (PatternIndex < 0) then PatternIndex := 0;
    if (PatternIndex > PatternCount-1) then PatternIndex := PatternCount-1;
    DrawMesh(Dest, Rect(X,Y,X+Width,Y+Height), Rects[PatternIndex],
      0, 0, 0, bmAlpha);
  end;
end;

procedure TAdImage.DrawAdd(Dest: TAdDraw; const DestRect: TRect;
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

procedure TAdImage.DrawAlpha(Dest: TAdDraw; const DestRect: TRect;
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

procedure TAdImage.DrawMask(Dest: TAdDraw; const DestRect: TRect;
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
    DrawMesh(Dest, Rect(X,Y,X+Width,Y+Height), Rects[PatternIndex], Angle,
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
      DrawMesh(Dest, Rect(X,Y,X+Width,Y+Height), Rects[PatternIndex], Angle,
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
    DrawMesh(Dest, Rect(X,Y,X+Width,Y+Height), Rects[PatternIndex], Angle,
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
    DrawMesh(Dest,Rect(X,Y,X+Width,Y+Height),Rects[PatternIndex],Angle,CenterX,CenterY,bmMask);
  end;
end;

procedure TAdImage.StretchBltAdd(Dest: TAdDraw; SourceRect,
  DestRect: TRect; CenterX, CenterY:double; Angle, Alpha: Integer);
begin
  if (Texture.Texture.Loaded) and (Dest.CanDraw) and (AdMesh <> nil) then
  begin
    SetCurrentColor(Alpha);
    DrawMesh(Dest,DestRect,SourceRect,Angle,CenterX,CenterY,bmAdd);
  end;
end;

procedure TAdImage.StretchBltAlpha(Dest: TAdDraw; SourceRect,
  DestRect: TRect; CenterX, CenterY:double; Angle, Alpha: Integer);
begin
  if (Texture.Texture.Loaded) and (Dest.CanDraw) and (AdMesh <> nil) then
  begin
    SetCurrentColor(Alpha);
    DrawMesh(Dest,DestRect,SourceRect,Angle,CenterX,CenterY,bmAlpha);
  end;
end;

procedure TAdImage.StretchDraw(Dest: TAdDraw; const DestRect: TRect; PatternIndex: integer);
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

function TAdImage.GetPatternRect(ANr: Integer):TRect;
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

procedure TAdImageList.SetCompressor(ACompressor: TCompressorClass);
var i:integer;
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
end;

{ THAICompressor }

function THAICompressor.GetInitial: TInitialLetters;
begin
  result := #3+'HAI'
end;

procedure THAICompressor.Read(AStream: TStream; ABmp:TAdBitmap);
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
  Compressor := THAICompressor;
  FBitDepth := 32;
end;

destructor TAdTexture.Destroy;
begin
  if FCompressor <> nil then
  begin
    FreeAndNil(FCompressor);
  end;
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
  result := FAd2DTexture <> nil;
end;

procedure TAdTexture.Initialize;
begin
  Finalize;
  FAd2DTexture := FParent.AdAppl.CreateBitmapTexture;
end;

procedure TAdTexture.SaveToFile(AFile: string);
var ms:TMemoryStream;
begin
  ms := TMemoryStream.Create;
  SaveToStream(ms);
  ms.SaveToFile(AFile);
end;

procedure TAdTexture.LoadFromFile(AFile: string);
var ms:TMemoryStream;
begin
  ms := TMemoryStream.Create;
  ms.LoadFromFile(AFile);
  ms.Position := 0;
  LoadFromStream(ms);
  ms.Free;
end;

procedure TAdTexture.LoadFromGraphic(AGraphic: TGraphic);
var
  fmt:TPictFormat;
  i:integer;
  cref:TPictFormatClass;
  bmp:TAdBitmap;
begin
  fmt := nil;
  for i := 0 to RegisteredFormats.Count-1 do
  begin
    cref := TPictFormatClass(GetClass(RegisteredFormats[i]));
    if cref <> nil then
    begin
      fmt := TPictFormat(cref.Create);
      if fmt.SupportsGraphicClass(TGraphicClass(AGraphic.ClassType)) then
      begin
        break;
      end;
      fmt.Free;
      fmt := nil;
    end;
  end;
  if fmt <> nil then
  begin
    bmp := TAdBitmap.Create;
    fmt.AssignGraphic(AGraphic,bmp);
    Texture.LoadFromBitmap(bmp,FBitDepth);
    fmt.Free;
    bmp.Free;
  end;
end;

procedure TAdTexture.LoadGraphicFromFile(AFile: string; Transparent: boolean;
  TransparentColor: TColor);
var
  fmt:TPictFormat;
  i:integer;
  cref:TPictFormatClass;
  ext:string;
  str:TStringList;
  bmp:TAdBitmap;
begin
  fmt := nil;
  ext := lowercase(ExtractFileExt(AFile));
  for i := 0 to RegisteredFormats.Count-1 do
  begin
    cref := TPictFormatClass(GetClass(RegisteredFormats[i]));
    if cref <> nil then
    begin
      fmt := TPictFormat(cref.Create);
      str := TStringlist.Create;
      fmt.FileExts(str);
      if str.IndexOf(ext) > -1 then
      begin
        str.Free;
        break;
      end;
      str.Free;
      fmt.Free;
      fmt := nil;
    end;
  end;
  if fmt <> nil then
  begin
    bmp := TAdBitmap.Create;
    fmt.LoadFromFile(AFile,bmp,transparent,transparentcolor);
    fmt.Free;
    Texture.LoadFromBitmap(bmp,FBitDepth);
    bmp.Free;
  end;
end;

procedure TAdTexture.LoadFromStream(AStream: TStream);
var c:char;
    i:integer;
    s:string;
    cref:TPersistentClass;
    atemp:TCompressor;
    bmp:TAdBitmap;
begin
  AStream.Read(c,1);
  if c = 'T' then
  begin
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
      bmp := TAdBitmap.Create;
      ATemp.Read(AStream,bmp);
      AStream.Read(FBitDepth,1);
      Texture.LoadFromBitmap(bmp,FBitDepth);
      Compressor := TCompressorClass(atemp.ClassType);
      ATemp.Free;
      bmp.Free;
    end
    else
    begin
      raise ENoCompressor.Create('The compressor '+s+' is not registered!');
    end;
  end;
end;

procedure TAdTexture.SaveToStream(AStream: TStream);
var c:char;
    bmp:TAdBitmap;
    s:string;
    bits:byte;
begin
  if (Texture.Loaded) and (FCompressor <> nil) then
  begin
    c := 'T'; AStream.Write(c,1);

    s := FCompressor.GetInitial;
    AStream.Write(s[1],4);

    bmp := TAdBitmap.Create;
    bmp.ReserveMemory(Texture.BaseWidth,Texture.BaseHeight);
    Texture.SaveToBitmap(bmp);
    FCompressor.Write(AStream,bmp);
    bmp.Free;

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

procedure TAdTexture.SetCompressor(AClass: TCompressorClass);
begin
  if FCompressor <> nil then
  begin
    FreeAndNil(FCompressor);
  end;
  FCompressor := AClass.Create;
  FCompressorClass := AClass;
end;

{ TAdSimpleFormat }

procedure TSimpleFormat.AssignGraphic(AGraphic: TGraphic; ABmp: TAdBitmap);
var bmp:TBitmap;
begin
  if SupportsGraphicClass(TGraphicClass(AGraphic.ClassType)) then
  begin
    bmp := TBitmap.Create;
    if AGraphic is TBitmap then
    begin
      bmp.Assign(AGraphic);
    end
    else
    begin
      bmp.Width := AGraphic.Width;
      bmp.Height := AGraphic.Height;
      bmp.Canvas.StretchDraw(rect(0,0,bmp.Width,bmp.Height),AGraphic);
    end;
    ABmp.AssignBitmap(bmp);
    bmp.Free;
  end
  else
  begin
    raise EFormatNotSupportet.Create('Can not assign the graphic with the format '+AGraphic.ClassName+'. Only support TBitmap, TIcon and TMetafile.');
  end;
end;

procedure TSimpleFormat.FileExts(strs: TStringList);
begin
  strs.Add('.bmp');
  strs.Add('.dib');
  strs.Add('.ico');
  strs.Add('.wmf');
  strs.Add('.emf');
end;

function TSimpleFormat.LoadFromFile(AFile: string; ABmp: TAdBitmap;Transparent:boolean;TransparentColor:TColor): boolean;
var
  pict:TPicture;
  bmp:TBitmap;
begin
  result := true;
  pict := TPicture.Create;
  try
    pict.LoadFromFile(AFile);
    bmp := TBitmap.Create;
    if pict.Graphic is TBitmap then
    begin
      bmp.Assign(pict.Graphic);
    end
    else
    begin
      bmp.Width := pict.Graphic.Width;
      bmp.Height := pict.Graphic.Height;
      bmp.Canvas.StretchDraw(rect(0,0,bmp.Width,bmp.Height),pict.Graphic);
    end;
    bmp.Transparent := Transparent;
    bmp.TransparentColor := TransparentColor;
    bmp.TransparentMode := tmFixed;
    ABmp.AssignBitmap(bmp);
    bmp.Free;
  except
    result := false;
  end;
  pict.Free;
end;

function TSimpleFormat.SupportsGraphicClass(AGraphicClass: TGraphicClass): boolean;
begin
  {$IFDEF FPC}
    result := (AGraphicClass = TBitmap) or (AGraphicClass = TIcon);
  {$ELSE}
    result := (AGraphicClass = TBitmap) or (AGraphicClass = TMetafile) or (AGraphicClass = TIcon);
  {$ENDIF}
end;

{ TAdFont }

constructor TAdFont.Create(AParent: TAdDraw);
begin
  inherited Create;
  FParent := AParent;
  FMeshList := TAdImageList.Create(FParent);
  FTexture := TAdTexture.Create(FParent);
  FCompressor := FTexture.Compressor;
  FLetterCount := 0;
  FColor := clWhite;
  FAlpha := 255;
  CreatedByList := false;
  ClearFont;
end;

destructor TAdFont.Destroy;
begin
  FMeshList.Free;
  FTexture.Free;
  inherited;
end;

{$IFNDEF FPC}
procedure SetFontQuality (aFont: TFont;  aQuality: Byte);
var
  LF : TLogFont;
begin
  GetObject(aFont.Handle, SizeOf(TLogFont), @LF);
  LF.lfQuality := aQuality;
  aFont.Handle := CreateFontIndirect(LF);
end;
{$ENDIF}

procedure TAdFont.CreateFont(AFont: string; AStyle: TFontStyles; ASize: integer;
  AShadow: boolean; AShadowDepth, AShadowBlur: integer; AShadowAlpha: byte);
var
  bmp,bmp2:TBitmap;
  adbmp:TAdBitmap;
  maxw,maxh:integer;
  ax,ay:integer;
  i: integer;
begin
  ClearFont;

  with FMetadata do
  begin
    Name := AFont;
    Style := AStyle;
    Size := ASize;
    Shadow := AShadow;
    ShadowDepth := ShadowDepth;
    ShadowBlur := ShadowBlur;
    ShadowAlpha := ShadowAlpha;
  end;

  FName := AFont+inttostr(ASize);

  bmp := TBitmap.Create;
  bmp.Width := 1;
  bmp.Height := 1;
  with bmp.Canvas do
  begin
    with Font do
    begin
      Name := AFont;
      Style := AStyle;
      Size := ASize;
      Color := clWhite;
    end;

    maxw := 0;
    maxh := 0;
    for i := 0 to 255 do
    begin
      ax := TextWidth(chr(i));
      ay := TextHeight(chr(i));
      if (i = 0) or (ax > maxw) then maxw := ax;
      if (i = 0) or (ay > maxh) then maxh := ay;
      FLetterSize[i] := point(ax,ay);
    end;

    maxw := maxw + abs(AShadowDepth) + AShadowBlur;
    maxh := maxh + abs(AShadowDepth) + AShadowBlur;

    bmp.Width := (maxw+1)*16;
    bmp.Height := (maxh+1)*16;
    Brush.Color := clBlack;
    FillRect(ClipRect);

    Brush.Style := Graphics.bsClear;

    bmp2 := nil;
    if AShadow then
    begin
      bmp2 := TBitmap.Create;
      bmp2.Assign(bmp);
      bmp2.Canvas.Brush.Assign(Brush);
      bmp2.Canvas.Font.Assign(Font);
      bmp2.Canvas.Font.Color := RGB(AShadowAlpha,AShadowAlpha,AShadowAlpha);
    end;

    {$IFNDEF FPC}
      SetFontQuality (bmp.Canvas.Font, NONANTIALIASED_QUALITY);
    {$ENDIF}

    for ay := 0 to 15 do
    begin
      for ax := 0 to 15 do
      begin
        if AShadow then
        begin
          if AShadowDepth > 0 then
          begin
            Textout(ax*(maxw+1),
                    ay*(maxh+1),chr(ay*16+ax));
            bmp2.Canvas.Textout(ax*(maxw+1)+AShadowDepth,
                                ay*(maxh+1)+AShadowDepth,chr(ay*16+ax));
          end
          else
          begin
            Textout(ax*(maxw+1)+abs(AShadowDepth),
                    ay*(maxh+1)+abs(AShadowDepth),chr(ay*16+ax));
            bmp2.Canvas.Textout(ax*(maxw+1),
                                ay*(maxh+1),chr(ay*16+ax));
          end;
        end
        else
        begin
          Textout(ax*(maxw+1),ay*(maxh+1),chr(ay*16+ax));
        end;
      end;
    end;
  end;

  bmp.Transparent := true;
  bmp.TransparentColor := clBlack;

  if AShadow then
  begin
    if AShadowBlur > 0 then
    begin
      {$IFDEF FPC}{$ELSE}
      BmpGBlur(bmp2,AShadowBlur);
      {$ENDIF}
    end;
    bmp2.Canvas.Draw(0,0,bmp);
    adbmp := TAdBitmap.Create;
    adbmp.AssignBitmap(bmp);
    adbmp.AssignAlphaChannel(bmp2);
    FTexture.Texture.LoadFromBitmap(AdBmp,32);
    adbmp.Free;
  end
  else
  begin
    FTexture.LoadFromGraphic(bmp);
  end;

  FLetterCount := 256;

  for i := 0 to 255 do
  begin
    with FMeshList.Add('') do
    begin
      Texture := self.FTexture;
      PatternWidth := maxw;
      PatternHeight := maxh;
      SkipWidth := 1;
      SkipHeight := 1;
    end;
  end;
  FMeshList.Restore;

  FPatternWidth := maxw;
  FPatternHeight := maxh;

  bmp.Free;
  if AShadow then bmp2.Free;
end;

procedure TAdFont.ClearFont;
var i:integer;
begin
  FTexture.Clear;
  FMeshList.Clear;
  for i := 0 to 255 do
  begin
    FLetterSize[i] := point(0,0);
  end;
  FLetterCount := 0;
end;

function TAdFont.TextHeight(AText: string): integer;
var i:integer;
begin
  result := 0;
  for i := 1 to length(AText) do
  begin
    if (FLetterSize[ord(AText[i])].Y > result) then
    begin
      result := FLetterSize[ord(AText[i])].Y;
    end;
  end;
end;

function TAdFont.TextWidth(AText: string): integer;
var i:integer;
begin
  result := 0;
  for i := 1 to length(AText) do
  begin
    result := result + FLetterSize[ord(AText[i])].X;
  end;
end;

procedure TAdFont.TextOut(AX, AY: integer;AText: string);
var i:integer;
    x:integer;
    c:byte;
begin
  if Loaded then
  begin
    x := 0;
    for i := 1 to length(AText) do
    begin
      c := ord(AText[i]);
      FMeshList[c].DrawAlpha(FParent,bounds(AX+x,AY,FPatternWidth,FPatternHeight),c,Alpha);
      x := x + FLetterSize[c].X;
    end;
  end;
end;

procedure TAdFont.TextOutEx(ARect: TRect; AText:string; ADrawType: TAdFontDrawTypes);
var
  lines:TStringList;
  w,h,i,y,lastspace,lastpos:integer;
  s:string;
begin
  lines := TStringList.Create;

  w := ARect.Right - ARect.Left;

  //Calculate text lines if neccessary
  if dtWordWrap in ADrawType then
  begin
    s := '';
    i := 0;
    lastspace := -1;
    lastpos := 0;
    while i < length(AText) do
    begin
      i := i + 1;
      if ((AText[i] = #10) or (AText[i] = #13)) and (dtDoLineFeeds in ADrawType) then
      begin
        lines.Add(s);
        lastspace := -1;
        lastpos := i;
        s := '';
      end
      else
      begin
        s := s + AText[i];
        if AText[i] = ' ' then
        begin
          lastspace := length(s);
          lastpos := i;
        end;
        if (TextWidth(s) > w) and (lastspace <> -1) then
        begin
          i := lastpos;
          s := Copy(s,1,lastspace-1);
          lines.Add(s);
          lastspace := -1;
          s := '';
        end;
      end;
    end;
    lines.Add(s);
  end
  else
  begin
    lines.Add(AText);
  end;

  h := 0;
  y := 0;

  if (dtMiddle in ADrawType) or (dtBottom in ADrawType) then
  begin
    for i := 0 to lines.Count - 1 do
    begin
      h := h + TextHeight(lines[i]);
    end;
  end;

  if (dtTop in ADrawType) then
  begin
    y := ARect.Top;
  end else
  if dtBottom in ADrawType then
  begin
    y := ARect.Bottom - h;
  end else
  if dtMiddle in ADrawType then
  begin
    y := ARect.Top + (ARect.Bottom - ARect.Top - h) div 2;
  end;

  for i := 0 to lines.Count - 1 do
  begin
    if dtLeft in ADrawType then
    begin
      TextOut(ARect.Left,y,lines[i]);
    end else
    if dtRight in ADrawType then
    begin
      TextOut(ARect.Right-TextWidth(lines[i]),y,lines[i]);
    end else
    if dtCenter in ADrawType then
    begin
      TextOut(ARect.Left+(w-TextWidth(lines[i])) div 2,y,lines[i]);
    end;
    y := y + TextHeight(lines[i]);
  end;
  
  lines.Free;
end;

procedure TAdFont.LoadFromFile(AFile: string);
var ms:TMemoryStream;
begin
  ms := TMemoryStream.Create;
  ms.LoadFromFile(AFile);
  ms.Position := 0;
  LoadFromStream(ms);
  ms.Free;
end;

procedure TAdFont.LoadFromStream(AStream: TStream);
var
  i,l:integer;
  c:char;
begin
  AStream.Read(l,SizeOf(l));
  SetLength(FName,l);
  AStream.Read(FName[1],l);
  AStream.Read(c,1);
  if c = 'R' then
  begin
    ClearFont;
    AStream.Read(FLetterSize[0],SizeOf(FLetterSize));
    AStream.Read(FPatternWidth,SizeOf(Integer));
    AStream.Read(FPatternHeight,SizeOf(Integer));
    FTexture.LoadFromStream(AStream);
    if FTexture.FAd2DTexture.Loaded then
    begin
      FLetterCount := 255;
      for i := 0 to 255 do
      begin
        with FMeshList.Add('') do
        begin
          Texture := self.FTexture;
          PatternWidth := self.FPatternWidth;
          PatternHeight := self.FPatternHeight;
          SkipWidth := 1;
          SkipHeight := 1;
        end;
      end;
      FMeshList.Restore;
    end;
  end
  else
  begin
    AStream.Read(FMetadata,SizeOf(TFontMetadata));
    with FMetadata do
    begin
      CreateFont(Name,Style,Size,Shadow,ShadowDepth,ShadowBlur,ShadowAlpha);
    end;
    FOnlyMetadata := true;
  end;
end;

procedure TAdFont.SaveToFile(AFile: string);
var ms:TMemoryStream;
begin
  ms := TMemoryStream.Create;
  SaveToStream(ms);
  ms.SaveToFile(AFile);
  ms.Free;
end;

procedure TAdFont.SaveToStream(AStream: TStream);
var
  c:char;
  l:integer;
begin
  l := Length(FName);
  AStream.Write(l,SizeOf(l));
  AStream.Write(FName[1],l);
  if FOnlyMetadata then
  begin
    c := 'M';
    AStream.Write(c,1);
    AStream.Write(FMetadata,SizeOf(TFontMetadata));
  end
  else
  begin
    c := 'R';
    AStream.Write(c,1);
    AStream.Write(FLetterSize[0],SizeOf(FLetterSize));
    AStream.Write(FPatternWidth,SizeOf(Integer));
    AStream.Write(FPatternHeight,SizeOf(Integer));
    FTexture.SaveToStream(AStream);
  end;
end;

function TAdFont.GetLoaded: boolean;
begin
  result := FTexture.Texture.Loaded and (FMeshList.Count > 0) and (FLetterCount > 0);
end;

procedure TAdFont.SetAlpha(AValue: byte);
begin
  FAlpha := AValue;
end;

procedure TAdFont.SetColor(AValue: TColor);
var i:integer;
begin
  if FColor <> AValue then
  begin
    FColor := AValue;
    for i := 0 to FMeshList.Count - 1 do
    begin
      FMeshList[i].Color := AValue;
      FMeshList[i].Restore;
    end;
  end;
end;

procedure TAdFont.SetCompressor(AValue: TCompressorClass);
begin
  FTexture.Compressor := AValue;
  FCompressor := AValue;
end;

{ TAdFontCollection }

function TAdFontCollection.Add(AName, AFont: String; AStyle: TFontStyles;
  ASize: integer; AShadow: boolean; AShadowDepth, AShadowBlur: integer;
  AShadowAlpha: byte):TAdFont;
begin
  result := TAdFont.Create(FParent);
  result.CreateFont(AFont,AStyle,ASize,AShadow,AShadowDepth,AShadowBlur,AShadowAlpha);
  result.Name := AName;
  result.CreatedByList := true;
  result.SaveOnlyMetadata := SaveOnlyMetadata;
  Add(result);
end;

constructor TAdFontCollection.Create(AParent: TAdDraw);
begin
  inherited Create;
  FParent := AParent;
end;

destructor TAdFontCollection.Destroy;
begin
  inherited;
end;

function TAdFontCollection.GetItem(Index: integer): TAdFont;
begin
  result := inherited Items[Index];
end;

function TAdFontCollection.GetItemByName(Index: string): TAdFont;
var i:integer;
begin
  result := nil;
  for i := 0 to Count-1 do
  begin
    if Items[i].Name = Index then
    begin
      result := Items[i];
      break;
    end;
  end;
end;

procedure TAdFontCollection.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited;
  if Action = lnDeleted then
  begin
    if (ptr <> nil) and TAdFont(ptr).CreatedByList then
    begin
      TAdFont(ptr).Free;
    end;
  end;
end;

procedure TAdFontCollection.SetItem(Index: integer; Value: TAdFont);
begin
  inherited Items[Index] := Value;
end;

procedure TAdFontCollection.SetOnlyMetadata(Value: boolean);
var
  i:integer;
begin
  FOnlyMetadata := Value;
  for i := 0 to Count - 1 do
  begin
    Items[i].SaveOnlyMetadata := Value;
  end;
end;

procedure TAdFontCollection.LoadFromFile(AFile: string);
var
  ms:TMemoryStream;
begin
  ms := TMemoryStream.Create;
  ms.LoadFromFile(AFile);
  ms.Position := 0;
  LoadFromStream(ms);
  ms.Free;
end;

procedure TAdFontCollection.SaveToFile(AFile: string);
var
  ms:TMemoryStream;
begin
  ms := TMemoryStream.Create;
  SaveToStream(ms);
  ms.SaveToFile(AFile);
  ms.Free;
end;

procedure TAdFontCollection.LoadFromStream(AStream: TStream);
var
  c,i:integer;
  tmp:TAdFont;
begin
  Clear;
  AStream.Read(c,SizeOf(c));
  for i := 0 to c - 1 do
  begin
    tmp := TAdFont.Create(FParent);
    tmp.LoadFromStream(AStream);
    tmp.CreatedByList := true;
    Add(tmp);    
  end;
end;

procedure TAdFontCollection.SaveToStream(AStream: TStream);
var
  c,i:integer;
begin
  c := Count;
  AStream.Write(c,SizeOf(c));
  for i := 0 to Count - 1 do
  begin
    Items[i].SaveToStream(AStream);
  end;
end;

{ TBMPCompressor }

function TBMPCompressor.GetInitial: TInitialLetters;
begin
  result := #5+'BMP'
end;

procedure TBMPCompressor.Read(AStream: TStream; ABmp: TAdBitmap);
begin
  ABmp.LoadFromStream(AStream);
end;

procedure TBMPCompressor.Write(AStream: TStream; ABmp: TAdBitmap);
begin
  ABmp.SaveToStream(AStream);
end;

{ TAdPerformanceCounter }

{$IFNDEF WIN32}
function GetTickCount:Cardinal;
var
  tv:timeval;
begin
  GetTimeOfDay(tv, nil);
  result := int64(tv.tv_sec) * 1000 + tv.tv_usec div 1000;
end;
{$ENDIF}

{ TAdPerformanceCounter }

constructor TAdPerformanceCounter.Create(ACreatePaused: boolean);
begin
  inherited Create;

  if ACreatePaused then
  begin
    FState := psPaused;
  end
  else
  begin
    FState := psResumed;
  end;

  FTempTime := 0;
  FLastTickCount := GetTickCount;
  FInterpolate := true;
  FInterpolationFactor := 10;
end;

procedure TAdPerformanceCounter.Calculate;
var
  tc,td:LongInt;
begin
  tc := GetTickCount;
  td := tc - FLastTickCount;

  if FState = psRunning then
  begin
    if FInterpolate then
    begin
      FTimeGap := (FTimeGap * FInterpolationFactor + (td)) / (FInterpolationFactor + 1);
    end
    else
    begin
      FTimeGap := td;
    end;
  end else
  begin
    if FState = psResumed then
    begin
      FTimeGap := 1;
      FState := psRunning;
    end
    else
    begin
      FTimeGap := 0;
    end;
  end;

  FLastTickCount := tc;

  FTempTime := FTempTime + td;
  FTempFPS := FTempFPS + 1;
  if FTempTime > 1000 then
  begin
    FTempTime := 0;
    FFPS := FTempFPS;
    FTempFPS := 0;
  end;
end;

procedure TAdPerformanceCounter.Pause;
begin
  FState := psPaused;
end;

procedure TAdPerformanceCounter.Resume;
begin
  if FState = psPaused then
  begin
    FState := psResumed;
  end;
end;

initialization
  RegisteredCompressors := TStringList.Create;
  RegisteredFormats := TStringList.Create;
  RegisterCompressor(TBMPCompressor);
  RegisterCompressor(THAICompressor);
  RegisterFormat(TSimpleFormat);

finalization
  RegisteredCompressors.Free;
  RegisteredFormats.Free;

end.
