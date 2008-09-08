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
* Comment: Contains the main Andorra 2D classes for graphic output
}

{ Contains the main Andorra 2D classes for graphic output }
unit AdDraws;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

{'$DEFINE DO_NOT_INCLUDE_STD_FORMATS}
{'$DEFINE DO_NOT_INCLUDE_STD_WINDOWMGR}

uses

  SysUtils, Classes,
  AdClasses, AdTypes, AdList, AdPersistent,
  AdWindowFramework, AdDLLLoader, AdLog, AdMath, AdMessages,
  AdCanvas, AdBitmap, AdFontFactory, AdFont, AdEvents
  {$IFNDEF DO_NOT_INCLUDE_STD_FORMATS}
  ,AdStandardFontGenerator, AdSimpleCompressors, AdFormats
  {$ENDIF}
  {$IFNDEF DO_NOT_INCLUDE_STD_WINDOWMGR}
  ,AdComponentWindow
  {$ENDIF};

type

  {Exception class raised when having problems while loading/saving something
   within AdDraws.pas}
  EAdIO = class(Exception);
  {Problem that occured while loading an image.}
  EAdLoadImage = class(EAdIO);
  {Problem that occured while saving an image.}
  EAdSaveImage = class(EAdIO);
  {Problem when creating an Andorra interface}
  EAdInterfaceCreation = class(Exception);

  TAdDraw = class;
  TAdCustomImage = class;
  TAdImage = class;
  TAdCustomTexture = class;
  TAdRenderTargetTexture = class;
  TAdTexture = class;

  {Event type used by TAdRenderingObject.
   @param(AModelViewProjection specifies the matrix, that results when
     multiplying the model, the view and the projection matrix)
   @seealso(TAdRenderingObject)}
  TAdBeginRenderEvent =
    procedure(Sender: TObject; AModelViewProjection: TAdMatrix) of object;

  {Objects derived from this class provide a set of render events. Other classes
   may link themselves to those events and can activate/deactivate certain
   settings before and after the object was rendered. This mechanism is used
   for simply attaching shaders to an object.}
  TAdRenderingObject = class
    private
      FBeginRender: TAdBeginRenderEvent;
      FEndRender: TAdNotifyEvent;
    public
      {Triggered before the object is rendered. The "ModelViewProjejectionMatrix"
       is passed as a single parameter.}
      property OnBeginRender: TAdBeginRenderEvent read FBeginRender write FBeginRender;
      {Triggered when rendering has been finished.}
      property OnEndRender: TAdNotifyEvent read FEndRender write FEndRender;
  end;

  {Specifies the event which called the procedure}
  TAdSurfaceEventState = (
    seInitialize, {< Triggered when TAdDraw is initialized - all Andorra 2D
      components should obtain a new Andorra 2D plugin object}
    seInitialized, {< Triggered after all Andorra 2D components have been
      initialized}
    seFinalize, {< Triggered when TAdDraw is finalized - all Andorra 2D
      components should free their reserved Andorra 2D plugin object because
      it may not be valid anymore.}
    seBeginScene, {< Triggered when the "BeginScene" method of TAdDraw is
      called. }
    seEndScene {< Triggered when the "EngScene" method of TAdDraw is called.}
  );

  {The declaration of the surface event handler callback procedure.}
  TAdSurfaceEvent = procedure(Sender:TObject;
    AEvent:TAdSurfaceEventState) of object;

  {A pointer on TSurfaceEvent}
  PAdSurfaceEvent = ^TAdSurfaceEvent;

  {A list class for registering the surface events in TAdDraw.
   @seealso(TAdSurfaceEvent)}
  TAdSurfaceEventList = class(TAdList)
    private
      function GetItem(AIndex:integer):TAdSurfaceEvent;
      procedure SetItem(AIndex:integer;AItem:TAdSurfaceEvent);
    protected
      procedure Notify(Ptr: Pointer; Action: TListNotification);override;
    public
      {Allows access on each TAdSurfaceEvent element in the list.}
      property Items[AIndex:integer]:TAdSurfaceEvent read GetItem write SetItem; default;
      {Adds a callback procedure to the list.}
      procedure Add(Item:TAdSurfaceEvent);
      {Removes a specific callback procedure to the list.}
      procedure Remove(Item:TAdSurfaceEvent);
  end;


  {Used internally in TAdPluginProperty list to store set property data.}
  TAdPluginPropertyListPointerSet = record
    {Pointer to the property data.}
    SetPtr: Pointer;
    {Size of the data.}
    SetSize: integer;
  end;

  {List used to store the plugin properties. TAdPluginPropertyList permits acces
   on available properties in the plugin and allows the user to set them.
   An instance of TAdPluginPropertyList is held by TAdDraw by default.}
  TAdPluginPropertyList = class
    private
      FProps: array of TAd2dProperty;
      FValues: array of TAd2dPropertyValue;
      FPtrs: array of TAdPluginPropertyListPointerSet;
      FCount: integer;
      function GetProperty(Index: integer): TAd2dProperty;
      procedure AddValue(AName: string; var AValue; ASize: Integer);
      procedure AddProp(AProp: TAd2dProperty);
    public
      {Creates an instance of TAdPluginPropertyList. Properties from the plugin
       are read via the "ReadProperties" method.}
      constructor Create;
      {Destroys the instance of TAdPluginPropertyList.}
      destructor Destroy; override;

      {Clears all read properties and all settings that have been made.}
      procedure Clear;

      {Reads the available properties from a loaded plugin library. After this
       properties may be accesed by the "Items" property of
       TAdPluginPropertyList.}
      procedure ReadProperties(ADllLoader: TAdDllLoader);
      {Writes all set properties into a created Andorra 2D application. If a
       property does not exists, nothing will happen. The WriteProperties
       is normally automatically be called by TAdDraw.}
      procedure WriteProperties(AAppl: TAd2DApplication);

      {Sets a boolean property. Properties are stored when the "WriteProperties"
       method is called.}
      procedure SetProp(AName: string; AValue: boolean);overload;
      {Sets a integer property. Properties are stored when the "WriteProperties"
       method is called.}
      procedure SetProp(AName: string; AValue: integer);overload;
      {Sets a resolution property. Properties are stored when the
       "WriteProperties" method is called.}
      procedure SetProp(AName: string; AValue: TAd2dResolution);overload;

      {Returns whether a certain property exists.}
      function PropertyExists(AName: string): boolean;

      {Allows to read plugin the properties directly.}
      property Items[Index: integer]: TAd2dProperty read GetProperty; default;
      {Returns the current property count.}
      property Count: integer read FCount;
  end;

  {TAdSurface is the abstract base class for all Andorra 2D surfaces. A surface
   represents a kind of virtual picture where drawing operations may target on.
   In fact, although many drawing operations require a "Surface" parameter, they
   are not bound to a specific surface, since they allways affect the active
   surface. A surface is activated by calling the "Activate" function. Graphic
   operations requiring a "Surface" parameter only call this activate function
   before performing graphic operations.
   @seealso(TAdDraw)
   @seealso(TAdScene)
   @seealso(TAdRenderingSurface)
   @seealso(TAdTextureSurface)}
  TAdSurface = class
    private
      FDraw : TAdDraw;
      FActivated: boolean;
      procedure SetDraw(AValue:TAdDraw);
    protected
      property AdDraw:TAdDraw read FDraw write SetDraw;

      procedure DoActivation;virtual;
      procedure DoDeactivation;virtual;
      procedure Notify(ASender:TObject;AEvent:TAdSurfaceEventState);virtual;
    public
      {Creates an instance of TAdSurface and links itself to the TAdDraw object.
       Remember that TAdSurface is only an abstract class and so you should
       never create an instance of it directly. Use TAdTextureSurface or TAdDraw
       instead.}
      constructor Create(ADraw:TAdDraw);virtual;

      {Activates the surface so that graphic operations will affect this surface
       from now on.}
      procedure Activate;
      {Deactivates the surface. This procedure is called automatically by the
       parent TAdDraw when another surface is activated.}
      procedure Deactivate;

      {Indicates wheter the surface is currently activated or not.}
      property Activated: boolean read FActivated;

      {Clears the surface with a specific color.}
      procedure ClearSurface(AColor: LongInt);virtual;

      {Returns wheteher the surface is able to be activated and used.}
      function CanDraw:boolean;virtual;abstract;

      {Pointer on the parent AdDraw.}
      property Parent: TAdDraw read FDraw;
  end;

  {Simple class that copes with scene management. TAdScene holds its own copy of
   the current view and projection matrix and applies it to the graphic system
   when needed. TAdScene is used in TAdRenderingSurface (and so in TAdDraw and
   TAdTextureSurface, as they are descendants of TAdRenderingsurface) to allow
   easy access on the scene settings of the surface.}
  TAdScene = class
    private
      FDraw: TAdDraw;
      
      FWidth: integer;
      FHeight: integer;
      FViewport: TAdRect;

      FNearZ, FFarZ: double;

      FViewMatrix: TAdMatrix;
      FProjectionMatrix: TAdMatrix;

      FActivated: boolean;

      FAmbientColor: TAndorraColor;

      FOnActivate: TAdNotifyEvent;

      procedure UpdateMatrix;
      procedure UpdateViewport;
      procedure UpdateAmbientColor;

      procedure SetViewPort(AValue:TAdRect);
      procedure SetViewMatrix(AValue:TAdMatrix);
      procedure SetProjectionMatrix(AValue:TAdMatrix);
      procedure SetAmbientColor(AValue:TAndorraColor);
    public
      {Creates an instance of TAdScene and links itself to the TAdDraw object.}
      constructor Create(ADraw: TAdDraw);
      {Destroys the instance of TAdScene.}
      destructor Destroy;override;
      
      {Pushs the current scene settings in the graphic system and indicates this
       scene object as being "active".}
      procedure Activate;
      {Actually does not more than indicating this scene object to be inactive.}
      procedure Deactivate;

      {Creates an orhtogonal 2D-Scene with a specific width and height.
       SceneWidth and SceneHeight are set to the values passed here.
       The clipping planes are set to tha values in ClipPlaneNearZ and
       ClipPlaneFarZ.}
      procedure Setup2DScene(AWidth, AHeight: integer);
      {Creates an perspectivic 3D-Scene. SceneWidth and SceneHeight are set to
       the values passed here.
       @param(APos defines the position of the camera)
       @param(ADir defines the target point of the camara)
       @param(AUp is a normalized vector that targets at the head of the scene)}
      procedure Setup3DScene(AWidth, AHeight: integer; APos, ADir, AUp: TAdVector3);

      {The ViewMatrix specifies the position of the objects in the scene. It is
       normally established by the SetupScene functions. You can set this
       property to transform the objects on the screen in a special way.}
      property ViewMatrix:TAdMatrix read FViewMatrix write SetViewMatrix;
      {The ProjectionMatrix specifies the way the objects are projected on the
       surface. It is normally established by the SetupScene functions.
       You can set this property to project the objects on the screen in a
       special way.}
      property ProjectionMatrix:TAdMatrix read FProjectionMatrix write SetProjectionMatrix;

      {Defines the rectangle where graphic operations take place. Viewport is
       automatically set when a change in the size of the surface occurs. If you
       want to use your own viewport, don't forget to reset it after such a change
       happened. The viewport is always given in absolute coordinates.}
      property Viewport:TAdRect read FViewPort write SetViewPort;

      {The ambient color of the scene. The ambient color is the base color all
       lights are added to. To use an ambient color, lights have to be enabled.
       @seealso(TAdRenderingSurface.Options)}
      property AmbientColor: TAndorraColor read FAmbientColor write SetAmbientColor;

      {Returns the width of the scene, that specifies the width of the relative
       coordinate system.}
      property Width: integer read FWidth;
      {Returns the height of the scene, that specifies the height of the
       relative coordinate system.}
      property Height: integer read FHeight;

      {The position of the near Z-Clipping plane. The distance between the
       near and the far clipping plane should always be as small as possible,
       because it changes the resolution of the Z-Buffer. A change of the
       value will be in force after setting up a new 2D/3D scene.}
      property ClipPlaneNearZ: double read FNearZ write FNearZ;
      {The position of the far Z-Clipping plane. The distance between the
       near and the far clipping plane should always be as small as possible,
       because it changes the resolution of the Z-Buffer. A change of the
       value will be in force after setting up a new 2D/3D scene.}
      property ClipPlaneFarZ: double read FFarZ write FFarZ;

      {Returns whether the scene is currently activated.}
      property Activated: boolean read FActivated;
      {Points on the parent AdDraw instance.}
      property AdDraw: TAdDraw read FDraw;

      {This event is raised when the current scene is activated. OnActivate
       may already be in use internally, so you have to test if it really is
       set to nil if you want to assign to it.}
      property OnActivate: TAdNotifyEvent read FOnActivate write FOnActivate;
  end;

  {A basic, abstract surface class that extends TAdSurface and adds its own
   canvas and scene object.}
  TAdRenderingSurface = class(TAdSurface)
    private
      FCanvas: TAdCanvas;
      FScene: TAdScene;
      FOptions: TAd2dOptions;

      function GetDisplayRect: TAdRect;
      procedure SetOptions(AOptions: TAd2dOptions);
      procedure SceneActivate(Sender: TObject);
    protected
      procedure Notify(ASender:TObject;AEvent:TAdSurfaceEventState);override;
      procedure DoInitialize;virtual;
      procedure DoFinalize;virtual;
      procedure DoBeginScene;virtual;
      procedure DoEndScene;virtual;
      procedure DoActivation;override;
      procedure DoDeactivation;override;
      function GetWidth: integer;virtual;abstract;
      function GetHeight: integer;virtual;abstract;
      procedure CanvasRelease(Sender:TObject);
    public
      {Creates an instance of TAdRenderingSurface. Remember that
       TAdRenderingSurface is an abstract class, that should not be
       created directly. Use TAdDraw or TAdTextureSurface instead.}
      constructor Create(ADraw:TAdDraw);override;
      {Destroys the instance of TAdRenderingSurface.}
      destructor Destroy;override;

      procedure ClearSurface(AColor: LongInt);override;

      {A hardware accelerated canvas. All drawing operations will automatically
       be made on the owner of the canvas property.}
      property Canvas:TAdCanvas read FCanvas;
      {This is the place where the scene settings for the surface are made.
       @seealso(TAdScene)}
      property Scene: TAdScene read FScene;
      {Options that control the rendering process. When TAdRenderingSurface is
       created, the options are copied from the parent TAdDraw.}
      property Options: TAd2dOptions read FOptions write SetOptions;
      {DisplayRect returns the size of the current szene. Those coordinates are
       relative. }
      property DisplayRect: TAdRect read GetDisplayRect;
      {The width of the surface in pixels.}
      property Width: integer read GetWidth;
      {The height of the surface in pixels.}
      property Height: integer read GetHeight;

      {Calls Scene.Setup2DScene with the width and the height of the surface.
       The viewport is changed, so that it covers the whole screen.
       After calling Setup2DScene the relative and the absolute coordinate
       system are equal.}
      procedure Setup2DScene;
  end;

  {A surface object that allows you to render your graphics in a texture.}
  TAdTextureSurface = class(TAdRenderingSurface)
    private
      FImage : TAdCustomImage;
      FTexture: TAdRenderTargetTexture;
    protected
      procedure DoActivation;override;
      function GetWidth: integer;override;
      function GetHeight: integer;override;
    public
      {Creates an instance of TAdTextureSurface.}
      constructor Create(ADraw:TAdDraw);override;
      {Destroys the instance of TAdTextureSurface}
      destructor Destroy;override;

      {Sets the size of the surface to a new value. Normally the surface has
       a size of 128x128 pixels.}
      procedure SetSize(AWidth, AHeight: integer);

      {Returns whether graphic operations can be performed on the surface.}
      function CanDraw:boolean;override;

      {Image object that can be used to draw the texture directly on another
       surface.}
      property Image: TAdCustomImage read FImage;

      {Pointer on the texture, that contains the scene surface.}
      property Texture: TAdRenderTargetTexture read FTexture;
  end;

  {TAdDisplay represents the video display settings. This record is used for
   setting the size of the video area used by TAdDraw.}
  TAdDisplay = record
    {The width of the video surface.}
    Width: integer;
    {The hieght of the video surface.}
    Height: integer;
    {The bit depth the video surface should be created with. This property only
     affects the fullscreen mode.}
    BitDepth: TAdBitDepth;
    {The horizontal display refresh frequncy. This property only affects the
     fullscreen mode.}
    Freq: integer;
    {The mode the display is created in.
     @seealso(TAdDisplayMode)}
    DisplayMode: TAdWindowDisplayMode;
  end;

  {This is the main class for using Andorra 2D. It represents the main surface.
   Remember that there can only be one instance of TAdDraw at one time. TAdDraw
   is not a singleton, but the underlying graphic plugin may only support one
   graphic context per application. If you want to have more than one surface
   in your application, you may render your content to a TAdTextureSurface and
   finally render this to TAdDraw.}
  TAdDraw = class(TAdRenderingSurface)
  private
    FParent:Pointer;

    FFinalize: TNotifyEvent;
    FInitialize: TNotifyEvent;
    FInitialized: boolean;

    FDisplay: TAdDisplay;

    FSurfaceEventList: TAdSurfaceEventList;
    FActiveSurface: TAdSurface;
    FWnd: TAdWindowFramework;
    FFonts: TAdFontFactory;
    FProperties: TAdPluginPropertyList;
    FLog: TAdLog;

    FDllLoader: TAdDllLoader;
    FDllName: string;

    FAdAppl: TAd2dApplication;

    FLastError: string;

    procedure SetDllName(val : string);
    function SearchWindowFramework:boolean;
    procedure SetActiveSurface(ASurface:TAdSurface);

    procedure SetupDisplay;
    procedure InitDisplay;

    procedure SetLastError(AError: string);

    function GetSurfaceRect: TAdRect;
  protected
    procedure DoActivation;override;
    procedure CallNotifyEvent(AEventState:TAdSurfaceEventState);
    function GetWidth: integer;override;
    function GetHeight: integer;override;
    procedure LogProc(AModule: PChar; ASeverity: TAd2dLogSeverity;
      AMessage: PChar);
  public
    {Creates a new instance of TAdDraw.
     @param(AParent specifies the underlying window control. If you want TAdDraw
       to create its own window, this parameter may be nil. If the window
       control type you've chosen not available, TAdDraw does not initialize
       correctly.)}
    constructor Create(AParent: Pointer); reintroduce;
    {Destroys the instance of TAdDraw. Destroy cause in finalizing the active
     graphic surface. TAdDraw should be the first thing you create and the last
     thing you free in your Andorra 2D application.}
    destructor Destroy; override;

    {Returns true if the context is initialized and you can draw on it.}
    function CanDraw:boolean;override;

    {Initializes the graphic context. If the result is true, inizialization was
     successfull. Remember to setup the display mode and the plugin dll before
     initializing TAdDraw. These steps can automatically be done by TAdSetup.
     Initialize will call the "seInitialize" surface event and after that the
     "seInitialized" event. It an error occured, the result is false. The error
     message can be obtained via the "GetLastError" method.
     @seealso(TAdSetup)
     @seealso(TAdDllExplorer)
     @seealso(TAdDisplay)
     @seealso(RegisterNotifyEvent)
     @seealso(UnRegisterNotifyEvent)}
    function Initialize: boolean;
    {Finalizes the surface. The "seFinalize" surface event is called.}
    procedure Finalize;
    {Restores the surface (e.g. after a window resize).}
    procedure Restore;

    {Exchanges the front- and the backbuffer.}
    procedure Flip;
    {All graphic operations (except for ClearSurface) should stand between
     the "BeginScene" and "EndScene" command.}
    procedure BeginScene;
    {All graphic operations (except for ClearSurface) should stand between
     the "BeginScene" and "EndScene" command. End scene automatically causes
     all registered canvases to call their "release" function.}
    procedure EndScene;

    {Registeres a new SurfaceEvent function.
     @seealso(TAdSurfaceEvent)}
    procedure RegisterNotifyEvent(AProc:TAdSurfaceEvent);
    {Unregisters a registered SurfaceEvent function.}
    procedure UnRegisterNotifyEvent(AProc:TAdSurfaceEvent);

    {If TAdDraw created its own window, call the Run function to make the
     application actually run.}
    procedure Run;

    {If an critical operation (e.g. initializing TAdDraw) failed (returned
     "false"), the "GetLastError" returns the last error message, that may
     be shown to the user. It such a critical error occurs, it is always logged
     in the log file too.}
    function GetLastError: string;

    {The filename of the graphic plugin that should be loaded.}
    property DllName : string read FDllName write SetDllName;
    {Reference to the DllLoader class. This reference can be used to call
     some of the dll functions directly.}
    property DllLoader: TAdDllLoader read FDllLoader;
    {Returns whether the surface is initialized.}
    property Initialized : boolean read FInitialized;
    {TAdDraw contains its own font factory that may be used to display fonts on
     the surface.}
    property Fonts:TAdFontFactory read FFonts;
    {The underlying window framework. Use this property to change properites
     of the window framework like the window caption or to connect surface
     events.}
    property Window:TAdWindowFramework read FWnd;
    {Reference on the currently active surface.}
    property ActiveSurface:TAdSurface read FActiveSurface write SetActiveSurface;
    {Reference on the partent parameter you've passed in the constructor.}
    property Parent: Pointer read FParent;
    {Reference on the Andorra 2D graphic system abstraction layer.}
    property AdAppl: TAd2dApplication read FAdAppl;
    {Provides access on the plugin properties. Those properties can be used to
     pass graphic system specific parameters to the graphic plugin.}
    property Properties: TAdPluginPropertyList read FProperties;
    {Reference on the log file wrapper.}
    property Log: TAdLog read FLog;
    {The display settings.}
    property Display: TAdDisplay read FDisplay;
    {Returns the size of the surface.}
    property SurfaceRect: TAdRect read GetSurfaceRect;

    {Called when TAdDraw is finalized.}
    property OnFinalize : TNotifyEvent read FFinalize write FFinalize;
    {Called when TAdDraw is initialized.}
    property OnInitialize : TNotifyEvent read FInitialize write FInitialize;
  end;

  {TAdCustomTexture is the base, abstract texture class. Descendants of
   TAdCustomTexture specify which type of texture this texture class contains.
   This can either be a bitmap texture or a render target texture. Don't use
   this class directly, use TAdTexture or TAdRenderTargetTexture instead.
   TAdCustomTexture can be assigned to many different graphic interfaces like
   TAdCustomImage, TAdMesh or TAdShader.
   @seealso(TAdTexture)
   @seealso(TAdRenderTargetTexture)
   @seealso(TAdCustomImage)}
  TAdCustomTexture = class
    private
      FParent:TAdDraw;
      FBitDepth: TAdBitDepth;
      FFilter: TAd2dTextureFilter;

      function GetBitDepth: TAdBitDepth;
      function GetInitialized:boolean;
      procedure SetBitDepth(AValue: TAdBitDepth);
      procedure SetAd2DTexture(AValue: TAd2dTexture);
      procedure SetFilter(AValue: TAd2dTextureFilter);
    protected
      FOwnTexture: Boolean;
      FAd2dTexture:TAd2dTexture;
    public
      {Creates a new instance of TAdCustomTexture. You shouldn't create an
       instance of this abstract class directly. Use TAdTexture or
       TAdRenderTargetTexture instead.
       @param(AParent specifies the parent TAdDraw)}
      constructor Create(AParent:TAdDraw);virtual;
      {Destroys the instance of this class and destroys all texture data.}
      destructor Destroy;override;

      {Initializes a new texture object.}
      procedure Initialize;virtual;abstract;
      {Destroys the created texture object.}
      procedure Finalize;virtual;abstract;

      {Removes the texture from the memory.}
      procedure Clear;virtual;abstract;

      {Provides access on the Andorra 2D graphic plugin internal texture class.}
      property Texture: TAd2dTexture read FAd2DTexture write SetAd2DTexture;
      {Returns whether the texture is currently initialized. If true, a texture
       object is created.}
      property Initialized: boolean read GetInitialized;
      {Use this property to read and set the bit depth of the texture. If you set
       this property, this bit depth will be used when a texture is loaded the
       next time. When reading this property and a texture is loaded, the
       bitdepth of the currently loaded texture will be returned.}
      property BitDepth: TAdBitDepth read GetBitDepth write SetBitDepth;
      {The parent TAdDraw you've set in the constructor.}
      property Parent: TAdDraw read FParent;
      {The filter that is applied to the texture and will be used when drawing
       it.}
      property Filter: TAd2dTextureFilter read FFilter write SetFilter;
  end;

  {TAdRenderTargetTexture extends TAdCustomTexture. It provides access to a
   render target texture. If you want to render to a texture use
   TAdTextureSurface instead. This class wrapps around TAdRenderTargetTexture
   and supports drawing objects to this surface.}
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
      procedure Notify(ASender:TObject;AEvent:TAdSurfaceEventState);
    public
      {Creates a new instance of TAdRenderTargetTexture.}
      constructor Create(AParent:TAdDraw);override;
      {Destroys the instance of TAdRenderTargetTexture.}
      destructor Destroy;override;

      procedure Clear;override;
      procedure Initialize;override;
      procedure Finalize;override;

      property Texture:TAd2dRenderTargetTexture read GetTexture write SetTexture;
      {Provides access on the size of the texture. Set this property to change
       the size of the texture.}
      property Width: integer read FWidth write SetWidth;
      {Provides access on the size of the texture. Set this property to change
       the size of the texture.}
      property Height: integer read FHeight write SetHeight;    
  end;

  {TAdTexture extends TAdCustomTexture and contains a bitmap texture. Those
   textures can be loaded and saved to and from files or streams. You're also
   able to assign other graphic objects to this class. To support other file
   types or compressors, you may write your own "format" or "compressor" class.
   @seealso(TAdBitmap)
   @seealso(TAdGraphicCompressor)
   @seealso(TAdGraphicFormat)}
  TAdTexture = class(TAdCustomTexture)
    private
      FCache: TMemoryStream;
      FCompressorClass:TAdGraphicCompressorClass;
      function GetTexture:TAd2dBitmapTexture;
      procedure SetTexture(AValue:TAd2dBitmapTexture);
    protected
      procedure Notify(ASender:TObject;AEvent:TAdSurfaceEventState);
    public
      {Creates a new instance of TAdTexture.}
      constructor Create(AParent:TAdDraw);override;
      {Destroys the instance of TAdTexture.}
      destructor Destroy;override;

      procedure Clear;override;
      procedure Initialize;override;
      procedure Finalize;override;

      {Loads the texture from a stream from the Andorra 2D internal file format.
       The data saved can be compressed using the so called "compressors".
       This alows game programers to save space and to protect their game data
       by using their own file format.
       @seealso(TAdGraphicCompressor)
      }
      procedure LoadFromStream(AStream:TStream);
      {Saves the texture to a stream in the Andorra 2D internal file format.
       The data saved can be compressed using the so called "compressors".
       This alows game programers to save space and to protect their game data
       by using their own file format.
       @seealso(TAdGraphicCompressor)
       @seealso(Compressor)}
      procedure SaveToStream(AStream:TStream);

      {Loads the texture from a file from the Andorra 2D internal file format.
       The data loaded can be compressed using the so called "compressors".
       This alows game programers to save space and to protect their game data
       by using their own file format.
       @seealso(TAdGraphicCompressor)}
      procedure LoadFromFile(AFile:string);
      {Saves the texture to a file in the Andorra 2D internal file format.
       The data saved can be compressed using the so called "compressors".
       This alows game programers to save space and to protect their game data
       by using their own file format.
       @seealso(TAdGraphicCompressor)
       @seealso(Compressor)}
      procedure SaveToFile(AFile:string);

      {Loads the graphic of the texture from another graphic object. Which
       graphic objects are supported depends on the formats that are bound in.
       Normally you can load your graphic from all VCL/LCL graphic objects.
       @seealso(TAdGraphicFormat)}
      procedure LoadFromGraphic(AGraphic:TObject);

      {Saves the graphic of the texture to another graphic object. Which graphic
       objects are supported depends on the format classes that are bound in.
       Normally you can save your graphic to any VCL/LCL graphic classes.}
      procedure SaveToGraphic(AGraphic:TObject);

      {Loads the graphic from a file.
       @param(AFile is the path to the file that should be loaded.)
       @param(Transparent defines whether the alpha channel should be loaded.)
       @param(TransparentColor defines which color of the graphic should be
         transparent. This only works with formats that don't have their own
         alpha channel. Transparent has to be true for this to work. The default
         value is clNone ($1FFFFFFF).)
       @seealso(TAdGraphicFormat)}
      procedure LoadGraphicFromFile(AFile: string; Transparent: boolean = true;
        TransparentColor: Longint = $1FFFFFFF);

      {Specifies the compressor class that should be used when saving the image data.
       When loading an image, the compressor property is automatically set to the
       compressor class used in this file/stream.
       @seealso(SaveToStream)
       @seealso(SaveToFile)}
      property Compressor:TAdGraphicCompressorClass read FCompressorClass
        write FCompressorClass;

      property Texture: TAd2dBitmapTexture read GetTexture write SetTexture;
  end;

  {A list that contains a set of rectangles. TAdRectList is used by TAdCustomImage
   in order to save the precalculated pattern images.}
  TAdRectList = class(TAdList)
    private
     	function GetItem(AIndex:integer):TAdRect;
     	procedure SetItem(AIndex:integer;AItem:TAdRect);
    protected
      procedure Notify(Ptr: Pointer; Action: TListNotification);override;
    public
      {Read/Write access to the rectangles.}
     	property Items[AIndex:integer]:TAdRect read GetItem write SetItem;default;
      {Add a rectangle to the list.}
      procedure Add(ARect:TAdRect);
  end;

  {TAdCustomImage is the base image class. Images are objects, that manage
   drawing operations of texturized planes. TAdCustomImage doesn't generate its
   own texture object. If you want to use a bitmap texture, use TAdImage instead.
   TAdCustomImage is able to draw the image stretched, rotated, blended. You are
   also able to draw an excerpt of the image.
   @seealso(TAdImage)
   @seealso(TAdTexture)
   @seealso(TAdCustomTexture)}
  TAdCustomImage = class(TAdRenderingObject)
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
      FFilter: TAd2dTextureFilter;

      procedure SetFilter(AValue: TAd2dTextureFilter);
      function GetFilter: TAd2dTextureFilter;
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
      Rects:TAdRectList;
      procedure DrawMesh(DestApp:TAdSurface;DestRect,SourceRect:TAdRect;Rotation:integer;
        RotCenterX,RotCenterY:single;BlendMode:TAd2DBlendMode);
      procedure BuildVertices;
      procedure SetSrcRegion;
      procedure CreatePatternRects;
      procedure Notify(ASender:TObject;AEvent:TAdSurfaceEventState);

      procedure CallBeginRenderEvent(AModelMat: TAdMatrix; AScene: TAdScene);
      procedure CallEndRenderEvent;
    public
      {The underlying mesh object used to display the image.}
      AdMesh:TAd2DMesh;

      {Creates a new instance of TAdCustomImage.
       @param(AAdDraw specifies the parent TAdDraw component. This is not the
         surface drawing operations should be performed on. If you want to draw
         an image on another surface use the "Dest" parameter instead.)}
      constructor Create(AAdDraw:TAdDraw);virtual;
      {Destroys the instance of TAdCustomImage.}
      destructor Destroy;override;

      {Draws the image at a specified position. All positions are relative to
       the coordinate system of the scene of the surface.
       @param(Dest specifies the target surface. If "Dest" is @nil, the active
         surface will be used.)
       @param(X specifies the X-Position of the image.)
       @param(Y specifies the Y-Position of the image.)
       @param(PatternIndex specifies the index of the image pattern.)
       @seealso(PatternWidth)
       @seealso(PatternHeight)
       @seealso(SkipWidth)
       @seealso(SkipHeight)}
      procedure Draw(Dest:TAdSurface;X,Y,PatternIndex:integer);

      {Draws the image to the specified rectangular region. All positions are
        relative to the coordinate system of the scene of the surface.
       @param(Dest specifies the target surface. If "Dest" is @nil, the active
         surface will be used.)
       @param(DestRect specifies the destination region the image should be drawn
         to. You can flip the image if you replace the top/bottom and the
         left/right coordinates. Remember to turn culling of if you want to do
         this.)
       @param(PatternIndex specifies the index of the image pattern.)
       @seealso(PatternWidth)
       @seealso(PatternHeight)
       @seealso(SkipWidth)
       @seealso(SkipHeight)}
      procedure StretchDraw(Dest:TAdSurface; const DestRect:TAdRect;
        PatternIndex:integer);

      {Draws the image to the specified rectangular region using additive
        blending. All positions are relative to the coordinate system of the
        scene of the surface.
       @param(Dest specifies the target surface. If "Dest" is @nil, the active
         surface will be used.)
       @param(DestRect specifies the destination region the image should be drawn
         to. You can flip the image if you replace the top/bottom and the
         left/right coordinates. Remember to turn culling of if you want to do
         this.)
       @param(PatternIndex specifies the index of the image pattern.)
       @param(Alpha specifies the transparency of the image.)
       @seealso(PatternWidth)
       @seealso(PatternHeight)
       @seealso(SkipWidth)
       @seealso(SkipHeight)}
      procedure DrawAdd(Dest: TAdSurface; const DestRect: TAdRect;
        PatternIndex: Integer; Alpha: Integer);

      {Draws the image to the specified rectangular region using alpha blending.
       All positions are relative to the coordinate system of the scene of the
       surface.
       @param(Dest specifies the target surface. If "Dest" is @nil, the active
         surface will be used.)
       @param(DestRect specifies the destination region the image should be drawn
         to. You can flip the image if you replace the top/bottom and the
         left/right coordinates. Remember to turn culling of if you want to do
         this.)
       @param(PatternIndex specifies the index of the image pattern.)
       @param(Alpha specifies the transparency of the image.)
       @seealso(PatternWidth)
       @seealso(PatternHeight)
       @seealso(SkipWidth)
       @seealso(SkipHeight)}
      procedure DrawAlpha(Dest: TAdSurface; const DestRect: TAdRect;
        PatternIndex: Integer; Alpha: Integer);

      {Draws the image to the specified rectangular region. This special blend
       mode draws the alpha channel only. So this blend mode can be used for
       drawing shadows.
       All positions are relative to the coordinate system of the scene of the
       surface.
       @param(Dest specifies the target surface. If "Dest" is @nil, the active
         surface will be used.)
       @param(DestRect specifies the destination region the image should be drawn
         to. You can flip the image if you replace the top/bottom and the
         left/right coordinates. Remember to turn culling of if you want to do
         this.)
       @param(PatternIndex specifies the index of the image pattern.)
       @param(Alpha specifies the transparency of the image.)
       @seealso(PatternWidth)
       @seealso(PatternHeight)
       @seealso(SkipWidth)
       @seealso(SkipHeight)}
      procedure DrawMask(Dest: TAdSurface; const DestRect: TAdRect; PatternIndex: Integer;
        Alpha: Integer);

      {Draws the image to the specified rectangular region. The image can be
       rotated around a user defined rotation center.
       All positions are relative to the coordinate system of the scene of the
       surface.
       @param(Dest specifies the target surface. If "Dest" is @nil, the active
         surface will be used.)
       @param(X specifies the X-Position of the image.)
       @param(Y specifies the Y-Position of the image.)
       @param(Width specifies the width of the image. If this value is negative
         the image will be flipped. If you want to do this, remember to turn
         culling off.)
       @param(Height specifies the height of the image. If this value is negative
         the image will be flipped. If you want to do this, remember to turn
         culling off.)
       @param(PatternIndex specifies the index of the image pattern.)
       @param(CenterX specifies the relative center of the rotation. A value of
         0.5 is the center of the image.)
       @param(CenterY specifies the relative center of the rotation. A value of
         0.5 is the center of the image.)
       @param(Angle is the rotation angle in degrees.)
       @param(Alpha specifies the transparency of the image.)
       @seealso(PatternWidth)
       @seealso(PatternHeight)
       @seealso(SkipWidth)
       @seealso(SkipHeight)}
      procedure DrawRotate(Dest: TAdSurface; X, Y, Width, Height: Integer;
        PatternIndex: Integer; CenterX, CenterY: Double; Angle: Integer);

      {Draws the image to the specified rectangular region. The image can be
       rotated around a user defined rotation center. The image is drawn using
       additive blending.
       All positions are relative to the coordinate system of the scene of the
       surface.
       @param(Dest specifies the target surface. If "Dest" is @nil, the active
         surface will be used.)
       @param(X specifies the X-Position of the image.)
       @param(Y specifies the Y-Position of the image.)
       @param(Width specifies the width of the image. If this value is negative
         the image will be flipped. If you want to do this, remember to turn
         culling off.)
       @param(Height specifies the height of the image. If this value is negative
         the image will be flipped. If you want to do this, remember to turn
         culling off.)
       @param(PatternIndex specifies the index of the image pattern.)
       @param(CenterX specifies the relative center of the rotation. A value of
         0.5 is the center of the image.)
       @param(CenterY specifies the relative center of the rotation. A value of
         0.5 is the center of the image.)
       @param(Angle is the rotation angle in degrees.)
       @param(Alpha specifies the transparency of the image.)
       @seealso(PatternWidth)
       @seealso(PatternHeight)
       @seealso(SkipWidth)
       @seealso(SkipHeight)}
      procedure DrawRotateAdd(Dest: TAdSurface; X, Y, Width, Height: Integer;
        PatternIndex: Integer; CenterX, CenterY: Double; Angle: Integer;
        Alpha: Integer);

      {Draws the image to the specified rectangular region. The image can be
       rotated around a user defined rotation center. The image is drawn using
       aplha blending.
       All positions are relative to the coordinate system of the scene of the
       surface.
       @param(Dest specifies the target surface. If "Dest" is @nil, the active
         surface will be used.)
       @param(X specifies the X-Position of the image.)
       @param(Y specifies the Y-Position of the image.)
       @param(Width specifies the width of the image. If this value is negative
         the image will be flipped. If you want to do this, remember to turn
         culling off.)
       @param(Height specifies the height of the image. If this value is negative
         the image will be flipped. If you want to do this, remember to turn
         culling off.)
       @param(PatternIndex specifies the index of the image pattern.)
       @param(CenterX specifies the relative center of the rotation. A value of
         0.5 is the center of the image.)
       @param(CenterY specifies the relative center of the rotation. A value of
         0.5 is the center of the image.)
       @param(Angle is the rotation angle in degrees.)
       @param(Alpha specifies the transparency of the image.)
       @seealso(PatternWidth)
       @seealso(PatternHeight)
       @seealso(SkipWidth)
       @seealso(SkipHeight)}
      procedure DrawRotateAlpha(Dest: TAdSurface; X, Y, Width, Height: integer;
        PatternIndex: integer; CenterX, CenterY: double; Angle: integer;
        Alpha: integer);

      {Draws the image to the specified rectangular region. The image can be
       rotated around a user defined rotation center. The image is drawn using
       the so called "mask" mode. This special blend mode draws the alpha
       channel only. So this blend mode can be used for drawing shadows.
       All positions are relative to the coordinate system of the scene of the
       surface.
       @param(Dest specifies the target surface. If "Dest" is @nil, the active
         surface will be used.)
       @param(X specifies the X-Position of the image.)
       @param(Y specifies the Y-Position of the image.)
       @param(Width specifies the width of the image. If this value is negative
         the image will be flipped. If you want to do this, remember to turn
         culling off.)
       @param(Height specifies the height of the image. If this value is negative
         the image will be flipped. If you want to do this, remember to turn
         culling off.)
       @param(PatternIndex specifies the index of the image pattern.)
       @param(CenterX specifies the relative center of the rotation. A value of
         0.5 is the center of the image.)
       @param(CenterY specifies the relative center of the rotation. A value of
         0.5 is the center of the image.)
       @param(Angle is the rotation angle in degrees.)
       @param(Alpha specifies the transparency of the image.)
       @seealso(PatternWidth)
       @seealso(PatternHeight)
       @seealso(SkipWidth)
       @seealso(SkipHeight)}
      procedure DrawRotateMask(Dest: TAdSurface; X, Y, Width, Height: integer;
        PatternIndex: integer; CenterX, CenterY: double; Angle: integer;
        Alpha: integer);

      {The DrawEx function can be used to draw a specific part of an image with
       a user definded rotation and in every available blendmode. All other
       drawing functions can be done using this function. If you want to do
       animations you can use the "GetPatternRect" function of TAdCustomImage
       and pass it to SourceRect.
       @param(Dest specifies the target surface. If "Dest" is @nil, the active
         surface will be used.)
       @param(SourceRect specifies the rectangle, from which image data is
         copied. The coordinates are absolute. If they are bigger or smaller
         than the texture sizes, the image will be tiled. Due to hardware
         limitations all textures are internally scaled to a power of two
         texture. If you want to draw your texture tiled and it isn't in a
         power of two size, you'll see strange gaps between the tiles.)
       @param(DestRect specifies the destination region the image should be drawn
         to. You can flip the image if you replace the top/bottom and the
         left/right coordinates. Remember to turn culling of if you want to do
         this.)
       @param(CenterX specifies the relative center of the rotation. A value of
         0.5 is the center of the image.)
       @param(CenterY specifies the relative center of the rotation. A value of
         0.5 is the center of the image.)
       @param(Angle is the rotation angle in degrees.)
       @param(Alpha specifies the transparency of the image.)
       @param(BlendMode spcifies how the image should be blended over the
         existing surface content.)
       @seealso(TAd2dBlendMode)
       @seealso(GetPatternRect)
      }
      procedure DrawEx(Dest: TAdSurface; SourceRect, DestRect: TAdRect;
        CenterX, CenterY: double; Angle: integer; Alpha: integer;
        BlendMode: TAd2dBlendMode);

      {If you make any changes to the texture or set a new one, call restore to
       assing the new texture to the mesh and recalculates the pattern rects and
       the vertex grid.}
      procedure Restore;

      {Frees the mesh object. Finalize is normally automatically called when
       the main TAdDraw is finalized.
       @seealso(Restore)
       @seealso(Initialize)}
      procedure Finalize;

      {Initialize creates a new mesh object.}
      procedure Initialize;

      {Returns the texture rectangle of a pattern. ANr is clamped to [0;PatternCount]}
      function GetPatternRect(ANr: integer):TAdRect;
      
      {Returns the parent TAdDraw you've set in the constructor.}
      property Parent: TAdDraw read FParent write FParent;

      {Returns the width of the image. If pattern width and pattern height is
       set, this size is returned.}
      property Width: integer read GetWidth;
      {Returns the height of the image. If pattern width and pattern height is
       set, this size is returned.}
      property Height: integer read GetHeight;

      {Set the width of one animation pattern.}
      property PatternWidth: integer read FPatternWidth write SetPatternWidth;
      {Set the height of one animation pattern.}
      property PatternHeight: integer read FPatternHeight write SetPatternHeight;

      {The horizontal space between the animation patterns.}
      property SkipWidth: integer read FSkipWidth write SetSkipWidth;
      {The vertical space between the animation patterns.}
      property SkipHeight: integer read FSkipHeight write SetSkipHeight;

      {The texture assigned to the image. Call restore after you've set a
       new texture.
       @seealso(Restore)}
      property Texture: TAdCustomTexture read FTexture write SetTexture;

      {Returns the count of animation patterns.
       @seealso(PatternWidth)
       @seealso(PatternHeight)
       @seealso(SkipWidth)
       @seealso(SkipHeight)}
      property PatternCount: integer read GetPatternCount;
      {If you have empty patterns, you may set PatternStop. PatternCount will be
       decrased by PatternStop.
       @seealso(PatternStop)}
      property PatternStop: integer read FPatternStop write FPatternStop;

      {Defines the color the image is drawn in. The color is in BGR order and
       compatible to the color constants in the VCL/LCL.}
      property Color: longint read FColor write FColor;

      {Important for using lights: How many grid cols and rows does the image
       have. Grid count is Details*Details.}
      property Details: integer read FDetails write SetDetails;

      {The filter that should be used when a texture is mini- or magnified.
       Setting this property will cause in overriding the filter of the texture.
       @seealso(TAd2dTextureFilter)}
      property Filter: TAd2dTextureFilter read GetFilter write SetFilter;
  end;

  TAdImageList = class;

  {A image class that contains a bitmap texture. The TAdImageList contains a set
   of TAdImages.}
  TAdImage = class(TAdCustomImage)
    private
      FName:AnsiString;
      function GetTexture:TAdTexture;
      procedure SetTexture(AValue:TAdTexture);
    public
      {Contains a pointer to the image list which created the image.
       Set to nil if you don't want the image to be freed by the ImageList.}
      FreeByList:TAdImageList;

      constructor Create(AAdDraw:TAdDraw);override;

      {Assings the settings of another item.}
      procedure Assign(AItem:TAdImage);

      {Saves the image and all settings to a stream.}
      procedure SaveToStream(AStream:TStream);
      {Loads the image and all settings from a stream.}
      procedure LoadFromStream(AStream:TStream);
      {Saves the image data to a file. This function save the data in a Andorra
       2D internal file format.}
      procedure SaveToFile(AFile:string);
      {Loads the image data from a file. This function save the data in a Andorra
       2D internal file format.}
      procedure LoadFromFile(AFile:string);

      {Name of the image in the imagelist. The find procedure of the imagelist
       uses this property.}
      property Name: string read FName write FName;

      {Access to the texture abstraction layer.}
      property Texture: TAdTexture read GetTexture write SetTexture;
  end;

  {A list of TAdImage objects. The whole image list can be saved and loaded from
   a single file. To create such an imagelist file you can use the tool
   "imgedit".}
  TAdImageList = class(TAdList)
    private
      FParent:TAdDraw;
      FCompressor:TAdGraphicCompressorClass;
      FFilter: TAd2dTextureFilter;
     	function GetItem(AIndex:integer):TAdImage;
     	procedure SetItem(AIndex:integer;AItem:TAdImage);
      procedure SetCompressor(ACompressor:TAdGraphicCompressorClass);
      procedure SetFilter(AValue: TAd2dTextureFilter);
    protected
      procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    public
      {Creates a new instance of TAdImageList.}
      constructor Create(AAdDraw:TAdDraw);
      {Frees the instance of TAdImageList. Images that were created by the
       imagelist are freed too.
       @seealso(TAdImage.FreeByList)}
      destructor Destroy;override;

      {Returns an image item to you.}
     	property Items[AIndex:integer]:TAdImage read GetItem write SetItem;default;

      {Creates and adds a new image item to the list. This item will automatically
       freed by the list.
       @seealso(TAdImage.FreeByList)}
      function Add(AName:string):TAdImage;overload;
      {Searches for an item by name and returns its index.}
      function IndexOf(AName:string):integer;
      {Finds an image in the list by its name.}
      function Find(AName:string):TAdImage;

      {Returns a new imagelist, that contains all images containing the given
       substring. The returned imagelist has to be freed manually.}
      function FindEx(ASubStr:string):TAdImageList;
      {Calls the restore function of every item in the list.}
      procedure Restore;

      {Save the whole list to a stream.}
      procedure SaveToStream(AStream:TStream);
      {Load a whole list from a stream.}
      procedure LoadFromStream(AStream:TStream);

      {Saves the whole list to a file.}
      procedure SaveToFile(AFile:string);
      {Loads a whole list from a file.}
      procedure LoadFromFile(AFile:string);

      {The parent TAdDraw you've specified in the constructor.}
      property Parent:TAdDraw read FParent;

      {Apply the same graphic compressor class to each item in the list.}
      property Compressor:TAdGraphicCompressorClass read FCompressor write SetCompressor;
      {Texture filter that should be applied to all images in the list. By setting
       this property, the filter property of all image items is overwritten.}
      property Filter: TAd2dTextureFilter read FFilter write SetFilter;
    end;

implementation

{ TAdDraw }

constructor TAdDraw.Create(AParent : Pointer);
begin
	inherited Create(nil);

  //Store the parent control
  FParent := AParent;

  //Create classes
  FDllLoader := TAdDllLoader.Create;
  FSurfaceEventList := TAdSurfaceEventList.Create;
  FProperties := TAdPluginPropertyList.Create;
  FLog := TAdLog.Create;
  FLog.AutoSave := true;

  //Set display presets
  SetupDisplay;

  //Set the TAdSurface "AdDraw" property
  AdDraw := self;
end;

destructor TAdDraw.Destroy;
begin
  //Destroy the window object
  if FWnd <> nil then
    FreeAndNil(FWnd);

  //Free all loaded objects
  if AdAppl <> nil then
    Finalize;

  FProperties.Free;
  FSurfaceEventList.Free;
  FDllLoader.Free;
  FLog.Free;

	inherited Destroy;
end;

procedure TAdDraw.SetupDisplay;
begin
  //Set default display settings
  with FDisplay do
  begin
    Width := 800;
    Height := 600;
    BitDepth := ad32Bit;
    Freq := 0;
    DisplayMode := dmDefault;
  end;

  //Set default options
  Options := [aoTextures, aoBlending, aoCulling];
end;

procedure TAdDraw.DoActivation;
begin
  if Initialized then
  begin
    AdAppl.SetRenderTarget(nil);
    inherited DoActivation;
  end;
end;

procedure TAdDraw.UnRegisterNotifyEvent(AProc: TAdSurfaceEvent);
begin
  FSurfaceEventList.Remove(AProc)
end;

procedure TAdDraw.RegisterNotifyEvent(AProc: TAdSurfaceEvent);
begin
  FSurfaceEventList.Add(AProc);
end;    

procedure TAdDraw.Run;
begin
  if FWnd <> nil then
    FWnd.Run;
  //TODO: Raise exception here
end;

procedure TAdDraw.SetActiveSurface(ASurface: TAdSurface);
begin
  //Deactivate the currently active surface
  if FActiveSurface <> nil then
    FActiveSurface.Deactivate;

  //Set the new active surface
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
    FDllLoader.LoadLibrary(val);

    if FDllLoader.LibraryLoaded then
    begin
      //Read the plugin properties
      FProperties.ReadProperties(FDllLoader);
    end;
  end;
end;

procedure TAdDraw.SetLastError(AError: string);
begin
  //Set the last error variable and log it.
  FLastError := AError;
  LogProc('TAdDraw', lsError, PChar(AError));
end;

function TAdDraw.SearchWindowFramework: boolean;
var
  PSS:^ShortString;
  cref:TAdWindowFrameworkClass;
begin
  //Assume that no framework is found
  result := false;

  //If a window frame work is still loaded, check whether this is compatible
  if FWnd <> nil then
  begin
    if AdAppl.SupportsWindowFramework(FWnd.IdentStr) then
    begin
      //The window framework is still compatible - continue
      result := true;
      exit;
    end
    else
      FreeAndNil(FWnd);
  end;

  //Iterate through the registered frameworks
  RegisteredWindowFrameworks.StartIteration;
  while not RegisteredWindowFrameworks.ReachedEnd do
  begin
    //Create a temporaty window framework and store it in "FWnd".
    PSS := RegisteredWindowFrameworks.GetCurrent;
    cref := TAdWindowFrameworkClass(AdGetClass(PSS^));
    FWnd := cref.Create;

    //Check whether the window framework matches to the chosen target window
    //and is supported by the rendering plugin
    result := FWnd.BindTo(FParent) and AdAppl.SupportsWindowFramework(FWnd.IdentStr);
    if not result then
    begin
      FWnd.Free; FWnd := nil;
    end else
    begin
      InitDisplay;
      exit;
    end;
  end;
end;

procedure TAdDraw.InitDisplay;
var
  wndprops:TAdDisplayProperties;
  plgprops:TAd2dResolution;  
begin
  //Setup the properties for the windowframeworks  
  wndprops.Width := FDisplay.Width;
  wndprops.Height := FDisplay.Height;
  wndprops.BitDepth := FDisplay.BitDepth;

  //If the graphic plugin supports the fullscreen mode, give the responsibility
  //instead to the window framework
  if Properties.PropertyExists('fullscreen') and
     (FDisplay.DisplayMode = dmFullscreen) then
  begin
    plgprops.Width := FDisplay.Width;
    plgprops.Height := FDisplay.Height;
    plgprops.BitDepth := FDisplay.BitDepth;
    wndprops.Mode := dmScreenRes;
    Properties.SetProp('fullscreen', true);
    Properties.SetProp('fullscreen_res', plgprops);
  end
  else
  begin
    wndprops.Mode := FDisplay.DisplayMode;
  end;

  //Initialize the window framework
  FWnd.InitDisplay(wndprops);
end;

function TAdDraw.Initialize: boolean;
begin
  //Fianlize the current TAdDraw instance
  Finalize;
  
  //Assume the the oppertation will fail
  result := false;
  FInitialized := false;

  if FDllLoader.LibraryLoaded then
  begin
    //Create the new Application
    FAdAppl := FDllLoader.CreateApplication;

    FAdAppl.LogCallback := LogProc;

    if AdAppl <> nil then
    begin
      //Search and initialize a window framework
      if SearchWindowFramework then
      begin
        //Store settings in the plugin
        FProperties.WriteProperties(AdAppl);

        //Initialize the Andorra 2D application with the created window framework
        if AdAppl.Initialize(FWnd) then
        begin
          //Call the on initialize event
          if Assigned(FInitialize) then
            FInitialize(Self);

          //Create the font system
          FFonts := TAdFontFactory.Create(AdAppl);

          //Initialization did not fail - congratulations
          result := true;
          FInitialized := true;

          //Call all registered initialization routines
          CallNotifyEvent(seInitialize);
          CallNotifyEvent(seInitialized);

          //Store options
          AdAppl.SetOptions(FOptions);

          //Setup view- and projection matrix
          Setup2DScene;

        end else
        begin
          //Initialization failed
          SetLastError(MsgInitializationFailed);
          FreeAndNil(FAdAppl);
        end;          
      end else
      begin
        //No window framework found
        SetLastError(MsgNoWindowFramework);
        FreeAndNil(FAdAppl);
      end;
    end else
    begin
      //AdAppl is nil
      SetLastError(MsgAdDrawInterfaceNotSupported);
    end;
  end else
  begin
    //Library not loaded
    SetLastError(MsgLibraryNotLoaded);
  end;
end;

procedure TAdDraw.LogProc(AModule: PChar; ASeverity: TAd2dLogSeverity;
  AMessage: PChar);
var
  msg: TAdLogMessage;
begin
  //Set message data
  case ASeverity of
    lsInfo: msg.Typ := MsgLogInfo;
    lsWarning: msg.Typ := MsgLogWarning;
    lsError: msg.Typ := MsgLogError;
    lsFatalError: msg.Typ := MsgLogFatalError;
  end;

  msg.Sender := AModule;
  msg.Text := AMessage;

  //Add message to log
  FLog.AddMessage(msg);
end;

procedure TAdDraw.Finalize;
begin
  //Call the "OnFinalize" Event

  if Assigned(FFinalize) then
    FFinalize(Self);

  //Destroy the fonts object
  if FFonts <> nil then
    FreeAndNil(FFonts);

  //Tell everybody that this instance of TAdDraw will be no longer initialized
  FInitialized := false;
  CallNotifyEvent(seFinalize);

  //Destroy the AdAppl object
  if FAdAppl <> nil then
  begin
    FAdAppl.Finalize;
    FreeAndNil(FAdAppl);
  end;
end;

procedure TAdDraw.Restore;
begin
  if Initialized then
  begin
    Finalize;
    Initialize;
  end;
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
    AdAppl.Flip;
end;

function TAdDraw.GetHeight: integer;
begin
  result := FWnd.ClientHeight;
end;

function TAdDraw.GetLastError: string;
begin
  result := FLastError;
end;

function TAdDraw.GetSurfaceRect: TAdRect;
begin
  result := AdRect(0, 0, FWnd.ClientWidth, FWnd.ClientHeight);
end;

function TAdDraw.GetWidth: integer;
begin
  result := FWnd.ClientWidth;
end;

procedure TAdDraw.CallNotifyEvent(AEventState: TAdSurfaceEventState);
var
  i:integer;
begin
  //Iterate through the list of registred surface event handlers
  for i := FSurfaceEventList.Count - 1 downto 0 do
    FSurfaceEventList.Items[i](self, AEventState);
end;

function TAdDraw.CanDraw:boolean;
begin
  result := (AdAppl <> nil) and (Initialized);
end;

{ TRectList }

procedure TAdRectList.Add(ARect: TAdRect);
var ar:PAdRect;
begin
  new(ar);
  ar^ := ARect;
  inherited Add(ar);
end;

function TAdRectList.GetItem(AIndex:integer):TAdRect;
begin
  result := PAdRect(inherited Items[AIndex])^;
end;

procedure TAdRectList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  //Free reserved memory
  if Action = lnDeleted then
    Dispose(PAdRect(Ptr));

  inherited;
end;

procedure TAdRectList.SetItem(AIndex:integer;AItem:TAdRect);
begin
  PAdRect(inherited Items[AIndex])^ := AItem;
end;   

{ TAdCustomImage }

constructor TAdCustomImage.Create(AAdDraw:TAdDraw);
begin
  inherited Create;

  FTexture := nil;

  FParent := AAdDraw;
  FParent.RegisterNotifyEvent(Notify);
  Rects := TAdRectList.Create;
  FSrcRect := AdRect(0, 0, 0, 0);
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
    SetSrcRegion;
  end;

  //Initialize the matrix
  mat1 := AdMatrix_Identity;
  mat2 := AdMatrix_Identity;

  //Set the scale matrix
  mat1 := AdMatrix_Scale((DestRect.Right-DestRect.Left)/FWidth,(DestRect.Bottom-DestRect.Top)/FHeight,1);
  mat2 := AdMatrix_Multiply(mat1,mat2);

  if (not FloatsEqual(Rotation, 0, 0.001)) then
  begin
    CurX := (DestRect.Right-DestRect.Left) * RotCenterX;
    CurY := (DestRect.Bottom-DestRect.Top) * RotCenterY;

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

  AdMesh.Matrix := mat2; 

  if AdMesh.Indices <> nil then
    Mode := adTriangles
  else
    Mode := adTriangleStrips;

  if (DestApp <> nil) and (DestApp is TAdRenderingSurface) then
    CallBeginRenderEvent(AdMesh.Matrix, TAdRenderingSurface(DestApp).Scene);

  AdMesh.Draw(BlendMode, Mode);

  CallEndRenderEvent;
end;

procedure TAdCustomImage.CallBeginRenderEvent(AModelMat: TAdMatrix; AScene: TAdScene);
var
  mat: TAdMatrix;
begin
  if Assigned(OnBeginRender) then
  begin
    //!TODO: Only calculate VM*PM once
    mat := AdMatrix_Multiply(AModelMat, AScene.ViewMatrix);
    mat := AdMatrix_Multiply(mat, AScene.ProjectionMatrix);
    OnBeginRender(self, mat);
  end;
end;

procedure TAdCustomImage.CallEndRenderEvent;
begin
  if Assigned(OnEndRender) then
    OnEndRender(self);
end;

procedure TAdCustomImage.SetSrcRegion;
var
  texw, texh: integer;
  rectw, recth: integer;
  mat: TAdMatrix;

const
  add = 0;
  
begin
  texw := Texture.Texture.Width;
  texh := Texture.Texture.Height;

  rectw := FSrcRect.Right - FSrcRect.Left;
  recth := FSrcRect.Bottom - FSrcRect.Top;

  mat := AdMatrix_Identity;

  //Scale the texture
  mat[0,0] := (rectw) / texw;
  mat[1,1] := (recth) / texh;

  //Translate the texture
  mat[2,0] := (FSrcRect.Left + add) / texw;
  mat[2,1] := (FSrcRect.Top + add) / texh;

  AdMesh.TextureMatrix := mat;
end;

procedure TAdCustomImage.BuildVertices;
var
  Vertices: TAdVertexArray;
  Indices: TAdIndexArray;
  i, x, y: integer;
  ax, ay: double;
  vc, ic: integer;
  c: TAndorraColor;
  reciprocaldetails: double;
begin
  if AdMesh <> nil then
  begin
    vc := sqr(FDetails+1);

    SetLength(Vertices, vc);

    c := GetColor;

    reciprocaldetails := 1 / FDetails;

    //Set vertex coordinates
    i := 0;
    for y := 0 to FDetails do
    begin
      for x := 0 to FDetails do
      begin
        ay := y*fheight/FDetails;
        ax := x*fwidth/FDetails;
        Vertices[i].Position := AdVector3(ax, ay, 0);
        Vertices[i].Color := c;
        Vertices[i].Texture := AdVector2(
          reciprocaldetails * x,
          reciprocaldetails * y);
        Vertices[i].Normal := AdVector3(0, 0, -1);
        i := i + 1;
      end;
    end;
    AdMesh.Vertices := Vertices;

    //Create indices
    if FDetails > 1 then
    begin

      //Create index array
      ic := sqr(FDetails) * 6;
      SetLength(Indices, ic);

      i := 0;
      for y := 0 to FDetails - 1 do
      begin
        for x := 0 to FDetails - 1 do
        begin
          Indices[i] :=   y     * (FDetails + 1) + x + 1;
          Indices[i+1] := (y+1) * (FDetails + 1) + x;
          Indices[i+2] := y     * (FDetails + 1) + x;
          Indices[i+3] := y     * (FDetails + 1) + x + 1;
          Indices[i+4] := (y+1) * (FDetails + 1) + x + 1;
          Indices[i+5] := (y+1) * (FDetails + 1) + x;
          i := i + 6;
        end;
      end;
      AdMesh.Indices := Indices;
    end
    else
      AdMesh.Indices := nil;

    AdMesh.PrimitiveCount := sqr(FDetails) * 2;
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
  if (Texture.Texture.Loaded) and (AdMesh <> nil) then
  begin
    SetCurrentColor(255);
    DrawMesh(Dest, AdRect(X,Y,X+Width,Y+Height), GetPatternRect(PatternIndex),
      0, 0, 0, bmAlpha);
  end;
end;

procedure TAdCustomImage.DrawAdd(Dest: TAdSurface; const DestRect: TAdRect;
  PatternIndex, Alpha: Integer);
begin
  if (Texture.Texture.Loaded) and (AdMesh <> nil) then
  begin
    SetCurrentColor(Alpha);
    DrawMesh(Dest,DestRect,GetPatternRect(PatternIndex),0,0,0,bmAdd);
  end;
end;

procedure TAdCustomImage.DrawAlpha(Dest: TAdSurface; const DestRect: TAdRect;
  PatternIndex, Alpha: Integer);
begin
  if (Texture.Texture.Loaded) and (AdMesh <> nil) then
  begin
    SetCurrentColor(Alpha);
    DrawMesh(Dest,DestRect,GetPatternRect(PatternIndex),0,0,0,bmAlpha);
  end;
end;

procedure TAdCustomImage.DrawMask(Dest: TAdSurface; const DestRect: TAdRect;
  PatternIndex, Alpha: Integer);
begin
  if (Texture.Texture.Loaded) and (AdMesh <> nil) then
  begin
    SetCurrentColor(Alpha);
    DrawMesh(Dest,DestRect,GetPatternRect(PatternIndex),0,0,0,bmMask);
  end;
end;

procedure TAdCustomImage.DrawRotate(Dest: TAdSurface; X, Y, Width, Height,
  PatternIndex: Integer; CenterX, CenterY: Double; Angle: Integer);
begin
  if (Texture.Texture.Loaded) and (AdMesh <> nil) then
  begin
    SetCurrentColor(255);
    DrawMesh(Dest, AdRect(X,Y,X+Width,Y+Height), GetPatternRect(PatternIndex), Angle,
     CenterX, CenterY, bmAlpha);
  end;
end;

procedure TAdCustomImage.DrawRotateAdd(Dest: TAdSurface; X, Y, Width,
  Height, PatternIndex: Integer; CenterX, CenterY: Double; Angle,
  Alpha: Integer);
begin
  if (Texture.Texture.Loaded) and (AdMesh <> nil) then
  begin
    SetCurrentColor(Alpha);
    DrawMesh(Dest, AdRect(X,Y,X+Width,Y+Height), GetPatternRect(PatternIndex), Angle,
      CenterX, CenterY, bmAdd);
  end;
end;

procedure TAdCustomImage.DrawRotateAlpha(Dest: TAdSurface; X, Y, Width,
  Height, PatternIndex: Integer; CenterX, CenterY: Double; Angle,
  Alpha: Integer);
begin
  if (Texture.Texture.Loaded) and (AdMesh <> nil) then
  begin
    SetCurrentColor(Alpha);
    DrawMesh(Dest, AdRect(X,Y,X+Width,Y+Height), GetPatternRect(PatternIndex), Angle,
      CenterX,CenterY,bmAlpha);
  end;
end;

procedure TAdCustomImage.DrawRotateMask(Dest: TAdSurface; X, Y, Width,
  Height, PatternIndex: Integer; CenterX, CenterY: Double; Angle,
  Alpha: Integer);
begin
  if (Texture.Texture.Loaded) and (AdMesh <> nil) then
  begin
    SetCurrentColor(Alpha);
    DrawMesh(Dest, AdRect(X,Y,X+Width,Y+Height), GetPatternRect(PatternIndex),
      Angle, CenterX, CenterY, bmMask);
  end;
end;

procedure TAdCustomImage.DrawEx(Dest: TAdSurface; SourceRect,
  DestRect: TAdRect; CenterX, CenterY:double; Angle, Alpha: Integer;
  BlendMode: TAd2dBlendMode);
begin
  if (Texture.Texture.Loaded) and (AdMesh <> nil) then
  begin
    SetCurrentColor(Alpha);
    DrawMesh(Dest, DestRect, SourceRect, Angle, CenterX, CenterY, BlendMode);
  end;
end;

procedure TAdCustomImage.StretchDraw(Dest: TAdSurface; const DestRect: TAdRect; PatternIndex: integer);
begin
  if (Texture.Texture.Loaded) and (AdMesh <> nil) then
  begin
    SetCurrentColor(255);
    DrawMesh(Dest, DestRect, GetPatternRect(PatternIndex), 0, 0, 0, bmAlpha);
  end;
end;

procedure TAdCustomImage.Restore;
begin
  //Set sizes
  FWidth := Texture.Texture.BaseWidth;
  FHeight := Texture.Texture.BaseHeight;

  //Assign texture to mesh
  AdMesh.Texture := Texture.Texture;

  //Calculate patern rectangles from the given patern parameters
  CreatePatternRects;

  //Reset source rectangle
  FSrcRect := AdRect(0, 0, 0, 0);

  //Save the last color of the image
  FLastColor := GetColor;

  //Build the vertex grid and store it in the mesh object
  BuildVertices;
end;

procedure TAdCustomImage.SetPatternWidth(AValue: Integer);
begin
  if AValue >= 0 then
    FPatternWidth := AValue
  else
    FPatternWidth := 0;
end;

procedure TAdCustomImage.SetPatternHeight(AValue: Integer);
begin
  if AValue >= 0 then
    FPatternHeight := AValue
  else
    FPatternHeight := 0;
end;

procedure TAdCustomImage.SetSkipHeight(AValue: integer);
begin
  if AValue >= 0 then
    FSkipHeight := AValue
  else
    FSkipHeight := 0;
end;

procedure TAdCustomImage.SetSkipWidth(AValue: integer);
begin
  if AValue >= 0 then
    FSkipWidth := AValue
  else
    FSkipWidth := 0;
end;

procedure TAdCustomImage.SetTexture(AValue: TAdCustomTexture);
begin
  //Free own texture
  if FOwnTexture then
    FTexture.Free;

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

procedure TAdCustomImage.SetFilter(AValue: TAd2dTextureFilter);
begin
  FFilter := AValue;
  if FTexture <> nil then
    FTexture.Filter := AValue;
end;

function TAdCustomImage.GetColor: TAndorraColor;
begin
  result := Ad_ARGB(FAlpha,GetRValue(FColor),GetGValue(FColor),GetBValue(FColor));
end;

function TAdCustomImage.GetFilter: TAd2dTextureFilter;
begin
  if FTexture <> nil then
    result := FTexture.Filter
  else
    result := FFilter;
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
  //Finalize the image, if it is still initialized
  Finalize;

  //Create a new mesh
  AdMesh := FParent.AdAppl.CreateMesh;
end;

procedure TAdCustomImage.Finalize;
begin
  if AdMesh <> nil then
    FreeAndNil(AdMesh);
end;

procedure TAdCustomImage.Notify(ASender: TObject;AEvent: TAdSurfaceEventState);
begin
  case AEvent of
    seInitialize: Initialize;
    seInitialized: Restore;
    seFinalize: Finalize;
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
    result := AdRect(0, 0, GetWidth, GetHeight);
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
  s:AnsiString;
  c:AnsiChar;
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
    raise EAdLoadImage.Create(MsgNoValidImage);
  end;
end;

procedure TAdImage.SaveToStream(AStream: TStream);
var
  c: AnsiChar;
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

  result.Filter := FFilter;
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
      temp.Filter := FFilter;
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
    raise EAdLoadImage.Create(MsgNoValidImage);
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
  i: integer;
begin
  FCompressor := ACompressor;
  for i := 0 to Count - 1 do
  begin
    Items[i].Texture.Compressor := FCompressor;
  end;
end;

procedure TAdImageList.SetFilter(AValue: TAd2dTextureFilter);
var
  i: integer;
begin
  FFilter := AValue;
  for i := 0 to Count - 1 do
  begin
    Items[i].Filter := FFilter;
  end;
end;

procedure TAdImageList.SetItem(AIndex:integer;AItem:TAdImage);
begin
 inherited Items[AIndex] := AItem;
end;

{ TSurfaceEventList }

procedure TAdSurfaceEventList.Add(Item: TAdSurfaceEvent);
var
  Event:PAdSurfaceEvent;
begin
  New(Event);
  Event^ := Item;
  inherited Add(Event);
end;

function TAdSurfaceEventList.GetItem(AIndex:integer):TAdSurfaceEvent;
begin
  result := PAdSurfaceEvent(inherited Items[AIndex])^;
end;

procedure TAdSurfaceEventList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then
    Dispose(PAdSurfaceEvent(Ptr));

  inherited;
end;

procedure TAdSurfaceEventList.Remove(Item: TAdSurfaceEvent);
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

procedure TAdSurfaceEventList.SetItem(AIndex:integer;AItem:TAdSurfaceEvent);
begin
  inherited Items[AIndex] := @AItem;
end;  

{ TAdCustomTexture }

constructor TAdCustomTexture.Create(AParent: TAdDraw);
begin
  inherited Create;
  
  FParent := AParent;
  FBitDepth := ad32Bit;
  
  Initialize;
end;

destructor TAdCustomTexture.Destroy;
begin
  Finalize;
  inherited;
end;

function TAdCustomTexture.GetBitDepth: TAdBitDepth;
begin
  if Initialized then
    result := Texture.BitDepth
  else
    result := FBitDepth;
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

  SetBitDepth(FAd2dTexture.BitDepth);
  SetFilter(FFilter);
end;

procedure TAdCustomTexture.SetBitDepth(AValue: TAdBitDepth);
begin
  FBitDepth := AValue;
end;   

procedure TAdCustomTexture.SetFilter(AValue: TAd2dTextureFilter);
begin
  FFilter := AValue;
  if (FAd2dTexture <> nil) then
    FAd2dTexture.Filter := FFilter;
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
  Clear;
  inherited Destroy;
end;

procedure TAdTexture.Initialize;
begin
  Finalize;
  FAd2dTexture := Parent.AdAppl.CreateBitmapTexture;
  FAd2dTexture.Filter := FFilter;  
end;

procedure TAdTexture.Finalize;
begin
  if (FOwnTexture) and (FAd2dTexture <> nil) then
  begin
    FAd2dTexture.Free;
    FAd2dTexture := nil;
  end;
end;

procedure TAdTexture.Clear;
begin
  if FCache <> nil then
    FreeAndNil(FCache);

  if Texture <> nil then
    Texture.FlushTexture;
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

    Texture.LoadFromBitmap(bmp, ad32Bit);
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
    Texture.LoadFromBitmap(bmp, ad32Bit);
  finally
    bmp.Free;
  end;
end;

procedure TAdTexture.LoadFromStream(AStream: TStream);
var
  c: AnsiChar;
  bmp: TAdBitmap;
begin
  AStream.Read(c,1);
  if c = 'T' then
  begin
    bmp := TAdBitmap.Create;
    try
      bmp.LoadFromStream(AStream);
      AStream.Read(FBitDepth,1);

      Texture.LoadFromBitmap(bmp, ad32Bit);
      bmp.Free;
    except
      bmp.Free;
      raise;
    end;
  end;
end;

procedure TAdTexture.SaveToStream(AStream: TStream);
var
  c: AnsiChar;
  bmp: TAdBitmap;
  bits: TAdBitDepth;
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

    bits := Texture.BitDepth;
    AStream.Write(bits,1);

  end
  else
  begin
    c := #0; AStream.Write(c,1);
  end;
end;

procedure TAdTexture.Notify(ASender: TObject; AEvent: TAdSurfaceEventState);
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

{ TAdScene }

constructor TAdScene.Create(ADraw: TAdDraw);
begin
  inherited Create;
  FDraw := ADraw;

  FNearZ := -100;
  FFarZ := 100;
  FAmbientColor := Ad_ARGB(255, 255, 255, 255);
end;

destructor TAdScene.Destroy;
begin
  inherited;
end;

procedure TAdScene.Activate;
begin
  if Assigned(FOnActivate) then
    FOnActivate(self);
    
  FActivated := true;
  UpdateMatrix;
  UpdateViewport;
  UpdateAmbientColor;
end;

procedure TAdScene.Deactivate;
begin
  FActivated := false;
end;

procedure TAdScene.SetProjectionMatrix(AValue: TAdMatrix);
begin
  FProjectionMatrix := AValue;
  UpdateMatrix;
end;

procedure TAdScene.SetViewMatrix(AValue: TAdMatrix);
begin
  FViewMatrix := AValue;
  UpdateMatrix;
end;

procedure TAdScene.SetViewPort(AValue: TAdRect);
begin
  FViewPort := AValue;
  UpdateViewport;
end;

procedure TAdScene.UpdateAmbientColor;
begin
  if FActivated and FDraw.CanDraw then
    FDraw.AdAppl.AmbientColor := FAmbientColor;
end;

procedure TAdScene.UpdateMatrix;
begin
  if FActivated and FDraw.CanDraw then
    FDraw.AdAppl.SetupManualScene(FViewMatrix, FProjectionMatrix);
end;

procedure TAdScene.UpdateViewport;
begin
  if FActivated and FDraw.CanDraw then
    FDraw.AdAppl.Viewport := FViewport;
end;

procedure TAdScene.SetAmbientColor(AValue: TAndorraColor);
begin
  FAmbientColor := AValue;
  UpdateAmbientColor;
end;

procedure TAdScene.Setup2DScene(AWidth, AHeight: integer);
var
  curscene:TAdSurface;
begin
  if AdDraw.CanDraw then
  begin
    //Store current surface
    curscene := nil;
    if not Activated then
      curscene := AdDraw.ActiveSurface;

    //Deactivate current scene, so that it can be reactivated 
    if curscene <> nil then
      curscene.Deactivate;

    Activate;

    //Set and store new scene settings
    FDraw.AdAppl.Setup2DScene(AWidth, AHeight, FFarZ, FNearZ);
    FDraw.AdAppl.GetScene(FViewMatrix, FProjectionMatrix);

    FWidth := AWidth;
    FHeight := AHeight;

    //Reset scene if neccessary
    if curscene <> nil then
      curscene.Activate;
  end;
end;

procedure TAdScene.Setup3DScene(AWidth, AHeight: integer; APos, ADir,
  AUp: TAdVector3);
var
  curscene:TAdSurface;
begin
  if AdDraw.CanDraw then
  begin
    //Store current surface
    curscene := nil;
    if not Activated then
      curscene := AdDraw.ActiveSurface;

    //Deactivate current scene, so that it can be reactivated 
    if curscene <> nil then
      curscene.Deactivate;

    Activate;

    //Set and store new scene settings
    FDraw.AdAppl.Setup3DScene(AWidth, AHeight, APos, ADir, AUp, FNearZ, FFarZ);
    FDraw.AdAppl.GetScene(FViewMatrix, FProjectionMatrix);

    //Store width and height settings
    FWidth := AWidth;
    FHeight := AHeight;

    //Reset scene if neccessary
    if curscene <> nil then
      curscene.Activate;
  end;
end;

{ TAdSurface }

constructor TAdSurface.Create(ADraw: TAdDraw);
begin
  inherited Create;
  if (ADraw = nil) and (self is TAdDraw) then
    FDraw := TAdDraw(self)
  else
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
  //Nothing to do in this class.
end;

procedure TAdSurface.DoDeactivation;
begin
  //Nothing to do in this class.
end;

procedure TAdSurface.Notify(ASender: TObject; AEvent: TAdSurfaceEventState);
begin
  //Nothing to do in this class.
end;

procedure TAdSurface.ClearSurface(AColor: LongInt);
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

constructor TAdRenderingSurface.Create(ADraw: TAdDraw);
begin
  inherited;

  FScene := TAdScene.Create(AdDraw);
  FOptions := AdDraw.Options;

  //Connect with the scene activate event
  Scene.OnActivate := SceneActivate;
end;

destructor TAdRenderingSurface.Destroy;
begin
  FScene.Free;
  inherited;
end;

procedure TAdRenderingSurface.DoInitialize;
begin
  DoFinalize;
  FCanvas := TAdCanvas.Create(AdDraw.AdAppl);
  FCanvas.OnRelease := CanvasRelease;
  FCanvas.Font := AdDraw.Fonts.GenerateFont('Tahoma', 10, []);

  Scene.Viewport := AdRect(0, 0, Width, Height);
end;

function TAdRenderingSurface.GetDisplayRect: TAdRect;
begin
  result :=
    AdRect(0, 0, Scene.Width, Scene.Height);
end;

procedure TAdRenderingSurface.DoFinalize;
begin
  if FCanvas <> nil then
  begin
    FCanvas.Free;
    FCanvas := nil;
  end;
end;

procedure TAdRenderingSurface.DoActivation;
begin
  FScene.Activate;
  AdDraw.AdAppl.SetOptions(FOptions);
end;

procedure TAdRenderingSurface.DoBeginScene;
begin
  FCanvas.StartFrame;
end;

procedure TAdRenderingSurface.DoDeactivation;
begin
  FScene.Deactivate;
end;

procedure TAdRenderingSurface.DoEndScene;
begin
  FCanvas.EndFrame;
end;

procedure TAdRenderingSurface.Notify(ASender: TObject;
  AEvent: TAdSurfaceEventState);
begin
  case AEvent of
    seBeginScene: DoBeginScene;
    seEndScene: DoEndScene;
    seFinalize: DoFinalize;
    seInitialized: DoInitialize;
  end;
end;

procedure TAdRenderingSurface.SceneActivate(Sender: TObject);
begin
  Activate;
end;

procedure TAdRenderingSurface.SetOptions(AOptions: TAd2dOptions);
begin
  FOptions := AOptions;
  if (Activated) and (AdDraw.CanDraw) then
    AdDraw.AdAppl.SetOptions(FOptions);
end;

procedure TAdRenderingSurface.Setup2DScene;
begin
  Scene.Setup2DScene(Width, Height);
  Scene.Viewport := AdRect(0, 0, Width, Height);
end;

procedure TAdRenderingSurface.CanvasRelease(Sender: TObject);
begin
  Activate;
end;

procedure TAdRenderingSurface.ClearSurface(AColor: Integer);
begin
  inherited;
  
  if AdDraw.CanDraw then
  begin
    //Clear the surface with the specified color.
    AdDraw.AdAppl.ClearSurface(
      Scene.Viewport, [alColorBuffer, alZBuffer, alStencilBuffer],
      ColorToAdColor(AColor), 1, 0);
  end;
end;

{ TAdTextureSurface }

constructor TAdTextureSurface.Create(ADraw: TAdDraw);
begin
  inherited Create(ADraw);

  //Create image and render surface texture
  FImage := TAdCustomImage.Create(ADraw);
  FTexture := TAdRenderTargetTexture.Create(ADraw);
  FImage.Texture := FTexture;


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
  if CanDraw then
  begin
    FTexture.Width := AWidth;
    FTexture.Height := AHeight;
    FImage.Restore;

    Setup2DScene;
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

function TAdTextureSurface.GetHeight: integer;
begin
  result := FTexture.Height;
end;

function TAdTextureSurface.GetWidth: integer;
begin
  result := FTexture.Width;
end;

function TAdTextureSurface.CanDraw: boolean;
begin
  result := FTexture.Initialized and AdDraw.CanDraw;
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

  //Break here if the result was nil.
  if FAd2dTexture = nil then
    raise EAdInterfaceCreation(MsgSurfaceInterfaceNotAvailable);
    
  FAd2dTexture.Filter := FFilter;
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
  AEvent: TAdSurfaceEventState);
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

{ TAdPluginPropertyList }

procedure AddPropProc(const ASender: TObject; const AProp: TAd2dProperty); stdcall;
begin
  TAdPluginPropertyList(ASender).AddProp(AProp);
end;

constructor TAdPluginPropertyList.Create;
begin
  inherited;       
end;

destructor TAdPluginPropertyList.Destroy;
begin
  Clear;
  inherited;
end;

procedure TAdPluginPropertyList.Clear;
var
  i: integer;
begin
  //Free all reserved memory
  for i := 0 to High(FPtrs) do
    FreeMem(FPtrs[i].SetPtr, FPtrs[i].SetSize);

  //Finalize all arrays
  SetLength(FProps, 0);
  SetLength(FPtrs, 0);
  SetLength(FValues, 0);

  FCount := 0;
end;

procedure TAdPluginPropertyList.ReadProperties(ADllLoader: TAdDllLoader);
begin
  //Clear the list
  Clear;

  //Store properties in the local list
  ADllLoader.LibProperties(self, AddPropProc);
end;

procedure TAdPluginPropertyList.WriteProperties(AAppl: TAd2DApplication);
begin
  //Store the values in the Ad2dAppliation instance
  if length(FValues) > 0 then
  begin
    AAppl.SetProperties(Length(FValues), @FValues[0]);
  end;
end;

function TAdPluginPropertyList.GetProperty(Index: integer): TAd2dProperty;
begin
  result := FProps[Index];
end;

function TAdPluginPropertyList.PropertyExists(AName: string): boolean;
var
  i: integer;
begin
  result := false;

  for i := 0 to FCount - 1 do
  begin
    if Lowercase(FProps[i].PropName) = Lowercase(AName) then
    begin
      result := true;
      break;
    end;
  end;
end;

procedure TAdPluginPropertyList.SetProp(AName: string; AValue: integer);
begin
  AddValue(AName, AValue, SizeOf(Integer));
end;

procedure TAdPluginPropertyList.SetProp(AName: string; AValue: boolean);
begin
  AddValue(AName, AValue, SizeOf(Boolean));
end;

procedure TAdPluginPropertyList.SetProp(AName: string; AValue: TAd2dResolution);
begin
  AddValue(AName, AValue, SizeOf(TAd2dResolution));
end;

procedure TAdPluginPropertyList.AddProp(AProp: TAd2dProperty);
begin
  //Write the new property set
  SetLength(FProps, Length(FProps) + 1);
  FProps[High(FProps)] := AProp;

  FCount := FCount + 1;
end;

procedure TAdPluginPropertyList.AddValue(AName: string; var AValue; ASize: Integer);
var
  ptr: Pointer;
begin
  //Reserve memory for the new value
  GetMem(ptr, ASize);
  Move(AValue, ptr^, ASize);

  //Add the pointer to the pointer array. The pointer array is needed to dispose
  //the reserved memory when the list is cleared.
  SetLength(FPtrs, Length(FPtrs) + 1);
  with FPtrs[High(FPtrs)] do
  begin
    SetPtr := ptr;
    SetSize := ASize;
  end;

  //Add a value that will be written into the plugin
  SetLength(FValues, Length(FValues) + 1);
  with FValues[High(FValues)] do
  begin
    PropName := Lowercase(AName);
    PropValue := ptr;
  end;
end;

end.
