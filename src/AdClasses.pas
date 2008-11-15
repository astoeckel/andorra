{
* This program is licensed under the Common Public License (CPL) Version 1.0
* You should have recieved a copy of the license with this file.
* If not, see http://www.opensource.org/licenses/cpl1.0.txt for more informations.
* 
* Inspite of the incompatibility between the Common Public License (CPL) and the GNU General Public License (GPL) you're allowed to use this program
* under the GPL. 
* You also should have recieved a copy of this license with this file. 
* If not, see http://www.gnu.org/licenses/gpl.txt for more informations.

* Project: Andorra 2D
* Author:  Andreas Stoeckel
* File: AdClasses.pas
* Comment: Contains all classes and types which are exchanged between the host
           and the plugin.
}

{AdClasses.pas contains all classes and types shared between the host and the plugin.}
unit AdClasses;

interface

uses
  AdWindowFramework, AdTypes, AdBitmapClass;

type
  {Specifies the severity of a log message.}
  TAd2dLogSeverity = (
    lsInfo, {< The log entry contains only some basic information.}
    lsWarning, {< The log entry warns of somthing, that could later be a problem.}
    lsError, {< An error occured, that does not prevent the program from further executing.}
    lsFatalError {< An fatal error occured. The application will stop now.}
  );

  {Callback function that is used to send log entries from the plugin to the
   host application.}
  TAd2dLogCallback = procedure(AModule: PChar; ASeverity: TAd2dLogSeverity;
    AMsg: PChar) of object; 

  {Specifies the dimensions of the display. }
  TAd2dResolution = record
    //The Width of the Display
    Width:integer;
    //The Height of the Display
    Height:integer;
    //The Bitcount of the Display (May be 16 or 32.)
    BitDepth: TAdBitDepth;
    //The horizontal refresh rate. Can be zero to use the desktops refresh rate.
    Freq:integer;
  end;
  {Pointer on TAd2dResolution}
  PAd2dResolution = ^TAd2dResolution;

  {A record which represents a mesh material.}
  TAd2dMaterial = record
    {The diffuse color of the material. This is the color that is reflected by
     the object when light shines on it. For example a object that has a red
     diffuse color would be black if only blue light shines on it. If white 
     light hits the object, it would appear red.}
    Diffuse: TAndorraColor;
    {The ambient color of the material. The ambient light defines the color 
     that is reflecten when ambient light shines on it. Normally the diffuse
     and the ambient color are always the same.}
    Ambient: TAndorraColor;
    {Adds a shine effect to the object when light shines on it. Use the power
     parameter to influence the strength of this effect.}
    Specular: TAndorraColor;
    {The emissive color is the color of the object that is emitted if no light
     shines on the object.}
    Emissive: TAndorraColor;
    {The internsity of the specular effect.}
    Power: single;
  end;
  {Pointer on TAd2dMaterial}
  PAd2dMaterial = ^TAd2dMaterial;

  {Used to set the type of a lightsource.}
  TAd2dLightType = (
    {All light rays are parallel. This type of light can for example be used
     to simulate sunlight.}
    altDirectional,
    {The light is emitted by a point light source with limited range.}
    altPoint
  );

  {A record that contains light source settings.}
  TAd2dLightData = record
    {The type of the light. Can be a directional or a
     point light.}
    LightType: TAd2dLightType;
    {The diffuse color of the light source.}
    Diffuse: TAndorraColor;
    {The speculat color of the light source.}
    Specular: TAndorraColor;
    {The ambient color of the light source.}
    Ambient: TAndorraColor;

    {The position of the light. If the light source is a directional light,
     position is equivalent to the direction of the light.}
    Position: TAdVector3;

    {The constant attenuation. Only used with point- and spotlights.}
    ConstantAttenuation: single;
    {The linear attenuation. Only used with point- and spotlights.}
    LinearAttenuation: single;
    {The quadratic attenuation. Only used with point- and spotlights.}
    QuadraticAttenuation: single;
  end;
  {Pointer on TAd2dLightData.}
  PAd2dLight = ^TAd2dLightData;

  {Declares, how a mesh is blended}
  TAd2dBlendMode = (
    bmAlpha,//< Normal mode
    bmAdd,//< Additive blending
    bmSub,//< Substractive blending
    bmMask//< The mesh apears black
  );

  {Specifies how the vertices are drawn}
  TAd2dDrawMode = (
    adPoints,//<The vertices are drawn as single points
    adLines,//<The vertices are drawn as lines
    adLineStrips,//<The vertices are drawn as a line strip
    adTriangles,//<The vertices are drawn as a list of triangles
    adTriangleStrips,//<The vertices are drawn as a triangle strip
    adTriangleFan,//<The vertices are drawn as a triangle fan
    adPointSprites //< The vertices are drawn as point sprites
  );

  {Specifies the way textures are filtered.}
  TAd2dTextureFilter = (
    atPoint,{< The filter with worst quality. The pixels won't be interpolated.}
    atLinear,{< The pixels will be interpolated using  a linear filter.}
    atAnisotropic{< The pixels will be interpolated using an anisotropic filter.}
  );

  {These options are used to en- or disable specific parts of the graphic system
   in order to have more FPS.}
  TAd2dOption = (
    aoLight, {< If this option is turned on, lights can be used}
    aoTextures, {< If this option is turned on, textures are used}
    aoBlending, {< If this option is turned on, the transparency effects are used}
    aoAlphaMask, {< If enabled, pixels with an alpha value of 0 will not be written in the Z-Buffer.}
    aoMipmaps, {< If this option is turned on, all textures loaded are equiped with a mipmap}
    aoZBuffer, {< If this option is turned on, the engine will write in the Z-Buffer}
    aoStencilBuffer, {< Enables the stencil buffer.}
    aoAntialias, {< If this option is turned on, antialias is used.
      Remember that this option is not available on all plugins and has to be
      setuped before initializing the graphic system using the "Properties" interface.}
    aoCulling {< If this option is turned on, culling is enabled.}
  );
  {Set of TAd2dOption used in TAdRenderingSurface. See TAd2dOption for more 
   details.}
  TAd2dOptions = set of TAd2dOption;

  {TAd2dSurfaceLayer is used by the TAd2dApplication.ClearSurface method. A set
   of TAd2dSurfaceLayer specifies, which parts of the surface should actually
   be cleared.}
  TAd2dSurfaceLayer = (
    alColorBuffer, {< Clears the visible color buffer.}
    alZBuffer, {< Clears the Z-Buffer, which holds the Z-Position of each pixel on the surface}
    alStencilBuffer {< Clears the stencil buffer, that is used by some special effects}
  );

  {This type is used in the stencil buffer functions in order to set when a 
   stencil buffer operation fails or not.
   @seealso(TAd2dApplication.SetStencilOptions)
   @seealso(TAd2dOption)}
  TAd2dStencilFunction = (
    asfNever, //< The stencil buffer comparison can never be achived.
    asfLessThan, //< The pixel is set if the reference value is smaller than the stencil buffer value 
    asfLessThanOrEqual, //< The pixel is set if the reference value is smaller or equal to the stencil value
    asfEqual, //< The stencil buffer comparison passes if the stencil value is equal to the reference value
    asfGreaterThanOrEqual, //< The pixel is set if the reference value is greater or equal to the stencil value
    asfGreaterThan, //< The pixel is set if the reference value is greater than the stencil buffer.
    asfAlways //< The stencil buffer operation will always pass
  );

  {The operation that will be performed on the stencil buffer if a stencil
   event is triggered.}
  TAd2dStencilOperation = (
    asoKeep, //< The current stencil value is kept
    asoReplace, //< The stencil value is replaced by the reference value
    asoIncrement, //< The current stencil value is incremented
    asoDecrase, //< The current stencil value decrased
    asoZero //< The stencil value is set to zero
  );

  {TAd2dStencilEvent represents the stencil states that can be triggered when
   the stencil test takes place.}
  TAd2dStencilEvent = (
    aseFail, //< Triggered if the stencil test fails
    aseZFail, //< The stencil test didn't fail, but the Z-Buffer test did.
    asePass //< The stencil test and the Z-Buffer test didn't were passed
  );

  {A set of TAd2dSurfaceLayer, that specifies, which parts of the surface should
   actually be cleared. @seealso(TAd2dSurfaceLayer)}
  TAd2dSurfaceLayers = set of TAd2dSurfaceLayer;
  
  {A class which represents a texture in Andorra's engine. }
  TAd2DTexture = class;
  {A class which represents a render target texture}
  TAd2DRenderTargetTexture = class;
  {An abstract class which represents a bitmap texture in Andorra's engine. }
  TAd2DBitmapTexture = class;
  {An abstract class which represents a mesh (a set of vertices)  in Andorra's engine. }
  TAd2DMesh = class;
  {An abstract class which represents a counter for the pixels drawn}
  TAd2dPixelCounter = class;
  {An abstract class that represents a single light}
  TAd2dLight = class;

  {A record that returns information about the current library.
   The information is returned by the Andorra2DLibraryInformation function.}
  TAd2DLibInfo = record
    {The library title}
    LibTitle:string[255];
    {The author of the library}
    LibAuthor:string[255];
    {The description of the library}
    LibDescription:string[255];
    {The version of the library}
    LibVersion:string[255];
    {A path to an image}
    LibImage:string[255];
  end;

  {Speifies the type of the property. @seealso(TAd2dProperty)}
  TAd2dPropertyType = (
    ptInteger, //< The property is a number
    ptBoolean, //< The property is a boolean
    ptResolution, //< The property is a TAd2dResolution structure
    ptReadOnly //< The property is used to transfer plugin capabilities
  );

  {The "TAd2dProperty"-Record contains informations about the settings that
   are published by the dll. The properties are accessed by calling the
   "Andorra2DApplicationProperties" function of the plugin dll. Properties are
   used to set library related properties that have to be done before
   initializing the system, e.g. fullscreen, antialias, v-sync, software- or
   hardware mode etc.}
  TAd2dProperty = record
    {The name of the property.}
    PropName: string[64];
    {The name of the property that will be viewed in the setup dialog.}
    PropViewName: string[64];
    {The group where the property will be inserted in the setup dialog.}
    PropGroup: string[64];
    {The type of the property.}
    PropType: TAd2dPropertyType;
  end;

  {"TAd2dPropertyValue" is used to send value information to the plugin dll.
   Setting the properties is done by calling the "TAd2dApplication.SetProperties"
   method.}
  TAd2dPropertyValue = record
    {Name of the property that will be set. If the property does not exist,
     nothing will happen.}
    PropName: string[64];
    {The value that will be assigned to the property specified by "PropName".}
    PropValue: Pointer;
  end;
  {Pointer on TAc2dPropertyValue.}
  PAd2dPropertyValue = ^TAd2dPropertyValue;

  {Callback procedure used to store the properties. @seealso(TAd2dProperty)}
  TAd2dPropertyProc = procedure(const ASender: TObject;
    const AProp: TAd2dProperty); stdcall;

  {Procedure used in the dll to receive information about the library.}
  TAndorra2DLibraryInformation = procedure(var libinfo:TAd2DLibInfo);stdcall;
  {Procedure used in the dll to receive the abilities of the library and to set
   properties before initialization.}
  TAndorra2DApplicationProperties = procedure(const ASender: TObject;
    const AddPropertyProc: TAd2dPropertyProc);stdcall;

  {Abstract class which represents an Andorra 2D application.}
  TAd2DApplication = class
    private
      FLogCallback: TAd2dLogCallback;
      FAmbientColor: TAndorraColor;
      procedure SetLogCallback(ALogCallback: TAd2dLogCallback);
    protected
      FWidth:integer;
      FHeight:integer;
      FMaxLightCount:integer;
      FViewPort:TAdRect;
      procedure SetViewPort(AValue:TAdRect);virtual;
      procedure SetAmbientColor(AValue: TAndorraColor);virtual;
    public
      {Creates and returns a TAd2DBitmapTexture}
      function CreateBitmapTexture: TAd2DBitmapTexture;virtual;abstract;
      {Creates and returns a TAd2dRenderTargetTexture}
      function CreateRenderTargetTexture: TAd2dRenderTargetTexture;virtual;abstract;
      {Creates and returns a TAd2DMesh}
      function CreateMesh: TAd2DMesh;virtual;abstract;
      {Creates and returns a TAd2DPixelCounter}
      function CreatePixelCounter: TAd2dPixelCounter;virtual;abstract;
      {Creates and returns a TAd2dLight}
      function CreateLight: TAd2dLight;virtual;abstract;

      {Sets the surface that the graphic system should render on. If ATarget is
       nil, the graphic system will rendern on its main surface.}
      procedure SetRenderTarget(ATarget:TAd2dRenderTargetTexture);virtual;abstract;
      {Loads "ACount" "TAd2dPropertyValue" properties from the memory specified
       by "APProps".}
      procedure SetProperties(ACount: integer; APProps: PAd2dPropertyValue);virtual;abstract;

      {Applies the options specified in "AOptions" to the graphic system.}
      procedure SetOptions(AOptions: TAd2dOptions);virtual;abstract;

      {Use this method to setup the stencil buffer.}
      procedure SetStencilOptions(AReference, AMask: Word;
        AFunction: TAd2dStencilFunction);virtual;abstract;

      {Using this function you can assign operations to the stencil buffer operations.}
      procedure SetStencilEvent(AEvent: TAd2dStencilEvent;
        AOperation: TAd2dStencilOperation);virtual;abstract;

      {Initializes the engine. AWnd is the handle to the window.}
      function Initialize(AWnd:TAdWindowFramework):boolean;virtual;abstract;
      {Finalizes the engine.}
      procedure Finalize;virtual;abstract;

      {Clears the active surface. You can specify which layers and which
       rectangular region of the surface should be cleared. The coordinats are
       absolute.}
      procedure ClearSurface(ARect: TAdRect; ALayers: TAd2dSurfaceLayers;
        AColor: TAndorraColor; AZValue: double; AStencilValue: integer); virtual; abstract;

      {Should be called after clearing the surface and before drawing on it.}
      procedure BeginScene;virtual;abstract;
      {Should be called after drawing on the surface and before flipping it.}
      procedure EndScene;virtual;abstract;
      {Flips front and back buffer - presents the scene on the screen.}
      procedure Flip;virtual;abstract;

      {Prepares a 2D view and a orthogonal projection matrix. Setting the
       coordinate systems is usually done by the TAdScene object. Every Andorra
       surface (e.g. TAdDraw) owns a instance of TAdScene.}
      procedure Setup2DScene(AWidth, AHeight:integer;
        ANearZ, AFarZ: double);virtual;abstract;
      {Prepares a 3D view and a perspective projection matrix. Setting the
       coordinate systems is usually done by the TAdScene object. Every Andorra
       surface (e.g. TAdDraw) owns a instance of TAdScene.}
      procedure Setup3DScene(AWidth, AHeight:integer;
        APos, ADir, AUp:TAdVector3; ANearZ, AFarZ: double);virtual;abstract;
      {Gives the possibility to setup the coordinate system manualy. Using this
       method to store own matrices may not work correctly. Only set matrices that
       were created by the "GetScene" function. @seealso(GetScene)}
      procedure SetupManualScene(AMatView, AMatProj:TAdMatrix);virtual;abstract;
      {Returns the current view and projection matrix. @seealso(SetupManualScene)}
      procedure GetScene(out AMatView:TAdMatrix; out AMatProj:TAdMatrix);virtual;abstract;

      {Returns whether the given windowframework is supported.}
      function SupportsWindowFramework(AClassId:ShortString):boolean;virtual;abstract;

      {Returns the width of the active surface}
      property Width:integer read FWidth;
      {Returns the height of the active surface}
      property Height:integer read FHeight;
      {Returns the number of max lights}
      property MaxLights:integer read FMaxLightCount;
      {The rectangle where the output is made}
      property Viewport:TAdRect read FViewPort write SetViewPort;
      {The scene ambient color}
      property AmbientColor: TAndorraColor read FAmbientColor write SetAmbientColor;

      {This procedure can be used to add a log message. If the log message
       callback is not equal to nil, it is called an the parameters are passed.}
      procedure Log(AModule: PChar; ASeverity: TAd2dLogSeverity; AMsg: PChar);

      {Callback used to log log messages from the plugin in the host
       application.}
      property LogCallback: TAd2dLogCallback read FLogCallback write SetLogCallback;
  end;

  {An class which represents a texture in Andorra's engine. }
  TAd2DTexture = class
    private
    protected
      FWidth:integer;
      FHeight:integer;
      FBitDepth:TAdBitDepth;
      FEditable:boolean;
      FTexture:Pointer;
      FBaseWidth:integer;
      FBaseHeight:integer;
      FFilter: TAd2dTextureFilter;
      function GetLoaded:boolean;virtual;abstract;
    public
      {The width of the texture in the memory. Is scaled to power of two.}
      property Width:integer read FWidth;
      {The height of the texture in the memory. Is scaled to power of two.}
      property Height:integer read FHeight;
      {Contains informations about the BitDepth of the texture. Can be 16 or 32.}
      property BitDepth:TAdBitDepth read FBitDepth;
      {Returns weather the texture can be edited.}
      property Editable:boolean read FEditable;
      {Returns weather a texture is loaded.}
      property Loaded:boolean read GetLoaded;
      {A pointer to the graphic systems texture.}
      property Texture:pointer read FTexture;
      {This value contains the original width of the texture. Important if the original texture's size was not power of two.}
      property BaseWidth:integer read FBaseWidth;
      {This value contains the original height of the texture. Important if the original texture's size was not power of two.}
      property BaseHeight:integer read FBaseHeight;
      {The texture filter used.}
      property Filter: TAd2dTextureFilter read FFilter write FFilter;
  end;

  {An abstract class which represents a mesh (a set of vertices)  in Andorra's engine. }
  TAd2DMesh = class
    protected
      FVertices:TAdVertexArray;
      FIndices:TAdIndexArray;
      FVertexCount:integer;
      FIndicesCount:integer;
      FPrimitiveCount:integer;
      FTexture:TAd2DTexture;
      FMatrix:TAdMatrix;
      FTextureMatrix: TAdMatrix;
      procedure SetVertices(AVertices:TAdVertexArray);virtual;abstract;
      procedure SetIndices(AIndex:TAdIndexArray);virtual;abstract;
      procedure SetTexture(ATexture:TAd2DTexture);virtual;
      function GetLoaded:boolean;virtual;abstract;
    public
      {Pushes the data into the graphic system's format.}
      procedure Update;virtual;abstract;
      {Draws the mesh.}
      procedure Draw(ABlendMode:TAd2DBlendMode;ADrawMode:TAd2DDrawMode);virtual;abstract;
      {Sets the material data. If AMaterial is nil, the vertex colors are used as
       material source.}
      procedure SetMaterial(AMaterial: PAd2dMaterial);virtual;abstract;
      {Returnes weather data is loaded.}
      property Loaded:boolean read GetLoaded;
      {The vertices a mesh has.}
      property Vertices:TAdVertexArray read FVertices write SetVertices;
      {The index buffer of a mesh.}
      property Indices:TAdIndexArray read FIndices write SetIndices;
      {Returnes the count of vertices.}
      property VertexCount:integer read FVertexCount;
      {Returnes the count of indices.}
      property IndicesCount:integer read FIndicesCount;
      {Set the amount of primitives here.}
      property PrimitiveCount:integer read FPrimitiveCount write FPrimitiveCount;
      {Set the texture of the mesh here. Set to nil, if you want no texture.}
      property Texture:TAd2DTexture read FTexture write SetTexture;
      {The transformation matrix of the model}
      property Matrix: TAdMatrix read FMatrix write FMatrix;
      {The texture matrix of the model}
      property TextureMatrix: TAdMatrix read FTextureMatrix write FTextureMatrix;
  end;

  {An abstract class which represents a bitmap texture in Andorra's engine. }
  TAd2DBitmapTexture = class(TAd2DTexture)
    public
      {Frees the textures memory}
      procedure FlushTexture;virtual;abstract;
      {Loads the texture from a TAd2dBitmap.}
      procedure LoadFromBitmap(ABmp:TAd2dBitmap; ABitDepth: TAdBitDepth);virtual;abstract;
      {Saves the texture to a TAd2dBitmap.}
      procedure SaveToBitmap(ABmp:TAd2dBitmap);virtual;abstract;
  end;
  
  {An abstract class that represents a texture that can act as a render target.}
  TAd2DRenderTargetTexture = class(TAd2dTexture)
    public
      {Resizes the render target texture. If no texture is loaded, a new texture will be created.}
      procedure SetSize(AWidth, AHeight: integer; ABitDepth: TAdBitDepth);virtual;abstract;
      {Flushes the current memory/texture assigned.}
      procedure FlushMemory;virtual;abstract;
      {Saves the content of the current render target texture to the bitmap specified by "ABmp". }
      procedure SaveToBitmap(ABmp:TAd2dBitmap);virtual;abstract;
  end;

  {Used for counting the count of pixels that passed the Z-Buffer test. The Z-Buffer 
   has to be activated while counting pixels. Only one single pixel counter can be 
   active (counting pixels) at a time.}
  TAd2dPixelCounter = class
    public
      {Starts counting pixels.}
      procedure StartCount;virtual;abstract;
      {Stops counting pixels.}
      procedure StopCount;virtual;abstract;
      {Returns the pixel count.}
      function GetCount: Cardinal;virtual;abstract;
  end;

  {A class that represents a light in the current scene. This type of light is only
   capable of doing vertex lighting.
   @seealso(TAd2dLightData)}
  TAd2dLight = class
    private
      FData: TAd2dLightData;
    protected    
      procedure SetData(AValue: TAd2dLightData);virtual;
    public
      {Enables the current light and assigns it to the given light number. A Andorra 2D
       graphic system implementation has to be capable of supporting the light numbers
       0-7. The maximum light count may be read in the TAd2dApplication.MaxLights
       property. The light settings have to be set before activating the light source.
       @seealso(TAd2dLight.Data)
       @seealso(TAd2dApplication.MaxLights)}
      procedure EnableLight(ALight: Cardinal);virtual;abstract;
      {Disables the light, if it had been activated.}
      procedure DisableLight;virtual;abstract;
      
      {The light settings. Set these settings before enabling the light.}
      property Data: TAd2dLightData read FData write SetData;
  end;

  //Used to import the CreateApplication function form the DLL.
  TAdCreateApplicationProc = function:TAd2dApplication;stdcall;

const
  {The current Andorra 2D version. If version between Plugin and Source is different,
   loading stops.}
{$IFDEF FPC}
  LibraryVersion = 'VER 0.4.5 FPC';
{$ELSE}
  LibraryVersion = 'VER 0.4.5.002';
{$ENDIF}

implementation

{ TAd2dApplication }

procedure TAd2DApplication.Log(AModule: PChar; ASeverity: TAd2dLogSeverity;
  AMsg: PChar);
begin
  if @FLogCallback <> nil then
  begin
    FLogCallback(AModule, ASeverity, AMsg);
  end;
end;

procedure TAd2DApplication.SetAmbientColor(AValue: TAndorraColor);
begin
  FAmbientColor := AValue;
end;

procedure TAd2DApplication.SetLogCallback(ALogCallback: TAd2dLogCallback);
begin
  FLogCallback := ALogCallback;
end;

procedure TAd2DApplication.SetViewPort(AValue: TAdRect);
begin
  FViewPort := AValue;
end;

{ TAd2dMesh }

procedure TAd2DMesh.SetTexture(ATexture:TAd2DTexture);
begin
  FTexture := ATexture;
end;

{ TAd2dLight }

procedure TAd2dLight.SetData(AValue: TAd2dLightData);
begin
  FData := AValue;
end;

//Make floating point operations more accurate on a x86-CPU

{$IFDEF FPC}
  {$IFDEF CPU386}
  {$ASMMODE intel}

const
  Default8087CW: Word = $1332;

procedure Set8087CW(NewCW: Word); assembler;
asm
  MOV Default8087CW, AX
end;

  {$ENDIF}
{$ENDIF}

initialization

{$IFDEF CPU386}
  Set8087CW($133F);
{$ENDIF}

end.
