{
* This program is licensed under the Common Public License (CPL) Version 1.0
* You should have recieved a copy of the license with this file.
* If not, see http://www.opensource.org/licenses/cpl1.0.txt for more informations.
* 
* Inspite of the incompatibility between the Common Public License (CPL) and the GNU General Public License (GPL) you're allowed to use this program * under the GPL. 
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
  {Contains information about how the scene is displayed.}
  TAdOption = (
    doFullscreen, {<The scene will be displayed in the fullscreen mode}
    doVSync, {<The scene will be displayed using V-Sync. The frame rate is linked to the vertical frequncy of the screen}
    doHardware, {< The scene will be displayed using hardware acceleration.}
    doAntialias, {< Antialias will be enabled when drawing the scene.}
    doZBuffer {< The scene will contain a Z-Buffer}
  );


  {Declares a set of TAdDrawMode. See above to learn what all these settings mean.}
  TAdOptions = set of TAdOption;

  {Specifies the dimensions of the display. }
  TAdDisplay = record
    //The Width of the Display
    Width:integer;
    //The Height of the Display
    Height:integer;
    //The Bitcount of the Display (May be 16 or 32.)
    BitCount:byte;
    //The horizontal refresh rate. Can be zero to use the desktops refresh rate.
    Freq:integer;
  end;

  {Declares the different types of log information.}
  TAdLogTyp = (
    ltInfo, //< Only an information
    ltWarning, //< A warning means that the next steps the engine does, could be influenced by this occurence.
    ltError, //< Something didn't work as espected
    ltFatalError, //< Something didn't work as espected and the engine can not be runned anymore
    ltNone //< Something else.
  );

  {A record which represents one log item.}
  TAdLogItem = record
    {The information}
    Text:PChar;
    {The typ of the log entry}
    Typ:TAdLogTyp;
  end;
  
  {Declares a procedure which is called by the engine when it wants to log something.}
  TAdLogProc = procedure(LogItem:TAdLogItem) of object;

  {Declares, how a mesh is blended}
  TAd2DBlendMode = (
    bmAlpha,//< Normal mode
    bmAdd,//< Additive blending
    bmSub,//< Substractive blending
    bmMask//< The mesh apears black
  );

  {Specifies how the vertices are drawn}
  TAd2DDrawMode = (
    adPoints,//<The vertices are drawn as single points
    adLines,//<The vertices are drawn as lines
    adLineStrips,//<The vertices are drawn as a line strip
    adTriangles,//<The vertices are drawn as a list of triangles
    adTriangleStrips,//<The vertices are drawn as a triangle strip
    adTriangleFan,//<The vertices are drawn as a triangle fan
    adPointSprites //< The vertices are drawn as point sprites
  );

  TAd2DTextureFilter = (
    atPoint,{< The filter with worst quality. The pixels won't be interpolated.}
    atLinear,{< The pixels will be interpolated using  a linear filter.}
    atAnisotropic{< The pixels will be interpolated using an anisotropic filter.}
  );
  
  {An abstract class which represents a light in Andorra's engine. }
  TAd2DLight = class;
  {A class which represents a texture in Andorra's engine. }
  TAd2DTexture = class;
  {A class which represents a render target texture}
  TAd2DRenderTargetTexture = class;
  {An abstract class which represents a bitmap texture in Andorra's engine. }
  TAd2DBitmapTexture = class;
  {An abstract class which represents a mesh (a set of vertices)  in Andorra's engine. }
  TAd2DMesh = class;

  {A record that returns information about the current library. The information is returned by the Andorra2DLibraryInformation function}
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

  {A record that stores abilities of a plugin}
  TAd2DLibAbilities = record
    {Plugin may do fullscreen}
    LibFullscreen:boolean;
    {Plugin may do windowed applications}
    LibWindowed:boolean;
    {Plugin may do hardware acceleration}
    LibHardware:boolean;
    {Plugin may do software rendering}
    LibSoftware:boolean;
    {Plugin may do antialiasing}
    LibAntialias:boolean;
    {Library can do V-Sync}
    LibVSync:boolean;
    {Library may handle lights}
    LibLights:boolean;
    {Library may do 3D-rendering}
    Lib3D:boolean;
  end;

  {Procedure used in the dll to receive information about the library.}
  TAndorra2DLibraryInformation = procedure(var libinfo:TAd2DLibInfo);stdcall;
  {Procedure used in the dll to receive the abilities of the library.}
  TAndorra2DLibraryAbilities = procedure(var libabilities:TAd2DLibAbilities);stdcall;

  {Abstract class which represents an Andorra 2D application.}
  TAd2DApplication = class
    private
      FLogProc:TAdLogProc;
    protected
      FOptions:TAdOptions;
      FWidth:integer;
      FHeight:integer;
      FMaxLightCount:integer;
      FViewPort:TAdRect;
      procedure SetOptions(AValue:TAdOptions);virtual;
      procedure WriteLog(Typ:TAdLogTyp;Text:PChar);
      procedure SetViewPort(AValue:TAdRect);virtual;
    public
      {Creates and returns a TAd2DLight}
      function CreateLight:TAd2DLight;virtual;abstract;
      {Creates and returns a TAd2DBitmapTexture}
      function CreateBitmapTexture:TAd2DBitmapTexture;virtual;abstract;
      {Creates and returns a TAd2dRenderTargetTexture}
      function CreateRenderTargetTexture:TAd2dRenderTargetTexture;virtual;abstract;
      {Creates and returns a TAd2DMesh}
      function CreateMesh:TAd2DMesh;virtual;abstract;

      procedure SetRenderTarget(ATarget:TAd2dRenderTargetTexture);virtual;abstract;

      {Sets the procedure which will recive a new log entry.}
      procedure SetLogProc(ALogProc:TAdLogProc);

      {Initializes the engine. AWnd is the handle to the window.}
      function Initialize(AWnd:TAdWindowFramework; AOptions:TAdOptions;
        ADisplay:TAdDisplay):boolean;virtual;abstract;
      {Finalizes the engine.}
      procedure Finalize;virtual;abstract;

      {Clears the surface with a specific color.}
      procedure ClearSurface(AColor: TAndorraColor);virtual;abstract;
      {Begins a scene}
      procedure BeginScene;virtual;abstract;
      {Ends a scene}
      procedure EndScene;virtual;abstract;
      {Presents the scene on the screen}
      procedure Flip;virtual;abstract;

      {Prepares a 2D coordinate system }
      procedure Setup2DScene(AWidth,AHeight:integer);virtual;abstract;
      {Prepares a 3D coordinate system }
      procedure Setup3DScene(AWidth,AHeight:integer;APos,ADir,AUp:TAdVector3);virtual;abstract;
      {Prepares gives the possibility to setup the coordinate system manualy}
      procedure SetupManualScene(AMatView, AMatProj:TAdMatrix);virtual;abstract;
      {Returns the current view and projection matrix}
      procedure GetScene(out AMatView:TAdMatrix; out AMatProj:TAdMatrix);virtual;abstract;

      {Returns whether the given windowframework is supported.}
      function SupportsWindowFramework(AClassId:ShortString):boolean;virtual;abstract;

      {Returns the width of the active surface}
      property Width:integer read FWidth;
      {Returns the height of the active surface}
      property Height:integer read FHeight;
      {Returns the options set. You can also set new options.}
      property Options:TAdOptions read FOptions write SetOptions;
      {Returns the number of max lights}
      property MaxLights:integer read FMaxLightCount;
      {The rectangle where the output is made}
      property Viewport:TAdRect read FViewPort write SetViewPort;
  end;

  {An abstract class which represents a light in Andorra's engine. }
  TAd2DLight = class
    public
      {The position of the light}
      X,Y,Z:double;
      {The range, where a light shines}
      Range:double;
      {The color of the light}
      Color:TAndorraColor;
      {How the light falls off.}
      Falloff:double;
      {Writes all data into the engine.}
      procedure Restore;virtual;abstract;
      {Enables the light. Notice that all lights will be disabled in Ad2DApplications EndScene.}
      procedure Enable;virtual;abstract;     
      {Disables the light.}
      procedure Disable;virtual;abstract;
  end;

  {An class which represents a texture in Andorra's engine. }
  TAd2DTexture = class
    private
    protected
      FWidth:integer;
      FHeight:integer;
      FBitCount:byte;
      FEditable:boolean;
      FTexture:Pointer;
      FBaseWidth:integer;
      FBaseHeight:integer;
      function GetLoaded:boolean;virtual;abstract;
    public
      {The width of the texture in the memory. Is scaled to power of two.}
      property Width:integer read FWidth;
      {The height of the texture in the memory. Is scaled to power of two.}
      property Height:integer read FHeight;
      {Contains informations about the BitDepth of the texture. Can be 16 or 32.}
      property BitCount:byte read FBitCount;
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
  end;

  {An abstract class which represents a mesh (a set of vertices)  in Andorra's engine. }
  TAd2DMesh = class
    private
    protected
      FVertices:TAdVertexArray;
      FIndices:TAdIndexArray;
      FVertexCount:integer;
      FIndicesCount:integer;
      FPrimitiveCount:integer;
      FTexture:TAd2DTexture;
      function GetUseIndexBuffer:boolean;
      procedure SetVertices(AVertices:TAdVertexArray);virtual;abstract;
      procedure SetIndex(AIndex:TAdIndexArray);virtual;abstract;
      procedure SetTexture(ATexture:TAd2DTexture);virtual;
      function GetLoaded:boolean;virtual;abstract;
    public
      {Pushes the data into the graphic system's format.}
      procedure Update;virtual;abstract;
      {Draws the mesh.}
      procedure Draw(ABlendMode:TAd2DBlendMode;ADrawMode:TAd2DDrawMode);virtual;abstract;
      {Sets the transformation matrix.}
      procedure SetMatrix(AMatrix:TAdMatrix);virtual;abstract;
      {Returnes weather data is loaded.}
      property Loaded:boolean read GetLoaded;
      {The vertices a mesh has.}
      property Vertices:TAdVertexArray read FVertices write SetVertices;
      {The index buffer of a mesh.}
      property IndexBuffer:TAdIndexArray read FIndices write SetIndex;
      {Returnes whether the mesh uses an index buffer.}
      property UseIndexBuffer:boolean read GetUseIndexBuffer;
      {Returnes the count of vertices.}
      property VertexCount:integer read FVertexCount;
      {Returnes the count of indices.}
      property IndicesCount:integer read FIndicesCount;
      {Set the amount of primitives here.}
      property PrimitiveCount:integer read FPrimitiveCount write FPrimitiveCount;
      {Set the texture of the mesh here. Set to nil, if you want no texture.}
      property Texture:TAd2DTexture read FTexture write SetTexture;
  end;

  {Defines how the texture should be processed when loading from a TAd2dBitmap.}
  TAd2dBitmapTextureParameters = record
    BitDepth:byte;{< Can be 16 or 32.}
    UseMipMaps:boolean;{< Specifies wether MipMaps should be generated and used when loading the texture}
    MagFilter:TAd2dTextureFilter;{< Specifies the filter the texture should be drawn with when it is displayed in a large size.}
    MinFilter:TAd2dTextureFilter;{< Specifies the filter the texture should be drawn with when it is displayed in a tall size.}
    MipFilter:TAd2dTextureFilter;{< Specifies the filter which should be used to interpolate between the mipmaps.}
  end;

  {An abstract class which represents a bitmap texture in Andorra's engine. }
  TAd2DBitmapTexture = class(TAd2DTexture)
    private
    protected
    public
      {Frees the textures memory}
      procedure FlushTexture;virtual;abstract;
      {Loads the texture from a TAd2dBitmap.}
      procedure LoadFromBitmap(ABmp:TAd2dBitmap; AParams:TAd2dBitmapTextureParameters);virtual;abstract;
      {Saves the texture to a TAd2dBitmap.}
      procedure SaveToBitmap(ABmp:TAd2dBitmap);virtual;abstract;
    end;

  TAd2DRenderTargetTexture = class(TAd2dTexture)
    public
      procedure SetSize(AWidth, AHeight: integer; ABitCount: Byte);virtual;abstract;
      procedure FlushMemory;virtual;abstract;
      procedure SaveToBitmap(ABmp:TAd2dBitmap);virtual;abstract;
  end;


  //Used to import the CreateApplication function form the DLL.
  TAdCreateApplicationProc = function:TAd2DApplication;stdcall;

const
{$IFDEF FPC}
  LibraryVersion = 'VER 0.4.0 FPC';
{$ELSE}
  LibraryVersion = 'VER 0.4.0';
{$ENDIF}

implementation

{ TAdApplication }

procedure TAd2DApplication.SetLogProc(ALogProc: TAdLogProc);
begin
  TMethod(FLogProc).Code := TMethod(ALogProc).Code;
  TMethod(FLogProc).Data := TMethod(ALogProc).Data;
end;

procedure TAd2DApplication.SetOptions(AValue: TAdOptions);
begin
  FOptions := AValue;
end;

procedure TAd2DApplication.SetViewPort(AValue: TAdRect);
begin
  FViewPort := AValue;
end;

procedure TAd2DApplication.WriteLog(Typ: TAdLogTyp; Text: PChar);
var LogItem:TAdLogItem;
begin
  if @FLogProc <> nil then
  begin
    LogItem.Text := Text;
    LogItem.Typ := Typ;
    FLogProc(LogItem);
  end;
end;

{ TAdMesh }

function TAd2DMesh.GetUseIndexBuffer: boolean;
begin
  result := FIndices <> nil;
end;

procedure TAd2DMesh.SetTexture(ATexture:TAd2DTexture);
begin
  FTexture := ATexture;
end;

end.
