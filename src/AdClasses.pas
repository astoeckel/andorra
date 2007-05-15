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

uses {$IFDEF FPC}intfgraphics, FPImage, LclType,{$ENDIF} SysUtils, Classes, {$INCLUDE AdTypes.inc},Graphics;

type
  {Represents an RGBA Color format with more than 8-Bit per channel. (But usually it is used as a 8-Bit format and the values are from 0 to 255. )}
  TAndorraColor = packed record
    {Contains the color informations. They are stored in an integer (not in a byte as usual), because light sources can be more intensive (and each channel can have a value bigger than 255)}
    r,g,b,a:integer;
  end;

  {A simple vector (used for texture formates)}
  TAdVector2 = packed record
    {Stores the vectors information.}
    x,y:single;
  end;

  {Another simple vector with 3 parameters}
  TAdVector3 = packed record
    {Stores the vectors information.}
    x,y,z:single;
  end;

  {Andorras vertex format}
  TAdVertex = record
    {The position of the vertex}
    Position:TAdVector3;
    {The color. If you change the alpha channels value to a value less than 255, the vertex will be transparent.}
    Color:TAndorraColor;
    {A normal vector stores information about how light is reflected}
    Normal:TAdVector3;
    {Contains the position of the texture. Normaly each value lies between 0 and 1. If a value is bigger/smaller the texture will be wrapped}
    Texture:TAdVector2;
  end;

  {An array of the vertex}
  TAdVertexArray = array of TAdVertex;

  {Represtents an index buffer}
  TAdIndexArray = array of Word;

  {A matrix}
  TAdMatrix = array[0..3] of array[0..3] of single;

  
  {Contains information about how the scene is displayed.}
  TAdOption = (
    doFullscreen, //< Specifies weather the application should run in the fullscreen mode or not
    doVSync, //< If turned on, the frame rate is equal to the vertical frequenzy of the screen
    doHardware,//< Run in hardware mode? (WARNING: Should be set!)
    doZBuffer, //< The ZBuffer has to be used if you are using 3D Objects in your scene
    doAntialias,//< should Antialiasing be used
    doLights//<Turn lights off/on.
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

  PByte = ^Byte;

  {A 32-Bit Bitmap}
  TAdBitmap = class
    private
      FMemory:PByte;
      FWidth:integer;
      FHeight:integer;
      FSize:int64;
      procedure ClearMemory;
    protected
    public
      {Creates an instance of TAdBitmap}
      constructor Create;
      {Destroys the instance of this TAdBitmap and clears all memory.}
      destructor Destroy;override;
      {Reserves memory for the bitmap. Old memory will be cleared. Use ReserveMemory(0,0) to clear the bitmap's memory.}
      procedure ReserveMemory(AWidth,AHeight:integer);
      {Assignes a bitmap. If ABitmap.Transparent is turned on, an alpha channel will be generated automaticly.}
      procedure AssignBitmap(ABitmap:TBitmap);
      {Assignes a bitmap as AlphaChannel.}
      procedure AssignAlphaChannel(ABitmap:TBitmap);
      {Copies the loaded bitmap into a TBitmap. If AIgnoreAlphaChannel is false, the bitmap will be drawed transparent.}
      procedure AssignToBitmap(ABitmap:TBitmap;AIgnoreAlphaChannel:boolean=true);
      {Copies the loaded bitmap's alphachannel into a TBitmap.}
      procedure AssignAlphaChannelToBitmap(ABitmap:TBitmap);
      {Saves the loaded bitmap into a stream}
      procedure SaveToStream(AStream:TStream);
      {Loads a bitmap from a stream}
      procedure LoadFromStream(AStream:TStream);
      {Saves the bitmap to a file}
      procedure SaveToFile(AFile:string);
      {Loads a bitmap from a file}
      procedure LoadFromFile(AFile:string);
      {Returns a pointer on the first pixel of a line.}
      function ScanLine(AY:integer):pointer;overload;
      {Returns a pointer on the first pixel in the bitmap.}
      function ScanLine:pointer;overload;
      {Returnes weather memory is reserved.}
      function Loaded:boolean;
      {Returns the width of the bitmap.}
      property Width:integer read FWidth;
      {Returns the height of the bitmap.}
      property Height:integer read FHeight;
      {Returns the size of the bitmap in bytes. (Width*Height*4)}
      property Size:int64 read FSize;
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
    bmMask//< The mesh apears black
  );

  {Specifies how the vertices are drawn}
  TAd2DDrawMode = (
    adPoints,//<The vertices are drawn as single points
    adLines,//<The vertices are drawn as lines
    adLineStrips,//<The vertices are drawn as a line strip
    adTriangles,//<The vertices are drawn as a list of triangles
    adTriangleStrips,//<The vertices are drawn as a triangle strip
    adTriangleFan//<The vertices are drawn as a triangle fan
  );

  TAd2DFilterMode = (
    fmMagFilter,//< Filter for textures, which are displayed bigger than in original size
    fmMinFilter,//< Filter for textures, which are displayed smaller than in original size
    fmMipFilter//< Filter for the translation between mipmaps
  );

  TAd2DTextureFilter = (
    atPoint,//<Worst picture quality, default filter
    atLinear,//<Good filter quality
    atAnisotropic//<Another good filter ^^
  );
  
  {An abstract class which represents a light in Andorra's engine. }
  TAd2DLight = class;
  {An class which represents a texture in Andorra's engine. }
  TAd2DTexture = class;  
  //TAdRenderTargetTexture = class;  
  {An abstract class which represents a bitmap texture in Andorra's engine. }
  TAd2DBitmapTexture = class;
  {An abstract class which represents a mesh (a set of vertices)  in Andorra's engine. }
  TAd2DMesh = class;

  {Abstract class which represents an Andorra 2D application.}
  TAd2DApplication = class
    private
      FLogProc:TAdLogProc;
      FAmbientLight: TAndorraColor;
    protected
      FOptions:TAdOptions;
      FWidth:integer;
      FHeight:integer;
      FMaxLightCount:integer;
      FAmbientColor:TAndorraColor;
      procedure SetOptions(AValue:TAdOptions);virtual;
      procedure WriteLog(Typ:TAdLogTyp;Text:PChar);
      procedure SetAmbientLight(AValue:TAndorraColor);virtual;
    public
      {Creates and returns a TAd2DLight}
      function CreateLight:TAd2DLight;virtual;abstract;
      {Creates and returns a TAd2DBitmapTexture}
      function CreateBitmapTexture:TAd2DBitmapTexture;virtual;abstract;
      //function CreateRenderTargetTexture:TAdRenderTargetTexture;virtual;abstract;
      {Creates and returns a TAd2DMesh}
      function CreateMesh:TAd2DMesh;virtual;abstract;

      //procedure SetRenderTarget(ATarget:TAdRenderTargetTexture);virtual;abstract;

      {Sets the procedure which will recive a new log entry.}
      procedure SetLogProc(ALogProc:TAdLogProc);

      {Initializes the engine. AWnd is the handle to the window.}
      function Initialize(AWnd:LongWord; AOptions:TAdOptions; ADisplay:TAdDisplay):boolean;virtual;abstract;
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

      {Set the texture filter for better picture quality}
      procedure SetTextureFilter(AFilterMode:TAd2DFilterMode;AFilter:TAd2DTextureFilter);virtual;abstract;

      {Returns the width of the engines surface}
      property Width:integer read FWidth;
      {Returns the height of the engines surface}
      property Height:integer read FHeight;
      {Returns the options set. You can also set new options.}
      property Options:TAdOptions read FOptions write SetOptions;
      {Returns the number of max lights}
      property MaxLights:integer read FMaxLightCount;
      {Read and write the AmbientLightColor. This will only work, if doLights is included in "options".}
      property AmbientLightColor:TAndorraColor read FAmbientLight write SetAmbientLight;
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

  {An abstract class which represents a bitmap texture in Andorra's engine. }
  TAd2DBitmapTexture = class(TAd2DTexture)
    private
    protected
    public
      {Set the texture of the mesh here. Set to nil, if you want no texture.}
      procedure FlushTexture;virtual;abstract;
      {Loads the texture from a TAdBitmap.}
      procedure LoadFromBitmap(ABmp:TAdBitmap;ABitDepth:byte=32);virtual;abstract;
      {Saves the texture to a TAdBitmap.}
      procedure SaveToBitmap(ABmp:TAdBitmap);virtual;abstract;
    end;


  //Used to import the CreateApplication function form the DLL.
  TAdCreateApplicationProc = function:TAd2DApplication;stdcall;

  //A record which contains one pixel of the data stored in a normal 24Bit TBitmap.
  TRGBRec = packed record
    //The colour data
    r,g,b:byte;
  end;
  //Pointer on TRGBRec.
  PRGBRec = ^TRGBRec;

  //A record which contains one pixel of the data stored in a TAdBitmap
  TRGBARec = packed record
    r,g,b,a:byte;
  end;
  //Pointer on TRGBARec
  PRGBARec = ^TRGBARec;

{Returns a TAndorraColor value with alphachannel.}
function Ad_ARGB(a,r,g,b:byte):TAndorraColor;
{Returns a TAndorraColor value without alphachannel. (To be percise, the alphachannel is 255.)}
function Ad_RGB(r,g,b:byte):TAndorraColor;
{Converts an AndorraColor into a string.}
function AdColorToString(AColor:TAndorraColor):string;
{Converts a string into an andorra color.}
function StringToAdColor(AString:string):TAndorraColor;    
{Converts a TAndorraColor into a TColor value.}
function AdColorToColor(AAdColor:TAndorraColor):LongWord;
{Converts a TColor into a TAndorraColor value}
function ColorToAdColor(AColor:LongWord):TAndorraColor;

{Returns the "R" segment of a TColor}
function GetRValue(AColor:LongWord):byte;
{Returns the "G" segment of a TColor}
function GetGValue(AColor:LongWord):byte;
{Returns the "B" segment of a TColor}
function GetBValue(AColor:LongWord):byte;

{Converts a R, a G and a B value into a TColor value.}
function RGB(r,g,b:byte):LongWord;

{Compares two AndorraColors.}
function CompareColors(col1,col2:TAndorraColor):boolean;

{Converts an integer into a byte. If the value was smaller than zero the funktion returns 0, if it was bigger than 255 it returns 255.}
function Cut(AValue:integer):byte;

{Creates an Andora Vector.}
function AdVector3(AX,AY,AZ:double):TAdVector3;
{Creates an Andora Vector.}
function AdVector2(AX,AY:double):TAdVector2;

{Multiplies two matrices.}
function AdMatrix_Multiply(amat1,amat2:TAdMatrix):TAdMatrix;
{Creates a translation matrix.}
function AdMatrix_Translate(tx,ty,tz:single):TAdMatrix;
{Creates a scalation matrix.}
function AdMatrix_Scale(sx,sy,sz:single):TAdMatrix;
{Creates a rotation matrix.}
function AdMatrix_RotationX(angle:single):TAdMatrix;
{Creates a rotation matrix.}
function AdMatrix_RotationY(angle:single):TAdMatrix;
{Creates a rotation matrix.}
function AdMatrix_RotationZ(angle:single):TAdMatrix;
{Creates a identity matrix.}
function AdMatrix_Identity:TAdMatrix;
{Creates a clear matrix. (With zeros everywhere)}
function AdMatrix_Clear:TAdMatrix;

{Compares two rectangles.}
function CompRects(Rect1,Rect2:TRect):boolean;

implementation

function CompRects(Rect1,Rect2:TRect):boolean;
begin
  result := (Rect1.Left = Rect2.Left) and
            (Rect1.Right = Rect2.Right) and
            (Rect1.Top = Rect2.Top) and
            (Rect1.Bottom = Rect2.Bottom);
end;


function AdVector3(AX,AY,AZ:double):TAdVector3;
begin
  with result do
  begin
    x := ax;
    y := ay;
    z := az;
  end;
end;

function AdVector2(AX,AY:double):TAdVector2;
begin
  with result do
  begin
    x := ax;
    y := ay;
  end;
end;

function Ad_ARGB(a,r,g,b:byte):TAndorraColor;
begin
  result.a := a;
  result.r := r;
  result.g := g;
  result.b := b;
end;

function Ad_RGB(r,g,b:byte):TAndorraColor;
begin
  result := Ad_ARGB(255,r,g,b);
end;

function CompareColors(col1,col2:TAndorraColor):boolean;
begin
  result := (col1.a = col2.a) and
            (col1.r = col2.r) and
            (col1.g = col2.g) and
            (col1.b = col2.b);
end;

function GetRValue(AColor:LongWord):byte;
begin
  result := AColor and 255;
end;

function GetGValue(AColor:LongWord):byte;
begin
  result := (AColor shr 8) and 255;
end;

function GetBValue(AColor:LongWord):byte;
begin
  result := (AColor shr 16) and 255;
end;

function AdColorToString(AColor:TAndorraColor):string;
begin
  result := FormatFloat('000',AColor.a)+FormatFloat('000',AColor.r)+
            FormatFloat('000',AColor.g)+FormatFloat('000',AColor.b);
end;

function StringToAdColor(AString:string):TAndorraColor;
begin
  result.a  := StrToInt(Copy(AString,1,3));
  result.r  := StrToInt(Copy(AString,4,3));
  result.g  := StrToInt(Copy(AString,7,3));
  result.b  := StrToInt(Copy(AString,10,3));
end;

function RGB(r,g,b:byte):LongWord;
begin
  result := R + G shl 8 + B shl 16; 
end;

function Cut(AValue:integer):byte;
begin
  if AValue < 255 then
  begin
    if AValue < 0 then
    begin
      result := 0;
    end
    else
    begin
      result := AValue;
    end;
  end
  else
  begin
    result := 255;
  end;
end;

function AdColorToColor(AAdColor:TAndorraColor):LongWord;
begin
  result := RGB(AAdColor.r,AAdColor.g,AAdColor.b);
end;

function ColorToAdColor(AColor:LongWord):TAndorraColor;
begin
  result := Ad_RGB(AColor,AColor shr 8,AColor shr 16);
end;

//Matrix functions
function AdMatrix_Multiply(amat1,amat2:TAdMatrix):TAdMatrix;
var x,y:integer;
begin
  for x := 0 to 3 do
  begin
    for y := 0 to 3 do
    begin
      result[x,y] := amat2[0,y]*amat1[x,0] + amat2[1,y]*amat1[x,1] + amat2[2,y]*amat1[x,2] +amat2[3,y]*amat1[x,3];
    end;
  end;
end;

function AdMatrix_Clear:TAdMatrix;
var x,y:integer;
begin
  for x := 0 to 3 do
  begin
    for y := 0 to 3 do
    begin
      result[x,y] := 0;
    end;
  end;
end;

function AdMatrix_Identity:TAdMatrix;
begin
  result := AdMatrix_Clear;
  result[0,0] := 1;
  result[1,1] := 1;
  result[2,2] := 1;
  result[3,3] := 1;
end;

function AdMatrix_Translate(tx,ty,tz:single):TAdMatrix;
begin
  result := AdMatrix_Identity;
  result[3,0] := tx;
  result[3,1] := ty;
  result[3,2] := tz;
end;

function AdMatrix_Scale(sx,sy,sz:single):TAdMatrix;
begin
  result := AdMatrix_Clear;
  result[0,0] := sx;
  result[1,1] := sy;
  result[2,2] := sz;
  result[3,3] := 1;
end;

function AdMatrix_RotationX(angle:single):TAdMatrix;
begin
  result := AdMatrix_Clear;
  result[0,0] := 1;
  result[1,1] := cos(angle);
  result[1,2] := sin(angle);
  result[2,1] := -sin(angle);
  result[2,2] := cos(angle);
  result[3,3] := 1;
end;

function AdMatrix_RotationY(angle:single):TAdMatrix;
begin
  result := AdMatrix_Clear;
  result[0,0] := cos(angle);
  result[0,2] := -sin(angle);
  result[1,1] := 1;
  result[2,0] := sin(angle);
  result[2,2] := cos(angle);
  result[3,3] := 1;
end;

function AdMatrix_RotationZ(angle:single):TAdMatrix;
begin
  result := AdMatrix_Clear;
  result[0,0] := cos(angle);
  result[0,1] := sin(angle);
  result[1,0] := -sin(angle);
  result[1,1] := cos(angle);
  result[2,2] := 1;
  result[3,3] := 1;
end;

{ TAdBitmap }

constructor TAdBitmap.Create;
begin
  inherited Create;
  FWidth := 0;
  FHeight := 0;
  FSize := 0;
  FMemory := nil;
end;

destructor TAdBitmap.Destroy;
begin
  ClearMemory;
  inherited;
end;

procedure TAdBitmap.AssignAlphaChannel(ABitmap: TBitmap);
var sl2:PRGBARec;
    x,y:integer;
    {$IFDEF FPC}
      IntfBmp:TLazIntfImage;
      acol:TFPColor;
    {$ELSE}
      sl1:PRGBRec;
    {$ENDIF}
begin
  {$IFDEF FPC}
    if Loaded then
    begin
      IntfBmp := TLazIntfImage.Create(0,0);
      IntfBmp.LoadFromBitmap(ABitmap.Handle,ABitmap.MaskHandle);
      sl2 := Scanline;
      for x := 0 to FHeight - 1 do
      begin
        for y := 0 to FWidth - 1 do
        begin
          acol := IntfBmp.Colors[x,y];
          sl2^.a := (acol.red +
                     acol.green +
                     acol.blue) div 3;
          inc(sl2);
        end;
      end;
      IntfBmp.Free;
    end;
  {$ELSE}
    if Loaded then
    begin
      ABitmap.PixelFormat := pf24Bit;
      sl2 := Scanline;
      for y := 0 to FHeight - 1 do
      begin
        sl1 := ABitmap.ScanLine[y];
        for x := 0 to FWidth - 1 do
        begin
          sl2^.a := (sl1^.r+sl1^.g+sl1^.b) div 3;
          inc(sl1);
          inc(sl2);
        end;
      end;
    end;
  {$ENDIF}
end;

procedure TAdBitmap.AssignAlphaChannelToBitmap(ABitmap: TBitmap);
var
  sl2:PRGBARec;
  x,y:integer;
  {$IFDEF FPC}
    IntfBmp:TLazIntfImage;
    acol:TFPColor;
    h1,h2:HBitmap;
  {$ELSE}
    sl1:PRGBRec;
  {$ENDIF}
begin
  {$IFDEF FPC}
    if Loaded then
    begin
      IntfBmp := TLazIntfImage.Create(FWidth,FHeight);
      sl2 := Scanline;
      for y := 0 to FHeight - 1 do
      begin
        for x := 0 to FWidth - 1 do
        begin
          acol.blue := sl2^.a;
          acol.red := sl2^.a;
          acol.green := sl2^.a;
          IntfBmp.Colors[x,y] := acol;
          inc(sl2);
        end;
      end;
      ABitmap.FreeImage;
      IntfBmp.CreateBitmap(h1,h2,false);
      ABitmap.Handle := h1;
      ABitmap.MaskHandle := h2;
      IntfBmp.Free;
    end;
  {$ELSE}
    if Loaded then
    begin
      ABitmap.PixelFormat := pf24Bit;
      ABitmap.Width := FWidth;
      ABitmap.Height := FHeight;
      sl2 := Scanline;
      for y := 0 to FHeight - 1 do
      begin
        sl1 := ABitmap.Scanline[y];
        for x := 0 to FWidth - 1 do
        begin
          sl1^.r := sl2^.a;
          sl1^.g := sl2^.a;
          sl1^.b := sl2^.a;
          inc(sl2);
          inc(sl1);
        end;
      end;
    end;
  {$ENDIF}
end;

procedure TAdBitmap.AssignBitmap(ABitmap: TBitmap);
var
  sl2:PRGBARec;
  x,y:integer;
  a:byte;
  tr,tg,tb:byte;
  {$IFDEF FPC}
    IntfBmp:TLazIntfImage;
    acol:TFPColor;
  {$ELSE}
    sl1:PRGBRec;
  {$ENDIF}
begin
  ReserveMemory(ABitmap.Width,ABitmap.Height);
  
  {$IFDEF FPC}
    IntfBmp := TLazIntfImage.Create(0,0);
    IntfBmp.LoadFromBitmap(ABitmap.Handle,ABitmap.MaskHandle);
  {$ELSE}
    ABitmap.PixelFormat := pf24Bit;
  {$ENDIF}

  sl2 := Scanline;
  tr := GetRValue(ABitmap.TransparentColor);
  tg := GetGValue(ABitmap.TransparentColor);
  tb := GetBValue(ABitmap.TransparentColor);
  for y := 0 to FHeight - 1 do
  begin
    {$IFDEF FPC}{$ELSE}
      sl1 := ABitmap.ScanLine[y];
    {$ENDIF}
    for x := 0 to FWidth - 1 do
    begin
      {$IFDEF FPC}
        acol := IntfBmp.Colors[x,y];
        //The lazarus bitmap seems to be BGR and not RGB
        sl2^.r := acol.blue;
        sl2^.g := acol.green;
        sl2^.b := acol.red;
      {$ELSE}
        sl2^.r := sl1^.r;
        sl2^.g := sl1^.g;
        sl2^.b := sl1^.b;
        inc(sl1);
      {$ENDIF}
      if (ABitmap.Transparent) and (sl2^.b = tr) and (sl2^.g = tg) and (sl2^.r = tb) then
      begin
        a := 0;
      end
      else
      begin
        a := 255;
      end;
      sl2^.a := a;
      inc(sl2);
    end;
  end;
  {$IFDEF FPC}
    IntfBmp.Free;
  {$ENDIF}
end;

procedure TAdBitmap.AssignToBitmap(ABitmap: TBitmap;AIgnoreAlphaChannel:boolean=true);
var
  sl2:PRGBARec;
  x,y:integer;
  a:single;
  {$IFDEF FPC}
    IntfBmp:TLazIntfImage;
    acol:TFPColor;
  {$ELSE}
    sl1:PRGBRec;
  {$ENDIF}
begin
  if Loaded then
  begin
     ABitmap.PixelFormat := pf24Bit;
     ABitmap.Width := FWidth;
     ABitmap.Height := FHeight;
    {$IFDEF FPC}
      IntfBmp := TLazIntfImage.Create(0,0);
      IntfBmp.LoadFromBitmap(ABitmap.Handle,ABitmap.MaskHandle);
    {$ENDIF}
    sl2 := Scanline;
    for y := 0 to FHeight - 1 do
    begin
      {$IFDEF FPC}{$ELSE}
        sl1 := ABitmap.Scanline[y];
      {$ENDIF}
      for x := 0 to FWidth - 1 do
      begin
        if AIgnoreAlphaChannel then
        begin
          {$IFDEF FPC}
            with acol do
            begin
              //The lazarus bitmap seems to be BGR and not RGB
              blue := sl2^.r;
              green := sl2^.g;
              red := sl2^.b;
            end;
            IntfBmp.Colors[x,y] := acol;
          {$ELSE}
            sl1^.r := sl2^.r;
            sl1^.g := sl2^.g;
            sl1^.b := sl2^.b;
          {$ENDIF}
        end
        else
        begin
          a := (sl2^.a / 255);
          {$IFDEF FPC}
            acol := IntfBmp.Colors[x,y];
            with acol do
            begin
              red   := round((acol.red*(1-a)) + (sl2^.r*a));
              green := round((acol.green*(1-a)) + (sl2^.g*a));
              blue  := round((acol.blue*(1-a)) + (sl2^.b*a));
            end;
          {$ELSE}
            sl1^.r := round((sl1^.r*(1-a)) + (sl2^.r*a));
            sl1^.g := round((sl1^.g*(1-a)) + (sl2^.g*a));
            sl1^.b := round((sl1^.b*(1-a)) + (sl2^.b*a));
          {$ENDIF}
        end;
        inc(sl2);
        {$IFDEF FPC}
        {$ELSE}
          inc(sl1);
        {$ENDIF}
      end;
    end;
  end;
end;

procedure TAdBitmap.SaveToStream(AStream: TStream);
var ASize:int64;
begin
  ASize := FSize;
  AStream.Write(ASize,SizeOf(ASize));
  AStream.Write(FWidth,SizeOf(FWidth));
  AStream.Write(FHeight,SizeOf(FHeight));
  AStream.Write(FMemory^,ASize)
end;

procedure TAdBitmap.LoadFromStream(AStream: TStream);
var ASize:int64;
begin
  ClearMemory;
  AStream.Read(ASize,SizeOf(ASize));
  AStream.Read(FWidth,SizeOf(FWidth));
  AStream.Read(FHeight,SizeOf(FHeight));
  ReserveMemory(FWidth,FHeight);
  AStream.Read(FMemory^,ASize)
end;

procedure TAdBitmap.LoadFromFile(AFile: string);
var
  ms:TMemoryStream;
begin
  ms := TMemoryStream.Create;
  ms.LoadFromFile(AFile);
  ms.Position := 0;
  LoadFromStream(ms);
  ms.Free;
end;

procedure TAdBitmap.SaveToFile(AFile: string);
var
  ms:TMemoryStream;
begin
  ms := TMemoryStream.Create;
  SaveToStream(ms);
  ms.SaveToFile(AFile);
  ms.Free;
end;

function TAdBitmap.Loaded: boolean;
begin
  result := (FMemory <> nil);
end;

procedure TAdBitmap.ReserveMemory(AWidth, AHeight: integer);
begin
  ClearMemory;
  FSize := AWidth*AHeight*4;
  FWidth := AWidth;
  FHeight := AHeight;
  GetMem(FMemory,FSize);
end;

procedure TAdBitmap.ClearMemory;
begin
  if Loaded then
  begin
    FreeMem(FMemory,FSize);
    FMemory := nil;
  end;
end;

function TAdBitmap.ScanLine: Pointer;
begin
  if Loaded then
  begin
    result := FMemory;
  end
  else
  begin
    result := nil;
  end;
end;

function TAdBitmap.ScanLine(AY: integer): Pointer;
var ptr:pByte;
begin
  if ay < Height then
  begin
    ptr := Scanline;
    inc(ptr,AY*4*FWidth);
    result := ptr;
  end
  else
  begin
    result := Scanline;
  end;
end;

{ TAdApplication }

procedure TAd2DApplication.SetAmbientLight(AValue: TAndorraColor);
begin
  FAmbientColor := AValue;
end;

procedure TAd2DApplication.SetLogProc(ALogProc: TAdLogProc);
begin
  TMethod(FLogProc).Code := TMethod(ALogProc).Code;
  TMethod(FLogProc).Data := TMethod(ALogProc).Data;
end;

procedure TAd2DApplication.SetOptions(AValue: TAdOptions);
begin
  FOptions := AValue;
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
