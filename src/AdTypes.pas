{
* This program is licensed under the Common Public License (CPL) Version 1.0
* You should have recieved a copy of the license with this file.
* If not, see http://www.opensource.org/licenses/cpl1.0.txt for more informations.
* You also should have recieved a copy of this license with this file.
* 
* Inspite of the incompatibility between the Common Public License (CPL) and the GNU General Public License (GPL) you're allowed to use this program 
* under the GPL. 
* If not, see http://www.gnu.org/licenses/gpl.txt for more informations.
*
* Project: Andorra 2D
* Author:  Andreas Stoeckel
* File: AdTypes.pas
* Comment: Contains some standard types
}

{Contains some standard types used everywhere in Andorra 2D.}
unit AdTypes;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$INCLUDE inc_andorra.inc}

interface

type
  {--- Bitmap structures ---}
  
  {Andorra 2Ds standard color type. Consists of four bytes for Red, Green, Blue and Alpha.}
  TAndorraColor = packed record
    r:byte;//<Red Value
    g:byte;//<Green Value
    b:byte;//<Blue Value
    a:byte;//<Alpha Value
  end;
  {Pointer type of TAndorraColor}
  PAndorraColor = ^TAndorraColor;
  {Another name for TAndorraColor. @seealso(TAndorraColor)}
  TRGBARec = TAndorraColor;
  {Pointer type of TRGBARec.}
  PRGBARec = ^TRGBARec;
  {Array used to acces a whole scanline pixel by pixel.}
  TRGBAArray = array[0..0] of TRGBARec;
  {Pointer type of TRGBAArray.}
  PRGBAArray = ^TRGBAArray;   

  {A standard color type without alpha channel. E.g. used for copying data from other bitmap sources into an TAdBitmap.}
  TRGBRec = packed record
    r:byte;//<Red Value
    g:byte;//Green Value
    b:byte;//Blue Value
  end;
  {Pointer type of TRGBRec.}
  PRGBRec = ^TRGBRec;
  {Array used to acces a whole scanline pixel by pixel.}
  TRGBArray = array[0..0] of TRGBRec;
  {Pointer type of TRGBArray.}
  PRGBArray = ^TRGBArray;

  {Used to specifies the bit depth of a bitmap. Because bitmaps in Andorra 2D 
   are allways RGBA, only 16 and 32 bits are supported. The ordinal values
   of TAdBitDepth are equivalent to their logic value.}
  TAdBitDepth = (
    ad16Bit = 16,  //< The bit depth is 16 Bit
    ad32Bit = 32 //< The bit depth is 32 Bit
  );

  {--- 3D-Data ---}

  {A vector type which has two components.}
  TAdVector2 = packed record
    {@exclude}
    x, y: single;
  end;
  PAdVector2 = ^TAdVector2;

  {A vector type which has three components.}
  TAdVector3 = packed record
    {@exclude}
    x, y, z: single;
  end;
  PAdVector3 = ^TAdVector3;

 {A vector type containing four components.}
  TAdVector4 = packed record
    {@exclude}
    x, y, z, w: single;
  end;
  PAdVector4 = ^TAdVector4;

  {A standard 3x3 matrix.}
  TAdMatrix = array[0..3] of array[0..3] of single;

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

  {--- Replacement of TPoint and TRect ---}
    
  {A replacement for TPoint.}
  TAdPoint = packed record
    X: Longint;//<X-Coordinate
    Y: Longint;//<Y-Coordinate
  end;
  {Pointer type of TAdPoint.}
  PAdPoint = ^TAdPoint;

  {A point which contains its coordinates as float values}
  TAdPointEx = packed record
    X: double;
    Y: double;
  end;
  {Pointer type of TAdPointEx}
  PAdPointEx = ^TAdPointEx;

  {A replacement for TRect.}
  TAdRect = packed record    
    case integer of
      0: (Left, Top, Right, Bottom: Longint);
      1: (TopLeft:TAdPoint; BottomRight: TAdPoint);
  end;
  {Pointer type of TAdRect.}
  PAdRect = ^TAdRect;

  {A TAdRect with float values.}
  TAdRectEx = packed record
    case integer of
      0: (Left, Top, Right, Bottom: Double);
      1: (TopLeft:TAdPoint; BottomRight: TAdPointEx);
  end;
  {Pointer type of TAdRectEx}
  PAdRectEx = ^TAdRectEx;

  { --- Types used in the font system ---}
  
  {Represents a single style a font may be rendered in.}
  TAdFontStyle = (
    afItalic,//< The font is rendered italic
    afBold,//< The font is rendererd bold
    afUnderline//< The font is rendered underlined
  );

  {Set of all three font styles.}
  TAdFontStyles = set of TAdFontStyle;

  {Array which contains the position of every char of a bitmap font in the texture.}
  TAdCharPatterns = array[0..255] of TAdRect;

  {Array which stores the size of a char.}
  TAdCharSizes = array[0..255] of TAdPoint;
  
  {Record used for type setting.}
  TAdTextSetChar = record
    Position:TAdRect;//< The position/size of the char
    TexCoords:TAdRect;//< The texutre coordinates of the char
  end;

  {Array returned by the type setter which returns the texture coordinates and the position/size of each char.}
  TAdTextSet = array of TAdTextSetChar;

  {--- Types used for storing data in streams ---}

  {A string with the length of five bytes.}
  TAdVeryShortString = string[4];

  {--- Triangle and polygon types ---}
  
  {A type which represents a polygon}
  TAdPolygon = array of TAdPoint;

  {A type which contains three points}
  TAdTriangle = array[0..2] of TAdPoint;
  {A type which represents a set of triangles}
  TAdTriangles = array of TAdTriangle;

  {--- Window properties ---}
  {Defines how the window is displayed.}
  TAdWindowDisplayMode = (
    dmDefault,{< The size of the parent control isn't changed. If the window framwork
      doesn't use a parent control (e.g. because it creates its own window),
      dmDefault is equivalent to dmWindowed.}
    dmWindowed,{< The window is displayed in the window mode.}
    dmScreenRes,{< The window is resized to the current screen resolution.}
    dmFullscreen{< The screen resolution will be changed and the window is displayed int the fullscreen mode.});

  {Defines the properties of the window}
  TAdDisplayProperties = record
    Width : integer;{< The width of the window.}
    Height : integer;{< The height of the window.}
    Mode : TAdWindowDisplayMode;{< The displaymode. @seealso(TAdWindowDisplayMode)}
    BitDepth : TAdBitDepth;{< The window bit depth. May be 16- or 32-Bit.}
  end;

  {---Types used in the shader system---}
  
  {Specifies the type of the shader sourcecode}
  TAd2dShaderSourceType = (
    assSource, //< You have passed the shader sourcecode to the system
    assCompiled //< The shader is already precompiled
  );

  {Specifies the type of the shader program}
  TAd2dShaderType = (
    astVertex, //< This program is a vertex program
    astFragment //< This program is a fragment (pixel) program
  );  


//Creates a rectangle of the type "TAdRect"  with specific x/y coordinates and a specific width/height
function AdBounds(X,Y,Width,Height:LongInt):TAdRect;
//Creates a rectangle of the type "TAdRectEx" with specific x/y coordinates and a specific width/height
function AdBoundsEx(X,Y,Width,Height:double):TAdRectEx;

{Moves an existing rectangle by X,Y.}
procedure AdOffsetRect(var Rect:TAdRect; X, Y:LongInt);

{Returns a TAdRect.}
function AdRect(X1,Y1,X2,Y2:LongInt):TAdRect;overload;{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
{Returns a TAdRect by rounding the coordinates given}
function AdRect(X1,Y1,X2,Y2:double):TAdRect;overload;{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}

{Returns a TAdRectEx}
function AdRectEx(X1,Y1,X2,Y2:double):TAdRectEx;{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}

{Returns a TAdPoint}
function AdPoint(X,Y:LongInt):TAdPoint;overload;{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
{Returns a TAdPoint by rounding the coordinates given}
function AdPoint(X,Y:double):TAdPoint;overload;{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}

{Returns true when the two rects have the same coordinates.}
function CompareRects(const Rect1,Rect2:TAdRect):boolean;
{Returns true, when the two rects overlap themselves.}
function OverlapRect(const Rect1, Rect2: TAdRect): boolean;
{Returns true, if the point lies within the rect}
function InRect(const X, Y: integer; const Rect: TAdRect): boolean;

{Returns a vector with three components.}
function AdVector3(AX,AY,AZ:double):TAdVector3;overload;
{Returns a vector with three components.}
function AdVector3(AVec: TAdVector2; AZ: double): TAdVector3;overload;
{Returns a vector with four components.}
function AdVector4(AX, AY, AZ, AW:double):TAdVector4;overload;
{Returns a vector with four components.}
function AdVector4(AVec: TAdVector3; AW: double): TAdVector4;overload;
{Returns a vector with two components.}
function AdVector2(AX,AY:double):TAdVector2;

{Creates an andorra color with the given components.}
function Ad_ARGB(a,r,g,b:byte):TAndorraColor;
{Creates an andorra color with the given components and alpha is set to 255}
function Ad_RGB(r,g,b:byte):TAndorraColor;
{Converts a color to a string}
function AdColorToString(AColor:TAndorraColor):string;
{Converts a string to a color}
function StringToAdColor(AString:string):TAndorraColor;
{Converts an Andorra color into a TColor.}
function AdColorToColor(AAdColor:TAndorraColor):Longint;
{Converts a TColor value into an andorra color and sets alpha to 255.}
function ColorToAdColor(AColor:LongInt):TAndorraColor;
{Returns a TColor with the given components.}
function RGB(r,g,b:byte):Longint;
{Extracts the Red-Value from a TColor value.}
function GetRValue(AColor:LongInt):byte;
{Extracts the Green-Value from a TColor value.}
function GetGValue(AColor:LongInt):byte;
{Extracts the Blue-Value from a TColor value.}
function GetBValue(AColor:LongInt):byte;
{Returns true if the given Andorra Colors are equal.}
function CompareColors(col1,col2:TAndorraColor):boolean;

{Returns true, when the two floats given lie within the area specified by epsilon.}
function FloatsEqual(v1, v2, e:double):boolean;overload;
{Returns true, when the two floats given lie within the area specified by epsilon.}
function FloatsEqual(v1, v2, e:single):boolean;overload;

{Converts an integer value into a byte value and prevents overflow.}
function Cut(AValue:integer):byte;


const
  {A matrix which only contains zero values}
  AdMatrix_Clear    : TAdMatrix = ((0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0));
  {A identity matrix.}
  AdMatrix_Identity : TAdMatrix = ((1,0,0,0),(0,1,0,0),(0,0,1,0),(0,0,0,1));

implementation

function AdPoint(X,Y:LongInt):TAdPoint;
begin
  result.X := X;
  result.Y := Y;
end;

function AdPoint(X,Y:Double):TAdPoint;
begin
  result.X := round(X);
  result.Y := round(Y);
end;

function AdBounds(X,Y,Width,Height:LongInt):TAdRect;
begin
  with result do
  begin
    Left := X;
    Top := Y;
    Right := X + Width;
    Bottom := Y + Height;
  end;
end;

procedure AdOffsetRect(var Rect:TAdRect; X, Y:LongInt);
begin
  with Rect do
  begin
    Left := Left + X;
    Right := Right + X;
    Top := Top + Y;
    Bottom := Bottom + Y;
  end;
end;

function AdRect(X1,Y1,X2,Y2:LongInt):TAdRect;
begin
  with result do
  begin
    Left := X1;
    Top := Y1;
    Right := X2;
    Bottom := Y2;
  end;
end;

function AdRect(X1,Y1,X2,Y2:double):TAdRect;
begin
  with result do
  begin
    Left := round(X1);
    Top := round(Y1);
    Right := round(X2);
    Bottom := round(Y2);
  end;
end;

function AdBoundsEx(X,Y,Width,Height:double):TAdRectEx;
begin
  with result do
  begin
    Left := X;
    Top := Y;
    Right := X + Width;
    Bottom := Y + Height;
  end;
end;

function AdRectEx(X1,Y1,X2,Y2:double):TAdRectEx;
begin
  with result do
  begin
    Left := X1;
    Top := Y1;
    Right := X2;
    Bottom := Y2;
  end;
end;

function OverlapRect(const Rect1, Rect2: TAdRect): Boolean;
begin
  Result:=
    (Rect1.Left < Rect2.Right) and
    (Rect1.Right > Rect2.Left) and
    (Rect1.Top < Rect2.Bottom) and
    (Rect1.Bottom > Rect2.Top);
end;

function CompareRects(const Rect1,Rect2:TAdRect):boolean;
begin
  result :=
    (Rect1.Left = Rect2.Left) and
    (Rect1.Right = Rect2.Right) and
    (Rect1.Top = Rect2.Top) and
    (Rect1.Bottom = Rect2.Bottom);
end;

function InRect(const X, Y: integer; const Rect:TAdRect):boolean;
begin
  result := (
     (Y >= Rect.Top)  and (Y <= Rect.Bottom) and
     (X >= Rect.Left) and (X <= Rect.Right));
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

function AdVector3(AVec: TAdVector2; AZ: double): TAdVector3;
begin
  PAdVector2(@result)^ := AVec;
  result.z := AZ;
end;

function AdVector4(AX, AY, AZ, AW:double):TAdVector4;
begin
  with result do
  begin
    x := AX;
    y := AY;
    z := AZ;
    w := AW;
  end;
end;

function AdVector4(AVec: TAdVector3; AW: double): TAdVector4;
begin
  PAdVector3(@result)^ := AVec;
  result.w := AW;
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

function GetRValue(AColor:LongInt):byte;
begin
  result := AColor and 255;
end;

function GetGValue(AColor:LongInt):byte;
begin
  result := (AColor shr 8) and 255;
end;

function GetBValue(AColor:LongInt):byte;
begin
  result := (AColor shr 16) and 255;
end;  

function ByteToHex(aval:Byte):ShortString;

  function ValueToChar(aval:Byte):char;
  begin
    result := '0';
    if (aval <= 9) then
      result := chr(aval + $30) else
    if (aval >= 10) and (aval <= 15) then
      result := chr(aval + $37);
  end;

begin
  SetLength(result, 2);
  result[1] := ValueToChar(aval div 16);
  result[2] := ValueToChar(aval mod 16);
end;

function CharToValue(achar:Char):byte;
begin
  result := 0;
  if achar in ['0'..'9'] then
    result := ord(achar) - 48 else
  if achar in ['A'..'F'] then
    result := ord(achar) - 55;
end;

function AdColorToString(AColor:TAndorraColor):string;
begin
  result :=
    ByteToHex(AColor.a) + ByteToHex(AColor.r) + ByteToHex(AColor.g) + ByteToHex(AColor.b);
end;

function StringToAdColor(AString:string):TAndorraColor;
begin
  if length(AString) <> 8 then exit;  
  result.a :=
    CharToValue(AString[1]) * 16 + CharToValue(AString[2]);
  result.r :=
    CharToValue(AString[3]) * 16 + CharToValue(AString[4]);
  result.g :=
    CharToValue(AString[5]) * 16 + CharToValue(AString[6]);
  result.b :=
    CharToValue(AString[7]) * 16 + CharToValue(AString[8]);
end;

function RGB(r,g,b:byte):Longint;
begin
  result := R + G shl 8 + B shl 16; 
end;

function AdColorToColor(AAdColor:TAndorraColor):Longint;
begin
  result := RGB(AAdColor.r, AAdColor.g, AAdColor.b);
end;

function ColorToAdColor(AColor:LongInt):TAndorraColor;
begin
  result := Ad_ARGB(255, AColor, AColor shr 8, AColor shr 16);
end;

function FloatsEqual(v1, v2, e:double):boolean;
begin
  result :=  abs(v1-v2) < e;
end;


function FloatsEqual(v1, v2, e:single):boolean;
begin
  result :=  abs(v1-v2) < e;
end;     

function Cut(AValue:integer):byte;
begin
  if AValue < 255 then
  begin
    if AValue < 0 then
      result := 0
    else
      result := AValue
  end
  else
    result := 255;
end;


end.

