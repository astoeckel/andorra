{
* This program is licensed under the Common Public License (CPL) Version 1.0
* You should have recieved a copy of the license with this file.
* If not, see http://www.opensource.org/licenses/cpl1.0.txt for more informations.
* 
* Inspite of the incompatibility between the Common Public License (CPL) and the GNU General Public License (GPL) you're allowed to use this program * under the GPL. 
* You also should have recieved a copy of this license with this file. 
* If not, see http://www.gnu.org/licenses/gpl.txt for more informations.
*
* Project: Andorra 2D
* Author:  Andreas Stoeckel
* File: AdTypes.pas
* Comment: Contains some standard types
}

unit AdTypes;

interface

type
  { Bitmap structures }
  TAndorraColor = packed record
    r,g,b,a:byte;
  end;
  PAndorraColor = ^TAndorraColor;
  TRGBARec = TAndorraColor;
  PRGBARec = ^TRGBARec;
  TRGBAArray = array[0..0] of TRGBARec;
  PRGBAArray = ^TRGBAArray;   

  TRGBRec = packed record
    r,g,b:byte;
  end;
  PRGBRec = ^TRGBRec;
  TRGBArray = array[0..0] of TRGBRec;
  PRGBArray = ^TRGBArray;

  { 3D-Data }

  TAdVector2 = packed record
    x,y:single;
  end;

  TAdVector3 = packed record
    x,y,z:single;
  end;

  TAdMatrix = array[0..3] of array[0..3] of single;

  { Replacement of TPoint and TRect }
  TAdPoint = packed record
    X: Longint;
    Y: Longint;
  end;
  PAdPoint = ^TAdPoint;

  TAdPointEx = packed record
    X: double;
    Y: double;
  end;
  PAdPointEx = ^TAdPointEx;

  TAdRect = packed record
    case integer of
      0: (Left, Top, Right, Bottom: Longint);
      1: (TopLeft:TAdPoint; BottomRight: TAdPoint);
  end;
  PAdRect = ^TAdRect;

  TAdRectEx = packed record
    case integer of
      0: (Left, Top, Right, Bottom: Double);
      1: (TopLeft:TAdPoint; BottomRight: TAdPointEx);
  end;
  PAdRectEx = ^TAdRectEx;

  { Types used in the font system }
  TAdFontStyle = (afItalic, afBold, afUnderline);

  TAdFontStyles = set of TAdFontStyle;

  TAdCharPatterns = array[0..255] of TAdRect;

  TAdCharSizes = array[0..255] of TAdPoint;

  TAdTextSetChar = record
    Position:TAdRect;
    TexCoords:TAdRect;
  end;

  TAdTextSet = array of TAdTextSetChar;

  { Types used for storing data in streams }

  TAdVeryShortString = string[4];

  { Triangle and polygon types }
  TAdPolygon = array of TAdPoint;

  TAdTriangle = array[0..2] of TAdPoint;
  TAdTriangles = array of TAdTriangle;

  { Misc types}

//  PByte = ^Byte;


function AdBounds(X,Y,Width,Height:LongInt):TAdRect;
procedure AdOffsetRect(var Rect:TAdRect; X, Y:LongInt);
function AdRect(X1,Y1,X2,Y2:LongInt):TAdRect;overload;
function AdRect(X1,Y1,X2,Y2:double):TAdRect;overload;
function AdBoundsEx(X,Y,Width,Height:double):TAdRectEx;
function AdRectEx(X1,Y1,X2,Y2:double):TAdRectEx;

function AdPoint(X,Y:LongInt):TAdPoint;overload;
function AdPoint(X,Y:Single):TAdPoint;overload;
function AdPoint(X,Y:Double):TAdPoint;overload;

function CompareRects(Rect1,Rect2:TAdRect):boolean;
function OverlapRect(const Rect1, Rect2: TAdRect): boolean;


function AdVector3(AX,AY,AZ:double):TAdVector3;

function AdVector2(AX,AY:double):TAdVector2;

function AdMatrix_Multiply(amat1,amat2:TAdMatrix):TAdMatrix;
function AdMatrix_Translate(tx,ty,tz:single):TAdMatrix;
function AdMatrix_Scale(sx,sy,sz:single):TAdMatrix;
function AdMatrix_RotationX(angle:single):TAdMatrix;
function AdMatrix_RotationY(angle:single):TAdMatrix;
function AdMatrix_RotationZ(angle:single):TAdMatrix;


function Ad_ARGB(a,r,g,b:byte):TAndorraColor;

function Ad_RGB(r,g,b:byte):TAndorraColor;

function AdColorToString(AColor:TAndorraColor):string;
function StringToAdColor(AString:string):TAndorraColor;
function AdColorToColor(AAdColor:TAndorraColor):Longint;
function ColorToAdColor(AColor:LongInt):TAndorraColor;
function RGB(r,g,b:byte):Longint;
function GetRValue(AColor:LongInt):byte;
function GetGValue(AColor:LongInt):byte;
function GetBValue(AColor:LongInt):byte;
function CompareColors(col1,col2:TAndorraColor):boolean;

function FloatsEqual(v1, v2, e:double):boolean;overload;
function FloatsEqual(v1, v2, e:single):boolean;overload;

function Cut(AValue:integer):byte;


const
  AdMatrix_Clear    : TAdMatrix = ((0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0));
  AdMatrix_Identity : TAdMatrix = ((1,0,0,0),(0,1,0,0),(0,0,1,0),(0,0,0,1));

implementation

function AdPoint(X,Y:LongInt):TAdPoint;
begin
  result.X := X;
  result.Y := Y;
end;

function AdPoint(X,Y:Single):TAdPoint;
begin
  result.X := round(X);
  result.Y := round(Y);
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
  Result:=(Rect1.Left<Rect2.Right)and
    (Rect1.Right>Rect2.Left)and
    (Rect1.Top<Rect2.Bottom)and
    (Rect1.Bottom>Rect2.Top);
end;

function CompareRects(Rect1,Rect2:TAdRect):boolean;
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
  result := RGB(AAdColor.r,AAdColor.g,AAdColor.b);
end;

function ColorToAdColor(AColor:LongInt):TAndorraColor;
begin
  result := Ad_RGB(AColor,AColor shr 8,AColor shr 16);
end;

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


end.

