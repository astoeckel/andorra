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
* File: AdCanvas.pas
* Comment: This unit contains the Andorra 2D canvas environment
}

{This unit contains classes and types which are used for a hardware accelerated canvas environment}
unit AdCanvas;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  SysUtils, Classes, Math,
  AdEvents, AdClasses, AdTypes, AdContainers, AdList, AdFont, AdPolygonUtils,
  AdMath, AdSpline;

type
  {A set of three colors used to define the colors of a quad-object.}
  TAdColors = array[0..3] of TAndorraColor;

  {Defines the style of the brush.}
  TAdBrushStyle = (
    abClear {<The filling isn't drawn}, 
    abSolid{<The filling is filled with one solid color}, 
    abGradient{<The filling is drawn using a gradient defined by
      "GradientDirection". @seealso(TAdCanvasGradientDirection)});
    
  {Defines the style of the pen.}
  TAdPenStyle = (
    apNone{<The outer line of the object won't be drawn.}, 
    apSolid{<The outer line is drawn and filled with a solid color.});
  
  {Defines the position of the pen.}
  TAdPenPosition = (
    ppOuter,{<The pen is possitionated on the outer bound of the object}
    ppMiddle,{<The pen is centered on the outer bound of the object}
    ppInner{<The pen is possitionated  on the inner bound of the object});
  
  {Defines how the texture is drawn.}
  TAdCanvasTextureMode = (
    tmTile, {<The texture is tiled in the object.}
    tmStretch,{<The texture is stretched to the size of the object.} 
    tmStretchAlign{<The texture is streched to the size of the object and
      aligned to the outer bounds.});
  
  {Defines the relative position of the texture.}
  TAdCanvasTexturePosition = (
    tpStatic, {<The position of the texture is relative to the position of the object}
    tpDynamic{The position of the texture is relative to the position of the
      coordinate system orgin});
    
  {Defines the direction of the gradient. @seealso(TAdBrushStyle)}
  TAdCanvasGradientDirection = (
    gdVertical,{<The gradient is drawn from left to right} 
    gdHorizontal{<The gradient is drawn from top to bottom});

  {Class which defines the look of the brush of the canvas. The brush is used to
   define the filling of a object.}
  TAdBrush = class
    private
      FColors:TAdColors;
      FColor:TAndorraColor;
      FGradientColor:TAndorraColor;
      FGradientDirection:TAdCanvasGradientDirection;
      FTextureMode:TAdCanvasTextureMode;
      FTexturePosition:TAdCanvasTexturePosition;
      FStyle:TAdBrushStyle;
      FTexture:TAd2dTexture;
      FBlendMode:TAd2dBlendMode;
      procedure SetColor(AValue:TAndorraColor);
      procedure SetGradientColor(AValue:TAndorraColor);
      procedure SetStyle(AValue:TAdBrushStyle);
      procedure SetTexture(AValue:TAd2dTexture);
      procedure SetGradientDirection(AValue:TAdCanvasGradientDirection);
      procedure UpdateColors;
    public
      {Defines the first color of the object. This color is used for the solid
       and the gradient mode. Changing this color will cause TAdBrush to set the
       brush style to "abSolid".
       @seealso(TAndorraColor)}
      property Color:TAndorraColor read FColor write SetColor;
      {Defines the second color of the object. This color is used for the
       gradient fill mode. Changing this color will cause TAdBrush to set the
       brush style to "abGradient".
       @seealso(TAndorraColor)}
      property GradientColor:TAndorraColor read FGradientColor write SetGradientColor;
      {Defines the direction of the gradient. Changing this mode will cause
       TAdBrush in set the brush style to "abGradient".}
      property GradientDirecton:TAdCanvasGradientDirection read FGradientDirection write SetGradientDirection;
      {Defines the style of the brush.
      @seealso(TAdBrushStyle)}
      property Style:TAdBrushStyle read FStyle write SetStyle;
      {Set a texture for filling the brush. You can recive a TAd2dTexture via
       TAdImage.Texture.Texture. Please notice, that the textures have to have
       a power of two size.}
      property Texture:TAd2dTexture read FTexture write SetTexture;
      {Sets the texture mode.
       @seealso(TAdCanvasTextureMode)}
      property TextureMode:TAdCanvasTextureMode read FTextureMode write FTextureMode;
      {Sets the position of the texture.
       @seealso(TAdCanvasTexturePosition)}
      property TexturePosition:TAdCanvasTexturePosition read FTexturePosition write FTexturePosition;
      {Sets the mode the filling of the object is drawn in.}
      property BlendMode:TAd2dBlendMode read FBlendMode write FBlendMode;

      {Creates an instance of TAdBrush.}
      constructor Create;
      {Saves the settings of TAdBrush to a stream.}
      procedure SaveToStream(AStream:TStream);
      {Loads the settings of TAdBrush from a stream.}
      procedure LoadFromStream(AStream:TStream);
      {Assigns the settings to another brush using Load/SaveToStream and TMemoryStream.}
      procedure Assign(ABrush:TAdBrush);
      {Test if this brush is equal to another brush.}
      function EqualTo(ABrush:TAdBrush):Boolean;
  end;

  {A class which defines the settings of the outer line of a canvas object.}
  TAdPen = class
    private
      FColor:TAndorraColor;
      FWidth:single;
      FStyle:TAdPenStyle;
      FDashLength:integer;
      FGapLength:integer;
      FTexture:TAd2dTexture;
      FTextureMode:TAdCanvasTextureMode;
      FTexturePosition:TAdCanvasTexturePosition;
      FPenPosition:TAdPenPosition;
      FBlendMode:TAd2dBlendMode;
      procedure SetColor(AValue:TAndorraColor);
      procedure SetWidth(AValue:single);
      procedure SetTexture(AValue:TAd2dTexture);
      procedure SetStyle(AValue:TAdPenStyle);
    public
      {Defines the color of the line.}
      property Color:TAndorraColor read FColor write SetColor;
      {Sets the width of the line. Accepts a equal or greater one.}
      property Width:single read FWidth write SetWidth;
      {Defines a texture the line is filled with. The texture is only used when
       width is greater one. Please notice, that the textures have to have a
       power of two size our you'll receive gaps between the texture
       boundaries.} 
      property Texture:TAd2dTexture read FTexture write SetTexture;
      {Sets the texture mode.
       @seealso(TAdCanvasTextureMode)}
      property TextureMode:TAdCanvasTextureMode read FTextureMode write FTextureMode;
      {Sets the position of the texture.
       @seealso(TAdCanvasTexturePosition)}
      property TexturePosition:TAdCanvasTexturePosition read FTexturePosition write FTexturePosition;
      {Sets the position of the pen.
       @seealso(TAdPenPosition)}
      property PenPosition:TAdPenPosition read FPenPosition write FPenPosition;
      {Sets the style of the pen.
       @seealso(TAdPenStyle)}
      property Style:TAdPenStyle read FStyle write SetStyle;
      {Sets the mode the line is blended in.}
      property BlendMode:TAd2dBlendMode read FBlendMode write FBlendMode;

      {Creates an instance of TAdPen.}
      constructor Create;
      {Saves the pen data to a stream.}
      procedure SaveToStream(AStream:TStream);
      {Loads the pen data from a stream.}
      procedure LoadFromStream(AStream:TStream);
      {Assigns the pen data to another pen using LoadFromStream/SaveToStream and
       a temporary TMemoryStream.}
      procedure Assign(APen:TAdPen);
      {Tests if the pen is equal to another pen.}
      function EqualTo(APen:TAdPen):Boolean;
  end;

  {Used internally by TAdCanvas and its canvas objects: Tells TAdCanvas what to
   do when comparing to canvas objects.}
  TAdCanvasUpdateState = (
    usEqual, {<The two objects are equal - nothing has to be done.}
    usUpdate {<The two objects are nearly equal and the object already existsing
      can be updated.},
    usDelete{<The two objects are totaly different. The object already existsing
      has to be deleted.});

  {An abstract class, which represents an object which can be present on the
   TAdCanvas surface.}
  TAdCanvasObject = class
    private
      FAppl:TAd2dApplication;
      FBrush:TAdBrush;
      FPen:TAdPen;
      FMatrix:TAdMatrix;
    protected
      property Appl:TAd2dApplication read FAppl;
      procedure SetMatrix(AValue:TAdMatrix);virtual;abstract;
    public
      {Creates an instance of TAdCanvasObject.}
      constructor Create(AAppl:TAd2DApplication);
      {Destroys the instance of TAdCanvasObject}
      destructor Destroy;override;

      {This procedure should be used to draw the canvas object.}
      procedure Draw;virtual;abstract;
      {This object is called to compare to canvas objects. @seealso(TAdCanvasUpdateState)}
      function CompareTo(AItem:TAdCanvasObject):TAdCanvasUpdateState;virtual;abstract;
      {When the "CompareTo" function returned "usUpdate", Update is  called to transform this objects to the other objects state.}
      procedure Update(AItem:TAdCanvasObject);virtual;abstract;
      {Generates the canvas object, so that it can be drawn.}
      procedure Generate;virtual;abstract;

      {Contains the brush which was active in TAdCanvas when creating the object.}
      property Brush:TAdBrush read FBrush write FBrush;
      {Contains the pen which was active in TAdCanvas when creating the object.}
      property Pen:TAdPen read FPen write FPen;
      {Use the matrix to transform the object in 3D space.}
      property Matrix:TAdMatrix read FMatrix write SetMatrix;
  end;

  {A pointer to TAdCanvasDisplayList. @seealso(TAdCanvasDisplayList)}
  PAdCanvasObjectList = ^TAdCanvasDisplayList;
  {Contains multiple TAdCanvasObject objects. The objects can be drawn at once
   and transformed in 3D space using simple functions.}
  TAdCanvasDisplayList = class(TAdList)
    private
      FTransformMatrix:TAdMatrix;
      FAppl:TAd2dApplication;
      function GetItem(AIndex:integer):TAdCanvasObject;
      procedure SetItem(AIndex:integer; AValue:TAdCanvasObject);
    protected
      procedure Notify(Ptr:Pointer; Action:TListNotification);override;
    public
      {Creates an instance of TAdCanvasDisplayList.}
      constructor Create(AAppl:TAd2dApplication);
      {Destroys the instance of TAdCanvasDisplayList.}
      destructor Destroy;override;

      {Resets the matrix used to transform the objects to an identity matrix.}
      procedure ResetTransform;
      {Creates a scale matrix and multiplies it with the transformation matrix.}
      procedure Scale(AX, AY, AZ: single);
      {Creates a translation matrix and multiplies it with the transformation matrix.}
      procedure Translate(AX, AY, AZ: single);
      {Creates a rotate x matrix and multiplies it with the transformation matrix.}
      procedure RotateX(ARotation: single);
      {Creates a rotate y matrix and multiplies it with the transformation matrix.}
      procedure RotateY(ARotation: single);
      {Creates a rotate z matrix and multiplies it with the transformation matrix.}
      procedure RotateZ(ARotation: single);
      {Multiplies "AMatrix" with the transformation matrix.}
      procedure MatrixTransform(AMatrix:TAdMatrix);

      {Draws all objects in the list.}
      procedure Draw;
      
      {Property used to have acces on each object in the list.}
      property Items[Index:integer]:TAdCanvasObject read GetItem write SetItem; default;      
  end;

  {@exclude}
  PAdLinePoint = ^TAdLinePoint;
  
  {@exclude}
  TAdLinePoint = record
    X,Y:integer;
    Color:TAndorraColor;
  end;

  {Represents the four positions of a quad.}
  TAdCanvasQuad = record
    {@exlude}
    p:array[0..3] of TAdVector2;
  end;
  {Pointer type of TAdCanvasQuad}
  PAdCanvasQuad = ^TAdCanvasQuad;

  {Represents the four positions of a quad with the correspondenting colors.}
  TAdCanvasColorQuad = record
    {@exclude}
    p:array[0..3] of TAdVector2;
    {@exclude}
    c:array[0..3] of TAndorraColor;
  end;

  {@exclude}
  TAdCanvasLine = class(TAdCanvasObject)
    private
      FMesh:TAd2dMesh;
      FPoints:TAdLinkedList;
      FOwnPen:boolean;
      FHash:integer;
      FLastPoint:PAdLinePoint;
      procedure HashPoint(APoint:TAdLinePoint);
      procedure GenerateTextureCoords(maxlen:double;var vertices:TAdVertexArray);
      function OrthogonalPoints(x1, y1, x2, y2 : integer; d: single): TAdCanvasQuad;
      function IntersectPoint(v1, v2, p, q : TAdVector2 ):TAdPoint;
      function Determinant(v1, v2 : TAdVector2):double;
    protected
      procedure SetMatrix(AValue:TAdMatrix);override;
    public
      constructor Create(AAppl:TAd2dApplication);
      destructor Destroy;override;

      procedure Rehash;

      procedure AddPoint(APoint:TAdLinePoint);
      procedure Draw;override;
      function CompareTo(AItem:TAdCanvasObject):TAdCanvasUpdateState;override;
      procedure Update(AItem:TAdCanvasObject);override;
      procedure Generate;override;

      property Points:TAdLinkedList read FPoints;
      property Hash:integer read FHash;
  end;

  {@exclude}
  TAdCanvasSplineObject = class(TAdCanvasObject)
    private
      FLine: TAdCanvasLine;
      FPoints: TAdLinkedList;
      FHash: integer;
      FLastPoint: PAdLinePoint;
      FOwnPen:boolean;
      procedure ClearData;
      procedure HashPoint(APoint:TAdLinePoint);
    protected
      procedure SetMatrix(AValue: TAdMatrix); override;
    public
      constructor Create(AAppl: TAd2DApplication);
      destructor Destroy;override;

      procedure Rehash;

      procedure AddPoint(APoint:TAdLinePoint);
      procedure Draw;override;
      procedure Update(AItem: TAdCanvasObject);override;
      function CompareTo(AItem: TAdCanvasObject): TAdCanvasUpdateState; override;

      procedure Generate;override;

      property Hash: integer read FHash;
  end;

  {@exclude}
  TAdCanvasQuadObject = class(TAdCanvasObject)
    private
      FQuad:TAdCanvasColorQuad;
      FLine:TAdCanvasLine;
      FMesh:TAd2dMesh;
      FOwnPen:boolean;
      FDrawMode:TAd2dDrawMode;
      FWidth, FHeight, FMinX, FMaxX, FMinY, FMaxY: single;
      procedure GenerateTextureCoords(var vertices:TAdVertexArray);
      procedure SetNormals(var vertices:TAdVertexArray);
      procedure CalcQuadSizes(aquad:TAdCanvasColorQuad);
    protected
      procedure SetMatrix(AValue:TAdMatrix);override;
    public
      constructor Create(AAppl:TAd2dApplication);
      destructor Destroy;override;

      procedure SetQuad(AQuad:TAdCanvasColorQuad);
      procedure Draw;override;
      function CompareTo(AItem:TAdCanvasObject):TAdCanvasUpdateState;override;
      procedure Update(AItem:TAdCanvasObject);override;
      procedure Generate;override;

      property Quad:TAdCanvasColorQuad read FQuad;
      property Width:single read FWidth;
      property Height:single read FHeight;
      property MinX:single read FMinX;
      property MinY:single read FMinY;
      property MaxX:single read FMaxX;
      property MaxY:single read FMaxY;
  end;

  {@exclude}
  TAdCanvasEllipseObject = class(TAdCanvasObject)
    private
      FLine:TAdCanvasLine;
      FMesh:TAd2dMesh;
      FOwnPen:boolean;
      FPos:TAdRect;
      FCenterX,FCenterY,FWidth,FHeight:single;
      procedure CalcCenter;
      procedure GenerateTextureCoords(var vertices:TAdVertexArray);
    protected
      procedure SetMatrix(AValue:TAdMatrix);override;
    public
      constructor Create(AAppl:TAd2dApplication);
      destructor Destroy;override;
                     
      procedure SetPos(ARect:TAdRect);
      procedure Draw;override;
      function CompareTo(AItem:TAdCanvasObject):TAdCanvasUpdateState;override;
      procedure Update(AItem:TAdCanvasObject);override;
      procedure Generate;override;

      property Pos:TAdRect read FPos;
      property CenterX:single read FCenterX;
      property CenterY:single read FCenterY;
  end;

  {@exclude}
  TAdCanvasTextObject = class(TAdCanvasObject)
    private
      FFont:TAdFont;
      FDrawFont:TAdFont;
      FText:string;
      FRect:TAdRect;
    protected
      procedure SetMatrix(AValue:TAdMatrix);override;
    public
      constructor Create(AAppl:TAd2dApplication);
      destructor Destroy;override;

      procedure Draw;override;
      function CompareTo(AItem:TAdCanvasObject):TAdCanvasUpdateState;override;
      procedure Update(AItem:TAdCanvasObject);override;
      procedure Generate;override;

      property Rect:TAdRect read FRect write FRect;
      property Font:TAdFont read FFont write FFont;
      property Text:string read FText write FText;
  end;

  {@exclude}
  TAdCanvasPointsObject = class(TAdCanvasObject)
    private
      FMesh:TAd2dMesh;
      FPoints:TAdLinkedList;
      FHash:integer;
    protected
      procedure SetMatrix(AValue:TAdMatrix);override;
      procedure HashPoint(APoint:TAdLinePoint);
    public
      constructor Create(AAppl:TAd2dApplication);
      destructor Destroy;override;

      procedure Rehash;

      procedure AddPoint(APoint:TAdLinePoint);
      procedure Draw;override;
      function CompareTo(AItem:TAdCanvasObject):TAdCanvasUpdateState;override;
      procedure Update(AItem:TAdCanvasObject);override;
      procedure Generate;override;

      property Hash:integer read FHash;
      property Points:TAdLinkedList read FPoints;
  end;

  {@exclude}
  TAdCanvasPolygonObject = class(TAdCanvasObject)
    private
      FMesh:TAd2dMesh;
      FPolygon : TAdPolygon;
      procedure GenerateTextureCoords(var vertices:TAdVertexArray);
    protected
      procedure SetMatrix(AValue:TAdMatrix);override;
    public
      constructor Create(AAppl:TAd2dApplication);
      destructor Destroy;override;

      procedure Draw;override;
      function CompareTo(AItem:TAdCanvasObject):TAdCanvasUpdateState;override;
      procedure Update(AItem:TAdCanvasObject);override;
      procedure Generate;override;

      property Polygon : TAdPolygon read FPolygon write FPolygon;
  end;

  {TAdCanvas is a hardware accelerated canvas object, that may perform the most
   comon graphic operations in high speed.
   Remember that more specialiced functions of Andorra 2D are even faster than
   TAdCanvas. So if you e.g. only want to draw a image, use TAdImage instead.}
  TAdCanvas = class
    private
      FReleaseIndex:integer;
      FDrawIndex:integer;

      FAppl:TAd2DApplication;
      FCurrentDisplayList:TAdCanvasDisplayList;

      FCurrentObject:TAdCanvasObject;
      FPen:TAdPen;
      FTempPen:TAdPen;
      FBrush:TAdBrush;
      FFont:TAdFont;
      FDisplayLists:TAdLinkedList;

      FOnRelease:TAdNotifyEvent;
      FOnBeginFrame:TAdNotifyEvent;
      FOnEndFrame:TAdNotifyEvent;

      procedure DeleteUnusedLists;
      procedure DeleteUnusedItems;
      procedure PushObject;

      procedure CreateLinesObject;
      procedure CreatePointsObject;
      procedure CreateSplineObject;
      procedure AddLinePoint(ax,ay:integer);
      procedure AddSplinePoint(ax, ay: integer);
      procedure ColorQuad(var aquad:TAdCanvasColorQuad);
      procedure DoTextOut(ARect:TAdRect; AText:string);
    public
      {Creates an instance of TAdCanvas.}
      constructor Create(AAppl:TAd2dApplication);
      {Destroys an instance of TAdCanvas.}
      destructor Destroy;override;

      {Does some preparations which have to be done at the beginning of a frame. 
       Normally this function is automatically called by TAdRenderingSurface.}
      procedure StartFrame;
      {Finalizes the frame, does some memory-cleanup and calles release.
       Normally this function is automatically called by TAdRenderingSurface.}
      procedure EndFrame;
      {The "Release" procedure draws the objects which are in the current
       display list and creates a new display list.
       @seealso(ReturnDisplayList)}
      procedure Release;
      {Returns the current display list without drawing and creates a new
       display list. Use ReturnDisplay list if you want to draw objects aside
       from TAdCanvas and without calling the draw functions each time.
       @seealso(Release)
       @seealso(TAdCanvasDisplayList)}
      function ReturnDisplayList:TAdCanvasDisplayList;
      {Draws the specified canvas object.}
      procedure DrawObject(var AObj:TAdCanvasObject);

      {Moves the start point of the line to the specified coordinates.
       @seealso(LineTo)
       @seealso(Line)}
      procedure MoveTo(ax,ay:integer);overload;
      {Draws a line from the start point specified by "MoveTo" to the specified
       coordinates.
       @seealso(MoveTo)
       @seealso(Line)}
      procedure LineTo(ax,ay:integer);overload;
      {Moves the start of the line to the specified coordinates.
       @seealso(LineTo)
       @seealso(Line)}
      procedure MoveTo(ap:TAdPoint);overload;
      {Draws a line from the start point specified by "MoveTo" to the specified
       coordinates.
       @seealso(MoveTo)
       @seealso(Line)}
      procedure LineTo(ap:TAdPoint);overload;
      {Draws a line from the start point to the specified coordinates.
       @seealso(MoveTo)
       @seealso(Line)}
      procedure Line(ax1,ay1,ax2,ay2:integer);overload;
      {Draws a line from the start point to the specified coordinates.
       @seealso(MoveTo)
       @seealso(Line)}
      procedure Line(ap1,ap2:TAdPoint);overload;

      {Moves the start point of the line to the specified coordinates.}
      procedure SplineMoveTo(ax, ay: integer);overload;
      {Moves the start point of the line to the specified coordinates.}
      procedure SplineMoveTo(ap: TAdPoint);overload;
      {Draws a spline from the start point specified by "SplineMoveTo" to the specified
       coordinates.}
      procedure SplineLineTo(ax, ay: integer);overload;
      {Draws a spline from the start point specified by "SplineMoveTo" to the specified
       coordinates.}
      procedure SplineLineTo(ap: TAdPoint);overload;


      {Draws an arrow from P1 to P2. ArrowSize specifies the length of the a
       rrowhead. ArrowAngle specifies the inner angle between the to arrowhead
       lines.}
      procedure Arrow(ArrowSize, ArrowAngle:Integer; P1, P2:TAdPoint);

      {Draws a text using the font specified in the "Font" property.}
      procedure TextOut(AX, AY:integer; AText:string);overload;

      {Draws a rectangle.}
      procedure Rectangle(AX1, AY1, AX2, AY2:integer);overload;
      {Draws a rectangle.}
      procedure Rectangle(AR:TAdRect);overload;
      {Draws a rectangle.}
      procedure Rectangle(AP1, AP2:TAdPoint);overload;
      {Draws a rectangle.}
      procedure Rectangle(AP:TAdPoint; AWidth, AHeight:integer);overload;

      {Draws a single pixel using the color specified in TAdPen.Color}
      procedure PlotPixel(AX, AY:integer);overload;
      {Draws a single pixel using the color specified in the "AColor"
       parameter.}
      procedure PlotPixel(AX, AY:integer; AColor:TAndorraColor);overload;

      {Draws an ellipse within the specified coordinates.}
      procedure Ellipse(AX1, AY1, AX2, AY2:integer);
      {Draws a circle with the center "acx;acy" and the readius "ar".}
      procedure Circle(ACX, ACY, AR:integer);

      {Draws a colored quad. @seealso(TAdCanvasColorQuad).}
      procedure DrawColoredQuad(AQuad:TAdCanvasColorQuad);
      {Draws a simple quad using the color specified in "Brush".
       @seealso(TAdCanvasQuad)
       @seealso(TAdBrush)}
      procedure DrawQuad(AQuad:TAdCanvasQuad);

      {Draws a simple polygon. The polygon points have to be in clockwise order
       and may not contain any intersections. }
      procedure Polygon(APolygon:TAdPolygon);

      {Draws a poly line.}
      procedure Polyline(APolygon: TAdPolygon);

      {Draws a spline.}
      procedure Spline(ASpline: TAdPolygon);

      {The pen object, which represents the settings for the outline of the
       objects.
       @seealso(TAdPen)}
      property Pen:TAdPen read FPen write FPen;
      {The brush object, which represents the settings for the filling of the
       objects.
       @seealso(TAdBrush)}
      property Brush:TAdBrush read FBrush write FBrush;
      {Set the "Font" property to the font you would like to use with "TextOut".}
      property Font:TAdFont read FFont write FFont;

      {This event triggered, when the "release" method of TAdCanvas is called.
       "OnRelease" is used internally by TAdRenderingSurface.
       When the release method of its own canvas is called, TAdRenderingSurface
       automatically sets itself to the current active surfacre.
       @seealso(TAdCanvas.Release)}
      property OnRelease:TAdNotifyEvent read FOnRelease write FOnRelease;
      {The "OnBeginFrame" event is triggered, when the "BeginFrame" method of
       TAdCanvas is called.}
      property OnBeginFrame:TAdNotifyEvent read FOnBeginFrame write FOnBeginFrame;
      {The "OnEndFrame" event is triggered, when the "EndFrame" method of
       TAdCanvas is called.}
      property OnEndFrame:TAdNotifyEvent read FOnEndFrame write FOnEndFrame;
  end;

implementation

{ TAdBrush }

constructor TAdBrush.Create;
begin
  inherited Create;

  Color := Ad_ARGB(255,255,255,255);
  GradientColor := Ad_ARGB(255,255,255,255);
  TextureMode := tmStretch;
  TexturePosition := tpDynamic;
  Style := abSolid;
end;

procedure TAdBrush.LoadFromStream(AStream: TStream);
begin
  AStream.Read(FStyle, SizeOf(FStyle));
  AStream.Read(FColors, SizeOf(FColors));
  AStream.Read(FColor, SizeOf(FColor));
  AStream.Read(FGradientColor, SizeOf(FGradientColor));
  AStream.Read(FGradientDirection, SizeOf(FGradientDirection));
  AStream.Read(FTextureMode, SizeOf(FTextureMode));
  AStream.Read(FTexturePosition, SizeOf(FTexturePosition));
  AStream.Read(FBlendMode, SizeOf(FBlendMode));
end;

procedure TAdBrush.SaveToStream(AStream: TStream);
begin
  AStream.Write(FStyle, SizeOf(FStyle));
  AStream.Write(FColors, SizeOf(FColors));
  AStream.Write(FColor, SizeOf(FColor));
  AStream.Write(FGradientColor, SizeOf(FGradientColor));
  AStream.Write(FGradientDirection, SizeOf(FGradientDirection));
  AStream.Write(FTextureMode, SizeOf(FTextureMode));
  AStream.Write(FTexturePosition, SizeOf(FTexturePosition));
  AStream.Write(FBlendMode, SizeOf(FBlendMode));
end;

procedure TAdBrush.Assign(ABrush: TAdBrush);
var
  ms:TMemoryStream;
begin
  ms := TMemoryStream.Create;
  ABrush.SaveToStream(ms);
  ms.Position := 0;
  LoadFromStream(ms);
  ms.Free;
  FTexture := ABrush.Texture; 
end;

function TAdBrush.EqualTo(ABrush: TAdBrush): Boolean;
begin
  result :=
    (ABrush.FStyle = FStyle) and
    (CompareColors(ABrush.FColors[0],FColors[0])) and
    (CompareColors(ABrush.FColors[1],FColors[1])) and
    (CompareColors(ABrush.FColors[2],FColors[2])) and
    (CompareColors(ABrush.FColors[3],FColors[3])) and
    (ABrush.FBlendMode = FBlendMode) and
    (ABrush.FTextureMode = FTextureMode) and
    (ABrush.FTexturePosition = FTexturePosition) and
    (ABrush.FTexture = FTexture);
end;

procedure TAdBrush.SetColor(AValue: TAndorraColor);
begin
  FStyle := abSolid;
  FColor := AValue;
  UpdateColors;
end;

procedure TAdBrush.SetGradientColor(AValue: TAndorraColor);
begin
  FStyle := abGradient;
  FGradientColor := AValue;
  UpdateColors;
end;

procedure TAdBrush.SetGradientDirection(AValue: TAdCanvasGradientDirection);
begin
  FStyle := abGradient;
  FGradientDirection := AValue;
  UpdateColors;
end;

procedure TAdBrush.SetStyle(AValue: TAdBrushStyle);
begin
  FStyle := AValue;
  UpdateColors;
end;

procedure TAdBrush.SetTexture(AValue: TAd2dTexture);
begin
  FTexture := AValue;
end;

procedure TAdBrush.UpdateColors;
var
  i:integer;
begin
  //0-1
  //3-2
  for i := 0 to High(FColors) do
  begin
    FColors[i] := FColor;
  end;

  if Style = abGradient then
  begin
    case FGradientDirection of
      gdVertical:
      begin
        FColors[1] := FGradientColor;
        FColors[2] := FGradientColor;
      end;
      gdHorizontal:
      begin
        FColors[2] := FGradientColor;
        FColors[3] := FGradientColor;
      end;
    end;
  end;
end;

{ TAdPen }

constructor TAdPen.Create;
begin
  inherited Create;
  FColor := AD_ARGB(255,255,255,255);
  FWidth := 1;
  FDashLength := 5;
  FGapLength := 3;
  FTexture := nil;
  FTextureMode := tmTile;
  FPenPosition := ppMiddle;
  FStyle := apSolid;
end;

procedure TAdPen.LoadFromStream(AStream: TStream);
begin
  AStream.Read(FStyle, SizeOf(FStyle));
  AStream.Read(FColor, SizeOf(FColor));
  AStream.Read(FTextureMode, SizeOf(FTextureMode));
  AStream.Read(FTexturePosition, SizeOf(FTexturePosition));
  AStream.Read(FPenPosition, SizeOf(FPenPosition));
  AStream.Read(FWidth, SizeOf(FWidth));
  AStream.Read(FDashLength, SizeOf(FDashLength));
  AStream.Read(FGapLength, SizeOf(FGapLength));
  AStream.Read(FBlendMode, SizeOf(FBlendMode));
end;

procedure TAdPen.SaveToStream(AStream: TStream);
begin
  AStream.Write(FStyle, SizeOf(FStyle));
  AStream.Write(FColor, SizeOf(FColor));
  AStream.Write(FTextureMode, SizeOf(FTextureMode));
  AStream.Write(FTexturePosition, SizeOf(FTexturePosition));
  AStream.Write(FPenPosition, SizeOf(FPenPosition));
  AStream.Write(FWidth, SizeOf(FWidth));
  AStream.Write(FDashLength, SizeOf(FDashLength));
  AStream.Write(FGapLength, SizeOf(FGapLength));
  AStream.Write(FBlendMode, SizeOf(FBlendMode));
end;

procedure TAdPen.Assign(APen: TAdPen);
var
  ms:TMemoryStream;
begin
  ms := TMemoryStream.Create;
  APen.SaveToStream(ms);
  ms.Position := 0;
  LoadFromStream(ms);
  ms.Free;
  FTexture := APen.Texture;
end;

function TAdPen.EqualTo(APen: TAdPen): Boolean;
begin
  result :=
    (APen.FStyle = FStyle) and
    (CompareColors(APen.FColor, FColor)) and
    (APen.FBlendMode = FBlendMode) and
    (APen.FTextureMode = FTextureMode) and
    (APen.FTexturePosition = FTexturePosition) and
    (APen.FPenPosition = FPenPosition) and
    (APen.FWidth = FWidth) and
    (APen.FDashLength = FDashLength) and
    (APen.FGapLength = FGapLength);
end;

procedure TAdPen.SetColor(AValue: TAndorraColor);
begin
  FColor := AValue;
end;

procedure TAdPen.SetStyle(AValue: TAdPenStyle);
begin
  FStyle := AValue;
end;

procedure TAdPen.SetTexture(AValue: TAd2dTexture);
begin
  FTexture := AValue;
end;

procedure TAdPen.SetWidth(AValue: single);
begin
  if AValue >= 1 then
  begin
    FWidth := AValue;
  end
  else
  begin
    FStyle := apNone;
  end;
end;

{ TAdObjectList }

constructor TAdCanvasDisplayList.Create(AAppl:TAd2dApplication);
begin
  inherited Create;
  FAppl := AAppl;
  ResetTransform;
end;

destructor TAdCanvasDisplayList.Destroy;
begin
  inherited;
end;

procedure TAdCanvasDisplayList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then
  begin
    TAdCanvasObject(Ptr).Free;
  end;
end;

procedure TAdCanvasDisplayList.ResetTransform;
begin
  FTransformMatrix := AdMatrix_Identity;
end;

procedure TAdCanvasDisplayList.RotateX(ARotation: single);
begin
  FTransformMatrix := AdMatrix_Multiply(FTransFormMatrix, AdMatrix_RotationX(ARotation));
end;

procedure TAdCanvasDisplayList.RotateY(ARotation: single);
begin
  FTransformMatrix := AdMatrix_Multiply(FTransFormMatrix, AdMatrix_RotationY(ARotation));
end;

procedure TAdCanvasDisplayList.RotateZ(ARotation: single);
begin
  FTransformMatrix := AdMatrix_Multiply(FTransFormMatrix, AdMatrix_RotationZ(ARotation));
end;

procedure TAdCanvasDisplayList.MatrixTransform(AMatrix: TAdMatrix);
begin
  FTransformMatrix := AMatrix;
end;

procedure TAdCanvasDisplayList.Scale(AX, AY, AZ: single);
begin
  FTransformMatrix := AdMatrix_Multiply(FTransformMatrix, AdMatrix_Scale(ax, ay, az));
end;

procedure TAdCanvasDisplayList.Translate(AX, AY, AZ: single);
begin
  FTransformMatrix := AdMatrix_Multiply(FTransformMatrix, AdMatrix_Translate(ax, ay, az));
end;

procedure TAdCanvasDisplayList.Draw;
var
  i:integer;
begin
  for i := 0 to Count - 1 do
  begin
    Items[i].Matrix := FTransformMatrix;
    Items[i].Draw;
  end;
end;

function TAdCanvasDisplayList.GetItem(AIndex: integer): TAdCanvasObject;
begin
  result := TAdCanvasObject(inherited Items[AIndex]);
end;

procedure TAdCanvasDisplayList.SetItem(AIndex: integer; AValue: TAdCanvasObject);
begin
  inherited Items[AIndex] := AValue;
end;

{ TAdCanvasObject }

constructor TAdCanvasObject.Create(AAppl: TAd2DApplication);
begin
  inherited Create;
  FAppl := AAppl;
end;

destructor TAdCanvasObject.Destroy;
begin
  inherited;
end;

{ TAdCanvas }

constructor TAdCanvas.Create(AAppl: TAd2dApplication);
begin
  inherited Create;
  FAppl := AAppl;

  FDisplayLists := TAdLinkedList.Create;

  FPen := TAdPen.Create;
  FTempPen := TAdPen.Create;
  FBrush := TAdBrush.Create;
end;

destructor TAdCanvas.Destroy;
begin
  FReleaseIndex := 0;
  DeleteUnusedLists;

  FDisplayLists.Free;

  FPen.Free;
  FTempPen.Free;
  FBrush.Free;

  inherited;
end;

procedure TAdCanvas.StartFrame;
begin
  FDisplayLists.StartIteration;
  if not FDisplayLists.ReachedEnd then
    FCurrentDisplayList := TAdCanvasDisplayList(FDisplayLists.GetCurrent)
  else
    FCurrentDisplayList := nil;
    
  FReleaseIndex := 0;
  FDrawIndex := 0;
end;


procedure TAdCanvas.EndFrame;
begin
  Release;
  DeleteUnusedLists;
end;

procedure TAdCanvas.DeleteUnusedItems;
var
  i:integer;
begin
  //Delete all unused canvas items
  for i := FCurrentDisplayList.Count - 1 downto FDrawIndex do
  begin
    FCurrentDisplayList.Delete(i);
  end;
end;

procedure TAdCanvas.DeleteUnusedLists;
var
  i:integer;
begin
  //Delete all unused display lists - FReleaseIndex defines how often "Release" was called.
  for i := FDisplayLists.Count - 1 downto FReleaseIndex do
  begin
    TAdCanvasDisplayList(FDisplayLists.Items[i]).Free;
    FDisplayLists.Delete(i);
  end;
end;

procedure TAdCanvas.DrawObject(var AObj: TAdCanvasObject);
begin
  //Create a new display list if necessary
  if (FCurrentDisplayList = nil) then
  begin
    FCurrentDisplayList := TAdCanvasDisplayList.Create(FAppl);
    FDisplayLists.Add(FCurrentDisplayList);
  end;

  if FDrawIndex <= FCurrentDisplayList.Count - 1 then
  begin
    //If this item already exists, check wether we might update it.
    case FCurrentDisplayList[FDrawIndex].CompareTo(AObj) of
      usDelete:
      begin
        AObj.Generate;
        //Old object is automaticly freed
        FCurrentDisplayList[FDrawIndex] := AObj;
      end;
      usUpdate:
      begin
        FCurrentDisplayList[FDrawIndex].Update(AObj);
        AObj.Free;
        AObj := FCurrentDisplayList[FDrawIndex];
      end;
      usEqual:
      begin
        AObj.Free;
        AObj := FCurrentDisplayList[FDrawIndex];
      end;
    end;
  end
  else
  begin
    AObj.Generate;
    FCurrentDisplayList.Add(AObj);
  end;

  FDrawIndex := FDrawIndex + 1;
end;

procedure TAdCanvas.PushObject;
begin
  if FCurrentObject <> nil then
  begin
    if (FCurrentObject is TAdCanvasLine) or
       (FCurrentObject is TAdCanvasSplineObject) then
      FCurrentObject.Pen := FTempPen
    else
      FCurrentObject.Pen := FPen;
      
    FCurrentObject.Brush := FBrush;
    DrawObject(FCurrentObject);
    FCurrentObject := nil;
  end;
end;

procedure TAdCanvas.Release;
begin
  PushObject;
  
  if FCurrentDisplayList <> nil then
  begin
    DeleteUnusedItems;

    if Assigned(FOnRelease) then
      FOnRelease(self);

    FCurrentDisplayList.Draw;

    FReleaseIndex := FReleaseIndex + 1;
    FDrawIndex := 0;
    if not FDisplayLists.ReachedEnd then
    begin
      FCurrentDisplayList := TAdCanvasDisplayList(FDisplayLists.GetCurrent);
    end
    else
    begin
      FCurrentDisplayList := nil;
    end;
  end;
end;

function TAdCanvas.ReturnDisplayList: TAdCanvasDisplayList;
var
  i:integer;
begin
  result := nil;
  PushObject;

  if FCurrentDisplayList <> nil then
  begin
    DeleteUnusedItems;

    result := FCurrentDisplayList;
    FDrawIndex := 0;
    FDisplayLists.Delete(FReleaseIndex);

    FDisplayLists.StartIteration;
    for i := 0 to FReleaseIndex do
      if not FDisplayLists.ReachedEnd then FDisplayLists.GetCurrent;

    if not FDisplayLists.ReachedEnd then
    begin
      FCurrentDisplayList := TAdCanvasDisplayList(FDisplayLists.GetCurrent);
    end
    else
    begin
      FCurrentDisplayList := nil;
    end;
  end;
end;

{ Canvas Drawing operations }

procedure TAdCanvas.CreateLinesObject;
var
  tmp:TAdPen;
begin
  if (FCurrentObject <> nil) then
  begin
    if FCurrentObject is TAdCanvasLine then
    begin
      tmp := FPen;
      FPen := FTempPen;
      PushObject;
      FPen := tmp;
    end
    else
    begin
      PushObject;
    end;
  end;
  FTempPen.Assign(FPen);
  FCurrentObject := TAdCanvasLine.Create(FAppl);
end;

procedure TAdCanvas.CreateSplineObject;
var
  tmp:TAdPen;
begin
  if (FCurrentObject <> nil) then
  begin
    if FCurrentObject is TAdCanvasSplineObject then
    begin
      tmp := FPen;
      FPen := FTempPen;
      PushObject;
      FPen := tmp;
    end
    else
    begin
      PushObject;
    end;
  end;
  FTempPen.Assign(FPen);
  FCurrentObject := TAdCanvasSplineObject.Create(FAppl);
end;

procedure TAdCanvas.CreatePointsObject;
begin
  if FCurrentObject = nil then
  begin
    FCurrentObject := TAdCanvasPointsObject.Create(FAppl)
  end
  else
  begin
    if not (FCurrentObject is TAdCanvasPointsObject) then
    begin
      PushObject;
      FCurrentObject := TAdCanvasPointsObject.Create(FAppl)
    end;
  end;
end;

procedure TAdCanvas.AddLinePoint(ax, ay: integer);
var
  p:TAdLinePoint;
begin
  with p do
  begin
    X := ax;
    Y := ay;
    Color := Pen.Color;
  end;

  TAdCanvasLine(FCurrentObject).AddPoint(p);
end;

procedure TAdCanvas.AddSplinePoint(ax, ay: integer);
var
  p:TAdLinePoint;
begin
  with p do
  begin
    X := ax;
    Y := ay;
    Color := Pen.Color;
  end;

  TAdCanvasSplineObject(FCurrentObject).AddPoint(p);
end;

procedure TAdCanvas.Circle(acx, acy, ar: integer);
begin
  Ellipse(acx-ar, acy-ar, acx+ar, acy+ar);
end;

procedure TAdCanvas.ColorQuad(var aquad: TAdCanvasColorQuad);
begin
  case FBrush.Style of
    abSolid, abClear:
    begin
      aquad.c[0] := FBrush.Color;
      aquad.c[1] := FBrush.Color;
      aquad.c[2] := FBrush.Color;
      aquad.c[3] := FBrush.Color;
    end;
    abGradient:
    begin
      case FBrush.GradientDirecton of
        gdVertical:
        begin
          aquad.c[0] := FBrush.Color;
          aquad.c[1] := FBrush.GradientColor;
          aquad.c[2] := FBrush.GradientColor;
          aquad.c[3] := FBrush.Color;
        end;
        gdHorizontal:
        begin
          aquad.c[0] := FBrush.Color;
          aquad.c[1] := FBrush.Color;
          aquad.c[2] := FBrush.GradientColor;
          aquad.c[3] := FBrush.GradientColor;
        end;
      end;
    end;
  end;
end;

procedure TAdCanvas.Line(ap1, ap2: TAdPoint);
begin
  MoveTo(ap1.X,ap1.Y);
  LineTo(ap2.X,ap2.Y);
end;

procedure TAdCanvas.Line(ax1, ay1, ax2, ay2: integer);
begin
  MoveTo(ax1,ay1);
  LineTo(ax2,ay2);
end;

procedure TAdCanvas.LineTo(ap: TAdPoint);
begin
  LineTo(ap.X,ap.Y);
end;

procedure TAdCanvas.MoveTo(ap: TAdPoint);
begin
  MoveTo(ap.X, ap.Y);
end;

procedure TAdCanvas.LineTo(ax, ay: integer);
begin
  if FCurrentObject is TAdCanvasLine then
    AddLinePoint(ax,ay)
  else
    MoveTo(ax,ay);
end;

procedure TAdCanvas.MoveTo(ax, ay: integer);
begin
  CreateLinesObject;
  AddLinePoint(ax,ay);
end;

procedure TAdCanvas.Arrow(ArrowSize, ArrowAngle: Integer; P1, P2: TAdPoint);
//original code by Christof Urbaczek, adaption by Andreas Stöckel
//http://www.delphipraxis.net/topic42773_einen+pfeil+zeichnen.html

var
  Alpha, AlphaZ : double;   

begin
  MoveTo(P1.X, P1.Y);
  LineTo(P2.X, P2.Y);

  Alpha := 0;
  if P2.X = P1.X then
    AlphaZ := 0
  else
    AlphaZ := RadToDeg(ArcTan((P2.Y - P1.Y) / (P2.X - P1.X)));

  if (P2.X > P1.X) and (P2.Y = P1.Y) then Alpha := 0
  else if (P2.X > P1.X) and (P2.Y < P1.Y) then Alpha := 0 - AlphaZ
  else if (P2.X = P1.X) and (P2.Y < P1.Y) then Alpha := 90
  else if (P2.X < P1.X) and (P2.Y < P1.Y) then Alpha := 180 - AlphaZ
  else if (P2.X < P1.X) and (P2.Y = P1.Y) then Alpha := 180
  else if (P2.X < P1.X) and (P2.Y > P1.Y) then Alpha := 180 - AlphaZ
  else if (P2.X = P1.X) and (P2.Y > P1.Y) then Alpha := 270
  else if (P2.X > P1.X) and (P2.Y > P1.Y) then Alpha := 360 - AlphaZ;

  MoveTo(P2.X, P2.Y);
  LineTo(
    round(P2.X - ArrowSize * cos(DegToRad(Alpha - ArrowAngle / 2))),
    round(P2.Y + ArrowSize * sin(DegToRad(Alpha - ArrowAngle / 2))));

  MoveTo(P2.X, P2.Y);
  LineTo(
    round(P2.X - ArrowSize * cos(DegToRad(Alpha + ArrowAngle / 2))),
    round(P2.Y + ArrowSize * sin(DegToRad(Alpha + ArrowAngle / 2))));
end;

procedure TAdCanvas.DrawQuad(aquad: TAdCanvasQuad);
var
  tmpquad:TAdCanvasColorQuad;
  i:integer;
begin
  PushObject;

  for i := 0 to 3 do
    tmpquad.p[i] := aquad.p[i];

  ColorQuad(tmpquad);

  FCurrentObject := TAdCanvasQuadObject.Create(FAppl);
  with FCurrentObject as TAdCanvasQuadObject do
  begin
    SetQuad(tmpquad);
  end;

  PushObject;
end;

procedure TAdCanvas.DrawColoredQuad(aquad: TAdCanvasColorQuad);
begin
  PushObject;

  FCurrentObject := TAdCanvasQuadObject.Create(FAppl);
  with FCurrentObject as TAdCanvasQuadObject do
  begin
    SetQuad(AQuad);
  end;

  PushObject;
end;

procedure TAdCanvas.Rectangle(ax1, ay1, ax2, ay2: integer);
var
  AQuad:TAdCanvasColorQuad;
begin
  PushObject;
  AQuad.p[0].x := ax1;
  AQuad.p[0].y := ay1;

  AQuad.p[1].x := ax2;
  AQuad.p[1].y := ay1;

  AQuad.p[2].x := ax2;
  AQuad.p[2].y := ay2;

  AQuad.p[3].x := ax1;
  AQuad.p[3].y := ay2;

  ColorQuad(AQuad);

  FCurrentObject := TAdCanvasQuadObject.Create(FAppl);
  with FCurrentObject as TAdCanvasQuadObject do
  begin
    SetQuad(AQuad);
  end;
  
  PushObject;
end;

procedure TAdCanvas.Rectangle(ar: TAdRect);
begin
  Rectangle(ar.Left, ar.Top, ar.Right, ar.Bottom);
end;

procedure TAdCanvas.Rectangle(ap1, ap2: TAdPoint);
begin
  Rectangle(ap1.X, ap1.Y, ap2.X, ap2.Y);
end;

procedure TAdCanvas.Rectangle(ap: TAdPoint; awidth, aheight: integer);
begin
  Rectangle(ap.X, ap.Y, awidth + ap.X, aheight + ap.Y);
end;

procedure TAdCanvas.Ellipse(ax1, ay1, ax2, ay2: integer);
begin
  PushObject;

  FCurrentObject := TAdCanvasEllipseObject.Create(FAppl);
  with FCurrentObject as TAdCanvasEllipseObject do
  begin
    SetPos(AdRect(ax1,ay1,ax2,ay2));
  end;
  
  PushObject;
end;

procedure TAdCanvas.DoTextOut(ARect: TAdRect; AText: string);
begin
  PushObject;
  
  FCurrentObject := TAdCanvasTextObject.Create(FAppl);
  TAdCanvasTextObject(FCurrentObject).Font := FFont;
  TAdCanvasTextObject(FCurrentObject).Rect := ARect;
  TAdCanvasTextObject(FCurrentObject).Text := AText;

  PushObject;
end;

procedure TAdCanvas.TextOut(AX, AY: integer; AText: string);
begin
  DoTextOut(AdRect(AX, AY, AX, AY), AText);
end;

procedure TAdCanvas.PlotPixel(ax, ay: integer);
begin
  PlotPixel(ax, ay, FPen.Color);
end;

procedure TAdCanvas.PlotPixel(ax, ay: integer; acolor: TAndorraColor);
var
  p:TAdLinePoint;
begin
  CreatePointsObject;

  p.X := ax;
  p.Y := ay;
  p.Color := acolor;
  TAdCanvasPointsObject(FCurrentObject).AddPoint(p);
end;

procedure TAdCanvas.Polygon(apolygon: TAdPolygon);
begin
  PushObject;
  FCurrentObject := TAdCanvasPolygonObject.Create(FAppl);
  with FCurrentObject as TAdCanvasPolygonObject do
    Polygon := APolygon;

  PushObject;  
end;

procedure TAdCanvas.Polyline(APolygon: TAdPolygon);
var
  i: integer;
  lp: TAdLinePoint;
begin
  PushObject;

  CreateLinesObject;
  with FCurrentObject as TAdCanvasLine do
  begin
    for i := 0 to High(APolygon) do
    begin
      lp.X := APolygon[i].X;
      lp.Y := APolygon[i].Y;
      lp.Color := self.FPen.Color;

      AddPoint(lp);
    end;
  end;

  PushObject;
end;

procedure TAdCanvas.Spline(ASpline: TAdPolygon);
var
  i: integer;
  lp: TAdLinePoint;
begin
  PushObject;

  CreateSplineObject;
  with FCurrentObject as TAdCanvasSplineObject do
  begin
    for i := 0 to High(ASpline) do
    begin
      lp.X := ASpline[i].X;
      lp.Y := ASpline[i].Y;
      lp.Color := self.FPen.Color;

      AddPoint(lp);
    end;
  end;

  PushObject;
end;         

procedure TAdCanvas.SplineLineTo(ap: TAdPoint);
begin
  SplineLineTo(ap.X, ap.Y);
end;

procedure TAdCanvas.SplineLineTo(ax, ay: integer);
begin
  if FCurrentObject is TAdCanvasSplineObject then
    AddSplinePoint(ax, ay)
  else
    SplineMoveTo(ax, ay);
end;

procedure TAdCanvas.SplineMoveTo(ap: TAdPoint);
begin
  SplineMoveTo(ap.X, ap.Y);
end;

procedure TAdCanvas.SplineMoveTo(ax, ay: integer);
begin
  CreateSplineObject;
  AddSplinePoint(ax,ay);
end;

{ TAdCanvasLines }

constructor TAdCanvasLine.Create(AAppl: TAd2dApplication);
begin
  inherited;
  FPoints := TAdLinkedList.Create;
  FMesh := nil;
  FMesh := Appl.CreateMesh;
  FHash := 0;
  FOwnPen := false;
  FLastPoint := nil;
end;

destructor TAdCanvasLine.Destroy;
begin
  FPoints.StartIteration;
  while not FPoints.ReachedEnd do
  begin
    Dispose(PAdLinePoint(FPoints.GetCurrent));
  end;
  FPoints.Free;

  FMesh.Free;
  FMesh := nil;

  if FOwnPen then
  begin
    Pen.Free;
  end;
  
  inherited;
end;

procedure TAdCanvasLine.AddPoint(APoint: TAdLinePoint);
var
  PPoint:PAdLinePoint;
begin
  if FLastPoint <> nil then
  begin
    if (FLastPoint^.X = APoint.X) and
       (FLastPoint^.Y = APoint.Y) then exit;
  end;
  HashPoint(APoint);
  New(PPoint);
  PPoint^ := APoint;
  FPoints.Add(PPoint);
  FLastPoint := PPoint;
end;

function TAdCanvasLine.CompareTo(AItem: TAdCanvasObject): TAdCanvasUpdateState;
begin
  result := usDelete;
  if AItem is TAdCanvasLine then
  begin
    if (FPoints.Count = TAdCanvasLine(AItem).Points.Count) and
       (AItem.Pen.EqualTo(Pen)) then
    begin
      result := usUpdate;

      if Hash = TAdCanvasLine(AItem).Hash then
      begin
        result := usEqual;
      end;
    end;
  end;
end;

procedure TAdCanvasLine.Draw;
begin
  if FMesh <> nil then
  begin
    if FPen.Width = 1 then
    begin
      FMesh.Draw(FPen.BlendMode,adLineStrips);
    end
    else
    begin
      FMesh.Draw(FPen.BlendMode,adTriangleStrips);
    end;
  end;
end;

function TAdCanvasLine.OrthogonalPoints(x1, y1, x2, y2:integer; d: single): TAdCanvasQuad;
var
  l:double;
  d1,d2:double;
  alpha:double;
begin
  FillChar(result, SizeOf(result), 0);

  d1 := 0; d2 := 0;

  case FPen.PenPosition of
    ppOuter: begin d1 := 0; d2 := d; end;
    ppMiddle: begin d1 := d / 2; d2 := d1; end;
    ppInner: begin d1 := d; d2 := 0; end;
  end;

  l := sqrt(sqr(x2-x1)+sqr(y2-y1));

  if l > 0 then
  begin
    alpha := arccos((x2-x1)/l);

    if (y2 > y1) then
    begin
      alpha := 2 * pi - alpha;
    end;
    alpha := - alpha;

    result.p[0].X := x1 + cos(alpha + 0.5*pi) * d1;
    result.p[0].Y := y1 + sin(alpha + 0.5*pi) * d1;
    result.p[1].X := x1 + cos(alpha - 0.5*pi) * d2;
    result.p[1].Y := y1 + sin(alpha - 0.5*pi) * d2;

    result.p[2].X := x2 + cos(alpha + 0.5*pi) * d1;
    result.p[2].Y := y2 + sin(alpha + 0.5*pi) * d1;
    result.p[3].X := x2 + cos(alpha - 0.5*pi) * d2;
    result.p[3].Y := y2 + sin(alpha - 0.5*pi) * d2;
  end;
end;


procedure TAdCanvasLine.Rehash;
begin
  FHash := 0;

  FPoints.StartIteration;
  while not FPoints.ReachedEnd do
  begin
    HashPoint(PAdLinePoint(FPoints.GetCurrent)^);
  end;
end;

procedure TAdCanvasLine.SetMatrix(AValue: TAdMatrix);
begin
  FMatrix := AValue;
  FMesh.Matrix := FMatrix;
end;

function TAdCanvasLine.Determinant(v1, v2: TAdVector2): double;
begin
  result := v1.x*v2.y - v1.y*v2.x;
end;

function TAdCanvasLine.IntersectPoint(v1, v2, p, q : TAdVector2 ):TAdPoint;
var
  ps:TAdPointEx;
  mu:double;
begin
  ps.x := - p.x + q.x;
  ps.y := - p.y + q.y;

  mu := (ps.y*v1.x - v1.y*ps.x) / Determinant(v1, v2);

  result.x := round(-v2.x * mu + q.X);
  result.y := round(-v2.y * mu + q.Y);
end;

procedure TAdCanvasLine.Generate;
var
  i, v : integer;

  APen:TAdPen;

  Vertices : TAdVertexArray;

  quad:TAdCanvasQuad;

  v1, v2:TAdVector2;
  maxlen, l:double;

  p1,q1,p2,q2:TAdVector2;
  k1, k2:TAdPoint;

  pnts: array of TAdLinePoint;

  vertexcount:integer;
  primitivecount:integer;
begin
  if FPoints.Count < 2 then
    exit;
  
  //Copy pen if necessary
  if not FOwnPen then
  begin
    FOwnPen := true;
    APen := FPen;
    FPen := TAdPen.Create;
    FPen.Assign(APen);
  end;

  if CompareValue(FPen.Width, 1, 0.001) = 0 then
  begin
    SetLength(Vertices,Points.Count);
    FPoints.StartIteration;

    i := 0;
    while not FPoints.ReachedEnd do
    begin
      with PAdLinePoint(FPoints.GetCurrent)^ do
      begin
        Vertices[i].Position := AdVector3(X,Y,0);
        Vertices[i].Color := Color;
        Vertices[i].Normal := AdVector3(0,0,-1);
      end;
      i := i + 1;
    end;

    FMesh.Vertices := Vertices;
    FMesh.PrimitiveCount := FPoints.Count - 1;
    FMesh.Indices := nil;
    FMesh.Update;
    FMesh.Matrix := AdMatrix_Identity;
  end
  else
  begin
    //Copy points into an array

    SetLength(pnts, FPoints.Count);
    FPoints.StartIteration;
    i := 0;
    while not FPoints.ReachedEnd do
    begin
      pnts[i] := PAdLinePoint(FPoints.GetCurrent)^;
      i := i + 1;
    end;

    vertexcount := FPoints.Count * 2;
    primitivecount := (FPoints.Count - 1) * 2;

    SetLength(Vertices,vertexcount);

    //Calculate Vertices
    i := 0;
    v := 0;
    maxlen := 0;

    quad := OrthogonalPoints(pnts[0].X, pnts[0].Y, pnts[1].X, pnts[1].Y, Pen.Width);

    Vertices[v].Color := pnts[i].Color;
    Vertices[v].Position := AdVector3(quad.p[0].x, quad.p[0].y, 0); inc(v);
    Vertices[v].Color := pnts[i].Color;
    Vertices[v].Position := AdVector3(quad.p[1].x, quad.p[1].y, 0); inc(v);

    while i < Length(pnts) - 2 do
    begin
      maxlen := maxlen + sqrt(sqr(pnts[i].X-pnts[i+1].X)+sqr(pnts[i].Y-pnts[i+1].Y));

      v1 := AdVector2(pnts[i+1].X - pnts[i].X, pnts[i+1].Y - pnts[i].Y);
      v2 := AdVector2(pnts[i+2].X - pnts[i+1].X, pnts[i+2].Y - pnts[i+1].Y);

      l := Determinant(v1, v2);
      if CompareValue(l, 0, 0.001) <> 0 then
      begin
        quad := OrthogonalPoints(
          pnts[i].X, pnts[i].Y, pnts[i+1].X, pnts[i+1].Y, Pen.Width);

        p1 := quad.p[0];
        q1 := quad.p[1];

        quad := OrthogonalPoints(
          pnts[i+1].X, pnts[i+1].Y, pnts[i+2].X, pnts[i+2].Y, Pen.Width);

        p2 := quad.p[2];
        q2 := quad.p[3];

        k1 := IntersectPoint(v1, v2, p1, p2);
        k2 := IntersectPoint(v1, v2, q1, q2);

        Vertices[v].Color := pnts[i].Color;
        Vertices[v].Position := AdVector3(k1.x, k1.y, 0); inc(v);
        Vertices[v].Color := pnts[i].Color;
        Vertices[v].Position := AdVector3(k2.x, k2.y, 0); inc(v);
      end
      else
      begin
        primitivecount := primitivecount - 2;
      end;


      i := i + 1;
    end;

    maxlen := maxlen + sqrt(sqr(pnts[i].X-pnts[i+1].X)+sqr(pnts[i].Y-pnts[i+1].Y));
    
    quad := OrthogonalPoints(
      pnts[High(pnts)-1].X, pnts[High(pnts)-1].Y,
      pnts[High(pnts)].X, pnts[High(pnts)].Y,
      Pen.Width);

    Vertices[v].Color := pnts[i].Color;
    Vertices[v].Position := AdVector3(quad.p[2].x, quad.p[2].y, 0); inc(v);
    Vertices[v].Color := pnts[i].Color;
    Vertices[v].Position := AdVector3(quad.p[3].x, quad.p[3].y, 0); inc(v);

    SetLength(Vertices, v);

    for i := 0 to Length(Vertices)-1 do
    begin
      Vertices[i].Normal := AdVector3(0,0,-1);
    end;

    //Calculate Texture coordinate
    if Pen.Texture <> nil then
    begin
      GenerateTextureCoords(maxlen, Vertices);
    end;

    FMesh.Vertices := Vertices;
    FMesh.PrimitiveCount := primitivecount;
    FMesh.Texture := FPen.Texture;
    FMesh.Update;
    FMesh.Matrix := AdMatrix_Identity;
  end;
end;

procedure TAdCanvasLine.GenerateTextureCoords(maxlen: double;
  var Vertices: TAdVertexArray);
var
  i : integer;
  ay1,ay2,len:double;
begin
  ay1 := 0.5 - (FPen.Width / 2) / (Pen.Texture.Height);
  ay2 := 0.5 + (FPen.Width / 2) / (Pen.Texture.Height);

  for i := 0 to Length(Vertices) div 2 - 1 do
  begin
    if Pen.TextureMode = tmTile then
    begin
      Vertices[i*2+0].Texture.Y := ay1;
      Vertices[i*2+1].Texture.Y := ay2;
    end else
    begin
      Vertices[i*2+0].Texture.Y := 0;
      Vertices[i*2+1].Texture.Y := 1;
    end;

    if i = 0 then
    begin
      Vertices[0].Texture.X := 0;
      Vertices[1].Texture.X := 0;
    end
    else
    begin
      len :=
        (sqrt(sqr(Vertices[i*2-2].Position.X - Vertices[i*2+0].Position.X)   +
              sqr(Vertices[i*2-2].Position.Y - Vertices[i*2+0].Position.Y))  +
         sqrt(sqr(Vertices[i*2-1].Position.X - Vertices[i*2+1].Position.X)   +
              sqr(Vertices[i*2-1].Position.Y - Vertices[i*2+1].Position.Y))) / 2;

      if Pen.TextureMode = tmTile then
      begin
        Vertices[i*2+0].Texture.X := Vertices[i*2-2].Texture.X + len / Pen.Texture.Width;
        Vertices[i*2+1].Texture.X := Vertices[i*2-1].Texture.X + len / Pen.Texture.Width;
      end else
      begin
        Vertices[i*2+0].Texture.X := Vertices[i*2-2].Texture.X + len / maxlen;
        Vertices[i*2+1].Texture.X := Vertices[i*2-1].Texture.X + len / maxlen;
      end;
    end;
  end;
end;

procedure TAdCanvasLine.HashPoint(APoint: TAdLinePoint);
begin
  FHash := FHash +
    ((APoint.X * FPoints.Count) + (APoint.Y * FPoints.Count * 10)) +
    (APoint.Color.r * FPoints.Count * 100) +
    (APoint.Color.g * FPoints.Count * 1000) +
    (APoint.Color.b * FPoints.Count * 10000) +
    (APoint.Color.a * FPoints.Count * 100000);
end;

procedure TAdCanvasLine.Update(AItem: TAdCanvasObject);
var
  p:PAdLinePoint;
begin
  FPoints.StartIteration;
  TAdCanvasLine(AItem).Points.StartIteration;

  FHash := 0;

  while not FPoints.ReachedEnd do
  begin
    p := PAdLinePoint(TAdCanvasLine(AItem).Points.GetCurrent);
    PAdLinePoint(FPoints.GetCurrent)^ := p^;
    HashPoint(p^);
  end;

  Generate;
end;

{ TAdCanvasQuad }

constructor TAdCanvasQuadObject.Create(AAppl: TAd2dApplication);
begin
  inherited Create(AAppl);

  FMesh := Appl.CreateMesh;
  FLine := TAdCanvasLine.Create(Appl);
  FOwnPen := false;
end;

destructor TAdCanvasQuadObject.Destroy;
begin
  FLine.Free;
  FMesh.Free;
  
  if FOwnPen then
  begin
    FPen.Free;
    FBrush.Free;
  end;
  inherited;
end;


function TAdCanvasQuadObject.CompareTo(AItem: TAdCanvasObject): TAdCanvasUpdateState;
var
  i: Integer;
begin
  result := usDelete;
  if AItem is TAdCanvasQuadObject then
  begin
    if (AItem.Brush.EqualTo(Brush)) and (AItem.Pen.EqualTo(Pen)) then
    begin
      result := usEqual;

      for i := 0 to 3 do
      begin
        if not ((FQuad.p[i].x = TAdCanvasQuadObject(AItem).Quad.p[i].x) and
               (FQuad.p[i].y = TAdCanvasQuadObject(AItem).Quad.p[i].y) and
               (CompareColors(FQuad.c[i], TAdCanvasQuadObject(AItem).Quad.c[i]))) then
        begin
          result := usUpdate;
          exit;
        end;
      end;
    end;
  end;
end;

procedure TAdCanvasQuadObject.Draw;
begin
  //Draw rectangle
  if (FBrush.Style <> abClear) then
  begin
    FMesh.Draw(FBrush.BlendMode,FDrawMode);
  end;

  //Draw outer line
  if (FPen.Style <> apNone) then
  begin
    FLine.Draw;
  end;
end;

procedure TAdCanvasQuadObject.Generate;
var
  APen:TAdPen;
  ABrush:TAdBrush;
  i:integer;
  p:TAdLinePoint;
  vertices:TAdVertexArray;
begin
  CalcQuadSizes(quad);
  
  //Copy pen and brush if necessary
  if not FOwnPen then
  begin
    FOwnPen := true;

    APen := FPen;
    FPen := TAdPen.Create;
    FPen.Assign(APen);

    ABrush := FBrush;
    FBrush := TAdBrush.Create;
    FBrush.Assign(ABrush);
  end;

  if (FPen.Style <> apNone) then
  begin
    //Set line object properties if necessary
    if (FLine.Points.Count < 4) then
    begin
      if FLine.Points.Count > 0 then
      begin
        FLine.Free;
        FLine := TAdCanvasLine.Create(FAppl);
      end;
      
      FLine.Pen := FPen;
      for i := 0 to 3 do
      begin
        p.X := round(FQuad.p[i].X);
        p.Y := round(FQuad.p[i].Y);
        p.Color := FPen.Color;
        FLine.AddPoint(p)
      end;

      //Close line
      p.X := round(FQuad.p[0].X);
      p.Y := round(FQuad.p[0].Y);
      p.Color := FPen.Color;
      FLine.AddPoint(p);

      FLine.Generate;
    end
    else
    begin
      FLine.Points.StartIteration;
      for i := 0 to 3 do
      begin
        p.X := round(FQuad.p[i].X);
        p.Y := round(FQuad.p[i].Y);
        p.Color := FPen.Color;
        PAdLinePoint(FLine.Points.GetCurrent)^ := p;
      end;

      //Clsoe line
      p.X := round(FQuad.p[0].X);
      p.Y := round(FQuad.p[0].Y - trunc(Pen.Width / 2));
      p.Color := FPen.Color;
      PAdLinePoint(FLine.Points.GetCurrent)^ := p;

      FLine.Generate;
    end;
  end;

  if (FBrush.Style <> abClear) then
  begin

    if (FBrush.Texture = nil) or (FBrush.TextureMode <> tmStretchAlign) then
    begin
      SetLength(vertices, 4);

      FDrawMode := adTriangleStrips;

      vertices[0].Position := AdVector3(FQuad.p[0].x,FQuad.p[0].y, 0);
      vertices[0].Color := FQuad.c[0];
      vertices[1].Position := AdVector3(FQuad.p[1].x,FQuad.p[1].y, 0);
      vertices[1].Color := FQuad.c[1];
      vertices[2].Position := AdVector3(FQuad.p[3].x,FQuad.p[3].y, 0);
      vertices[2].Color := FQuad.c[3];
      vertices[3].Position := AdVector3(FQuad.p[2].x,FQuad.p[2].y, 0);
      vertices[3].Color := FQuad.c[2];

      if FBrush.Texture <> nil then
      begin
        GenerateTextureCoords(vertices);
      end;

      FMesh.PrimitiveCount := 2;
    end
    else
    begin
      SetLength(vertices,6);

      vertices[0].Position := AdVector3(FMinX + FWidth / 2, FMinY + FHeight / 2, 0);
      vertices[0].Color := FQuad.c[0];
      vertices[0].Texture := AdVector2(0.5,0.5);
      vertices[1].Position := AdVector3(FQuad.p[0].x,FQuad.p[0].y, 0);
      vertices[1].Color := FQuad.c[0];
      vertices[1].Texture := AdVector2(0,0);
      vertices[2].Position := AdVector3(FQuad.p[1].x,FQuad.p[1].y, 0);
      vertices[2].Color := FQuad.c[1];
      vertices[2].Texture := AdVector2(1,0);
      vertices[3].Position := AdVector3(FQuad.p[2].x,FQuad.p[2].y, 0);
      vertices[3].Color := FQuad.c[2];
      vertices[3].Texture := AdVector2(1,1);
      vertices[4].Position := AdVector3(FQuad.p[3].x,FQuad.p[3].y, 0);
      vertices[4].Color := FQuad.c[3];
      vertices[4].Texture := AdVector2(0,1);
      vertices[5].Position := AdVector3(FQuad.p[0].x,FQuad.p[0].y, 0);
      vertices[5].Color := FQuad.c[0];
      vertices[5].Texture := AdVector2(0,0);
      FMesh.PrimitiveCount := 4;

      FDrawMode := adTriangleFan;
    end;

    SetNormals(vertices);
    
    FMesh.Vertices := vertices;
    FMesh.Indices := nil;
    FMesh.Update;
    FMesh.Matrix := AdMatrix_Identity;
    FMesh.Texture := FBrush.Texture;
  end;
end;

procedure TAdCanvasQuadObject.CalcQuadSizes(aquad:TAdCanvasColorQuad);
var
  i:integer;
begin
  FMinX := aquad.p[0].x;
  FMinY := aquad.p[0].y;
  FMaxX := aquad.p[0].x;
  FMaxY := aquad.p[0].y;

  for i := 1 to High(aquad.p) do
  begin
    if aquad.p[i].x > FMaxX then
      FMaxX := aquad.p[i].x;

    if aquad.p[i].x < FMinX then
      FMinX := aquad.p[i].x;

    if aquad.p[i].y > FMaxY then
      FMaxY := aquad.p[i].y;

    if aquad.p[i].y < FMinY then
      FMinY := aquad.p[i].y;
  end;

  FWidth := FMaxX - FMinX;
  FHeight := FMaxY - FMinY;
end;

procedure TAdCanvasQuadObject.GenerateTextureCoords(
  var vertices: TAdVertexArray);
var
  i: integer;
  sx,sy: single;
begin
  if FBrush.TexturePosition = tpStatic then
  begin
    sx := 0;
    sy := 0;
  end
  else
  begin
    sx := FMinX;
    sy := FMinY;
  end;    

  if FBrush.TextureMode = tmTile then
  begin
    for i := 0 to high(vertices) do
    begin
      vertices[i].Texture := AdVector2(
        (vertices[i].Position.x - sx) / FBrush.FTexture.Width,
        (vertices[i].Position.y - sy) / FBrush.FTexture.Height);
    end;
  end else
  if FBrush.TextureMode = tmStretch then
  begin
    for i := 0 to high(vertices) do
    begin
      vertices[i].Texture := AdVector2(
        (vertices[i].Position.x - sx) / FWidth,
        (vertices[i].Position.y - sy) / FHeight);
    end;
  end else
  if FBrush.TextureMode = tmStretchAlign then
  begin
    vertices[0].Texture := AdVector2(0,0);
    vertices[1].Texture := AdVector2(1,0);
    vertices[2].Texture := AdVector2(0,1);
    vertices[3].Texture := AdVector2(1,1);
  end;
end;

procedure TAdCanvasQuadObject.SetMatrix(AValue: TAdMatrix);
begin
  FMatrix := AValue;
  FMesh.Matrix := AValue;
  FLine.SetMatrix(AValue);
end;

procedure TAdCanvasQuadObject.SetNormals(var vertices: TAdVertexArray);
var
  i:integer;
begin
  for i := 0 to High(vertices) do
  begin
    vertices[i].Normal := AdVector3(0,0,-1);
  end;    
end;

procedure TAdCanvasQuadObject.SetQuad(AQuad: TAdCanvasColorQuad);
begin
  FQuad := AQuad;
end;

procedure TAdCanvasQuadObject.Update(AItem: TAdCanvasObject);
begin
  FQuad := TAdCanvasQuadObject(AItem).Quad;
  Generate;
end;

{ TAdCanvasEllipseObject }

constructor TAdCanvasEllipseObject.Create(AAppl: TAd2dApplication);
begin
  inherited Create(AAppl);

  FMesh := Appl.CreateMesh;
  FLine := TAdCanvasLine.Create(Appl);
  FOwnPen := false;
end;

destructor TAdCanvasEllipseObject.Destroy;
begin
  FLine.Free;
  FMesh.Free;
  
  if FOwnPen then
  begin
    FPen.Free;
    FBrush.Free;
  end;

  inherited;
end;

procedure TAdCanvasEllipseObject.Draw;
begin
  //Draw filling
  if FBrush.Style <> abClear then
  begin
    FMesh.Draw(FBrush.BlendMode, adTriangleFan);
  end;

  //Draw outer line
  if FPen.Style <> apNone then
  begin
    FLine.Draw;
  end;
end;

function TAdCanvasEllipseObject.CompareTo(
  AItem: TAdCanvasObject): TAdCanvasUpdateState;
begin
  result := usDelete;
  if AItem is TAdCanvasEllipseObject then
  begin
    if (AItem.Brush.EqualTo(Brush)) and (AItem.Pen.EqualTo(Pen)) then
    begin
      result := usUpdate;

      if (TAdCanvasEllipseObject(AItem).Pos.Left = FPos.Left) and
         (TAdCanvasEllipseObject(AItem).Pos.Right = FPos.Right) and
         (TAdCanvasEllipseObject(AItem).Pos.Top = FPos.Top) and
         (TAdCanvasEllipseObject(AItem).Pos.Bottom = FPos.Bottom) then
      begin
        result := usEqual;
      end;
    end;
  end;
end;

procedure TAdCanvasEllipseObject.CalcCenter;
begin
  FCenterX := (FPos.Left + FPos.Right) / 2;
  FCenterY := (FPos.Top + FPos.Bottom) / 2;
  FWidth := FPos.Right - FPos.Left;
  FHeight := FPos.Bottom - FPos.Top;
end;

procedure TAdCanvasEllipseObject.Generate;
var
  i:integer;
  steps:integer;
  vertices:TAdVertexArray;
  ax, ay, ar, w, v:double;
  lx,ly:integer;
  lp:TAdLinePoint;
  APen:TAdPen;
  ABrush:TAdBrush;
begin
  CalcCenter;

  //Copy pen and brush if necessary
  if not FOwnPen then
  begin
    FOwnPen := true;

    APen := FPen;
    FPen := TAdPen.Create;
    FPen.Assign(APen);

    ABrush := FBrush;
    FBrush := TAdBrush.Create;
    FBrush.Assign(ABrush);
  end;

  ar := (abs(FWidth) + abs(FHeight)) / 4;
  steps := round(Pi * 2 * ar * (0.1/(ar*0.01)));

  if FBrush.Style <> abClear then
  begin
    SetLength(vertices, steps + 1);

    Vertices[0].Position := AdVector3(FCenterX, FCenterY, 0);
    Vertices[0].Color := FBrush.Color;
    Vertices[0].Normal := AdVector3(0, 0, -1);
  end;

  if FPen.Style <> apNone then
  begin
    if FLine.Points.Count <> 0 then
    begin
      FLine.Free;
      FLine := TAdCanvasLine.Create(FAppl);
    end;
    FLine.Pen := FPen;
  end;

  v := FHeight / FWidth;
  lx := 0;
  ly := 0;

  for i := 0 to steps-2 do
  begin
    w := (2 * PI) / (steps-1) * i;
    ax := FCenterX + cos(w) * (FWidth / 2);
    ay := FCenterY + sin(w) * (FWidth / 2) * v;
    if FBrush.Style <> abClear then
    begin
      Vertices[i+1].Position := AdVector3(ax, ay, 0);
      case FBrush.Style of
        abSolid: Vertices[i+1].Color := FBrush.Color;
        abGradient: Vertices[i+1].Color := FBrush.GradientColor;
      end;                                                      
      Vertices[i+1].Normal := AdVector3(0, 0, -1);
    end;
    if FPen.Style <> apNone then
    begin
      if (round(ax) <> lx) or (round(ay) <> ly) then
      begin
        lp.X := round(ax);
        lp.Y := round(ay);
        lp.Color := FPen.Color;
        FLine.AddPoint(lp);
      end;
    end;
    lx := round(ax);
    ly := round(ay);
  end;

  //Close circle
  ax := FCenterX + cos(0) * (FWidth / 2);
  ay := FCenterY + sin(0) * (FWidth / 2) * v;

  if FPen.Style <> apNone then
  begin
    if (round(ax) <> lx) or (round(ay) <> ly) then
    begin
      lp.X := round(ax);
      lp.Y := round(ay);
      lp.Color := FPen.Color;
      FLine.AddPoint(lp);
    end;
    FLine.Generate;
  end;

  if FBrush.Style <> abClear then
  begin
    Vertices[steps].Position := AdVector3(ax, ay, 0);
    case FBrush.Style of
      abSolid: Vertices[steps].Color := FBrush.Color;
      abGradient: Vertices[steps].Color := FBrush.GradientColor;
    end;                                                      
    Vertices[steps].Normal := AdVector3(0, 0, -1);

    if FBrush.Texture <> nil then
    begin
      GenerateTextureCoords(Vertices);
    end;

    FMesh.PrimitiveCount := steps - 1;

    FMesh.Vertices := vertices;
    FMesh.Indices := nil;
    FMesh.Update;
    FMesh.Matrix := AdMatrix_Identity;
    FMesh.Texture := FBrush.Texture;
  end;
end;

procedure TAdCanvasEllipseObject.GenerateTextureCoords(
  var vertices: TAdVertexArray);
var
  i,c:integer;
  r:double;
  wx,wy,fac:double;
  mx, my:integer;
begin
  if FBrush.TexturePosition = tpStatic then
  begin
    mx := 0;
    my := 0;
  end
  else
  begin
    mx := FPos.Left;
    my := FPos.Top; 
  end;
  
  if FBrush.TextureMode = tmStretch then
  begin
    for i := 0 to High(Vertices) do
    begin
      Vertices[i].Texture.x := (Vertices[i].Position.x - FPos.Left + mx) / FWidth;
      Vertices[i].Texture.y := (Vertices[i].Position.y - FPos.Top + my) / FHeight;
    end;
  end else
  if FBrush.TextureMode = tmTile then
  begin
    for i := 0 to High(Vertices) do
    begin
      Vertices[i].Texture.x := (Vertices[i].Position.x - FPos.Left + mx) / FBrush.Texture.Width;
      Vertices[i].Texture.y := (Vertices[i].Position.y - FPos.Top + my) / FBrush.Texture.Height;
    end;
  end else
  if FBrush.TextureMode = tmStretchAlign then
  begin
    Vertices[0].Texture := AdVector2(0.5,0.5);
    c := High(Vertices)-1;
    for i := 1 to High(Vertices) do
    begin
      r := (2*PI)/c * (i-1);

      wx := cos(r)/2+0.5;
      wy := sin(r)/2+0.5;
      fac := abs(cos(r));

      wx := round(wx) * fac + wx * (1-fac);
      wy := wy * fac + round(wy) * (1-fac);

      Vertices[i].Texture := AdVector2(wx,wy);
    end;
  end;
end;

procedure TAdCanvasEllipseObject.Update(AItem: TAdCanvasObject);
begin
  FPos := TAdCanvasEllipseObject(AItem).Pos;
  Generate;
end;

procedure TAdCanvasEllipseObject.SetMatrix(AValue: TAdMatrix);
begin
  FMatrix := AValue;
  FMesh.Matrix := AValue;
  FLine.SetMatrix(AValue);
end;

procedure TAdCanvasEllipseObject.SetPos(ARect: TAdRect);
begin
  FPos := ARect;
end;

{ TAdCanvasTextObject }

constructor TAdCanvasTextObject.Create(AAppl: TAd2dApplication);
begin
  inherited;

  FDrawFont := TAdFont.Create(AAppl);
end;

destructor TAdCanvasTextObject.Destroy;
begin
  FDrawFont.Free;
  inherited;
end;

function TAdCanvasTextObject.CompareTo(
  AItem: TAdCanvasObject): TAdCanvasUpdateState;
begin
  result := usDelete;
  if AItem is TAdCanvasTextObject then
  begin
    if (TAdCanvasTextObject(AItem).Font = FFont) then
    begin
      result := usUpdate;
      if Pen.EqualTo(AItem.Pen) and (TAdCanvasTextObject(AItem).Text = FText) and
         (CompareRects(TAdCanvasTextObject(AItem).Rect,FRect)) and
         (TAdCanvasTextObject(AItem).Font.TypeSetter.CompareTo(FDrawFont.TypeSetter)) then
      begin
        result := usEqual;
      end;
    end;
  end;
end;

procedure TAdCanvasTextObject.Draw;
begin
  if (FRect.Left = FRect.Right) and (FRect.Top = FRect.Bottom) then
  begin
    FDrawFont.TextOut(FRect.Left, FRect.Top, FText);
  end
  else
  begin
    FDrawFont.TextOut(FRect, FText);
  end;
end;

procedure TAdCanvasTextObject.Generate;
begin
  if FDrawFont.Texture = nil then
  begin
    FDrawFont.Color := FPen.Color;
    FDrawFont.Assign(FFont);
  end;  
end;

procedure TAdCanvasTextObject.SetMatrix(AValue: TAdMatrix);
begin
  FDrawFont.TransformationMatrix := AValue;
end;

procedure TAdCanvasTextObject.Update(AItem: TAdCanvasObject);
begin
  Pen.Assign(AItem.Pen);
  FText := TAdCanvasTextObject(AItem).Text;
  FRect := TAdCanvasTextObject(AItem).Rect;
  FDrawFont.TypeSetter.Assign(TAdCanvasTextObject(AItem).Font.TypeSetter);
  FFont.Color := FPen.Color;
end;

{ TAdCanvasPointObject }

constructor TAdCanvasPointsObject.Create(AAppl: TAd2dApplication);
begin
  inherited Create(AAppl);

  FPoints := TAdLinkedList.Create;
  FMesh := Appl.CreateMesh;
end;

destructor TAdCanvasPointsObject.Destroy;
begin
  FPoints.StartIteration;
  while not FPoints.ReachedEnd do
  begin
    Dispose(PAdLinePoint(FPoints.GetCurrent));
  end;
  FPoints.Free;
  FMesh.Free;  
  inherited;
end;

procedure TAdCanvasPointsObject.AddPoint(APoint: TAdLinePoint);
var
  PPoint:PAdLinePoint;
begin
  HashPoint(APoint);
  New(PPoint);
  PPoint^ := APoint;
  FPoints.Add(PPoint);
end;

function TAdCanvasPointsObject.CompareTo(
  AItem: TAdCanvasObject): TAdCanvasUpdateState;
begin
  result := usDelete;
  if AItem is TAdCanvasPointsObject then
  begin
    if (FPoints.Count = TAdCanvasPointsObject(AItem).Points.Count) and
       (AItem.Pen.EqualTo(Pen)) then
    begin
      result := usUpdate;

      if Hash = TAdCanvasPointsObject(AItem).Hash then
      begin
        result := usEqual;
      end;
    end;
  end;
end;

procedure TAdCanvasPointsObject.Draw;
begin
  FMesh.Draw(FBrush.BlendMode, adPoints);
end;

procedure TAdCanvasPointsObject.Generate;
var
  Vertices:TAdVertexArray;
  PPoint:PAdLinePoint;
  i:integer;
begin
  SetLength(Vertices, FPoints.Count);
  i := 0;
  FPoints.StartIteration;
  while not FPoints.ReachedEnd do
  begin
    PPoint := PAdLinePoint(FPoints.GetCurrent);
    Vertices[i].Position := AdVector3(PPoint^.X, PPoint^.Y, 0);
    Vertices[i].Color := PPoint^.Color;
    Vertices[i].Normal := AdVector3(0, 0, -1);
    i := i + 1;
  end;

  FMesh.Vertices := Vertices;
  FMesh.Indices := nil;
  FMesh.PrimitiveCount := FPoints.Count;
  FMesh.Update;
end;

procedure TAdCanvasPointsObject.HashPoint(APoint: TAdLinePoint);
begin
  FHash := FHash +
    ((APoint.X * FPoints.Count) + (APoint.Y * FPoints.Count * 10)) +
    (APoint.Color.r * FPoints.Count * 100) +
    (APoint.Color.g * FPoints.Count * 1000) +
    (APoint.Color.b * FPoints.Count * 10000) +
    (APoint.Color.a * FPoints.Count * 100000);
end;

procedure TAdCanvasPointsObject.Rehash;
begin
  FHash := 0;

  FPoints.StartIteration;
  while not FPoints.ReachedEnd do
  begin
    HashPoint(PAdLinePoint(FPoints.GetCurrent)^);
  end;
end;

procedure TAdCanvasPointsObject.Update(AItem: TAdCanvasObject);
var
  p:PAdLinePoint;
begin
  FPoints.StartIteration;
  TAdCanvasPointsObject(AItem).Points.StartIteration;

  FHash := 0;

  while not FPoints.ReachedEnd do
  begin
    p := PAdLinePoint(TAdCanvasPointsObject(AItem).Points.GetCurrent);
    PAdLinePoint(FPoints.GetCurrent)^ := p^;
    HashPoint(p^);
  end;

  Generate;
end;

procedure TAdCanvasPointsObject.SetMatrix(AValue: TAdMatrix);
begin
  FMesh.Matrix := AValue;
end;

{ TAdCanvasPolygonObject }

constructor TAdCanvasPolygonObject.Create(AAppl: TAd2dApplication);
begin
  inherited Create(AAppl);

  FMesh := Appl.CreateMesh;
end;

destructor TAdCanvasPolygonObject.Destroy;
begin
  FMesh.Free;  
  inherited;
end;

function TAdCanvasPolygonObject.CompareTo(
  AItem: TAdCanvasObject): TAdCanvasUpdateState;
begin
  result := usDelete;
  if AItem is TAdCanvasPolygonObject then
  begin
    if (Length(Polygon) = Length(TAdCanvasPolygonObject(AItem).Polygon)) and
       (AItem.Pen.EqualTo(Pen) and AItem.Brush.EqualTo(Brush)) then
    begin
      result := usUpdate;

      if CompareMem(
        @Polygon[0], @TAdCanvasPolygonObject(AItem).Polygon[0],
        SizeOf(Polygon) * SizeOf(TAdPoint)) then
      begin
        result := usEqual;
      end;
    end;
  end;
end;

procedure TAdCanvasPolygonObject.Draw;
begin
  FMesh.Draw(Pen.BlendMode, adTriangles);
end;

procedure TAdCanvasPolygonObject.Generate;
var
  Triangles : TAdTriangles;
  Vertices : TAdVertexArray;
  i : integer;
begin
  Triangulate(FPolygon, Triangles);
  begin
    SetLength(Vertices, Length(Triangles) * 3);
    for i := 0 to High(Triangles) do
    begin
      Vertices[i*3+0].Position :=
        AdVector3(Triangles[i][0].X, Triangles[i][0].Y, 0);
      Vertices[i*3+1].Position :=
        AdVector3(Triangles[i][1].X, Triangles[i][1].Y, 0);
      Vertices[i*3+2].Position :=
        AdVector3(Triangles[i][2].X, Triangles[i][2].Y, 0);
    end;

    for i := 0 to High(Vertices) do
    begin
      Vertices[i].Normal := AdVector3(0,0,-1);
      Vertices[i].Texture := AdVector2(0,0);
      Vertices[i].Color := Brush.Color;
    end;

    if Brush.Texture <> nil then
      GenerateTextureCoords(Vertices);

    FMesh.Vertices := Vertices;
    FMesh.Indices := nil;
    FMesh.Texture := Brush.Texture;
    FMesh.Matrix := AdMatrix_Identity;
    FMesh.PrimitiveCount := Length(Triangles);
    FMesh.Update;
  end;
end;

procedure TAdCanvasPolygonObject.GenerateTextureCoords(
  var vertices: TAdVertexArray);
var
  i:integer;
  mx, my:single;
  left, top, bottom, right, width, height: single;
begin
  left := 0; top := 0; bottom := 0; right := 0;
  for i := 0 to High(vertices) do
  begin
    with vertices[i].Position do
    begin
      if (i = 0) or (x < left) then left := x;
      if (i = 0) or (y < top) then top := y;
      if (i = 0) or (x > right) then right := x;
      if (i = 0) or (y > bottom) then bottom := y;
    end;
  end;
  width := right - left;
  height := bottom - top;
  
  if FBrush.TexturePosition = tpStatic then
  begin
    mx := 0;
    my := 0;
  end
  else
  begin
    mx := left;
    my := top;
  end;
  
  if FBrush.TextureMode = tmStretch then
  begin
    for i := 0 to High(Vertices) do
    begin
      Vertices[i].Texture.x := (Vertices[i].Position.x - left + mx) / width;
      Vertices[i].Texture.y := (Vertices[i].Position.y - top + my) / height;
    end;
  end else
  begin
    for i := 0 to High(Vertices) do
    begin
      Vertices[i].Texture.x := (Vertices[i].Position.x - left + mx) / FBrush.Texture.Width;
      Vertices[i].Texture.y := (Vertices[i].Position.y - top + my) / FBrush.Texture.Height;
    end;
  end;
end;

procedure TAdCanvasPolygonObject.SetMatrix(AValue: TAdMatrix);
begin
  FMesh.Matrix := AValue;
end;

procedure TAdCanvasPolygonObject.Update(AItem: TAdCanvasObject);
begin
  FPolygon := TAdCanvasPolygonObject(AItem).Polygon;
  Generate;
end;

{ TAdCanvasSpline }

constructor TAdCanvasSplineObject.Create(AAppl: TAd2DApplication);
begin
  inherited Create(AAppl);

  FPoints := TAdLinkedList.Create;
  FOwnPen := false;
end;

destructor TAdCanvasSplineObject.Destroy;
begin
  ClearData;
  FPoints.Free;

  if FOwnPen then
  begin
    FPen.Free;
    FBrush.Free;
  end;
  
  inherited;
end;

procedure TAdCanvasSplineObject.AddPoint(APoint: TAdLinePoint);
var
  PPoint:PAdLinePoint;
begin
  if FLastPoint <> nil then
  begin
    if (FLastPoint^.X = APoint.X) and
       (FLastPoint^.Y = APoint.Y) then exit;
  end;
  HashPoint(APoint);
  New(PPoint);
  PPoint^ := APoint;
  FPoints.Add(PPoint);
  FLastPoint := PPoint;
end;

procedure TAdCanvasSplineObject.ClearData;
begin
  //Free the line object if it isn't nil
  if FLine <> nil then
    FLine.Free;

  FLine := nil;

  //Free all points conected to the list
  FPoints.StartIteration;
  while not FPoints.ReachedEnd do
    Dispose(PAdLinePoint(FPoints.GetCurrent));

  FPoints.Clear;
end;

function TAdCanvasSplineObject.CompareTo(AItem: TAdCanvasObject): TAdCanvasUpdateState;
begin
  result := usDelete;
  if AItem is TAdCanvasSplineObject then
  begin
    if (TAdCanvasSplineObject(AItem).FPoints.Count = FPoints.Count) and
       (TAdCanvasSplineObject(AItem).Hash = FHash) and
       (TAdCanvasSplineObject(AItem).Pen.EqualTo(FPen)) then
    begin
      result := usEqual;
    end;    
  end;
end;

procedure TAdCanvasSplineObject.Draw;
begin
  if FLine <> nil then  
    FLine.Draw;
end;

procedure TAdCanvasSplineObject.Generate;
var
  spline: TAdCubicSpline;
  p: PAdLinePoint;
  x, y: TAdFloatArray;
  cols: array of TAndorraColor;
  i, j, itstart, itstop, splstart, splcount, dist, c: integer;
  xp, yp: Single;
  t, cp, step: Single;
  APen: TAdPen;
  ABrush: TAdBrush;
  lp: TAdLinePoint;

  function Range(const AStart, AMin, AMax, ARange, ABorder: integer; var AItStart, AItStop, ASplStart, ASplCount: integer): boolean;
  var
    ASplStop: integer;
  begin
    AItStart := AStart;
    AItStop :=  AStart + ARange;
    ASplStart := AItStart - ABorder;
    ASplStop := AItStop + ABorder;

    if AItStart < AMin then
      AItStart := AMin;
    if ASplStart < AMin then
      ASplStart := AMin;
    if AItStop > AMax then
      AItStop := AMax;
    if ASplStop > AMax then
      ASplStop := AMax;

    ASplCount := ASplStop - ASplStart + 1;

    result := AStart < AMax;
  end;

  function InterpolateColor(ACol1, ACol2: TAndorraColor; APos: single): TAndorraColor;
  begin
    with result do
    begin
      r := round(ACol1.r * (1 - APos) + ACol2.r * APos);
      g := round(ACol1.g * (1 - APos) + ACol2.g * APos);
      b := round(ACol1.b * (1 - APos) + ACol2.b * APos);
      a := round(ACol1.a * (1 - APos) + ACol2.a * APos);
    end;
  end;
  
begin
  if FLine <> nil then
    FLine.Free;
  FLine := nil;

  //Exit if there are less than two points in the list
  if FPoints.Count <= 1 then
    exit;

  FLine := TAdCanvasLine.Create(Appl);

  //Copy all components of the points to a seperate array
  SetLength(x, FPoints.Count);
  SetLength(y, FPoints.Count);
  SetLength(cols, FPoints.Count);

  i := 0;     
  FPoints.StartIteration;
  while not FPoints.ReachedEnd do
  begin
    p := FPoints.GetCurrent;

    x[i] := p^.X;
    y[i] := p^.Y;
    cols[i] := p^.Color;

    i := i + 1;
  end;

  //Copy pen and brush if necessary
  if not FOwnPen then
  begin
    FOwnPen := true;

    APen := FPen;
    FPen := TAdPen.Create;
    FPen.Assign(APen);

    ABrush := FBrush;
    FBrush := TAdBrush.Create;
    FBrush.Assign(ABrush);
  end;

  //Create a new spline all 20 points
  itstart := 0;
  itstop := 0;
  splstart := 0;
  splcount := 0;
  xp := 0; yp := 0;
  while Range(itstart, 0, FPoints.Count - 1, 20, 5, itstart, itstop, splstart, splcount) do
  begin
    spline := TAdCubicSpline.Create(
      @x[splstart], @y[splstart], nil, splcount);

    //Iterate through all points in the list
    for i := itstart + 1 to itstop do
    begin
      //Calculate the distance between this and the last point
      dist :=
        Round(
          Sqrt(
            Sqr(x[i] - x[i - 1]) +
            Sqr(y[i] - y[i - 1])));

      //Calculate the step "t" has to make after a point was added
      //to the line
      c := (dist div 10) - 1;
      if c <= 0 then
      begin
        c := 1;
        step := 1;
      end
      else
       step := 1 / c;

      t := (i - splstart) - 1;
      cp := 0;

      for j := 0 to c do
      begin
        //Get a point from the spline
        spline.SplineXY(t, xp, yp);

        //Increase "t" by "step"
        t := t + step;
        cp := cp + step;

        //Add the point to the line
        lp.X := round(xp);
        lp.Y := round(yp);
        lp.Color := InterpolateColor(cols[i - 1], cols[i], cp);
        FLine.AddPoint(lp);
      end;
    end;

    spline.Free;

    itstart := itstop;
  end;

  //Prepare the generated points for drawing
  FLine.Pen := FPen;
  FLine.Brush := FBrush;

  FLine.Generate;
end;

procedure TAdCanvasSplineObject.HashPoint(APoint: TAdLinePoint);
begin
  //Multiply the components of the point with some numbers to create a
  //value that changes with a little change in one of the value
  FHash := FHash xor
    ((APoint.X * FPoints.Count) + (APoint.Y * FPoints.Count * 10)) xor
    (PInteger(@APoint.Color)^);
end;

procedure TAdCanvasSplineObject.Rehash;
begin
  FHash := 0;

  FPoints.StartIteration;
  while not FPoints.ReachedEnd do
  begin
    HashPoint(PAdLinePoint(FPoints.GetCurrent)^);
  end;
end;

procedure TAdCanvasSplineObject.SetMatrix(AValue: TAdMatrix);
begin
  if FLine <> nil then
    FLine.Matrix := AValue;
end;

procedure TAdCanvasSplineObject.Update(AItem: TAdCanvasObject);
begin
  //Updating a spline is not possible now
end;

end.
