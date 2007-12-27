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

unit AdCanvas;

interface

uses
  Classes, AdClasses, AdTypes, AdContainers, AdList, AdFont, Math;

type
  TAdColors = array[0..3] of TAndorraColor;

  TAdBrushStyle = (abClear, abSolid, abGradient);
  TAdPenStyle = (apNone, apSolid);
  TAdPenPosition = (ppOuter,ppMiddle,ppInner);
  TAdCanvasTextureMode = (tmTile, tmStretch, tmStretchAlign);
  TAdCanvasTexturePosition = (tpStatic, tpDynamic);
  TAdCanvasGradientDirection = (gdVertical, gdHorizontal);

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
      property Color:TAndorraColor read FColor write SetColor;
      property GradientColor:TAndorraColor read FGradientColor write SetGradientColor;
      property GradientDirecton:TAdCanvasGradientDirection read FGradientDirection write SetGradientDirection;
      property Style:TAdBrushStyle read FStyle write SetStyle;
      property Texture:TAd2dTexture read FTexture write SetTexture;
      property TextureMode:TAdCanvasTextureMode read FTextureMode write FTextureMode;
      property TexturePosition:TAdCanvasTexturePosition read FTexturePosition write FTexturePosition;
      property BlendMode:TAd2dBlendMode read FBlendMode write FBlendMode;

      constructor Create;
      procedure SaveToStream(AStream:TStream);
      procedure LoadFromStream(AStream:TStream);
      procedure Assign(ABrush:TAdBrush);
      function EqualTo(ABrush:TAdBrush):Boolean;
  end;

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
      property Color:TAndorraColor read FColor write SetColor;
      property Width:single read FWidth write SetWidth;
      property Texture:TAd2dTexture read FTexture write SetTexture;
      property TextureMode:TAdCanvasTextureMode read FTextureMode write FTextureMode;
      property TexturePosition:TAdCanvasTexturePosition read FTexturePosition write FTexturePosition;
      property PenPosition:TAdPenPosition read FPenPosition write FPenPosition;
      property Style:TAdPenStyle read FStyle write SetStyle;
      property BlendMode:TAd2dBlendMode read FBlendMode write FBlendMode;

      constructor Create;
      procedure SaveToStream(AStream:TStream);
      procedure LoadFromStream(AStream:TStream);
      procedure Assign(APen:TAdPen);
      function EqualTo(APen:TAdPen):Boolean;
  end;

  TAdCanvasUpdateState = (usEqual, usUpdate, usDelete);

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
      constructor Create(AAppl:TAd2DApplication);
      destructor Destroy;override;

      procedure Draw;virtual;abstract;
      function CompareTo(AItem:TAdCanvasObject):TAdCanvasUpdateState;virtual;abstract;
      procedure Update(AItem:TAdCanvasObject);virtual;abstract;
      procedure Generate;virtual;abstract;

      property Brush:TAdBrush read FBrush write FBrush;
      property Pen:TAdPen read FPen write FPen;
      property Matrix:TAdMatrix read FMatrix write SetMatrix;
  end;

  PAdCanvasObjectList = ^TAdCanvasDisplayList;
  TAdCanvasDisplayList = class(TAdList)
    private
      FTransformMatrix:TAdMatrix;
      FAppl:TAd2dApplication;
      function GetItem(AIndex:integer):TAdCanvasObject;
      procedure SetItem(AIndex:integer; AValue:TAdCanvasObject);
    protected
      procedure Notify(Ptr:Pointer; Action:TListNotification);override;
    public
      property Items[Index:integer]:TAdCanvasObject read GetItem write SetItem; default;

      constructor Create(AAppl:TAd2dApplication);
      destructor Destroy;override;

      procedure ResetTransform;
      procedure Scale(AX, AY, AZ: single);
      procedure Translate(AX, AY, AZ: single);
      procedure RotateX(ARotation: single);
      procedure RotateY(ARotation: single);
      procedure RotateZ(ARotation: single);
      procedure MatrixTransform(AMatrix:TAdMatrix);

      procedure Draw;
  end;

  PAdLinePoint = ^TAdLinePoint;
  TAdLinePoint = record
    X,Y:integer;
    Color:TAndorraColor;
  end;

  TAdCanvasQuad = record
    p:array[0..3] of TAdVector2;
  end;

  TAdCanvasColorQuad = record
    p:array[0..3] of TAdVector2;
    c:array[0..3] of TAndorraColor;
  end;

  TAdCanvasLine = class(TAdCanvasObject)
    private
      FMesh:TAd2dMesh;
      FPoints:TAdLinkedList;
      FOwnPen:boolean;
      FHash:integer;
      FLastQuad:TAdCanvasQuad;
      procedure HashPoint(APoint:TAdLinePoint);
      procedure GenerateTextureCoords(maxlen:double;var vertices:TAdVertexArray);
      function OrthogonalPoints(x1, y1, x2, y2:integer; d: single): TAdCanvasQuad;
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

  TAdCanvas = class
    private
      FReleaseIndex:integer;
      FDrawIndex:integer;

      FAppl:TAd2DApplication;
      FDrawIn2d:boolean;
      FCurrentDisplayList:TAdCanvasDisplayList;

      FCurrentObject:TAdCanvasObject;
      FPen:TAdPen;
      FTempPen:TAdPen;
      FBrush:TAdBrush;
      FFont:TAdFont;

      procedure DeleteUnusedLists;
      procedure DeleteUnusedItems;
      procedure PushObject;

      procedure CreateLinesObject;
      procedure CreatePointsObject;
      procedure AddLinePoint(ax,ay:integer);
      procedure ColorQuad(var aquad:TAdCanvasColorQuad);
      procedure DoTextOut(ARect:TAdRect; AText:string);
    public
      FDisplayLists:TAdLinkedList;

      constructor Create(AAppl:TAd2dApplication);
      destructor Destroy;override;

      procedure StartFrame;
      procedure EndFrame;
      procedure Release;
      function ReturnDisplayList:TAdCanvasDisplayList;
      procedure DrawObject(var AObj:TAdCanvasObject);

      procedure MoveTo(ax,ay:integer);overload;
      procedure LineTo(ax,ay:integer);overload;
      procedure MoveTo(ap:TAdPoint);overload;
      procedure LineTo(ap:TAdPoint);overload;
      procedure Line(ax1,ay1,ax2,ay2:integer);overload;
      procedure Line(ap1,ap2:TAdPoint);overload;

      procedure Arrow(ArrowSize, ArrowAngle:Integer; P1, P2:TAdPoint);

      procedure TextOut(AX, AY:integer; AText:string);overload;

      procedure Rectangle(ax1,ay1,ax2,ay2:integer);overload;
      procedure Rectangle(ar:TAdRect);overload;
      procedure Rectangle(ap1,ap2:TAdPoint);overload;
      procedure Rectangle(ap:TAdPoint; awidth,aheight:integer);overload;

      procedure PlotPixel(ax, ay:integer);overload;
      procedure PlotPixel(ax, ay:integer;acolor:TAndorraColor);overload;

      procedure Ellipse(ax1,ay1,ax2,ay2:integer);
      procedure Circle(acx,acy,ar:integer);

      procedure DrawColoredQuad(aquad:TAdCanvasColorQuad);
      procedure DrawQuad(aquad:TAdCanvasQuad);

      property DrawIn2d:boolean read FDrawIn2d write FDrawIn2d;
      property Pen:TAdPen read FPen write FPen;
      property Brush:TAdBrush read FBrush write FBrush;
      property Font:TAdFont read FFont write FFont;
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
  result := inherited Items[AIndex];
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

    FCurrentDisplayList.Draw;

    FReleaseIndex := FReleaseIndex + 1;
    FDrawIndex := 0;
    if not FDisplayLists.ReachedEnd then
    begin
      FCurrentDisplayList := FDisplayLists.GetCurrent;
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
      FCurrentDisplayList := FDisplayLists.GetCurrent;
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
  begin
    AddLinePoint(ax,ay);
  end
  else
  begin
    MoveTo(ax,ay);
  end;
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

{ TAdCanvasLines }

constructor TAdCanvasLine.Create(AAppl: TAd2dApplication);
begin
  inherited;
  FPoints := TAdLinkedList.Create;
  FMesh := nil;
  FMesh := Appl.CreateMesh;
  FHash := 0;
  FOwnPen := false;
end;

destructor TAdCanvasLine.Destroy;
begin
  FPoints.StartIteration;
  while not FPoints.ReachedEnd do
  begin
    Dispose(FPoints.GetCurrent);
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
  HashPoint(APoint);
  New(PPoint);
  PPoint^ := APoint;
  FPoints.Add(PPoint);
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
      FMesh.Draw(FPen.BlendMode,adTriangles);
    end;
  end;
end;

function TAdCanvasLine.OrthogonalPoints(x1, y1, x2, y2:integer; d: single): TAdCanvasQuad;
var
  alpha:double;
  l:double;
  d1,d2:double;
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
  FMesh.SetMatrix(AValue);
end;

procedure TAdCanvasLine.Generate;
var
  i : integer;

  APen:TAdPen;

  Vertices : TAdVertexArray;
  Indices : TAdIndexArray;

  quad:TAdCanvasQuad;

  lp,cp:PAdLinePoint;
  maxlen:double;
begin
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
    FMesh.IndexBuffer := nil;
    FMesh.Update;
    FMesh.SetMatrix(AdMatrix_Identity);
  end
  else
  begin
    FillChar(quad, SizeOf(quad), 0);
    SetLength(Vertices,FPoints.Count * 2);
    SetLength(Indices,(FPoints.Count-1) * 6);

    //Calculate Vertices
    FPoints.StartIteration;
    i := 0;
    lp := nil;
    maxlen := 0;
    while not FPoints.ReachedEnd do
    begin
      cp := PAdLinePoint(FPoints.GetCurrent);

      if lp <> nil then
      begin
        FLastQuad := quad;
        quad := OrthogonalPoints(lp^.x, lp^.y, cp^.x, cp^.y, FPen.Width);

        if (Pen.Texture <> nil) and (Pen.TextureMode = tmStretch) then
        begin
          maxlen := maxlen +
            sqrt( sqr(lp^.X - cp^.X) + sqr(lp^.Y - cp^.Y));
        end;


        Vertices[i*2].Position   := AdVector3(quad.p[2].x, quad.p[2].y, 0);
        Vertices[i*2].Color := cp^.Color;
        Vertices[i*2].Normal := AdVector3(0,0,-1);
        Vertices[i*2+1].Position := AdVector3(quad.p[3].x, quad.p[3].y, 0);
        Vertices[i*2+1].Color := cp^.Color;
        Vertices[i*2+1].Normal := AdVector3(0,0,-1);

        if i = 1 then
        begin
          Vertices[0].Position   := AdVector3(quad.p[0].x, quad.p[0].y, 0);
          Vertices[0].Color := lp^.Color;
          Vertices[1].Position := AdVector3(quad.p[1].x, quad.p[1].y, 0);
          Vertices[1].Color := lp^.Color;
        end;
      end;

      lp := cp;
      i := i + 1;
    end;

    //Calculate Texture coordinate
    if Pen.Texture <> nil then
    begin
      GenerateTextureCoords(maxlen, Vertices);
    end;

    //Calculate Indices
    for i := 0 to FPoints.Count - 2 do
    begin
      Indices[i * 6]     := (i * 2) + 1;
      Indices[i * 6 + 1] := (i * 2);
      Indices[i * 6 + 2] := (i * 2) + 2;
      Indices[i * 6 + 3] := (i * 2) + 1;
      Indices[i * 6 + 4] := (i * 2) + 2;
      Indices[i * 6 + 5] := (i * 2) + 3;
    end;

    FMesh.Vertices := Vertices;
    FMesh.IndexBuffer := Indices;
    FMesh.PrimitiveCount := (FPoints.Count-1) * 2;
    FMesh.Texture := FPen.Texture;
    FMesh.Update;
    FMesh.SetMatrix(AdMatrix_Identity);
  end;
end;

procedure TAdCanvasLine.GenerateTextureCoords(maxlen: double;
  var Vertices: TAdVertexArray);
var
  i:integer;
  lp,cp:PAdLinePoint;
  ax,ay1,ay2,len:double;
begin
  //Stretch coordinates
  if Pen.TextureMode = tmStretch then
  begin
    FPoints.StartIteration;
    i := 0;
    lp := nil;
    ax := 0;
    while not FPoints.ReachedEnd do
    begin
      cp := PAdLinePoint(FPoints.GetCurrent);

      Vertices[i*2].Texture.Y := 0;
      Vertices[i*2+1].Texture.Y := 1;
      if lp = nil then
      begin
        Vertices[0].Texture.X := 0;
        Vertices[1].Texture.X := 0;
      end
      else
      begin
        len := sqrt(sqr(lp^.X - cp^.X) + sqr(lp^.Y - cp^.Y));
        ax := ax + len/maxlen;
        Vertices[i*2].Texture.X := ax;
        Vertices[i*2+1].Texture.X := ax;
      end;
      lp := cp;
      i := i + 1;
    end;
  end;

  //Tile Coordinates
  if Pen.TextureMode = tmTile then
  begin
    FPoints.StartIteration;
    i := 0;
    lp := nil;
    ax := 0;
    ay1 := 0.5 - (FPen.Width / 2) / (Pen.Texture.Height);
    ay2 := 0.5 + (FPen.Width / 2) / (Pen.Texture.Height);
    while not FPoints.ReachedEnd do
    begin
      cp := PAdLinePoint(FPoints.GetCurrent);

      Vertices[i*2].Texture.Y := ay1;
      Vertices[i*2+1].Texture.Y := ay2;

      if lp = nil then
      begin
        Vertices[0].Texture.X := 0;
        Vertices[1].Texture.X := 0;
      end
      else
      begin
        len := sqrt(sqr(lp^.X - cp^.X) + sqr(lp^.Y - cp^.Y));
        ax := ax + len/Pen.Texture.Width;
        Vertices[i*2].Texture.X := ax;
        Vertices[i*2+1].Texture.X := ax;
      end;
      lp := cp;
      i := i + 1;
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
    if (FLine.Points.Count = 0) then
    begin
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
      p.Y := round(FQuad.p[0].Y);
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
    FMesh.IndexBuffer := nil;
    FMesh.Update;
    FMesh.SetMatrix(AdMatrix_Identity);
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
  FMesh.SetMatrix(AValue);
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
    FMesh.IndexBuffer := nil;
    FMesh.Update;
    FMesh.SetMatrix(AdMatrix_Identity);
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
  FMesh.SetMatrix(AValue);
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
  FMesh.IndexBuffer := nil;
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
  FMesh.SetMatrix(AValue);
end;

end.
