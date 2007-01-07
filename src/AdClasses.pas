{
* This program is licensed under the GNU Lesser General Public License Version 2
* You should have recieved a copy of the license with this file.
* If not, see http://www.gnu.org/licenses/lgpl.html for more informations
*
* Project: Andorra 2D
* Author:  Andreas Stoeckel
* File: AndorraClasses.pas
* Comment: Contains all classes and types which are exchanged between the host
           and the plugin.
}

//Contains all classes and types which are exchanged between the host and the plugin.
unit AdClasses;

interface

uses SysUtils, Classes, Graphics, Types;

type
  //Represents an RGBA Color format with more than 8-Bit per channel. (But usually it is used as a 8-Bit format and the values are from 0 to 255. )
  TAndorraColor = record
    r,g,b,a:integer;
  end;

  //A simple vector
  TAdVector2 = record
    x,y:double;
  end;

  //Another simple vector with 3 parameters
  TAdVector3 = record
    x,y,z:double;
  end;

  //Andorras vertex format
  TAdVertex = record
    Position:TAdVector3;
    Color:TAndorraColor;
    Normal:TAdVector3;
    Texture:TAdVector2;
  end;

  //An array of the vertex
  TAdVertexArray = array of TAdVertex;

  //Represtents an index buffer
  TAdIndexArray = array of Word;

  //A matrix
  TAdMatrix = array[0..3] of array[0..3] of double;

  TAdOption = (
    doFullscreen, //< Specifies weather the application should run in the fullscreen mode or not
    doVSync, //< If turned on, the frame rate is equal to the vertical frequenzy of the screen
    doStretch, //< Should the picture be stretched when the window resizes?
    doHardware,//< Run in hardware mode? (WARNING: Should be set!)
    doZBuffer, //< The ZBuffer has to be used if you are using 3D Objects in your scene
    doAntialias,//< should Antialiasing be used
    doLights//Turn lights off/on.
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
    //The horizontal refresh rate
    Freq:integer;
  end;

  //A 32-Bit Bitmap
  TAdBitmap = class
    private
      FMemory:PByte;
      FWidth:integer;
      FHeight:integer;
      FSize:int64;
      procedure ClearMemory;
    protected
    public
      constructor Create;
      destructor Destroy;override;
      procedure ReserveMemory(AWidth,AHeight:integer);
      procedure AssignBitmap(ABitmap:TBitmap);
      procedure AssignAlphaChannel(ABitmap:TBitmap);
      procedure AssignToBitmap(ABitmap:TBitmap;AIgnoreAlphaChannel:boolean=true);
      procedure AssignAlphaChannelToBitmap(ABitmap:TBitmap);
      procedure SaveToStream(AStream:TStream);
      procedure LoadFromStream(AStream:TStream);
      function ScanLine(AY:integer):pointer;overload;
      function ScanLine:pointer;overload;
      function Loaded:boolean;
      property Width:integer read FWidth;
      property Height:integer read FHeight;
      property Size:int64 read FSize;
  end;

  TAdLogTyp = (ltInfo,ltWarning,ltError,ltFatalError,ltNone);

  TAdLogItem = record
    Text:PChar;
    Typ:TAdLogTyp;
  end;
  
  TAdLogProc = procedure(LogItem:TAdLogItem) of object;
  
  //TAdLight = class;
  TAdTexture = class;
  //TAdRenderTargetTexture = class;
  TAdBitmapTexture = class;
  TAdMesh = class;

  TAdApplication = class
    private
      FLogProc:TAdLogProc;
    protected
      FOptions:TAdOptions;
      FWidth:integer;
      FHeight:integer;
      FMaxLightCount:integer;
      procedure SetOptions(AValue:TAdOptions);virtual;
      procedure WriteLog(Typ:TAdLogTyp;Text:PChar);
    public
      constructor Create;virtual;abstract;

      //function CreateLight:TAdLight;virtual;abstract;
      function CreateBitmapTexture:TAdBitmapTexture;virtual;abstract;
      //function CreateRenderTargetTexture:TAdRenderTargetTexture;virtual;abstract;
      function CreateMesh:TAdMesh;virtual;abstract;

      //procedure SetRenderTarget(ATarget:TAdRenderTargetTexture);virtual;abstract;

      procedure SetLogProc(ALogProc:TAdLogProc);

      function Initialize(AWnd:LongWord; AOptions:TAdOptions; ADisplay:TAdDisplay):boolean;virtual;abstract;
      procedure Finalize;virtual;abstract;

      procedure ClearSurface(AColor: TAndorraColor);virtual;abstract;
      procedure BeginScene;virtual;abstract;
      procedure EndScene;virtual;abstract;
      procedure Flip;virtual;abstract;

      procedure Setup2DScene(AWidth,AHeight:integer);virtual;abstract;

      property Width:integer read FWidth;
      property Height:integer read FHeight;
      property Options:TAdOptions read FOptions write SetOptions;
      property MaxLights:integer read FMaxLightCount;
  end;

  TAdTexture = class
    private
    protected
      FWidth:integer;
      FHeight:integer;
      FBitCount:byte;
      FEditable:boolean;
      FTexture:Pointer;
      function GetLoaded:boolean;virtual;abstract;      
    public
      property Width:integer read FWidth;
      property Height:integer read FHeight;
      property BitCount:byte read FBitCount;
      property Editable:boolean read FEditable;
      property Loaded:boolean read GetLoaded;
      property Texture:pointer read FTexture;
  end;

  TAdMesh = class
    private
    protected
      FVertices:TAdVertexArray;
      FIndices:TAdIndexArray;
      FVertexCount:integer;
      FIndicesCount:integer;
      FPrimitiveCount:integer;
      FTexture:TAdTexture;
      function GetUseIndexBuffer:boolean;
      procedure SetVertices(AVertices:TAdVertexArray);virtual;abstract;
      procedure SetIndex(AIndex:TAdIndexArray);virtual;abstract;
      procedure SetTexture(ATexture:TAdTexture);virtual;
      function GetLoaded:boolean;virtual;abstract;
    public
      procedure Update;virtual;abstract;
      procedure Draw;virtual;abstract;
      procedure SetMatrix(AMatrix:TAdMatrix);virtual;abstract;
      property Loaded:boolean read GetLoaded;
      property Vertices:TAdVertexArray read FVertices write SetVertices;
      property IndexBuffer:TAdIndexArray read FIndices write SetIndex;
      property UseIndexBuffer:boolean read GetUseIndexBuffer;
      property VertexCount:integer read FVertexCount;
      property IndicesCount:integer read FIndicesCount;
      property PrimitiveCount:integer read FPrimitiveCount write FPrimitiveCount;
      property Texture:TAdTexture read FTexture write SetTexture;
  end;

  TAdBitmapTexture = class(TAdTexture)
    private
    protected
      FBaseWidth:integer;
      FBaseHeight:integer;
    public
      procedure FlushTexture;virtual;abstract;
      procedure LoadFromBitmap(ABmp:TAdBitmap;ABitDepth:byte=32);virtual;abstract;
      procedure SaveToBitmap(ABmp:TAdBitmap);virtual;abstract;
      property BaseWidth:integer read FBaseWidth;
      property BaseHeight:integer read FBaseHeight;
    end;


  TAdCreateApplicationProc = function:TAdApplication;stdcall;

  TRGBRec = packed record
    r,g,b:byte;
  end;
  PRGBRec = ^TRGBRec;

  TRGBARec = packed record
    r,g,b,a:byte;
  end;
  PRGBARec = ^TRGBARec;

function Ad_ARGB(a,r,g,b:byte):TAndorraColor;
function Ad_RGB(r,g,b:byte):TAndorraColor;
function AdColorToString(AColor:TAndorraColor):string;
function StringToAdColor(AString:string):TAndorraColor;

function AdColorToColor(AAdColor:TAndorraColor):LongWord;

function GetRValue(AColor:LongWord):byte;
function GetGValue(AColor:LongWord):byte;
function GetBValue(AColor:LongWord):byte;

function RGB(r,g,b:byte):LongWord;

function CompareColors(col1,col2:TAndorraColor):boolean;

function Cut(AValue:integer):byte;

function AdVector3(AX,AY,AZ:double):TAdVector3;
function AdVector2(AX,AY:double):TAdVector2;

function AdMatrix_Multiply(amat1,amat2:TAdMatrix):TAdMatrix;
function AdMatrix_Translate(tx,ty,tz:single):TAdMatrix;
function AdMatrix_Scale(sx,sy,sz:single):TAdMatrix;
function AdMatrix_RotationX(angle:single):TAdMatrix;
function AdMatrix_RotationY(angle:single):TAdMatrix;
function AdMatrix_RotationZ(angle:single):TAdMatrix;
function AdMatrix_Identity:TAdMatrix;
function AdMatrix_Clear:TAdMatrix;

implementation

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
var sl1:PRGBRec;
    sl2:PRGBARec;
    x,y:integer;
begin
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
end;

procedure TAdBitmap.AssignAlphaChannelToBitmap(ABitmap: TBitmap);
var sl1:PRGBRec;
    sl2:PRGBARec;
    x,y:integer;
begin
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
end;

procedure TAdBitmap.AssignBitmap(ABitmap: TBitmap);
var sl1:PRGBRec;
    sl2:PRGBARec;
    x,y:integer;
begin
  ReserveMemory(ABitmap.Width,ABitmap.Height);
  ABitmap.PixelFormat := pf24Bit;
  sl2 := Scanline;
  for y := 0 to FHeight - 1 do
  begin
    sl1 := ABitmap.ScanLine[y];
    for x := 0 to FWidth - 1 do
    begin
      sl2^.r := sl1^.r;
      sl2^.g := sl1^.g;
      sl2^.b := sl1^.b;
      sl2^.a := 255;
      inc(sl1);
      inc(sl2);
    end;
  end;
end;

procedure TAdBitmap.AssignToBitmap(ABitmap: TBitmap;AIgnoreAlphaChannel:boolean=true);
var sl1:PRGBRec;
    sl2:PRGBARec;
    x,y:integer;
    a:single;
begin
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
        if AIgnoreAlphaChannel then
        begin
          sl1^.r := sl2^.r;
          sl1^.g := sl2^.g;
          sl1^.b := sl2^.b;
        end
        else
        begin
          a := (sl2^.a/255);
          sl1^.r := round((sl1^.r*(1-a)) + (sl2^.r*a));
          sl1^.g := round((sl1^.g*(1-a)) + (sl2^.g*a));
          sl1^.b := round((sl1^.b*(1-a)) + (sl2^.b*a));
        end;
        inc(sl2);
        inc(sl1);
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

procedure TAdApplication.SetLogProc(ALogProc: TAdLogProc);
begin
  TMethod(FLogProc).Code := TMethod(ALogProc).Code;
  TMethod(FLogProc).Data := TMethod(ALogProc).Data;
end;

procedure TAdApplication.SetOptions(AValue: TAdOptions);
begin
  FOptions := AValue;
end;

procedure TAdApplication.WriteLog(Typ: TAdLogTyp; Text: PChar);
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

function TAdMesh.GetUseIndexBuffer: boolean;
begin
  result := FIndices <> nil;
end;

procedure TAdMesh.SetTexture(ATexture:TAdTexture);
begin
  FTexture := ATexture;
end;

end.
