unit AdClasses;

interface

uses Classes, Graphics;

type
  TAndorraColor = record
    r,g,b,a:integer;
  end;

  TAdVector2 = record
    x,y:double;
  end;

  TAdVector3 = record
    x,y,z:double;
  end;

  TAdVertex = record
    Color:TAndorraColor;
    Position:TAdVector3;
    Normal:TAdVector3;
    Texture:TAdVector2;
  end;

  TAdVertexArray = array of TAdVertex;

  TAdIndexArray = array of Word;

  TAdMatrix = record
    _00,_01,_02,_03:double;
    _10,_11,_12,_13:double;
    _20,_21,_22,_23:double;
    _30,_31,_32,_33:double;
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

  TAdApplication = class

  end;

  TRGBRec = packed record
    r,g,b:byte;
  end;
  PRGBRec = ^TRGBRec;

  TRGBARec = packed record
    r,g,b,a:byte;
  end;
  PRGBARec = ^TRGBARec;


implementation

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

end.
