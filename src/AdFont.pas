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
* File: AdFont.pas
* Comment: This unit contains the TAdFont class, which draws the font and the TAdTypeSetter class, which sets the text.
}

{This unit contains the TAdFont class, which draws the font and the TAdTypeSetter class, which sets the text.}
unit AdFont;

interface

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

uses
  AdClasses, AdTypes;

type
  //Defines how the output of the TextOut-Ex function will be
  TAdFontDrawMode = (
    dtBottom,
    dtTop,
    dtMiddle,
    dtLeft,
    dtRight,
    dtCenter,
    dtWordWrap,
    dtCharWrap,
    dtDoLineFeeds,
    dtCut
  );

  //A set of the draw type, used by TAdFont.FontOutEx
  TAdFontDrawModes = set of TAdFontDrawMode;

  TAdTypeSetter = class
    private
      FCharSizes:TAdCharSizes;
      FCharPatterns:TAdCharPatterns;
      FChanged:boolean;
    protected
      FTextWidth:integer;
      FTextHeight:integer;
    public
      constructor Create;virtual;

      function MaxHeight:double;
      function MaxWidth:double;

      procedure GenerateLine(AX,AY:double; AText:string; var ATextSet:TAdTextSet);virtual;abstract;
      procedure Generate(ARect:TAdRect; AText:string; var ATextSet:TAdTextSet);virtual;abstract;

      procedure Assign(ASource:TAdTypeSetter);virtual;abstract;
      function CompareTo(ATypeSetter:TAdTypeSetter):boolean;virtual;abstract;

      property CharSizes:TAdCharSizes read FCharSizes write FCharSizes;
      property CharPatterns:TAdCharPatterns read FCharPatterns write FCharPatterns;
      property Changed:boolean read FChanged write FChanged;

      property TextWidth:integer read FTextWidth;
      property TextHeight:integer read FTextHeight;
  end;

  TAdTypeSetterClass = class of TAdTypeSetter;

  TAdSimpleTypeSetter = class(TAdTypeSetter)
    private
      FLineHeight:double;
      FCharSpacing:double;
      FCharWidth:double;
      FCharHeight:double;
      FDrawMode:TAdFontDrawModes;
      procedure SetData(AIndex:integer;AValue:double);
      procedure SetDrawMode(AValue:TAdFontDrawModes);
    protected
      procedure CutTextSet(AScissorRect:TAdRect; var ATextSet:TAdTextSet);
      procedure LineGeneration(AX,AY, AWidth:double; AText:string; var ATextSet:TAdTextSet; AOffset,ACount:integer);
    public
      constructor Create;override;

      procedure GenerateLine(AX,AY:double; AText:string; var ATextSet:TAdTextSet);override;
      procedure Generate(ARect:TAdRect; AText:string; var ATextSet:TAdTextSet);override;

      procedure Assign(ASource:TAdTypeSetter);override;
      function CompareTo(ATypeSetter:TAdTypeSetter):boolean;override;

      property LineHeight:double index 0 read FLineHeight write SetData;
      property CharSpacing:double index 1 read FCharSpacing write SetData;
      property CharWidth:double index 2 read FCharWidth write SetData;
      property CharHeight:double index 3 read FCharHeight write SetData;
      property DrawMode:TAdFontDrawModes read FDrawMode write SetDrawMode;
  end;


  PAdFont = ^TAdFont;
  TAdFont = class
    private
      FMesh:TAd2dMesh;
      FAppl:TAd2dApplication;

      FTexture:TAd2dBitmapTexture;
      FCharSizes:TAdCharSizes;
      FCharPatterns:TAdCharPatterns;

      FText:string;
      FColor:TAndorraColor;
      FRect:TAdRect;

      FTypeSetter:TAdTypeSetter;
      FOwnTypeSetter:boolean;
      FBlendMode:TAd2dBlendMode;
      FTextSet:TAdTextSet;
      FAutoFreeTexture:boolean;
      FMatrix:TAdMatrix;

      FLastWidth:integer;
      FLastHeight:integer;
      FLastText:string;

      FCreator:Pointer;

      procedure SetTypeSetter(ATypeSetter:TAdTypeSetter);
      procedure SetColor(AValue:TAndorraColor);
      procedure CalcTextSizes(AText:string);
    protected
      property Mesh:TAd2dMesh read FMesh;

      procedure FreeTypeSetter;
      procedure GenerateText;
      procedure Draw;
    public
      constructor Create(AAppl:TAd2dApplication);
      destructor Destroy;override;

      procedure TextOut(ARect:TAdRect;AText:string);overload;
      procedure TextOut(AX, AY:integer;AText:string);overload;

      function TextWidth(AText:string):integer;
      function TextHeight(AText:string):integer;

      procedure Assign(AFont:TAdFont);

      property AutoFreeTexture:boolean read FAutoFreeTexture write FAutoFreeTexture;
      property TypeSetter:TAdTypeSetter read FTypeSetter write SetTypeSetter;
      property Color:TAndorraColor read FColor write SetColor;
      property Texture:TAd2dBitmapTexture read FTexture write FTexture;
      property CharSizes:TAdCharSizes read FCharSizes write FCharSizes;
      property CharPatterns:TAdCharPatterns read FCharPatterns write FCharPatterns;
      property TransformationMatrix:TAdMatrix read FMatrix write FMatrix;
      property Creator:Pointer read FCreator write FCreator;
  end;

implementation

{ TAdTypeSetter }

constructor TAdTypeSetter.Create;
begin
  inherited Create;
end;

function TAdTypeSetter.MaxHeight: double;
var
  i:integer;
begin
  result := 0;
  for i := 0 to High(FCharSizes) do
    if FCharSizes[i].Y > result then
      result := FCharSizes[i].Y;
end;

function TAdTypeSetter.MaxWidth: double;
var
  i:integer;
begin
  result := 0;
  for i := 0 to High(FCharSizes) do
    if FCharSizes[i].X > result then
      result := FCharSizes[i].X;
end;

{ TAdSimpleTypeSetter }

constructor TAdSimpleTypeSetter.Create;
begin
  inherited;

  FLineHeight := 1;
  FCharSpacing := 0;
  FCharWidth := 1;
  FCharHeight := 1;
  FDrawMode := [dtTop, dtLeft];
end;

procedure TAdSimpleTypeSetter.Assign(ASource: TAdTypeSetter);
var
  src:TAdSimpleTypeSetter;
begin
  if ASource is TAdSimpleTypeSetter then
  begin
    Changed := true;
    src := TAdSimpleTypeSetter(ASource);
    FLineHeight := src.LineHeight;
    FCharSpacing := src.CharSpacing;
    FCharWidth := src.CharWidth;
    FCharHeight := src.CharHeight;
    FDrawMode := src.DrawMode;
  end;
end;

function TAdSimpleTypeSetter.CompareTo(ATypeSetter: TAdTypeSetter): boolean;
var
  src:TAdSimpleTypeSetter;
begin
  result := false;
  if ATypeSetter is TAdSimpleTypeSetter then
  begin
    src := TAdSimpleTypeSetter(ATypeSetter);
    result := (FLineHeight = src.LineHeight) and
              (FCharSpacing = src.CharSpacing) and
              (FCharWidth = src.CharWidth) and
              (FCharHeight = src.CharHeight) and
              (FDrawMode = src.DrawMode);
  end;
end;

procedure TAdSimpleTypeSetter.Generate(ARect: TAdRect; AText: string;
  var ATextSet: TAdTextSet);
var
  i:integer;
  s:string;
  lineheight, linewidth, ypos:double;
  width:double;
  lastpos:integer;
  lastspace:integer;
  deletedchars:integer;
begin
  FTextWidth := 0;
  FTextHeight := 0;

  FChanged := false;
  ypos := 0;
  lineheight := MaxHeight * FCharHeight * FLineHeight;

  if not ((dtWordWrap in FDrawMode) or (dtCharWrap in FDrawMode) or (dtDoLineFeeds in FDrawMode))  then
  begin
    SetLength(ATextSet, length(AText));
    LineGeneration(ARect.Left, 0, ARect.Right-ARect.Left, AText, ATextSet, 0, Length(AText));
    ypos := lineheight; 
  end
  else
  begin
    width := ARect.Right - ARect.Left;
    i := 1;
    linewidth := 0;
    lastpos := 0;
    s := '';
    lastspace := -1;
    deletedchars := 0;

    while i <= length(AText) do
    begin
      if ((AText[i] = #10) or (AText[i] = #13)) and (dtDoLineFeeds in FDrawMode) then
      begin
        if s <> '' then
        begin
          LineGeneration(ARect.Left, ypos, width, s, ATextSet, lastpos-deletedchars, length(s));
          s := '';
          ypos := ypos + lineheight;
        end;
        linewidth := 0;
        lastspace := -1;
        lastpos := i;
        deletedchars := deletedchars + 1;
        SetLength(ATextSet, High(ATextSet));
      end
      else
      begin
        s := s + AText[i];
        linewidth := linewidth + CharSizes[ord(AText[i])].X * FCharWidth + FCharSpacing;
        if (AText[i] = ' ') or (dtCharWrap in FDrawMode) then
        begin
          lastspace := i;
        end;
        if ((linewidth > width) and (lastspace <> -1) and
           ((dtWordWrap in FDrawMode) or (dtCharWrap in FDrawMode))) then
        begin
          if dtCharWrap in FDrawMode then
          begin
            LineGeneration(ARect.Left, ypos, width, s, ATextSet, lastpos-deletedchars, length(s));
            lastpos := i;
          end
          else if dtWordWrap in FDrawMode then
          begin
            LineGeneration(ARect.Left, ypos, width, s, ATextSet, lastpos-deletedchars, length(s) - i + lastspace);
            i := lastspace;
            lastpos := lastspace;
          end;
          ypos := ypos + lineheight;
          s := '';
          linewidth := 0;
          lastspace := -1;
        end;
      end;
      i := i + 1;
    end;
    if s <> '' then
    begin
      LineGeneration(ARect.Left, ypos, width, s, ATextSet, lastpos-deletedchars, length(s));
      ypos := ypos + lineheight;
    end;
  end;
  if ypos > FTextHeight then FTextHeight := round(ypos);

  //Adjust text
  for i := 0 to High(ATextSet) do
  begin
    with ATextSet[i] do
    begin
      if (dtTop in FDrawMode) then
        Position := AdRect(
          Position.Left, Position.Top + ARect.Top,
          Position.Right, Position.Bottom + ARect.Top)
      else if dtBottom in FDrawMode then
        Position := AdRect(
          Position.Left, Position.Top + (ARect.Bottom - ypos),
          Position.Right, Position.Bottom + (ARect.Bottom - ypos))
      else if dtMiddle in FDrawMode then
        Position := AdRect(
          Position.Left, Position.Top + ARect.Top + round((ARect.Bottom - ARect.Top - ypos) / 2),
          Position.Right, Position.Bottom + ARect.Top + round((ARect.Bottom - ARect.Top - ypos) / 2));
    end;
  end;

  if dtCut in FDrawMode then
  begin
    CutTextSet(ARect, ATextSet);
  end;
end;

procedure TAdSimpleTypeSetter.GenerateLine(AX, AY: double; AText: string;
  var ATextSet: TAdTextSet);
begin
  FTextWidth := 0;
  FTextHeight := 0;
  FChanged := false;
  SetLength(ATextSet, length(AText));
  LineGeneration(AX, AY, 0, AText, ATextSet, 0, Length(AText));
end;

procedure TAdSimpleTypeSetter.LineGeneration(AX, AY, AWidth: double; AText: string;
  var ATextSet: TAdTextSet; AOffset,ACount: integer);
var
  i:integer;
  width:double;
  charw, patw, path:double;
begin
  i := 1;
  width := 0;
  while i <= ACount do
  begin
    charw := CharSizes[ord(AText[i])].X * FCharWidth;
    patw := (CharPatterns[ord(AText[i])].Right - CharPatterns[ord(AText[i])].Left) * FCharWidth;
    path := (CharPatterns[ord(AText[i])].Bottom - CharPatterns[ord(AText[i])].Top) * FCharHeight;
    if path > FTextHeight then FTextHeight := round(path);

    ATextSet[i-1+AOffset].Position := AdRect(width, ay, width + patw, ay + path);
    ATextSet[i-1+AOffset].TexCoords := CharPatterns[ord(AText[i])];

    width := width + charw + FCharSpacing;
    i := i + 1;
  end;
  if width > FTextWidth then FTextWidth := round(width);

  for i := 0 to ACount-1 do
  begin
    with ATextSet[i+AOffset] do
    begin
      if (dtLeft in FDrawMode) or (FloatsEqual(AWidth, 0, 0.001)) then
        Position := AdRect(
          Position.Left + AX, Position.Top, Position.Right + AX, Position.Bottom)
      else if dtRight in FDrawMode then
        Position := AdRect(
          Position.Left + AX + AWidth - Width, Position.Top,
          Position.Right + AX + AWidth - Width, Position.Bottom)
      else if dtCenter in FDrawMode then
        Position := AdRect(
          Position.Left + AX + round((AWidth - Width) / 2), Position.Top,
          Position.Right + AX + round((AWidth - Width) / 2), Position.Bottom);
    end;
  end;
end;

procedure TAdSimpleTypeSetter.CutTextSet(AScissorRect: TAdRect;
  var ATextSet: TAdTextSet);
var
  i:integer;
  dif:integer;
begin
  for i := High(ATextSet) downto 0 do
  begin
    if OverlapRect(ATextSet[i].Position, AScissorRect) then
    begin
      with ATextSet[i] do
      begin
        if Position.Left < AScissorRect.Left then
        begin
          dif := AScissorRect.Left - Position.Left;
          TexCoords.Left := round(
            TexCoords.Left + ((dif / (Position.Right - Position.Left)) * (TexCoords.Right - TexCoords.Left)));
          Position.Left := AScissorRect.Left;
        end;
        if Position.Right > AScissorRect.Right then
        begin
          dif := Position.Right - AScissorRect.Right;
          TexCoords.Right := round(
            TexCoords.Right - ((dif / (Position.Right - Position.Left)) * (TexCoords.Right - TexCoords.Left)));
          Position.Right := AScissorRect.Right;
        end;
        
        if Position.Top < AScissorRect.Top then
        begin
          dif := AScissorRect.Top - Position.Top;
          TexCoords.Top := round(
            TexCoords.Top + ((dif / (Position.Bottom - Position.Top)) * (TexCoords.Bottom - TexCoords.Top)));
          Position.Top := AScissorRect.Top;
        end;
        if Position.Bottom > AScissorRect.Bottom then
        begin
          dif := Position.Bottom - AScissorRect.Bottom;
          TexCoords.Bottom := round(
            TexCoords.Bottom - ((dif / (Position.Bottom - Position.Top)) * (TexCoords.Bottom - TexCoords.Top)));
          Position.Bottom := AScissorRect.Bottom;
        end;
      end;
    end
    else
    begin
      ATextSet[i].TexCoords := AdRect(0,0,0,0);
      ATextSet[i].Position := AdRect(0,0,0,0);
    end;
  end;
end;

procedure TAdSimpleTypeSetter.SetData(AIndex: integer; AValue: double);
begin
  case AIndex of
    0:if FLineHeight <> AValue then begin FLineHeight := AValue; FChanged := true; end;
    1:if FCharSpacing <> AValue then begin FCharSpacing := AValue; FChanged := true; end;
    2:if FCharWidth <> AValue then begin FCharWidth := AValue; FChanged := true; end;
    3:if FCharHeight <> AValue then begin FCharHeight := AValue; FChanged := true; end;
  end;
end;

procedure TAdSimpleTypeSetter.SetDrawMode(AValue: TAdFontDrawModes);
begin
  if AValue <> FDrawMode then
  begin
    Changed := true;
    FDrawMode := AValue;
  end;
end;

{ TAdFont }

procedure TAdFont.Assign(AFont: TAdFont);
begin
  TypeSetter := TAdTypeSetterClass(AFont.TypeSetter.ClassType).Create;
  FOwnTypeSetter := true;
  TypeSetter.Assign(AFont.FTypeSetter);
  FTexture := AFont.Texture;
  FCharSizes := AFont.CharSizes;
  FCharPatterns := AFont.CharPatterns;
end;

constructor TAdFont.Create(AAppl:TAd2dApplication);
begin
  inherited Create;

  FAppl := AAppl;

  FMesh := FAppl.CreateMesh;

  FOwnTypeSetter := true;
  FTypeSetter := TAdSimpleTypeSetter.Create;
  Color := Ad_ARGB(255,255,255,255);
  FAutoFreeTexture := false;
  FMatrix := AdMatrix_Identity;
end;

destructor TAdFont.Destroy;
begin
  FreeTypeSetter;

  if (FTexture <> nil) and (FAutoFreeTexture) then
    FTexture.Free;

  FMesh.Free;

  inherited Destroy;
end;

procedure TAdFont.FreeTypeSetter;
begin
  if FOwnTypeSetter then
  begin
    FTypeSetter.Free;
    FOwnTypeSetter := false;
  end;
end;

procedure TAdFont.GenerateText;
var
  i, j:integer;
  vertices:TAdVertexArray;
  indices:TAdIndexArray;
  texw, texh : integer;

begin
  if Length(FText) = 0 then
  begin
    if FMesh.Loaded then
    begin
      FMesh.Free;
      FMesh := FAppl.CreateMesh;
    end;
    exit;
  end;
  
  SetLength(vertices, Length(FTextSet) * 4);
  SetLength(indices, Length(FTextSet) * 6);

  texw := FTexture.Width;
  texh := FTexture.Height;

  for i := 0 to High(FTextSet) do
  begin
    with FTextSet[i] do
    begin
      vertices[i*4+0].Position := AdVector3(Position.Left, Position.Top, 0);
      vertices[i*4+1].Position := AdVector3(Position.Right, Position.Top, 0);
      vertices[i*4+2].Position := AdVector3(Position.Left, Position.Bottom, 0);
      vertices[i*4+3].Position := AdVector3(Position.Right, Position.Bottom, 0);


      vertices[i*4+0].Texture := AdVector2((TexCoords.Left + AdTextureOffset) / texw, (TexCoords.Top + AdTextureOffset) / texh);
      vertices[i*4+1].Texture := AdVector2((TexCoords.Right + AdTextureOffset) / texw, (TexCoords.Top + AdTextureOffset) / texh);
      vertices[i*4+2].Texture := AdVector2((TexCoords.Left + AdTextureOffset) / texw, (TexCoords.Bottom + AdTextureOffset) / texh);
      vertices[i*4+3].Texture := AdVector2((TexCoords.Right + AdTextureOffset) / texw, (TexCoords.Bottom + AdTextureOffset) / texh);

      for j := 0 to 3 do
      begin
        vertices[i*4+j].Color := FColor;
      end;
    end;
  end;

  j := 0;
  for i := 0 to High(FTextSet) do
  begin
    indices[j+0] := i * 4 + 0;
    indices[j+1] := i * 4 + 1;
    indices[j+2] := i * 4 + 2;
    indices[j+3] := i * 4 + 1;
    indices[j+4] := i * 4 + 3;
    indices[j+5] := i * 4 + 2;
    j := j + 6;
  end;

  FMesh.Vertices := vertices;
  FMesh.Indices := indices;
  FMesh.PrimitiveCount := Length(FTextSet) * 2;
  FMesh.Texture := FTexture;
  FMesh.Update;
end;

procedure TAdFont.SetColor(AValue: TAndorraColor);
var
  i:integer;
begin
  if (not CompareColors(AValue, FColor)) and (FMesh.Loaded) then
  begin
    for i := 0 to High(FMesh.Vertices) do
    begin
      FMesh.Vertices[i].Color := AValue;
    end;
    FMesh.Vertices := FMesh.Vertices;
    FMesh.Update;
  end;
  FColor := AValue;
end;

procedure TAdFont.SetTypeSetter(ATypeSetter: TAdTypeSetter);
begin
  FreeTypeSetter;
  FTypeSetter := ATypeSetter;
end;

procedure TAdFont.TextOut(AX, AY: integer; AText: string);
var
  ARect:TAdRect;
begin
  ARect := AdRect(ax, ay, ax, ay); 
  if (not FMesh.Loaded) or (TypeSetter.Changed) or (not CompareRects(ARect, FRect)) or (AText <> FText) then
  begin
    FText := AText;
    FRect := ARect;

    TypeSetter.CharSizes := FCharSizes;
    TypeSetter.CharPatterns := FCharPatterns;
    SetLength(FTextSet, Length(FText));
    TypeSetter.GenerateLine(AX, AY, FText, FTextSet);

    FLastText := AText;
    FLastHeight := TypeSetter.FTextHeight;
    FLastWidth := TypeSetter.FTextWidth;

    GenerateText;
  end;

  Draw;
end;

procedure TAdFont.TextOut(ARect: TAdRect; AText: string);
begin
  if (not FMesh.Loaded) or (TypeSetter.Changed) or (not CompareRects(ARect, FRect)) or (AText <> FText) then
  begin
    FText := AText;
    FRect := ARect;

    TypeSetter.CharSizes := FCharSizes;
    TypeSetter.CharPatterns := FCharPatterns;
    SetLength(FTextSet, Length(FText));
    TypeSetter.Generate(FRect, FText, FTextSet);

    FLastText := AText;
    FLastHeight := TypeSetter.FTextHeight;
    FLastWidth := TypeSetter.FTextWidth;

    GenerateText;
  end;

  Draw;
end;

procedure TAdFont.CalcTextSizes(AText:string);
var
  ATextSet:TAdTextSet;
begin
  if (TypeSetter.Changed) or (AText <> FLastText) then
  begin
    TypeSetter.CharSizes := FCharSizes;
    TypeSetter.CharPatterns := FCharPatterns;
    TypeSetter.GenerateLine(0, 0, AText, ATextSet);
    FLastText := AText;
    FLastWidth := TypeSetter.TextWidth;
    FLastHeight := TypeSetter.TextHeight;
  end;
end;

function TAdFont.TextHeight(AText: string):integer;
begin
  CalcTextSizes(AText);
  result := FLastHeight;
end;

function TAdFont.TextWidth(AText: string):integer;
begin
  CalcTextSizes(AText);
  result := FLastWidth;
end;

procedure TAdFont.Draw;
begin
  FMesh.Matrix := FMatrix;
  FMesh.Draw(FBlendMode, adTriangles);
end;

end.
