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
* File: AdFontFactory.pas
* Comment: TAdFontFactory is used to create font objects
}

unit AdFontFactory;

interface

uses
  Classes, AdClasses, AdTypes, AdFont, AdFontGenerator, AdContainers;

type
  TAdFontDataKey = class(TAdMapKey)
    private
      FHasData:boolean;
      FHash:cardinal;
      FMetaData:Pointer;
      FMetaDataSize:Cardinal;
      FFont:TAdFont;
      FAutoFreeFont:boolean;
      procedure ClearData;
    public
      constructor Create;
      destructor Destroy;override;

      function Hash:Cardinal;override;
      function Equal(AItem:TAdMapKey):boolean;override;

      procedure InsertData(AMetaData:Pointer;AMetaDataSize:Cardinal);
      procedure SaveToStream(AStream:TStream);
      procedure LoadFromStream(AStream:TStream);

      property MetaData:Pointer read FMetaData;
      property MetaDataSize:Cardinal read FMetaDataSize;
      property Font:TAdFont read FFont write FFont;
      property AutoFreeFont:boolean read FAutoFreeFont write FAutoFreeFont;
  end;

  TAdFontMap = class(TAdMap)
    public
      destructor Destroy;override;
      procedure Clear;
  end;

  TAdFontFactory = class
    private
      FAppl:TAd2dApplication;
      FAutoFreeFonts:boolean;
      FFontMap:TAdFontMap;
      FOwnFontMap:boolean;
      FLastKey:TAdFontDataKey;
      procedure SetFontMap(AValue:TAdFontMap);
      procedure FreeMemory;
    public
      constructor Create(AAppl:TAd2dApplication);
      destructor Destroy;override;

      function GenerateFont(AData:Pointer;ASize:Cardinal):TAdFont;overload;
      function GenerateFont(AFontName:ShortString;AFontSize:integer;
        AFontStyles:TAdFontStyles; AShadowColor:longint=0;
        AShadowAlpha:byte=128; AShadowOffsetX:integer=0;
        AShadowOffsetY:integer=0; AShadowBlur:byte=0):TAdFont;overload;

      property AutoFreeFonts:boolean read FAutoFreeFonts write FAutoFreeFonts;
      property FontMap:TAdFontMap read FFontMap write SetFontMap;
      property LastKey:TAdFontDataKey read FLastKey;
  end;

implementation

{ TAdFontFactory }

constructor TAdFontFactory.Create(AAppl: TAd2dApplication);
begin
  inherited Create;
  FAppl := AAppl;
  FFontMap := TAdFontMap.Create(16);
  FOwnFontMap := true;
  FAutoFreeFonts := true;
end;

destructor TAdFontFactory.Destroy;
begin
  FreeMemory;
  inherited;
end;

procedure TAdFontFactory.SetFontMap(AValue: TAdFontMap);
begin
  FreeMemory;
  FFontMap := AValue;
end;

procedure TAdFontFactory.FreeMemory;
begin
  if FOwnFontMap then
  begin
    FFontMap.Free;
    FOwnFontMap := false;
  end;
end;

function TAdFontFactory.GenerateFont(AData: Pointer; ASize: Cardinal): TAdFont;
var
  i:integer;
  cref:TAdFontGeneratorClass;
  tmp:TAdFontGenerator;
  charsize:TAdCharSizes;
  charpat:TAdCharPatterns;
  tex:TAd2dBitmapTexture;
  key,fnt:TAdFontDataKey;
begin
  result := nil;
  FLastKey := nil;

  //Create a hash-map key with the given data
  key := TAdFontDataKey.Create;
  key.AutoFreeFont := FAutoFreeFonts;
  key.InsertData(AData, ASize);

  //Search for an entry with equal data
  fnt := TAdFontDataKey(FFontMap.GetValue(key));

  if fnt = nil then
  begin
    //An equal font was not found, generate a new one
    for i := 0 to RegisteredGenerators.Count-1 do
    begin
      cref := TAdFontGeneratorClass(GetClass(RegisteredGenerators[i]));
      if cref <> nil then
      begin
        tmp := cref.Create;
        if tmp.IsValidData(AData, ASize) then
        begin
          //Generator was found, generate the font
          tex := FAppl.CreateBitmapTexture;
          tmp.Generate(AData, ASize, charsize, charpat, tex);

          //Apply font data
          key.Font := TAdFont.Create(FAppl);
          key.Font.Texture := tex;
          key.Font.CharSizes := charsize;
          key.Font.CharPatterns := charpat;
          key.Font.Creator := key;

          //Insert font into font map
          FFontMap.Insert(key, key);

          result := key.Font;
          FLastKey := fnt;

          tmp.Free;

          exit;
        end;
        
        tmp.Free;
      end;
    end;

  end
  else
  begin
    //An equal font already exists, free the generated key
    result := fnt.Font;
    FLastKey := fnt;
    key.Free;
  end;
end;

function TAdFontFactory.GenerateFont(AFontName: ShortString; AFontSize: integer;
  AFontStyles: TAdFontStyles; AShadowColor: Integer; AShadowAlpha: byte;
  AShadowOffsetX, AShadowOffsetY: integer; AShadowBlur: byte): TAdFont;
var
  ms:TMemoryStream;
  ss:TAdVeryShortString;
  l:byte;
  i:integer;
begin
  ms := TMemoryStream.Create;

  //Write standard font generator ID
  ss := 'STDF';
  ms.Write(ss, SizeOf(ss));

  //Clear AFontName memory (necessary for hashing)
  l := length(AFontName);
  SetLength(AFontName, 255);
  for i := l+1 to 255 do
  begin
    AFontName[i] := #0;
  end;
  SetLength(AFontName, l);

  ms.Write(AFontName, SizeOf(AFontName));
  ms.Write(AFontSize, SizeOf(AFontSize));
  ms.Write(AFontStyles, SizeOf(TAdFontStyles));
  ms.Write(AShadowColor, SizeOf(AShadowColor));
  ms.Write(AShadowAlpha, SizeOf(AShadowAlpha));
  ms.Write(AShadowOffsetX, SizeOf(AShadowOffsetX));
  ms.Write(AShadowOffsetY, SizeOf(AShadowOffsetY));
  ms.Write(AShadowBlur, SizeOf(AShadowBlur));

  result := GenerateFont(ms.Memory, ms.Size);
  
  ms.Free;
end;

{ TAdFontMap }

procedure TAdFontMap.Clear;
var
  i,j:integer;
  p:PAdLinkedList;
  c:PAdMapPair;
begin
  p := Data;
  for i := 0 to Capacity - 1 do
  begin
    p^.StartIteration;
    while not p^.ReachedEnd do
    begin
      c := PAdMapPair(p^.GetCurrent);
      c^.Key.Free;
      Dispose(c);
      FCount := FCount - 1;
    end;
    for j := 0 to p^.Count - 1 do
    begin
      p^.Delete(0);
    end;
    inc(p);
  end;
end;

destructor TAdFontMap.Destroy;
begin
  Clear;
  inherited;
end;

{ TAdFontDataKay }

constructor TAdFontDataKey.Create;
begin
  inherited;

  FHasData := false;
  FMetaData := nil;
  FMetaDataSize := 0;
  FFont := nil;
  FHash := 0;
  FAutoFreeFont := true;
end;

destructor TAdFontDataKey.Destroy;
begin
  ClearData;
  
  inherited;
end;

function TAdFontDataKey.Equal(AItem: TAdMapKey): boolean;
var
  i:integer;
  akey:TAdFontDataKey;
  p1:PByte;
  p2:PByte;
begin
  akey := TAdFontDataKey(AItem);
  result := FHasData and (akey.MetaDataSize = FMetaDataSize);
  if result then
  begin
    p1 := FMetaData;
    p2 := akey.MetaData;
    for i := 0 to FMetaDataSize-1 do
    begin
      if p1^ <> p2^ then
      begin
        result := false;
        exit;
      end;
      inc(p1);
      inc(p2);
    end;
  end;
end;

function TAdFontDataKey.Hash: Cardinal;
begin
  result := 0;      
  if FHasData then
  begin
    result := FHash;
  end;
end;

procedure TAdFontDataKey.InsertData(AMetaData: Pointer; AMetaDataSize: Cardinal);
var
  p1:PByte;
  p2:PByte;
  i: Integer;
begin
  ClearData;

  GetMem(FMetaData, AMetaDataSize);
  p1 := AMetaData;
  p2 := FMetaData;
  for i := 0 to AMetaDataSize-1 do
  begin
    p2^ := p1^;
    FHash := FHash + (p1^ shl (i mod 32));
    inc(p1);
    inc(p2);
  end;

  FMetaDataSize := AMetaDataSize;
  FHasData := true;
end;

procedure TAdFontDataKey.ClearData;
begin
  if FHasData then
  begin
    if FAutoFreeFont then
    begin
      FFont.Free;
    end;
    FreeMem(FMetaData, FMetaDataSize);
  end;

  FHasData := false;
  FMetaData := nil;
  FMetaDataSize := 0;
  FFont := nil;
  FHash := 0;
end;

procedure TAdFontDataKey.LoadFromStream(AStream: TStream);
var
  buf:Pointer;
begin
  ClearData;
  AStream.Read(FMetaDataSize,SizeOf(Cardinal));
  GetMem(buf, FMetaDataSize);
  AStream.Read(buf^, FMetaDataSize);
  InsertData(buf, FMetaDataSize);
end;

procedure TAdFontDataKey.SaveToStream(AStream: TStream);
begin
  AStream.Write(FMetaDataSize, SizeOf(Cardinal));
  AStream.Write(FMetaData^, FMetaDataSize);
end;

end.

