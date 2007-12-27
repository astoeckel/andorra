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
* File: AdBitmap.pas
* Comment: This unit contains the TAdBitmap class.
}

unit AdBitmap;

interface

uses
  SysUtils, Classes, AdTypes, AdBitmapClass, AdPersistent;

type
  ENoValidCompressor = class(Exception);
  ENoValidFormat = class(Exception);

  TAdBitmap = class;

  TAdGraphicProgress = procedure(Sender:TObject; AMax, AValue:integer) of object;
  
  TAdGraphicCompressor = class(TAdPersistent)
    private
      FProgress:TAdGraphicProgress;
    public
      procedure Write(ABitmap:TAdBitmap; AStream:TStream);virtual;abstract;
      procedure Read(ABitmap:TAdBitmap; AStream:TStream);virtual;abstract;
      class function ID:TAdVeryShortString;virtual;abstract;

      property OnProgress:TAdGraphicProgress read FProgress write FProgress;
  end;
  TAdGraphicCompressorClass = class of TAdGraphicCompressor;

  TAdGraphicFormat = class(TAdPersistent)
    public
      class procedure FileExts(strs:TStrings);virtual;abstract;
      class function SupportsObject(AObj:TObject):boolean;virtual;abstract;
      function LoadFromFile(ABitmap:TAdBitmap; AFile:string;
        ATransparent:Boolean; ATransparentColor:LongInt):boolean;virtual;abstract;
      function Assign(ABitmap:TAdBitmap; AGraphic:TObject):boolean;reintroduce;virtual;abstract;
      function AssignTo(ABitmap:TAdBitmap; AGraphic:TObject):boolean;reintroduce;virtual;abstract;
      function AssignAlphaChannel(ABitmap:TAdBitmap; AGraphic:TObject):boolean;virtual;abstract;
      function AssignAlphaChannelTo(ABitmap:TAdBitmap; AGraphic:TObject):boolean;virtual;abstract;
  end;
  TAdGraphicFormatClass = class of TAdGraphicFormat;     

  TAdBitmap = class(TAd2dBitmap)
    private
      FCompressorClass:TAdGraphicCompressorClass;
      FProgress:TAdGraphicProgress;
      procedure ReadRawData(AStream:TStream);
      procedure WriteRawData(AStream:TStream);
      function GetObjectFormat(AObj:TObject):TAdGraphicFormat;
    public
      constructor Create;
      destructor Destroy;override;
      
      procedure Assign(AGraphic:TObject);
      procedure AssignAlphaChannel(AGraphic:TObject);
      procedure AssignTo(AGraphic:TObject);
      procedure AssignAlphaChannelTo(AGraphic:TObject);

      procedure LoadFromStream(AStream:TStream);
      procedure SaveToStream(AStream:TStream);
      procedure LoadFromFile(AFile:string);
      procedure SaveToFile(AFile:string);

      procedure LoadGraphicFromFile(AFile:string;
        ATransparent:Boolean=true; ATransparentColor:LongInt=0);

      property Compressor:TAdGraphicCompressorClass read FCompressorClass write FCompressorClass;
      property OnProgress:TAdGraphicProgress read FProgress write FProgress;
  end;    

procedure RegisterGraphicCompressor(AClass:TAdGraphicCompressorClass);
procedure RegisterGraphicFormat(AClass:TAdGraphicFormatClass);

var
  RegisteredGraphicCompressors:TStringList;
  RegisteredGraphicFormats:TStringList;

implementation

procedure RegisterGraphicCompressor(AClass:TAdGraphicCompressorClass);
begin
  AdRegisterClass(AClass);
  RegisteredGraphicCompressors.Add(AClass.ID+'='+AClass.ClassName);
end;

procedure RegisterGraphicFormat(AClass:TAdGraphicFormatClass);
begin
  AdRegisterClass(AClass);
  RegisteredGraphicFormats.Add(AClass.ClassName);
end;

{ TAdBitmap }

constructor TAdBitmap.Create;
begin
  inherited;
  FCompressorClass := nil;
end;

destructor TAdBitmap.Destroy;
begin
  inherited;
end;

function TAdBitmap.GetObjectFormat(AObj:TObject): TAdGraphicFormat;
var
  i:integer;
  cref:TAdGraphicFormatClass;
begin
  result := nil;
  for i := 0 to RegisteredGraphicFormats.Count - 1 do
  begin
    cref := TAdGraphicFormatClass(AdGetClass(RegisteredGraphicFormats[i]));
    if cref.SupportsObject(AObj) then
    begin
      result := cref.Create;
      exit;
    end;
  end;
end;

procedure TAdBitmap.Assign(AGraphic: TObject);
var
  tmp:TAdGraphicFormat;
begin
  tmp := GetObjectFormat(AGraphic);
  if tmp = nil then
    raise ENoValidFormat('No handler for '+AGraphic.ClassName+' found');
  tmp.Assign(self, AGraphic);
  tmp.Free;  
end;

procedure TAdBitmap.AssignAlphaChannel(AGraphic: TObject);
var
  tmp:TAdGraphicFormat;
begin
  tmp := GetObjectFormat(AGraphic);
  if tmp = nil then
    raise ENoValidFormat('No handler for '+AGraphic.ClassName+' found');
  tmp.AssignAlphaChannel(self, AGraphic);
  tmp.Free;
end;

procedure TAdBitmap.AssignAlphaChannelTo(AGraphic: TObject);
var
  tmp:TAdGraphicFormat;
begin
  tmp := GetObjectFormat(AGraphic);
  if tmp = nil then
    raise ENoValidFormat('No handler for '+AGraphic.ClassName+' found');
  tmp.AssignAlphaChannelTo(self, AGraphic);
  tmp.Free;                                
end;

procedure TAdBitmap.AssignTo(AGraphic: TObject);
var
  tmp:TAdGraphicFormat;
begin
  tmp := GetObjectFormat(AGraphic);
  if tmp = nil then
    raise ENoValidFormat('No handler for '+AGraphic.ClassName+' found');
  tmp.AssignTo(self, AGraphic);
  tmp.Free;
end;

procedure TAdBitmap.WriteRawData(AStream: TStream);
begin
  AStream.Write(FSize, SizeOf(FSize));
  AStream.Write(FWidth, SizeOf(FWidth));
  AStream.Write(FHeight, SizeOf(FHeight));
  AStream.Write(FMemory^, FSize);
end;

procedure TAdBitmap.ReadRawData(AStream: TStream);
begin
  ClearMemory;
  AStream.Read(FSize, SizeOf(FSize));
  AStream.Read(FWidth, SizeOf(FWidth));
  AStream.Read(FHeight, SizeOf(FHeight));
  if FSize = FWidth * FHeight * 4 then
  begin
    ReserveMemory(FWidth, FHeight);
    AStream.Read(FMemory^, FSize);
  end
  else
  begin
    ENoValidCompressor.Create('No compressor not found');
    ClearMemory;
  end;
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

procedure TAdBitmap.LoadFromStream(AStream: TStream);
var
  s:TAdVeryShortString;
  classname:string;
  cref:TAdGraphicCompressorClass;
  tmp:TAdGraphicCompressor;
begin
  ClearMemory;

  SetLength(s, 4);  
  AStream.Read(s[1], 4);
  classname := RegisteredGraphicCompressors.Values[s];
  if classname <> '' then
  begin
    cref := TAdGraphicCompressorClass(AdGetClass(classname));
    if cref <> nil then
    begin
      tmp := cref.Create;
      tmp.OnProgress := FProgress;
      try
        tmp.Read(self, AStream);
      finally
        tmp.Free;
      end;
    end;
  end
  else
  begin
    AStream.Position := AStream.Position - 4;
    ReadRawData(AStream);
  end;
end;

procedure TAdBitmap.SaveToStream(AStream: TStream);
var
  s:TAdVeryShortString;
  tmp:TAdGraphicCompressor;
begin
  if FCompressorClass = nil then
  begin
    WriteRawData(AStream);
  end
  else
  begin
    tmp := FCompressorClass.Create;
    s := tmp.ID;
    AStream.Write(s[1], 4);
    tmp.OnProgress := FProgress;
    try
      tmp.Write(self, AStream);
    finally
      tmp.Free;
    end;       
  end;
end;

procedure TAdBitmap.LoadGraphicFromFile(AFile: string; ATransparent: Boolean;
  ATransparentColor: Integer);
var
  tmp:TAdGraphicFormat;
  cref:TAdGraphicFormatClass;
  ext:string;
  i:integer;
  str:TStringList;
begin
  ext := lowercase(ExtractFileExt(AFile));
  for i := 0 to RegisteredGraphicFormats.Count-1 do
  begin
    cref := TAdGraphicFormatClass(AdGetClass(RegisteredGraphicFormats[i]));
    if cref <> nil then
    begin
      tmp := cref.Create;
      str := TStringlist.Create;
      tmp.FileExts(str);
      if str.IndexOf(ext) > -1 then
      begin
        tmp.LoadFromFile(self, AFile, ATransparent, ATransparentColor);
      end;
      str.Free;
      tmp.Free;
    end;
  end;
end;

initialization
  RegisteredGraphicFormats := TStringList.Create;
  RegisteredGraphicCompressors := TStringList.Create;

finalization
  RegisteredGraphicFormats.Free;
  RegisteredGraphicCompressors.Free;

end.
