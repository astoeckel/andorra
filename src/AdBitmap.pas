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

{This unit contains a improved bitmap, which may load data from different picture sources and gives the possibility of easy pixel access.}
unit AdBitmap;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  SysUtils, Classes, AdTypes, AdBitmapClass, AdPersistent;

type
  {This exception is raised when no proper compressor is found to load the
   bitmap data from a stream or a file}
  ENoValidCompressor = class(Exception);

  {This exception is raised when no format loader is found to load the bitmap
   data from the specified data source}
  ENoValidFormat = class(Exception);

  {TAdBitmap is the main bitmap class}
  TAdBitmap = class;

  {A method type declaration for showing the decoding/encoding progress of a
   bitmap}
  TAdGraphicProgress = procedure(Sender:TObject; AMax, AValue:integer) of object;
  
  {TAdGraphicCompressor is an abstract class, which allows to save bitmap data
   using different encoding and compressing technologies.}
  TAdGraphicCompressor = class(TAdPersistent)
    private
      FProgress:TAdGraphicProgress;
    public
      {This should be used to write the bitmap data into the specified stream.
       The compressor ID is automaticly written by the caller of the procedure.}
      procedure Write(ABitmap:TAdBitmap; AStream:TStream);virtual;abstract;
      {This should be used to read the bitmap data from the specified stream.
       The compressor ID has already be read by the caller of the procedure.}
      procedure Read(ABitmap:TAdBitmap; AStream:TStream);virtual;abstract;
      {This class function should be used to return a unique ID, which specifies
       the compressor.}
      class function ID:TAdVeryShortString;virtual;abstract;

      {This event should be called to show the progress of encoding/decoding the
       bitmap data}
      property OnProgress:TAdGraphicProgress read FProgress write FProgress;
  end;
  {A class declaration of TAdGraphicCompressor.}
  TAdGraphicCompressorClass = class of TAdGraphicCompressor;

  {The abstract TAdGraphicFormat class should be used to load various graphic
   classes into a TAdBitmap.}
  TAdGraphicFormat = class(TAdPersistent)
    public
      {This class procedure writes all file extensions the format can load into
       a string list. All extensions have to be written in lower case.}
      class procedure FileExts(strs:TStrings);virtual;abstract;
      {This class function should return true if the loader can handle the
       object specified by "AObj".}
      class function SupportsObject(AObj:TObject):boolean;virtual;abstract;
      {This abstract function should be used to load a file (wich has a file
       extension specified by the FileExts procedute) into the bitmap data.}
      function LoadFromFile(ABitmap:TAdBitmap; AFile:string;
        ATransparent:Boolean; ATransparentColor:LongInt):boolean;virtual;abstract;
      {This function should be used to copy the graphics data of the specified
       object as good as possible into the TAdBitmap.}
      function Assign(ABitmap:TAdBitmap; AGraphic:TObject):boolean;reintroduce;virtual;abstract;
      {This function should be used to copy the bitmap data of the specified
       object as good as possible into the specified graphic object.}
      function AssignTo(ABitmap:TAdBitmap; AGraphic:TObject):boolean;reintroduce;virtual;abstract;
      {This function should be used to copy the alpha channel of the specified
       object into the TAdBitmap. If the specified graphic does not contain an
       alphachannel, a RGB mixture value should be used.}
      function AssignAlphaChannel(ABitmap:TAdBitmap; AGraphic:TObject):boolean;virtual;abstract;
      {This function should be used to copy the bitmaps alpha channel into the
       specified graphic object. If the graphic object isn't able to handle
       alpha channels, the alpha channel sould be copied as a mixture of
       RGB values into the graphic.}
      function AssignAlphaChannelTo(ABitmap:TAdBitmap; AGraphic:TObject):boolean;virtual;abstract;
  end;
  {A class declaration of TAdGrahpicFormat.}
  TAdGraphicFormatClass = class of TAdGraphicFormat;     

  {TAdBitmap is the main bitmap class}
  TAdBitmap = class(TAd2dBitmap)
    private
      FCompressorClass:TAdGraphicCompressorClass;
      FProgress:TAdGraphicProgress;
      procedure ReadRawData(AStream:TStream);
      procedure WriteRawData(AStream:TStream);
      function GetObjectFormat(AObj:TObject):TAdGraphicFormat;
    public
      {This constructor creates an instance of TAdBitmap.}
      constructor Create;
      {This destructor destroys the instance of TAdBitmap and frees the used memory.}
      destructor Destroy;override;
      
      {This procedure assigns a graphic objet by searching a registered
       TAdGraphicFormat, which can handle this graphic type.
       If no valid TAdGraphicFormat is found, ENoValidFormat is raised.}
      procedure Assign(AGraphic:TObject);
      {This procedure assigns the alpha channel from a graphic objet
       by searching a registered TAdGraphicFormat, which can handle this graphic
       type.  If the graphic doesn't handel alpha channels, the average RGB
       value will be used to set the alpha channel of the bitmap.
       If no valid TAdGraphicFormat is found, ENoValidFormat is raised.}
      procedure AssignAlphaChannel(AGraphic:TObject);
      {This procedure assigns a graphic objet by searching a registered
       TAdGraphicFormat, which can handle this graphic type.
       If no valid TAdGraphicFormat is found, ENoValidFormat is raised.}
      procedure AssignTo(AGraphic:TObject);
      {This procedure assigns the bitmap alpha channel to a graphic objet
       by searching a registered TAdGraphicFormat, which can handle this
       graphic type.  If the graphic doesn't handel alpha channels, the
       RGB values of the graphic will be set to the alpha channel value.
       If no valid TAdGraphicFormat is found, ENoValidFormat is raised.}
      procedure AssignAlphaChannelTo(AGraphic:TObject);

      {This proecdure tries to load the bitmap data by searching a registered
       TAdGraphicCompressor which may decompress the bitmap data.
       If no compressor is found, TAdBitmap tries to interpret the specified
       data as RAW data. If this fails, ENoValidCompressor is raised.}
      procedure LoadFromStream(AStream:TStream);
      {This proecdure stores the bitmap data using the compressor class
       specified in "TAdGraphicCompressorClass" in a stream. If "Compressor"
       is nil, the bitmap stores the RAW bitmap data. The RAW-Data header
       contains the Size of the Bitmap in Byte, and the width/height of the
       bitmap in pixel. The RAW data is stored in 32-Bit RGBA value pairs
       starting in the top-left of the bitmap.}
      procedure SaveToStream(AStream:TStream);
      {This procedure tries to load the bitmap data using LoadFromStream and a
       memory stream from a file.}
      procedure LoadFromFile(AFile:string);
      {This procedure saves the bitmap data using "SaveToStream" and a memory
       stream in a file.}
      procedure SaveToFile(AFile:string);

      {This procedure searches for a registered graphic format, that is able to
       handle the given file. The image type is (currently) only determined by
       the file extension.
       @param(ATransparent specifies whether the picture should be loaded with
         an transparency channel.)
       @param(ATransparentColor)}
      procedure LoadGraphicFromFile(AFile:string;
        ATransparent:Boolean=true; ATransparentColor:LongInt=0);
        
      {Set the compressor class you want to use to compress the bitmap data
       here. If "Compressor" is nil, the bitmap will store the RAW bitmap data.}
      property Compressor:TAdGraphicCompressorClass read FCompressorClass write FCompressorClass;
      {This event notifies about the bitmaps decoding/encoding progress.}
      property OnProgress:TAdGraphicProgress read FProgress write FProgress;
  end;    

{Use this procedure in the initialization part of a unit to register a graphic
 compressor class.}
procedure RegisterGraphicCompressor(AClass:TAdGraphicCompressorClass);
{Use this procedure in the initialization part of a unit to register a graphic
 format class.}
procedure RegisterGraphicFormat(AClass:TAdGraphicFormatClass);

var
  {This stringlist contains the names and ids of all registered graphic
   compressors}
  RegisteredGraphicCompressors:TStringList;
  {This stringlist contains the names of all registered graphic formats. It can be
   used by editors to let the user know which graphic formats are supported.
   Do not change anything in this stringlist manually! }
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
    raise ENoValidFormat.Create('No handler for '+AGraphic.ClassName+' found');
  tmp.Assign(self, AGraphic);
  tmp.Free;  
end;

procedure TAdBitmap.AssignAlphaChannel(AGraphic: TObject);
var
  tmp:TAdGraphicFormat;
begin
  tmp := GetObjectFormat(AGraphic);
  if tmp = nil then
    raise ENoValidFormat.Create('No handler for '+AGraphic.ClassName+' found');
  tmp.AssignAlphaChannel(self, AGraphic);
  tmp.Free;
end;

procedure TAdBitmap.AssignAlphaChannelTo(AGraphic: TObject);
var
  tmp:TAdGraphicFormat;
begin
  tmp := GetObjectFormat(AGraphic);
  if tmp = nil then
    raise ENoValidFormat.Create('No handler for '+AGraphic.ClassName+' found');
  tmp.AssignAlphaChannelTo(self, AGraphic);
  tmp.Free;                                
end;

procedure TAdBitmap.AssignTo(AGraphic: TObject);
var
  tmp:TAdGraphicFormat;
begin
  tmp := GetObjectFormat(AGraphic);
  if tmp = nil then
    raise ENoValidFormat.Create('No handler for '+AGraphic.ClassName+' found');
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
  aclassname:string;
  cref:TAdGraphicCompressorClass;
  tmp:TAdGraphicCompressor;
begin
  ClearMemory;

  SetLength(s, 4);  
  AStream.Read(s[1], 4);
  aclassname := RegisteredGraphicCompressors.Values[s];
  if aclassname <> '' then
  begin
    cref := TAdGraphicCompressorClass(AdGetClass(aclassname));
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
        if tmp.LoadFromFile(self, AFile, ATransparent, ATransparentColor) then
        begin
          str.Free;
          tmp.Free;
          exit;
        end;
      end;
      str.Free;
      tmp.Free;
    end;
  end;
  
  //! RAISE EXCEPTION HERE
end;

initialization
  RegisteredGraphicFormats := TStringList.Create;
  RegisteredGraphicCompressors := TStringList.Create;

finalization
  RegisteredGraphicFormats.Free;
  RegisteredGraphicCompressors.Free;

end.
