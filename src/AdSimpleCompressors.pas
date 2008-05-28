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
* File: AdSimpleCompressors.pas
* Comment: Contains simple graphic compressors
}

{Contains simple graphic compressors}
unit AdSimpleCompressors;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, AdBitmap, AdTypes, Huffman;

type
  {A Andorra 2D graphic compressor class, that saves the graphic data in a 
   raw, Andorra 2D specific bitmap format.}
  TAdBMPCompressor = class(TAdGraphicCompressor)
    public
      {Returns #5BMP}
      class function ID:TAdVeryShortString;override;
      procedure Write(ABitmap:TAdBitmap; AStream:TStream);override;
      procedure Read(ABitmap:TAdBitmap; AStream:TStream);override;
  end;

  {A Andorra 2D graphic compressor class, that saves the bitmap data compressed
   with a Huffman compressor.}
  TAdHAICompressor = class(TAdGraphicCompressor)
    public
      {Returns #3HAI}
      class function ID:TAdVeryShortString;override;
      procedure Write(ABitmap:TAdBitmap; AStream:TStream);override;
      procedure Read(ABitmap:TAdBitmap; AStream:TStream);override;
  end;

  {Enables the possibility of assigning a TAdBitmap to another.}
  TAdBitmapFormat = class(TAdGraphicFormat)
    public
      class procedure FileExts(strs:TStrings);override;
      class function SupportsObject(AGraphic:TObject):boolean;override;
      function LoadFromFile(ABitmap:TAdBitmap; AFile:string;
        ATransparent:Boolean; ATransparentColor:LongInt):boolean;override;
      function Assign(ABitmap:TAdBitmap; AGraphic:TObject):boolean;override;
      function AssignTo(ABitmap:TAdBitmap; AGraphic:TObject):boolean;override;
      function AssignAlphaChannel(ABitmap:TAdBitmap; AGraphic:TObject):boolean;override;
      function AssignAlphaChannelTo(ABitmap:TAdBitmap; AGraphic:TObject):boolean;override;
  end;

implementation

{ TAdHAICompressor }

class function TAdHAICompressor.ID: TAdVeryShortString;
begin
  result := #3+'HAI';
end;

procedure TAdHAICompressor.Read(ABitmap: TAdBitmap; AStream: TStream);
var
  input:TMemoryStream;
  output:TMemoryStream;
  dec:THuffmanDecoder;
  s:int64;
begin
  input := TMemoryStream.Create;
  AStream.Read(s,SizeOf(s));
  input.CopyFrom(AStream,s);

  output := TMemoryStream.Create;
  input.Position := 0;
  dec := THuffmanDecoder.Create;
  dec.Input := input;
  dec.Output := output;
  dec.Decode;
  input.Free;
  dec.Free;
  output.Position := 0;

  ABitmap.LoadFromStream(output);
  output.Free;
end;

procedure TAdHAICompressor.Write(ABitmap: TAdBitmap; AStream: TStream);
var
  input:TMemoryStream;
  output:TMemoryStream;
  enc:THuffmanEncoder;
  s:int64;
  c:TAdGraphicCompressorClass;
begin
  input := TMemoryStream.Create;

  c := ABitmap.Compressor; ABitmap.Compressor := nil;
  ABitmap.SaveToStream(input);
  ABitmap.Compressor := c;

  output := TMemoryStream.Create;
  Input.Position := 0;
  enc := THuffmanEncoder.Create;
  enc.Input := input;
  enc.Output := output;
  enc.Encode;
  enc.Free;
  input.Free;
  s := Output.Size;
  AStream.Write(s,SizeOf(s));
  Output.SaveToStream(AStream);
  Output.Free;
end;

{ TAdBMPCompressor }

class function TAdBMPCompressor.ID: TAdVeryShortString;
begin
  result := #5 + 'BMP';
end;

procedure TAdBMPCompressor.Read(ABitmap: TAdBitmap; AStream: TStream);
begin
  ABitmap.LoadFromStream(AStream);
end;

procedure TAdBMPCompressor.Write(ABitmap: TAdBitmap; AStream: TStream);
var
  c:TAdGraphicCompressorClass;
begin
  c := ABitmap.Compressor; ABitmap.Compressor := nil;
  ABitmap.SaveToStream(AStream);
  ABitmap.Compressor := c;
end;

{ TAdBitmapFormat }

function TAdBitmapFormat.Assign(ABitmap: TAdBitmap; AGraphic: TObject): boolean;
begin
  result := true;
  with AGraphic as TAdBitmap do
  begin
    ABitmap.ReserveMemory(Width, Height);
    Move(Scanline^, ABitmap.Scanline^, ABitmap.Size);
  end;
end;

function TAdBitmapFormat.AssignAlphaChannel(ABitmap: TAdBitmap;
  AGraphic: TObject): boolean;
var
  p1, p2:PRGBARec;
  i:integer;
begin
  result := false;
  with AGraphic as TAdBitmap do
  begin
    if ABitmap.Loaded then
    begin
      result := true;
      p1 := ABitmap.ScanLine;
      p2 := Scanline;

      for i := 0 to (Size div 4) - 1 do
      begin
        p2^.a := p1^.a;
        inc(p1);
        inc(p2);
      end;
    end;
  end;
end;

function TAdBitmapFormat.AssignAlphaChannelTo(ABitmap: TAdBitmap;
  AGraphic: TObject): boolean;
begin
  result := true;
  TAdBitmap(AGraphic).AssignAlphaChannel(ABitmap);
end;

function TAdBitmapFormat.AssignTo(ABitmap: TAdBitmap;
  AGraphic: TObject): boolean;
begin
  result := true;
  TAdBitmap(AGraphic).Assign(ABitmap);
end;

class procedure TAdBitmapFormat.FileExts(strs: TStrings);
begin
  //
end;

function TAdBitmapFormat.LoadFromFile(ABitmap: TAdBitmap; AFile: string;
  ATransparent: Boolean; ATransparentColor: Integer): boolean;
begin
  result := false;
end;

class function TAdBitmapFormat.SupportsObject(AGraphic: TObject): boolean;
begin
  result := AGraphic is TAdBitmap;
end;

initialization
  RegisterGraphicCompressor(TAdBMPCompressor);
  RegisterGraphicCompressor(TAdHAICompressor);
  RegisterGraphicFormat(TAdBitmapFormat);

end.
