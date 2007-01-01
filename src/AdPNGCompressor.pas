{
* This program is licensed under the GNU Lesser General Public License Version 2
* You should have recieved a copy of the license with this file.
* If not, see http://www.gnu.org/licenses/lgpl.html for more informations
*
* Project: Andorra 2D
* Author:  Andreas Stoeckel
* File: AdPNGCompressor.pas
* Comment: A PNG compressor. It needs the PNG Delphi Sources which are available
           at http://pngdelphi.sourceforge.net/
}

//A PNG compressor. It needs the PNG Delphi Sources which are available at http://pngdelphi.sourceforge.net/
unit AdPNGCompressor;

interface

uses PngImage,AdDraws,Classes,Graphics;

type TPNGCompressor = class(TCompressor)
  public
    //Returns the initial letters of this compressor. Will be calles without creating the object!!!
    function GetInitial:TInitialLetters;override;
    //Writes the two bitmaps into a stream
    procedure Write(AStream:TStream;ABitmap:TBitmap;AAlphaChannel:TBitmap);override;
    //Reads the two bitmaps from the stream and copies them into ABitmap and AAlphaChannel.
    procedure Read(AStream:TStream;ABitmap:TBitmap;AAlphaChannel:TBitmap);override;
end;

implementation

type TRGBRec = packed record
  r,g,b:byte;
end;

type PRGBRec = ^TRGBRec;

{ TPNGCompressor }

function TPNGCompressor.GetInitial: TInitialLetters;
begin
  result := #2+'PNG'
end;

procedure TPNGCompressor.Read(AStream: TStream; ABitmap,
  AAlphaChannel: TBitmap);
var
  PNG:TPNGObject;
  procedure GetAlpha(APNG:TPNGObject;ABMP:TBitmap);
  var x,y:integer;
      sl1:PByteArray;
      sl2:PRGBRec;
      a:byte;
  begin
    for y := 0 to APNG.Height-1 do
    begin
      sl1 := APNG.AlphaScanline[y];
      sl2 := ABMP.ScanLine[y];
      for x := 0 to APNG.Width - 1 do
      begin
        a := sl1[x];
        sl2.r := a;
        sl2.g := a;
        sl2.b := a;
        inc(sl2);
      end;
    end;
  end;
begin
  PNG := TPNGObject.Create;
  PNG.LoadFromStream(AStream);
  ABitmap.Width := PNG.Width;
  ABitmap.Height := PNG.Height;
  ABitmap.PixelFormat := pf24Bit;
  AAlphaChannel.Width := PNG.Width;
  AAlphaChannel.Height := PNG.Height;
  AAlphaChannel.PixelFormat := pf24Bit;
  GetAlpha(PNG,AAlphaChannel);
  PNG.RemoveTransparency;
  PNG.AssignTo(ABitmap);
  PNG.Free;
end;

procedure TPNGCompressor.Write(AStream: TStream; ABitmap,
  AAlphaChannel: TBitmap);
var
  PNG:TPNGObject;
  procedure AddAlpha(APNG:TPNGObject;ABMP:TBitmap);
  var x,y:integer;
      sl1:PByteArray;
      sl2:PRGBRec;
      a:byte;
  begin
    for y := 0 to APNG.Height-1 do
    begin
      sl1 := APNG.AlphaScanline[y];
      sl2 := ABMP.ScanLine[y];
      for x := 0 to APNG.Width - 1 do
      begin
        a := (sl2^.r+sl2^.g+sl2^.b) div 3;
        sl1[x] := a;
        inc(sl2);
      end;
    end;
  end;
begin
  PNG := TPNGObject.Create;
  PNG.Assign(ABitmap);
  PNG.CreateAlpha;
  AddAlpha(PNG,AAlphaChannel);
  PNG.CompressionLevel := 9;
  PNG.SaveToStream(AStream);
  PNG.Free;
end;

initialization
  RegisterCompressor(TPNGCompressor);

end.
