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
* Author:  Manuel Eberl
* File: AdPNG.pas
* Comment: Adds png loading capabilities to Andorra 2D
}
{Adds png loading capabilities to Andorra 2D.}
unit AdPNG;

interface

uses
  Classes,
  AdTypes, AdBitmap, AdBitmapEffects, AdPNGImage, AdSimpleCompressors;


type
  {A compressor to store textures in the PNG format}
  TAdPNGCompressor = class(TAdGraphicCompressor)
    public
      class function ID:TAdVeryShortString;override;
      procedure Write(ABitmap:TAdBitmap; AStream:TStream);override;
      procedure Read(ABitmap:TAdBitmap; AStream:TStream);override;
  end;

  {A loader for PNG files and TAdPNGImage.}
  TAdPNGFormat = class(TAdGraphicFormat)
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

{ TPNGCompressor }

class function TAdPNGCompressor.ID: TAdVeryShortString;
begin
  result := #2+'PNG'
end;

procedure TAdPNGCompressor.Read(ABitmap:TAdBitmap; AStream:TStream);
var
  PNG:TAdPNGImage;
begin
  PNG := TAdPNGImage.Create;
  try
  	PNG.LoadFromStream(AStream);
    ABitmap.Assign(PNG.Bitmap);
  finally
    PNG.Free;
  end;
end;

procedure TAdPNGCompressor.Write(ABitmap:TAdBitmap; AStream:TStream);
{var
  PNG:TAdPNGImage;}
var
  comp: TAdBMPCompressor;
  str: TAdVeryShortString;
begin
{  PNG := TAdPNGImage.Create;
  try
    PNG.Bitmap:=ABitmap;
  	PNG.SaveToStream(AStream);
  finally
    PNG.Free;
  end;  }
{$MESSAGE HINT 'PNG compressing using the AdPNG.pas is currently disabled. Use AdFreeImage or AdDevIL instead.'}
  //! Temporally use the TAdBMPCompressor instead.

  //Rewind four bytes...
  AStream.Position := AStream.Position - 4;

  //... and write the bmp compressor ID
  str := TAdBMPCompressor.ID;
  AStream.Write(str[1], 4);

  comp := TAdBMPCompressor.Create;
  comp.Write(ABitmap, AStream);
  comp.Free;
end;

{ TPNGFormat }

class procedure TAdPNGFormat.FileExts(strs: TStrings);
begin
  strs.Add('.PNG')
end;

class function TAdPNGFormat.SupportsObject(AGraphic: TObject): boolean;
begin
  result := AGraphic is TAdPNGImage;
end;

function TAdPNGFormat.Assign(ABitmap: TAdBitmap; AGraphic:TObject): boolean;
var
  PNG:TAdPNGImage;
begin
  result := false;
  if AGraphic is TAdPNGImage then
  begin
    PNG := TAdPNGImage(AGraphic);
    ABitmap.Assign(PNG.Bitmap);
  end;
end;

function TAdPNGFormat.AssignAlphaChannel(ABitmap: TAdBitmap;
  AGraphic: TObject): boolean;
var
  PNG:TAdPNGImage;
begin
  result := false;
  if (AGraphic is TAdPNGImage) and (ABitmap.Loaded) then
  begin
    PNG := TAdPNGImage(AGraphic);
    PNG.AssignAlphaChannelTo(ABitmap);
    result := true;
  end;
end;

function TAdPNGFormat.AssignAlphaChannelTo(ABitmap: TAdBitmap;
  AGraphic: TObject): boolean;
var
  PNG:TAdPNGImage;
begin
  result := false;
  if AGraphic is TAdPNGImage then
  begin
    PNG := TAdPNGImage(AGraphic);
  	PNG.AssignAlphaChannel(ABitmap);
    result := true;
  end;
end;

function TAdPNGFormat.AssignTo(ABitmap: TAdBitmap;
  AGraphic: TObject): boolean;
var
  PNG:TAdPNGImage;
begin
  result := false;
  if (AGraphic is TAdPNGImage) and (ABitmap.Loaded) then
  begin
    PNG := TAdPNGImage(AGraphic);
    PNG.Bitmap:=ABitmap;
  end;
end;

function TAdPNGFormat.LoadFromFile(ABitmap:TAdBitmap; AFile:string;
        ATransparent:Boolean; ATransparentColor:LongInt): boolean;
var
  PNG: TAdPNGImage;
  eff: TAdTransparencyFilter;
begin
  result := true;
  PNG := TAdPNGImage.Create;
  try
    PNG.LoadFromFile(AFile); 
    Assign(ABitmap, PNG);

    //Apply transparency effect
    eff := TAdTransparencyFilter.Create;
    try
      eff.Transparent := ATransparent;
      eff.TransparentColor := ATransparentColor;
      eff.AssignEffect(ABitmap);
    finally
      eff.Free;
    end;       
  finally
    PNG.Free;
  end;
end;

initialization
  RegisterGraphicCompressor(TAdPNGCompressor);
  RegisterGraphicFormat(TAdPNGFormat);

end.
