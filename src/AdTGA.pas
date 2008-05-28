{
* This program is licensed under the Common Public License (CPL) Version 1.0
* You should have recieved a copy of the license with this file.
* If not, see http://www.opensource.org/licenses/cpl1.0.txt for more informations.
*
* Inspite of the incompatibility between the Common Public License (CPL) and the GNU General Public License (GPL) you're allowed to use this program 
* under the GPL.
* You also should have recieved a copy of this license with this file.
* If not, see http://www.gnu.org/licenses/gpl.txt for more informations.
*
* Project: Andorra 2D
* Author:  Manuel Eberl
* File: AdTGA.pas
* Comment: Adds the ability of loading TGA-Files to Andorra 2D
}

{Adds the ability of loading TGA-Files to Andorra 2D}
unit AdTGA;

interface

uses
  Classes, AdBitmap, AdTypes, AdVCLFormats, AdTGAImage;


type
  {A compressor to store textures in the TGA format}
  TAdTGACompressor = class(TAdGraphicCompressor)
    public
      class function ID:TAdVeryShortString;override;
      procedure Write(ABitmap:TAdBitmap; AStream:TStream);override;
      procedure Read(ABitmap:TAdBitmap; AStream:TStream);override;
  end;

  {A loader for TGA files and TAdTGAImage.}
  TAdTGAFormat = class(TAdGraphicFormat)
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

{ TTGACompressor }

class function TAdTGACompressor.ID: TAdVeryShortString;
begin
  result := #2+'TGA'
end;

procedure TAdTGACompressor.Read(ABitmap:TAdBitmap; AStream:TStream);
var
  TGA:TAdTGAImage;
begin
  TGA := TAdTGAImage.Create;
  try
  	TGA.LoadFromStream(AStream);
    ABitmap.Assign(TGA.Bitmap);
  finally
    TGA.Free;
  end;
end;

procedure TAdTGACompressor.Write(ABitmap:TAdBitmap; AStream:TStream);
var
  TGA:TAdTGAImage;
begin
  TGA := TAdTGAImage.Create;
  try
    TGA.Bitmap:=ABitmap;
  	TGA.SaveToStream(AStream);
  finally
    TGA.Free;
  end;
end;

{ TTGAFormat }

class procedure TAdTGAFormat.FileExts(strs: TStrings);
begin
  strs.Add('.TGA')
end;

class function TAdTGAFormat.SupportsObject(AGraphic: TObject): boolean;
begin
  result := AGraphic is TAdTGAImage;
end;

function TAdTGAFormat.Assign(ABitmap: TAdBitmap; AGraphic:TObject): boolean;
var
  TGA:TAdTGAImage;
begin
  result := false;
  if AGraphic is TAdTGAImage then
  begin
    TGA := TAdTGAImage(AGraphic);
    ABitmap.Assign(TGA.Bitmap);
  end;
end;

function TAdTGAFormat.AssignAlphaChannel(ABitmap: TAdBitmap;
  AGraphic: TObject): boolean;
var
  TGA:TAdTGAImage;
begin
  result := false;
  if (AGraphic is TAdTGAImage) and (ABitmap.Loaded) then
  begin
    TGA := TAdTGAImage(AGraphic);
    TGA.AssignAlphaChannelTo(ABitmap);
    result := true;
  end;
end;

function TAdTGAFormat.AssignAlphaChannelTo(ABitmap: TAdBitmap;
  AGraphic: TObject): boolean;
var
  TGA:TAdTGAImage;
begin
  result := false;
  if AGraphic is TAdTGAImage then
  begin
    TGA := TAdTGAImage(AGraphic);
  	TGA.AssignAlphaChannel(ABitmap);
    result := true;
  end;
end;

function TAdTGAFormat.AssignTo(ABitmap: TAdBitmap;
  AGraphic: TObject): boolean;
var
  TGA:TAdTGAImage;
begin
  result := false;
  if (AGraphic is TAdTGAImage) and (ABitmap.Loaded) then
  begin
    TGA := TAdTGAImage(AGraphic);
    TGA.Bitmap:=ABitmap;
  end;
end;

function TAdTGAFormat.LoadFromFile(ABitmap:TAdBitmap; AFile:string;
        ATransparent:Boolean; ATransparentColor:LongInt): boolean;
var
  TGA: TAdTGAImage;
begin
  result := true;
  TGA := TAdTGAImage.Create;
  try
    TGA.LoadFromFile(AFile);
    Assign(ABitmap, TGA);
  finally
    TGA.Free;
  end;
end;

initialization
  RegisterGraphicCompressor(TAdTGACompressor);
  RegisterGraphicFormat(TAdTGAFormat);

end.
