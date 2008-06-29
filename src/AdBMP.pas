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
* File: AdBMP.pas
* Comment: Adds native BMP support to Andorra 2D. No external libraries are needed.
}

{Adds native BMP support to Andorra 2D. No external libraries are needed.}
unit AdBMP;

interface

uses
  Classes,
  AdBitmap, AdTypes, AdBMPImage, AdBitmapEffects;

type
  {A loader for BMP files and TAdBMPImage.}
  TAdBMPFormat = class(TAdGraphicFormat)
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

{ TAdBMPFormat }

class procedure TAdBMPFormat.FileExts(strs: TStrings);
begin
  strs.Add('.bmp');
  strs.Add('.dib');
end;

function TAdBMPFormat.Assign(ABitmap: TAdBitmap; AGraphic: TObject): boolean;
begin
  result := true;
  
  ABitmap.Assign(TAdBMPImage(AGraphic).Bitmap);
end;

function TAdBMPFormat.AssignAlphaChannel(ABitmap: TAdBitmap;
  AGraphic: TObject): boolean;
begin
  result := false;
end;

function TAdBMPFormat.AssignAlphaChannelTo(ABitmap: TAdBitmap;
  AGraphic: TObject): boolean;
begin
  result := false;
end;

function TAdBMPFormat.AssignTo(ABitmap: TAdBitmap; AGraphic: TObject): boolean;
begin
  result := true;
  
  TAdBMPImage(AGraphic).Bitmap.Assign(ABitmap);
end;

function TAdBMPFormat.LoadFromFile(ABitmap: TAdBitmap; AFile: string;
  ATransparent: Boolean; ATransparentColor: Integer): boolean;
var
  bmp: TAdBMPImage;
  transpeff: TAdTransparencyFilter;
begin
  result := true;

  bmp := TAdBMPImage.Create;

  //Store bitmap data in the AdBitmap specified by "ABitmap"
  bmp.Bitmap := ABitmap;

  //Load the bitmap from file
  bmp.LoadFromFile(AFile);
  bmp.SaveToFile('test.bmp');

  //Set the transparent color
  transpeff := TAdTransparencyFilter.Create;
  transpeff.Transparent := ATransparent;
  transpeff.TransparentColor := ATransparentColor;
  transpeff.AssignEffect(ABitmap);
  transpeff.Free;

  //Free the bitmap
  bmp.Free;
end;

class function TAdBMPFormat.SupportsObject(AGraphic: TObject): boolean;
begin
  result := AGraphic is TAdBMPImage;
end;

initialization
  RegisterGraphicFormat(TAdBMPFormat);

end.
