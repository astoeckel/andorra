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
* File: AdJPEG.pas
* Comment: JPEG loader class
}

{JPEG loader class - just add "AdJPEG" to your uses-clausel}
unit AdJPEG;

interface

uses JPEG, AdDraws, AdClasses, AdVCLFormats, AdBitmap, Classes, Graphics;

type
  {A loader for JPEG files and TJPEGImage.}
  TAdJPEGFormat = class(TAdGraphicFormat)
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



{ TAdJPEGFormat }

class procedure TAdJPEGFormat.FileExts(strs: TStrings);
begin
  strs.Add('.jpg');
  strs.Add('.jpeg');
end;

class function TAdJPEGFormat.SupportsObject(AGraphic: TObject): boolean;
begin
  //All this stuff may be done by TAdVCLFormats
  result := false;
end;

function TAdJPEGFormat.Assign(ABitmap: TAdBitmap; AGraphic: TObject): boolean;
begin
  result := false;
end;

function TAdJPEGFormat.AssignAlphaChannel(ABitmap: TAdBitmap;
  AGraphic: TObject): boolean;
begin
  result := false;
end;

function TAdJPEGFormat.AssignAlphaChannelTo(ABitmap: TAdBitmap;
  AGraphic: TObject): boolean;
begin
  result := false;
end;

function TAdJPEGFormat.AssignTo(ABitmap: TAdBitmap; AGraphic: TObject): boolean;
begin
  result := false;
end;

function TAdJPEGFormat.LoadFromFile(ABitmap: TAdBitmap; AFile: string;
  ATransparent: Boolean; ATransparentColor: Integer): boolean;
var
  jpg:TJPEGImage;
  bmp:TBitmap;
begin
  result := true;
  jpg := TJPEGImage.Create;
  try
    jpg.LoadFromFile(AFile);
    bmp := TBitmap.Create;
    bmp.Assign(jpg);
    bmp.Transparent := ATransparent;
    bmp.TransparentColor := ATransparentColor;
    
    ABitmap.Assign(bmp);
    
    bmp.Free;
  except
    result := false;
  end;
  jpg.Free;
end;

initialization
  RegisterGraphicFormat(TAdJPEGFormat);

end.
