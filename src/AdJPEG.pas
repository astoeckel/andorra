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

uses JPEG, AdDraws, AdClasses, Classes, Graphics;

type
  {A loader for JPEG files and TJPEGImage.}
  TJPEGFormat = class(TPictFormat)
    public
      //Fills a list with its supported graphic extension.
      procedure FileExts(strs:TStringList);override;
      //Loads the graphic from a file and stros it in a TAdBitmap.
      function LoadFromFile(AFile:string;ABmp:TAdBitmap;Transparent:boolean;TransparentColor:TColor):boolean;override;
      //Assigns an TGraphic and  stores it in a TAdBitmap
      procedure AssignGraphic(AGraphic:TGraphic;ABmp:TAdBitmap);override;
      //Returns true if this format supports the graphicclass defined in AGraphicClass
      function SupportsGraphicClass(AGraphicClass:TGraphicClass):boolean;override;
end;

implementation

{ TJPEGFormat }

procedure TJPEGFormat.AssignGraphic(AGraphic: TGraphic; ABmp: TAdBitmap);
var
  jpeg:TJPEGImage;
  bmp:TBitmap;
begin
  if AGraphic is TJPEGImage then
  begin
    jpeg := TJPEGImage(AGraphic);
    ABmp.ReserveMemory(jpeg.Width,jpeg.Height);
    bmp := TBitmap.Create;
    bmp.Assign(jpeg);
    ABmp.AssignBitmap(bmp);
    bmp.Free;
  end;
end;

procedure TJPEGFormat.FileExts(strs: TStringList);
begin
  strs.Add('.jpg');
  strs.Add('.jpeg');
end;

function TJPEGFormat.LoadFromFile(AFile: string; ABmp: TAdBitmap;
  Transparent: boolean; TransparentColor: TColor): boolean;
var
  jpeg:TJpegImage;
  bmp:TBitmap;
begin
  result := false;
  jpeg := TJpegImage.Create;
  bmp := TBitmap.Create;
  try
    jpeg.LoadFromFile(AFile);

    ABmp.ReserveMemory(jpeg.Width,jpeg.Height);

    bmp.Assign(jpeg);
    bmp.Transparent := Transparent;
    bmp.TransparentMode := tmFixed;
    bmp.TransparentColor := TransparentColor;

    ABmp.AssignBitmap(bmp);

    result := true;
  finally
    bmp.Free;
    jpeg.Free;
  end;
end;

function TJPEGFormat.SupportsGraphicClass(AGraphicClass: TGraphicClass): boolean;
begin
  result := AGraphicClass = TJpegImage;
end;

initialization
  RegisterFormat(TJPEGFormat);

end.
