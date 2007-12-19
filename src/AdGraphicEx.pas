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
* File: AdGraphicEx.pas
* Comment: TGraphicExGraphic loader class
}

unit AdGraphicEx;

interface

uses
  GraphicEx, SysUtils, AdDrawsOld, AdClasses, Classes, Graphics;

type
  {A loader for all files supported by TGraphicExGraphic}
  TGraphicExFormat = class(TPictFormat)
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

{ TGraphicExFormat }

procedure TGraphicExFormat.AssignGraphic(AGraphic: TGraphic; ABmp: TAdBitmap);
var
  bmp:TBitmap;
begin
  if AGraphic is TGraphicExGraphic then
  begin
    bmp := TBitmap.Create;
    bmp.Assign(AGraphic);
    ABmp.ReserveMemory(bmp.Width, bmp.Height);
    ABmp.AssignBitmap(bmp);
    bmp.Free;
  end;
end;

procedure TGraphicExFormat.FileExts(strs: TStringList);
var
  tmp:TStringList;
  i:integer;
begin
  tmp := TStringList.Create;
  FileFormatList.GetExtensionList(tmp);
  for i := 0 to tmp.Count - 1 do
    strs.Add('.'+lowercase(tmp[i]));
  tmp.Free;
end;

function TGraphicExFormat.LoadFromFile(AFile: string; ABmp: TAdBitmap;
  Transparent: boolean; TransparentColor: TColor): boolean;
var
  Graphic:TGraphicExGraphic;
  GraphicClass:TGraphicExGraphicClass;
begin
  try
    GraphicClass := FileFormatList.GraphicFromContent(lowercase(AFile));
    if GraphicClass <> nil then
    begin
      Graphic := GraphicClass.Create;
      Graphic.LoadFromFile(AFile);
      AssignGraphic(Graphic, ABmp);
      Graphic.Free;
      result := true;
    end
    else
    begin
      result := false;
    end;
  except
    result := false;
  end;
end;

function TGraphicExFormat.SupportsGraphicClass(
  AGraphicClass: TGraphicClass): boolean;
begin
  result := AGraphicClass = TGraphicExGraphic;
end;

initialization
  RegisterFormat(TGraphicExFormat);

end.

