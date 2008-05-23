unit AdRAWFontData;

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
* File: AdRAWFontData.pas
* Comment: Contains a loader class and a save procedure for RAW Andorra Font Data
}

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
  Classes, AdTypes, AdClasses, AdFont, AdFontGenerator, AdBitmap;

type
  TAdRAWFontData = class(TAdFontGenerator)
    public
      procedure Generate(AData:Pointer;ASize:Cardinal;
        var ASizes:TAdCharSizes; var APatterns: TAdCharPatterns; ATexture:TAd2dBitmapTexture);override;
      function IsValidData(AData:Pointer;ASize:Cardinal):boolean;override;
  end;

procedure SaveRAWFontData(AFont:TAdFont;AStream:TStream);

implementation

procedure SaveRAWFontData(AFont:TAdFont;AStream:TStream);
var
  s:TAdVeryShortString;
  c:Cardinal;
  bmp:TAdBitmap;
  ms:TMemoryStream;
begin
  ms := TMemoryStream.Create;

  s := 'RAWF';
  ms.Write(s, SizeOf(s));

  ms.Write(AFont.CharSizes[0], SizeOf(TAdCharSizes));
  ms.Write(AFont.CharPatterns[0], SizeOf(TAdCharPatterns));

  bmp := TAdBitmap.Create;
  bmp.ReserveMemory(AFont.Texture.BaseWidth, AFont.Texture.BaseHeight);
  AFont.Texture.SaveToBitmap(bmp);
  bmp.SaveToStream(ms);
  bmp.Free;

  c := ms.Size;
  AStream.Write(c, SizeOf(c));
  ms.Position := 0;
  ms.SaveToStream(AStream);
  ms.Free;
end;

{ TAdRAWFontData }

procedure TAdRAWFontData.Generate(AData: Pointer; ASize: Cardinal;
  var ASizes: TAdCharSizes; var APatterns: TAdCharPatterns;
  ATexture: TAd2dBitmapTexture);
var
  ms:TMemoryStream;
  bmp:TAdBitmap;
begin
  ms := TMemoryStream.Create;

  ms.Write(AData^, ASize);
  ms.Position := 5;

  ms.Read(ASizes[0], SizeOf(TAdCharSizes));
  ms.Read(APatterns[0], SizeOf(TAdCharPatterns));

  bmp := TAdBitmap.Create;
  bmp.LoadFromStream(ms);

  ATexture.LoadFromBitmap(bmp, ad32Bit);

  bmp.Free;

  ms.Free;
end;

function TAdRAWFontData.IsValidData(AData: Pointer; ASize: Cardinal): boolean;
var
  pss:^TAdVeryShortString;
begin
  pss := AData;
  result := (ASize > 5) and (pss^ = 'RAWF') ;
end;

initialization
  RegisterFontGeneratorClass(TAdRAWFontData);

end.
