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
* File: AdFontGenerator.pas
* Comment: Contains an abstract class for font generators
}

{Contains an abstract class for font generators.}
unit AdFontGenerator;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, AdTypes, AdClasses, AdPersistent;

type
  {The base, abstract font generator class. TAdFontGenerator can be extended
   by other units in order to provide support of another font gernerator
   interface e.g. to use external libraries instead of the default vcl/lcl
   depenend font generator.}
  TAdFontGenerator = class(TAdPersistent)
    public
      {Generates the font data using the date given in "AData".
       @param(AData is a pointer to the data that represents font information)
       @param(ASize specifies the size of the data)
       @param(ASizes is used to store the size of every char in the font
         bitmap)
       @param(APatterns is a array of TAdRect that specifies the position of 
         each letter in the bitmap)
       @param(ATexture contains the bitmap font data)}
      procedure Generate(AData:Pointer; ASize:Cardinal;
        var ASizes:TAdCharSizes; var APatterns: TAdCharPatterns; 
        ATexture:TAd2dBitmapTexture);virtual;abstract;
      {Before calling the generate function of a font generator, the
       font manager will call the "IsValidData" of the class, to check 
       whether this font data can be read by the generator.}
      function IsValidData(AData:Pointer; ASize:Cardinal):boolean;virtual;abstract;
  end;

  {A class of TAdFontGenerator}
  TAdFontGeneratorClass = class of TAdFontGenerator;

var
  {Contains the names of all registered font generators. Content of the list
   should not be changed manually.}
  RegisteredGenerators:TStringList;

{Registers a font generator class that can be used by the font manager.
 Should be called in the "initialization" part of the font generator class
 unit.}
procedure RegisterFontGeneratorClass(AClass:TAdFontGeneratorClass);

implementation

procedure RegisterFontGeneratorClass(AClass:TAdFontGeneratorClass);
begin
  RegisteredGenerators.Add(AClass.ClassName);
  AdRegisterClass(AClass);
end;

initialization
  RegisteredGenerators := TStringList.Create;

finalization
  RegisteredGenerators.Free;

end.
