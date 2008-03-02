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

unit AdFontGenerator;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, AdTypes, AdClasses, AdPersistent;

type
  TAdFontGenerator = class(TAdPersistent)
    public
      procedure Generate(AData:Pointer;ASize:Cardinal;
        var ASizes:TAdCharSizes; var APatterns: TAdCharPatterns; ATexture:TAd2dBitmapTexture);virtual;abstract;
      function IsValidData(AData:Pointer;ASize:Cardinal):boolean;virtual;abstract;
  end;

  TAdFontGeneratorClass = class of TAdFontGenerator;

var
  RegisteredGenerators:TStringList;

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
