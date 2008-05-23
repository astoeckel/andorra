{
* This program is licensed under the Common Public License (CPL) Version 1.0
* You should have recieved a copy of the license with this file.
* If not, see http://www.opensource.org/licenses/cpl1.0.txt for more informations.
* 
* Inspite of the incompatibility between the Common Public License (CPL) and the GNU General Public License (GPL) you're allowed to use this program
* under the GPL. 
* You also should have recieved a copy of this license with this file. 
* If not, see http://www.gnu.org/licenses/gpl.txt for more informations.

* Project: Andorra 2D
* Author:  Andreas Stoeckel
* File: AdShaderClasses.pas
* Comment: Plugin interface unit for shader support in Andorra 2D.
}

{Plugin interface unit for shader support in Andorra 2D.}
unit AdShaderClasses;

interface

uses
  AdClasses, AdTypes;

type
  TAd2dShader = class;

  TAd2dShaderSystem = class
    protected
      function GetInitialized: boolean;virtual;abstract;
    public
      function CreateShader(AID: TAdVeryShortString): TAd2dShader;virtual;abstract;

      procedure Initialize(AApplication: TAd2dApplication);virtual;abstract;
      procedure Finalize;virtual;abstract;

      property Initialized: boolean read GetInitialized;
  end;

  TAd2dShader = class
    protected
      function GetLoaded: boolean;virtual;abstract;
    public
      procedure LoadProgramFromBuffer(ABuf: PChar;
        ASourceType: TAd2dShaderSourceType; AProgramName: PChar;
        AShaderType: TAd2dShaderType);virtual;abstract;

      procedure Initialize;virtual;abstract;
      procedure Finalize;virtual;abstract;

      procedure Bind;virtual;abstract;
      procedure Unbind;virtual;abstract;

      function GetParameter(AName: PChar): Pointer;virtual;abstract;

      procedure SetParameter(AParam: Pointer; AValue: single);overload;virtual;abstract;
      procedure SetParameter(AParam: Pointer; AValue: TAdMatrix);overload;virtual;abstract;
      procedure SetParameter(AParam: Pointer; AValue: TAd2dTexture);overload;virtual;abstract;
      procedure SetParameter(AParam: Pointer; AValue: TAndorraColor);overload;virtual;abstract;
      procedure SetParameter(AParam: Pointer; AValue: PChar; ASize: Cardinal);overload;virtual;abstract;

      property Loaded: boolean read GetLoaded;
  end;

  //
  TAdCreateShaderProc = function:TAd2dShaderSystem;stdcall;

implementation

end.
