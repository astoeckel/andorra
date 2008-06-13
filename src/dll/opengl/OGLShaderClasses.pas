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
* File: OGLShaderClasses.pas
* Comment: Interface unit
}

{Plugin interface for shader support in the OpenGL plugin.}
unit OGLShaderClasses;

interface

uses
  AdShaderClasses, AdClasses, AdTypes;

type
  TOGLShaderEngine = class
    private
      FLogProc: TAd2dLogCallback;
    protected
      function GetInitialized: boolean;virtual;abstract;
    public
      ID: TAdVeryShortString;

      procedure Initialize(ALogProc: TAd2dLogCallback);virtual;
      procedure Finalize;virtual;abstract;

      function CreateShader: TAd2dShader;virtual;abstract;
      property Log: TAd2dLogCallback read FLogProc;
      property Initialized: boolean read GetInitialized;
  end;

  TOGLCreateShaderEngineProc = function:TOGLShaderEngine;stdcall;

implementation

{ TOGLShaderEngine }

procedure TOGLShaderEngine.Initialize(ALogProc: TAd2dLogCallback);
begin
  Finalize;

  FLogProc := ALogProc;
end;

end.
