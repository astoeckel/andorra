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

  {A abstract shader generating class that is exchanged between the graphic 
   plugin and the host application. Is created by the plugin top layer function
   "CreateShaderSystem". To call this function, you have to use the 
   TAdDllLoader object. Instead of this class, you may also want to use the
   more abstracted TAdShaderSystem class from AdShader.
   @seealso(TAdDllLoader)
   @seealso(TAdShaderSystem)
  }
  TAd2dShaderSystem = class
    protected
      function GetInitialized: boolean;virtual;abstract;
    public
      {Creates a new shader class from the given shader id.}
      function CreateShader(AID: TAdVeryShortString): TAd2dShader;virtual;abstract;

      {Initialized the shader system. This function should be called when the
       after the main surface has been initialized.}
      procedure Initialize(AApplication: TAd2dApplication);virtual;abstract;
      {Finalizes the shader system. This method should be called before the 
       the main surface has been finalized.}
      procedure Finalize;virtual;abstract;

      {Returns whether the system is initialized.}
      property Initialized: boolean read GetInitialized;
  end;

  {The abstract shader class that is created by a shader system. Istead of 
   using this abstract shader class, you may also use TAdShader from the
   AdShader unit.
   @seealso(TAd2dShaderSystem)
   @seealso(TAdShaderSystem)
   @seealso(TAdShader)}
  TAd2dShader = class
    protected
      function GetLoaded: boolean;virtual;abstract;
    public
      {Loads a shader program from a buffer.
       @param(ASourceType defines the type of the source code: binary or 
         as sourcecode)
       @param(AProgramName specifies the name of the program in the shader
         sourcecode)
       @param(AShaderType defines the type of the shader program:
         a vertex or a fragment shader.)}
      procedure LoadProgramFromBuffer(ABuf: PAnsiChar;
        ASourceType: TAd2dShaderSourceType; AProgramName: PAnsiChar;
        AShaderType: TAd2dShaderType);virtual;abstract;

      {Initializes the shader. This function should be called after the main
       surface is initialized.}
      procedure Initialize;virtual;abstract;
      {Finalizes the shader. This function should be called before the main
       surface has been finalized.}
      procedure Finalize;virtual;abstract;

      {Activates the shader effect.}
      procedure Bind;virtual;abstract;
      {Unbinds the shader effect.}
      procedure Unbind;virtual;abstract;

      {Returns the pointer to the parameter with a specific name.}
      function GetParameter(AName: PAnsiChar): Pointer;virtual;abstract;

      {Sets a float parameter.}
      procedure SetParameter(AParam: Pointer; AValue: PSingle; ACount: integer);overload;virtual;abstract;
      {Sets a integer parameter.}
      procedure SetParameter(AParam: Pointer; AValue: PInteger; ACount: integer);overload;virtual;abstract;
      {Sets a texture parameter.}
      procedure SetParameter(AParam: Pointer; AValue: TAd2dTexture);overload;virtual;abstract;

      {Returns whether the shader is loaded.}
      property Loaded: boolean read GetLoaded;
  end;

  //Procedure used in the plugin library to create a shader system.
  TAdCreateShaderProc = function:TAd2dShaderSystem;stdcall;

implementation

end.
