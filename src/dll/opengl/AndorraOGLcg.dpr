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
* File: AndorraDX93Dcg.dpr
* Comment: This library adds the posibility of adding CG-Shader support to the DirectX9 plugin.
}

library AndorraOGLcg;

{$IFNDEF FPC}
  {$R *.res}
{$ENDIF}

uses
  OGLShaderClasses in 'OGLShaderClasses.pas',
  OGLCg in 'OGLCg.pas';

function OGLCreateShaderEngine: TOGLShaderEngine;stdcall;
begin
  result := TOGLCGEngine.Create;
end;

exports
  OGLCreateShaderEngine;

begin
end.
