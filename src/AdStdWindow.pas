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
* File: AdStdWindow.pas
* Comment: Just a switch unit for loading the right window framework unit.
}

{Just a switch unit for loading the right window framework unit.}
unit AdStdWindow;

interface

uses
  {$IFDEF FPC}
    {$IFDEF LINUX}
    AdLCLOGLWindow;
    {$ELSE}
    AdVCLWindow;
    {$ENDIF}
  {$ELSE}
    AdVCLWindow;
  {$ENDIF}

implementation

end.