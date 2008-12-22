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
* File: andorra_conf.inc
* Comment: This file is used for configuring Andorra 2D. Simply copy this file
  to your project directory and add it to your project directory.
  Deactivate or activate the compiler switches as you like.
}

//IMPORTANT! HOW TO USE THE CONFIGURATION FILE!
//If you do not want to use the settings in this file for all Andorra 2D projects,
//rename this file in the Andorra 2D source directory and copy this file to your Andorra 2D
//projects.

{$IFNDEF ANDORRA_CONF}
{$DEFINE ANDORRA_CONF}

//--------------------------------
//--- ANDORRA 2D CORE SETTINGS ---

//Activate this compiler switch in order to remove the Andorra 2D dependency to the unit
//"Graphics"
{'$DEFINE DO_NOT_INCLUDE_STD_FORMATS}

//Activate this compiler switch in order to remove all Andorra 2D dependencies to the VCL
{$DEFINE DO_NOT_INCLUDE_STD_WINDOWMGR}

//--------------------------------
//--- ANDORRA 2D MATH SETTINGS ---

//Remove ' if you do not want to use 3DNOW!. This may lead to problems with AMD processors.
//If any problem occurs deactivate this compiler switch and report this problem.
{$DEFINE DO_NOT_USE_3DNOW}

//Remove ' if you do not want to use any x86 ASM code
{'$DEFINE DO_NOT_USE_ASM}

//--------------------------------
//--- ANDORRA 2D PNG ---

{$IFDEF WIN32}
  //Deactivate the following switch to activate the internal (experimental) PNG loader
  {$DEFINE DO_NOT_USE_INTERNAL_PNG}
{$ENDIF}

//--------------------------------
//--- ANDORRA 2D MISC ---

//Activate this switch if you want to know, whether this configuration file
//is actually used.
{'$DEFINE TEST}
{$IFDEF TEST}
  {$MESSAGE HINT 'This instance of the andorra_conf.inc is used'}
{$ENDIF}   

{$ENDIF}

