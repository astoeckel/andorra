{$IFNDEF inc_andorra}
{$DEFINE inc_andorra}

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
* File: inc_andorra.inc
* Comment: Used to define some compiler switches
}

{$DEFINE SUPPORTS_INLINE}
{$IFNDEF FPC}
  {$IFDEF VER160}
    {$UNDEF SUPPORTS_INLINE}
  {$ENDIF}

  {$IFDEF VER150}
    {$UNDEF SUPPORTS_INLINE}
  {$ENDIF}

  {$IFDEF VER140}
    {$UNDEF SUPPORTS_INLINE}
  {$ENDIF}

  {$IFDEF VER130}
    {$UNDEF SUPPORTS_INLINE}
    {$DEFINE DELPHI5_DOWN}
  {$ENDIF}

  {$IFDEF VER125}
    {$UNDEF SUPPORTS_INLINE}
    {$DEFINE DELPHI5_DOWN}
  {$ENDIF}

  {$IFDEF VER120}
    {$UNDEF SUPPORTS_INLINE}
    {$DEFINE DELPHI5_DOWN}
  {$ENDIF}

  {$IFDEF VER110}
    {$UNDEF SUPPORTS_INLINE}
    {$DEFINE DELPHI5_DOWN}
  {$ENDIF}

  {$IFDEF VER100}
    {$UNDEF SUPPORTS_INLINE}
    {$DEFINE DELPHI5_DOWN}
  {$ENDIF}

  {$IFDEF VER93}
    {$UNDEF SUPPORTS_INLINE}
    {$DEFINE DELPHI5_DOWN}
  {$ENDIF}

  {$IFDEF VER90}
    {$UNDEF SUPPORTS_INLINE}
    {$DEFINE DELPHI5_DOWN}
  {$ENDIF}

  {$IFDEF VER80}
    {$UNDEF SUPPORTS_INLINE}
    {$DEFINE DELPHI5_DOWN}
  {$ENDIF}
{$ENDIF}


{$IFNDEF DELPHI5_DOWN}
{$IFNDEF SUPPORTS_INLINE}
  {$MESSAGE Hint 'Andorra 2D: The "inline" directive isn''t supported in this compiler version.'}
{$ENDIF}
{$ENDIF}

{$ENDIF}