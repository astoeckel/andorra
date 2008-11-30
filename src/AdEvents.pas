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
* File: AdEvents.pas
* Comment: Contains event handler and key-code declarations
}

{Contains event handler and key-code declarations}
unit AdEvents;

interface

type
  {Represents one of three mouse buttons}
  TAdMouseButton = (
    abLeft, //< Left mouse button
    abRight, //< Right mouse button
    abMiddle //< Middle mouse button
  );

  {Represents the current special key state.}
  TAdShiftStates = (
    asShift, //< A shift key is pressed
    asCtrl, //< A ctrl key is pressed
    asLeft, //< The left mouse button is pressed
    asRight, //< The right mouse button is pressed
    asMiddle, //< The middle mouse button is pressed
    asAlt, //< A alt key is pressed
    asDouble //< The left mouse key double clicked
  );

  {Set of TAdShiftState.}
  TAdShiftState = set of TAdShiftStates;

  TAdClickEvent = procedure(Sender: TObject; X, Y:integer) of object;
  TAdCloseEvent = procedure(Sender: TObject; var CanClose: Boolean) of object;
  TAdMouseEvent = procedure(Sender: TObject; Button: TAdMouseButton;
    Shift: TAdShiftState; X, Y: integer) of object;
  TAdMouseMoveEvent = procedure(Sender: TObject; Shift: TAdShiftState;
    X, Y: integer) of object;
  TAdKeyEvent = procedure(Sender: TObject; Key: Word; Shift: TAdShiftState) of object;
  TAdKeyPressEvent = procedure(Sender: TObject; Key: Char) of object;
  TAdMouseWheelEvent = procedure(Sender: TObject; Shift: TAdShiftState;
    WheelDelta: integer; X, Y: integer) of object;
    
  TAdIdleEvent = procedure(Sender: TObject; var Done: Boolean) of object;

const
  AVK_SPACE = $20;
  AVK_ESCAPE = $1B;
  AVK_F1 = $70;
  AVK_F2 = $71;
  AVK_F3 = $72;
  AVK_F4 = $73;
  AVK_F5 = $74;
  AVK_F6 = $75;
  AVK_F7 = $76;
  AVK_F8 = $77;
  AVK_F9 = $78;
  AVK_F10 = $79;
  AVK_F11 = $7A;
  AVK_F12 = $7B;
  AVK_F13 = $7C;
  AVK_F14 = $7D;
  AVK_F15 = $7E;
  AVK_F16 = $7F;
  AVK_F17 = $80;
  AVK_F18 = $81;
  AVK_F19 = $82;
  AVK_F20 = $83;
  AVK_F21 = $84;
  AVK_F22 = $85;
  AVK_F23 = $86;
  AVK_F24 = $87;
  AVK_UP  = $26;
  AVK_DOWN = $28;
  AVK_LEFT = $25;
  AVK_RIGHT  = $27;
  AVK_SHIFT = $10;
  AVK_LSHIFT = $A0;
  AVK_RSHIFT = $A1;
  AVK_CTRL = $11;
  AVK_LCTRL = $A2;
  AVK_RCTRL = $A3;
  AVK_ALT = $12;
  AVK_LALT = $A4;
  AVK_RALT = $A5;
  AVK_TAB = $09;
  AVK_ENTER = $0D;
  AVK_BACK = $08;
  AVK_INSERT = $2D;
  AVK_DELETE = $2E;
  AVK_PAGEUP = $21;
  AVK_PAGEDOWN = $22;
  AVK_HOME = $24;
  AVK_END = $23;
  AVK_NUMPAD_0 = $60;
  AVK_NUMPAD_1 = $61;
  AVK_NUMPAD_2 = $62;
  AVK_NUMPAD_3 = $63;
  AVK_NUMPAD_4 = $64;
  AVK_NUMPAD_5 = $65;
  AVK_NUMPAD_6 = $66;
  AVK_NUMPAD_7 = $67;
  AVK_NUMPAD_8 = $68;
  AVK_NUMPAD_9 = $69;


implementation

end.
