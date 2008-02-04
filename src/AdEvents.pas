unit AdEvents;

interface

type
  TAdMouseButton = (abLeft, abRight, abMiddle);
  TAdShiftStates = (asShift, asCtrl, asLeft, asRight, asMiddle, asAlt, asDouble);
  TAdShiftState = set of TAdShiftStates;

  TAdNotifyEvent = procedure(Sender: TObject) of object;
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
