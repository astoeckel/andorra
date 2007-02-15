//Source: http://www.delphipraxis.net/topic14072_5x5blur+bzw+antialiasing.html

unit AdBlur;

interface

uses
  Graphics;
  
procedure BmpGBlur(Bmp: TBitmap; radius: Single);

implementation

procedure BmpGBlur(Bmp: TBitmap; radius: Single);
Type
  TRGB      = Packed Record b, g, r: Byte End;
  TRGBs     = Packed Record b, g, r: Single End;
  TRGBArray = Array[0..0] of TRGB;
Var
  MatrixRadius: Byte;
  Matrix : Array[-100..100] of Single;

  Procedure CalculateMatrix;
  Var x: Integer; Divisor: Single;
  Begin
    radius:=radius+1; // der mittel/nullpunkt muss mitgerechnet werden
    MatrixRadius:=Trunc(radius);
    If Frac(radius)=0 Then Dec(MatrixRadius);
    Divisor:=0;
    For x:=-MatrixRadius To MatrixRadius Do Begin
      Matrix[x]:=radius-abs(x);
      Divisor:=Divisor+Matrix[x];
    End;
    For x:=-MatrixRadius To MatrixRadius Do
      Matrix[x]:=Matrix[x]/Divisor;
  End;

Var
  BmpSL              : ^TRGBArray;
  BmpRGB             : ^TRGB;
  BmpCopy            : Array of Array of TRGBs;
  BmpCopyRGB         : ^TRGBs;
  R, G, B            : Single;
  BmpWidth, BmpHeight: Integer;
  x, y, mx           : Integer;
Begin
  Bmp.PixelFormat:=pf24bit;
  If radius<=0 Then radius:=1 Else If radius>99 Then radius:=99; // radius bereich 0 < radius < 99
  CalculateMatrix;
  BmpWidth:=Bmp.Width;
  BmpHeight:=Bmp.Height;
  SetLength(BmpCopy,BmpHeight,BmpWidth);
  // Alle Bildpunkte ins BmpCopy-Array schreiben und gleichzeitig HORIZONTAL blurren
  For y:=0 To Pred(BmpHeight) Do Begin
    BmpSL:=Bmp.Scanline[y];
    BmpCopyRGB:=@BmpCopy[y,0];
    For x:=0 to Pred(BmpWidth) Do Begin
      R:=0; G:=0; B:=0;
      For Mx:=-MatrixRadius To MatrixRadius Do Begin
        If x+mx<0 Then
          BmpRGB:=@BmpSL^[0]              // erster Pixel
        Else If x+mx>=BmpWidth Then
          BmpRGB:=@BmpSL^[Pred(BmpWidth)] // letzter Pixel
        Else
          BmpRGB:=@BmpSL^[x+mx];
        B:=B+BmpRGB^.b*Matrix[mx];
        G:=G+BmpRGB^.g*Matrix[mx];
        R:=R+BmpRGB^.r*Matrix[mx];
      End;
      BmpCopyRGB^.b:=B;  // Farbwerte werden im Typ Single zwischengespeichert !
      BmpCopyRGB^.g:=G;
      BmpCopyRGB^.r:=R;
      Inc(BmpCopyRGB);
    End;
  End;
  // Alle Bildpunkte zurück ins Bmp-Bitmap schreiben und gleichzeitig VERTIKAL blurren
  For y:=0 To Pred(BmpHeight) Do Begin
    BmpRGB:=Bmp.ScanLine[y];
    For x:=0 to Pred(BmpWidth) Do Begin
      R:=0; G:=0; B:=0;
      For mx:=-MatrixRadius To MatrixRadius Do Begin
        If y+mx<=0 Then
          BmpCopyRGB:=@BmpCopy[0,x]                // erster Pixel
        Else If y+mx>=BmpHeight Then
          BmpCopyRGB:=@BmpCopy[Pred(BmpHeight),x]  // letzter Pixel
        Else
          BmpCopyRGB:=@BmpCopy[y+mx,x];
        B:=B+BmpCopyRGB^.b*Matrix[mx];
        G:=G+BmpCopyRGB^.g*Matrix[mx];
        R:=R+BmpCopyRGB^.r*Matrix[mx];
      End;
      BmpRGB^.b:=Round(B);
      BmpRGB^.g:=Round(G);
      BmpRGB^.r:=Round(R);
      Inc(BmpRGB);
    End;
  End;
End;

end.