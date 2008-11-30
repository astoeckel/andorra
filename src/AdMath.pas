{
* This program is licensed under the Common Public License (CPL) Version 1.0
* You should have recieved a copy of the license with this file.
* If not, see http://www.opensource.org/licenses/cpl1.0.txt for more informations.
* 
* Inspite of the incompatibility between the Common Public License (CPL) and 
* the GNU General Public License (GPL) you're allowed to use this program 
* under the GPL. 
* You also should have recieved a copy of this license with this file. 
* If not, see http://www.gnu.org/licenses/gpl.txt for more informations.
*
* Project: Andorra 2D
* Author:  Andreas Stoeckel
* File: AdMath.pas
* Comment: Contains mathematical functions for matrix calculations.
}

{Parts of this unit were taken from the "VectorGeometry.pas" originaly by Dipl. 
 Ing. Mike Lischke (public@lischke-online.de) from the GLScene project 
 (http://glscene.sf.net/). This code is published under the MPL.
 See http://glscene.cvs.sourceforge.net/viewvc/*checkout*/glscene/Source/Base/VectorGeometry.pas
 to get the original source code.
 If you define the "DO_NOT_USE_3DNOW" compiler switch, all code that was taken from
 this unit will be deactivated.}


{Contains optimized mathematical functions for matrix calculations.}
unit AdMath;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  AdTypes;

{$I inc_andorra.inc}

//Remove ' if you do not want to use 3DNOW!
{$DEFINE DO_NOT_USE_3DNOW}
//Remove ' if you do not want to use any x86amd asm code
{'$DEFINE DO_NOT_USE_ASM}

var
  {Multiplies two matrixes and returns the new matrix}
  AdMatrix_Multiply: function(const amat1,amat2:TAdMatrix):TAdMatrix;
  {Mutiplies a matrix with a vector and returns the result vector.}
  AdMatrix_Multiply_Vector: function(const amat: TAdMatrix; const avec: TAdVector4): TAdVector4;
  {Transposes and returns the given matrix}
  AdMatrix_Transpose: function(const mat: TAdMatrix): TAdMatrix;
  {Returns a translation matrix.}
  AdMatrix_Translate: function(const tx,ty,tz:single):TAdMatrix;
  {Returns a scale matrix.}
  AdMatrix_Scale: function(const sx,sy,sz:single):TAdMatrix;
  {Returns a matrix for rotation around the X-Axis}
  AdMatrix_RotationX: function(const angle:single):TAdMatrix;
  {Returns a matrix for rotation around the Y-Axis}
  AdMatrix_RotationY: function(const angle:single):TAdMatrix;
  {Returns a matrix for rotation around the Z-Axis}
  AdMatrix_RotationZ: function(const angle:single):TAdMatrix;
  {Returns a matrix for rotation around the X, Y and Z-Axis}
  AdMatrix_Rotation: function(const ax, ay, az: single):TAdMatrix;

implementation

{$IFDEF FPC}
  {$IFDEF CPU386}
    {$ASMMODE intel}
  {$ELSE}
    {$DEFINE DO_NOT_USE_ASM}
  {$ENDIF}
{$ENDIF}

{$IFDEF DO_NOT_USE_ASM}
  {$DEFINE DO_NOT_USE_3DNOW}
{$ENDIF}

{$IFDEF DO_NOT_USE_ASM}
procedure SinCos(const Alpha: Extended; var Sin, Cos: Extended);{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  Sin := System.Sin(Alpha);
  Cos := System.Cos(Alpha);
end;
{$ELSE}
procedure SinCos(const Alpha: Extended; var Sin, Cos: Extended);
begin
  asm
    FLD     Alpha
    FSINCOS
    FSTP    tbyte ptr [edx]
    FSTP    tbyte ptr [eax]
    FWAIT
  end;
end;
{$ENDIF}

function _PASCAL_AdMatrix_Multiply(const amat1, amat2:TAdMatrix):TAdMatrix;
var
  x,y:integer;
begin
  for x := 0 to 3 do
  begin
    for y := 0 to 3 do
    begin
      result[x,y] := 
        amat2[0,y] * amat1[x,0] + 
        amat2[1,y] * amat1[x,1] + 
        amat2[2,y] * amat1[x,2] +
        amat2[3,y] * amat1[x,3];
    end;
  end;
end;

function _PASCAL_AdMatrix_Multiply_Vector(const amat: TAdMatrix; const avec: TAdVector4): TAdVector4;
begin
  with result do
  begin
    X := amat[0, 0] * avec.x + amat[1, 0] * avec.y + amat[2, 0] * avec.z + amat[3, 0] * avec.w;
    Y := amat[0, 1] * avec.x + amat[1, 1] * avec.y + amat[2, 1] * avec.z + amat[3, 1] * avec.w;
    Z := amat[0, 2] * avec.x + amat[1, 2] * avec.y + amat[2, 2] * avec.z + amat[3, 2] * avec.w;
    W := amat[0, 3] * avec.x + amat[1, 3] * avec.y + amat[2, 3] * avec.z + amat[3, 3] * avec.w;
  end;
end;

function _PASCAL_AdMatrix_Transpose(const mat: TAdMatrix): TAdMatrix;
var
  i,j : integer;
begin
  for i := 0 to 3 do
    for j := 0 to 3 do
      result[i, j] := mat[j, i];
end;

function _PASCAL_AdMatrix_Translate(const tx,ty,tz:single):TAdMatrix;
begin
  result := AdMatrix_Identity;
  result[3,0] := tx;
  result[3,1] := ty;
  result[3,2] := tz;
end;

function _PASCAL_AdMatrix_Scale(const sx,sy,sz:single):TAdMatrix;
begin
  result := AdMatrix_Clear;
  result[0,0] := sx;
  result[1,1] := sy;
  result[2,2] := sz;
  result[3,3] := 1;
end;

function _PASCAL_AdMatrix_RotationX(const angle:single):TAdMatrix;
var
  asin, acos: Extended;
begin
  asin := 0; acos := 0;
  SinCos(angle, asin, acos);

  result := AdMatrix_Clear;
  result[0,0] := 1;
  result[1,1] := acos;
  result[1,2] := asin;
  result[2,1] := -asin;
  result[2,2] := acos;
  result[3,3] := 1;
end;

function _PASCAL_AdMatrix_RotationY(const angle:single):TAdMatrix;
var
  asin, acos: Extended;
begin
  SinCos(angle, asin, acos);

  result := AdMatrix_Clear;
  result[0,0] := acos;
  result[0,2] := -asin;
  result[1,1] := 1;
  result[2,0] := asin;
  result[2,2] := acos;
  result[3,3] := 1;
end;

function _PASCAL_AdMatrix_RotationZ(const angle:single):TAdMatrix;
var
  asin, acos: Extended;
begin
  SinCos(angle, asin, acos);
  
  result := AdMatrix_Clear;
  result[0,0] := acos;
  result[0,1] := asin;
  result[1,0] := -asin;
  result[1,1] := acos;
  result[2,2] := 1;
  result[3,3] := 1;
end;

function _PASCAL_AdMatrix_Rotation(const ax, ay, az:single):TAdMatrix;
var
  AMat1, AMat2: TAdMatrix;
begin
  //Calculate the X and the Y rotation matrix
  AMat1 := _PASCAL_AdMatrix_RotationX(ax);
  AMat2 := _PASCAL_AdMatrix_RotationY(ay); 
  AMat1 := AdMatrix_Multiply(AMat1, AMat2);

  //Calculate the Z rotation matrix and multiply it with the XY rotation matrix
  AMat2 := _PASCAL_AdMatrix_RotationZ(az);
  AMat1 := AdMatrix_Multiply(AMat1, AMat2);

  //Return the calculated XYZ Matrix
  result := AMat1;
end;

procedure UsePascal;
begin
  AdMatrix_Multiply := _PASCAL_AdMatrix_Multiply;
  AdMatrix_Multiply_Vector := _PASCAL_AdMatrix_Multiply_Vector;
  AdMatrix_Transpose := _PASCAL_AdMatrix_Transpose;
  AdMatrix_Translate := _PASCAL_AdMatrix_Translate;
  AdMatrix_Scale := _PASCAL_AdMatrix_Scale;
  AdMatrix_RotationX := _PASCAL_AdMatrix_RotationX;
  AdMatrix_RotationY := _PASCAL_AdMatrix_RotationY;
  AdMatrix_RotationZ := _PASCAL_AdMatrix_RotationZ;
  AdMatrix_Rotation := _PASCAL_AdMatrix_Rotation;
end;

{$IFNDEF DO_NOT_USE_3DNOW}

{The procedures listed here are taken from the VectorGeometry.pas from the
 GLScene project. For more details see above.}

{$MESSAGE HINT 'Andorra may use the AMD 3DNow optimization. This code is not tested!'}
{$MESSAGE HINT 'If you encounter any problem, activate the compilier switch at the beginning of the unit AdMath and report this problem to the Andorra 2D developers. Thank you.'}
{$MESSAGE HINT 'If you are using a AMD processor and everything works fine, it would be great if you could report this.'}

function _3DNOW_AdMatrix_Multiply(const amat1, amat2:TAdMatrix):TAdMatrix;
begin
  asm
    xchg eax, ecx
    db $0F,$6F,$01           /// movq        mm0,[ecx]
    db $0F,$6F,$49,$08       /// movq        mm1,[ecx+8]
    db $0F,$6F,$22           /// movq        mm4,[edx]
    db $0F,$6A,$D0           /// punpckhdq   mm2,mm0
    db $0F,$6F,$6A,$10       /// movq        mm5,[edx+16]
    db $0F,$6A,$D9           /// punpckhdq   mm3,mm1
    db $0F,$6F,$72,$20       /// movq        mm6,[edx+32]
    db $0F,$62,$C0           /// punpckldq   mm0,mm0
    db $0F,$62,$C9           /// punpckldq   mm1,mm1
    db $0F,$0F,$E0,$B4       /// pfmul       mm4,mm0
    db $0F,$6A,$D2           /// punpckhdq   mm2,mm2
    db $0F,$0F,$42,$08,$B4   /// pfmul       mm0, [edx+8]
    db $0F,$6F,$7A,$30       /// movq        mm7,[edx+48]
    db $0F,$0F,$EA,$B4       /// pfmul       mm5,mm2
    db $0F,$6A,$DB           /// punpckhdq   mm3,mm3
    db $0F,$0F,$52,$18,$B4   /// pfmul       mm2,[edx+24]
    db $0F,$0F,$F1,$B4       /// pfmul       mm6,mm1
    db $0F,$0F,$EC,$9E       /// pfadd       mm5,mm4
    db $0F,$0F,$4A,$28,$B4   /// pfmul       mm1,[edx+40]
    db $0F,$0F,$D0,$9E       /// pfadd       mm2,mm0
    db $0F,$0F,$FB,$B4       /// pfmul       mm7,mm3
    db $0F,$0F,$F5,$9E       /// pfadd       mm6,mm5
    db $0F,$0F,$5A,$38,$B4   /// pfmul       mm3,[edx+56]
    db $0F,$0F,$D1,$9E       /// pfadd       mm2,mm1
    db $0F,$0F,$FE,$9E       /// pfadd       mm7,mm6
    db $0F,$6F,$41,$10       /// movq        mm0,[ecx+16]
    db $0F,$0F,$DA,$9E       /// pfadd       mm3,mm2
    db $0F,$6F,$49,$18       /// movq        mm1,[ecx+24]
    db $0F,$7F,$38           /// movq        [eax],mm7
    db $0F,$6F,$22           /// movq        mm4,[edx]
    db $0F,$7F,$58,$08       /// movq        [eax+8],mm3

    db $0F,$6A,$D0           /// punpckhdq   mm2,mm0
    db $0F,$6F,$6A,$10       /// movq        mm5,[edx+16]
    db $0F,$6A,$D9           /// punpckhdq   mm3,mm1
    db $0F,$6F,$72,$20       /// movq        mm6,[edx+32]
    db $0F,$62,$C0           /// punpckldq   mm0,mm0
    db $0F,$62,$C9           /// punpckldq   mm1,mm1
    db $0F,$0F,$E0,$B4       /// pfmul       mm4,mm0
    db $0F,$6A,$D2           /// punpckhdq   mm2,mm2
    db $0F,$0F,$42,$08,$B4   /// pfmul       mm0,[edx+8]
    db $0F,$6F,$7A,$30       /// movq        mm7,[edx+48]
    db $0F,$0F,$EA,$B4       /// pfmul       mm5,mm2
    db $0F,$6A,$DB           /// punpckhdq   mm3,mm3
    db $0F,$0F,$52,$18,$B4   /// pfmul       mm2,[edx+24]
    db $0F,$0F,$F1,$B4       /// pfmul       mm6,mm1
    db $0F,$0F,$EC,$9E       /// pfadd       mm5,mm4
    db $0F,$0F,$4A,$28,$B4   /// pfmul       mm1,[edx+40]
    db $0F,$0F,$D0,$9E       /// pfadd       mm2,mm0
    db $0F,$0F,$FB,$B4       /// pfmul       mm7,mm3
    db $0F,$0F,$F5,$9E       /// pfadd       mm6,mm5
    db $0F,$0F,$5A,$38,$B4   /// pfmul       mm3,[edx+56]
    db $0F,$0F,$D1,$9E       /// pfadd       mm2,mm1
    db $0F,$0F,$FE,$9E       /// pfadd       mm7,mm6
    db $0F,$6F,$41,$20       /// movq        mm0,[ecx+32]
    db $0F,$0F,$DA,$9E       /// pfadd       mm3,mm2
    db $0F,$6F,$49,$28       /// movq        mm1,[ecx+40]
    db $0F,$7F,$78,$10       /// movq        [eax+16],mm7
    db $0F,$6F,$22           /// movq        mm4,[edx]
    db $0F,$7F,$58,$18       /// movq        [eax+24],mm3

    db $0F,$6A,$D0           /// punpckhdq   mm2,mm0
    db $0F,$6F,$6A,$10       /// movq        mm5,[edx+16]
    db $0F,$6A,$D9           /// punpckhdq   mm3,mm1
    db $0F,$6F,$72,$20       /// movq        mm6,[edx+32]
    db $0F,$62,$C0           /// punpckldq   mm0,mm0
    db $0F,$62,$C9           /// punpckldq   mm1,mm1
    db $0F,$0F,$E0,$B4       /// pfmul       mm4,mm0
    db $0F,$6A,$D2           /// punpckhdq   mm2,mm2
    db $0F,$0F,$42,$08,$B4   /// pfmul       mm0,[edx+8]
    db $0F,$6F,$7A,$30       /// movq        mm7,[edx+48]
    db $0F,$0F,$EA,$B4       /// pfmul       mm5,mm2
    db $0F,$6A,$DB           /// punpckhdq   mm3,mm3
    db $0F,$0F,$52,$18,$B4   /// pfmul       mm2,[edx+24]
    db $0F,$0F,$F1,$B4       /// pfmul       mm6,mm1
    db $0F,$0F,$EC,$9E       /// pfadd       mm5,mm4
    db $0F,$0F,$4A,$28,$B4   /// pfmul       mm1,[edx+40]
    db $0F,$0F,$D0,$9E       /// pfadd       mm2,mm0
    db $0F,$0F,$FB,$B4       /// pfmul       mm7,mm3
    db $0F,$0F,$F5,$9E       /// pfadd       mm6,mm5
    db $0F,$0F,$5A,$38,$B4   /// pfmul       mm3,[edx+56]
    db $0F,$0F,$D1,$9E       /// pfadd       mm2,mm1
    db $0F,$0F,$FE,$9E       /// pfadd       mm7,mm6
    db $0F,$6F,$41,$30       /// movq        mm0,[ecx+48]
    db $0F,$0F,$DA,$9E       /// pfadd       mm3,mm2
    db $0F,$6F,$49,$38       /// movq        mm1,[ecx+56]
    db $0F,$7F,$78,$20       /// movq        [eax+32],mm7
    db $0F,$6F,$22           /// movq        mm4,[edx]
    db $0F,$7F,$58,$28       /// movq        [eax+40],mm3

    db $0F,$6A,$D0           /// punpckhdq   mm2,mm0
    db $0F,$6F,$6A,$10       /// movq        mm5,[edx+16]
    db $0F,$6A,$D9           /// punpckhdq   mm3,mm1
    db $0F,$6F,$72,$20       /// movq        mm6,[edx+32]
    db $0F,$62,$C0           /// punpckldq   mm0,mm0
    db $0F,$62,$C9           /// punpckldq   mm1,mm1
    db $0F,$0F,$E0,$B4       /// pfmul       mm4,mm0
    db $0F,$6A,$D2           /// punpckhdq   mm2,mm2
    db $0F,$0F,$42,$08,$B4   /// pfmul       mm0,[edx+8]
    db $0F,$6F,$7A,$30       /// movq        mm7,[edx+48]
    db $0F,$0F,$EA,$B4       /// pfmul       mm5,mm2
    db $0F,$6A,$DB           /// punpckhdq   mm3,mm3
    db $0F,$0F,$52,$18,$B4   /// pfmul       mm2,[edx+24]
    db $0F,$0F,$F1,$B4       /// pfmul       mm6,mm1
    db $0F,$0F,$EC,$9E       /// pfadd       mm5,mm4
    db $0F,$0F,$4A,$28,$B4   /// pfmul       mm1,[edx+40]
    db $0F,$0F,$D0,$9E       /// pfadd       mm2,mm0
    db $0F,$0F,$FB,$B4       /// pfmul       mm7,mm3
    db $0F,$0F,$F5,$9E       /// pfadd       mm6,mm5
    db $0F,$0F,$5A,$38,$B4   /// pfmul       mm3,[edx+56]
    db $0F,$0F,$D1,$9E       /// pfadd       mm2,mm1
    db $0F,$0F,$FE,$9E       /// pfadd       mm7,mm6
    db $0F,$0F,$DA,$9E       /// pfadd       mm3,mm2
    db $0F,$7F,$78,$30       /// movq        [eax+48],mm7
    db $0F,$7F,$58,$38       /// movq        [eax+56],mm3
    db $0F,$0E               /// femms
  end;
end;

function _3DNOW_AdMatrix_Multiply_Vector(const amat: TAdMatrix; const avec: TAdVector4): TAdVector4;
begin
  asm
    db $0F,$6F,$00           /// movq        mm0,[eax]
    db $0F,$6F,$48,$08       /// movq        mm1,[eax+8]
    db $0F,$6F,$22           /// movq        mm4,[edx]
    db $0F,$6A,$D0           /// punpckhdq   mm2,mm0
    db $0F,$6F,$6A,$10       /// movq        mm5,[edx+16]
    db $0F,$62,$C0           /// punpckldq   mm0,mm0
    db $0F,$6F,$72,$20       /// movq        mm6,[edx+32]
    db $0F,$0F,$E0,$B4       /// pfmul       mm4,mm0
    db $0F,$6F,$7A,$30       /// movq        mm7,[edx+48]
    db $0F,$6A,$D2           /// punpckhdq   mm2,mm2
    db $0F,$6A,$D9           /// punpckhdq   mm3,mm1
    db $0F,$0F,$EA,$B4       /// pfmul       mm5,mm2
    db $0F,$62,$C9           /// punpckldq   mm1,mm1
    db $0F,$0F,$42,$08,$B4   /// pfmul       mm0,[edx+8]
    db $0F,$6A,$DB           /// punpckhdq   mm3,mm3
    db $0F,$0F,$52,$18,$B4   /// pfmul       mm2,[edx+24]
    db $0F,$0F,$F1,$B4       /// pfmul       mm6,mm1
    db $0F,$0F,$EC,$9E       /// pfadd       mm5,mm4
    db $0F,$0F,$4A,$28,$B4   /// pfmul       mm1,[edx+40]
    db $0F,$0F,$D0,$9E       /// pfadd       mm2,mm0
    db $0F,$0F,$FB,$B4       /// pfmul       mm7,mm3
    db $0F,$0F,$F5,$9E       /// pfadd       mm6,mm5
    db $0F,$0F,$5A,$38,$B4   /// pfmul       mm3,[edx+56]
    db $0F,$0F,$D1,$9E       /// pfadd       mm2,mm1
    db $0F,$0F,$FE,$9E       /// pfadd       mm7,mm6
    db $0F,$0F,$DA,$9E       /// pfadd       mm3,mm2

    db $0F,$7F,$39           /// movq        [ecx],mm7
    db $0F,$7F,$59,$08       /// movq        [ecx+8],mm3
    db $0F,$0E               /// femms
  end
end;

function Supports3DNow: boolean;
var
  vSIMD: integer;
begin
  vSIMD := 0;
  try
    // detect 3DNow! capable CPU (adapted from AMD's "3DNow! Porting Guide")
    asm
      pusha
      mov  eax, $80000000
      db $0F,$A2               /// cpuid
      cmp  eax, $80000000
      jbe @@No3DNow
      mov  eax, $80000001
      db $0F,$A2               /// cpuid
      test edx, $80000000
      jz @@No3DNow
      mov vSIMD, 1
     @@No3DNow:
      popa
    end;
  except
    // trap for old/exotics CPUs
    vSIMD := 0;
  end;
  
  result := vSIMD = 1;
end;

procedure Use3DNow;
begin
  UsePascal;
  AdMatrix_Multiply := _3DNOW_AdMatrix_Multiply;
  AdMatrix_Multiply_Vector := _3DNOW_AdMatrix_Multiply_Vector;
end;
 
{$ENDIF}

initialization
  {$IFDEF DO_NOT_USE_3DNOW}
    //If SSE optimizations are deactivated, connect the functions in the 
    //interface part of the unit with the functions written in standard 
    //pascal
    UsePascal;
  {$ELSE}
    //Check whether the optimizations that are needed by the optimized functions
    //are available on this processor
    if Supports3DNow then
      Use3DNow
    else
      UsePascal;
  {$ENDIF}
end.
