unit AdMath;

interface

uses
  AdTypes;

//Remove ' if you do not want to use SSE
{'$DEFINE DO_NOT_USE_SSE}

{Multiplies two matrixes and returns the new matrix}
function AdMatrix_Multiply(amat1,amat2:TAdMatrix):TAdMatrix;
{Transposes and returns the given matrix}
function AdMatrix_Transpose(mat: TAdMatrix): TAdMatrix;
{Returns a translation matrix.}
function AdMatrix_Translate(tx,ty,tz:single):TAdMatrix;
{Returns a scale matrix.}
function AdMatrix_Scale(sx,sy,sz:single):TAdMatrix;
{Returns a matrix for rotation around the X-Axis}
function AdMatrix_RotationX(angle:single):TAdMatrix;
{Returns a matrix for rotation around the Y-Axis}
function AdMatrix_RotationY(angle:single):TAdMatrix;
{Returns a matrix for rotation around the Z-Axis}
function AdMatrix_RotationZ(angle:single):TAdMatrix;

implementation

function AdMatrix_Multiply(amat1, amat2:TAdMatrix):TAdMatrix;
var
  x,y:integer;
begin
  for x := 0 to 3 do
  begin
    for y := 0 to 3 do
    begin
      result[x,y] := amat2[0,y]*amat1[x,0] + amat2[1,y]*amat1[x,1] + amat2[2,y]*amat1[x,2] +amat2[3,y]*amat1[x,3];
    end;
  end;
end;

function AdMatrix_Transpose(mat: TAdMatrix): TAdMatrix;
var
  i,j : integer;
begin
  for i := 0 to 3 do
    for j := 0 to 3 do
      result[i, j] := mat[j, i];
end;

function AdMatrix_Translate(tx,ty,tz:single):TAdMatrix;
begin
  result := AdMatrix_Identity;
  result[3,0] := tx;
  result[3,1] := ty;
  result[3,2] := tz;
end;

function AdMatrix_Scale(sx,sy,sz:single):TAdMatrix;
begin
  result := AdMatrix_Clear;
  result[0,0] := sx;
  result[1,1] := sy;
  result[2,2] := sz;
  result[3,3] := 1;
end;

function AdMatrix_RotationX(angle:single):TAdMatrix;
begin
  result := AdMatrix_Clear;
  result[0,0] := 1;
  result[1,1] := cos(angle);
  result[1,2] := sin(angle);
  result[2,1] := -sin(angle);
  result[2,2] := cos(angle);
  result[3,3] := 1;
end;

function AdMatrix_RotationY(angle:single):TAdMatrix;
begin
  result := AdMatrix_Clear;
  result[0,0] := cos(angle);
  result[0,2] := -sin(angle);
  result[1,1] := 1;
  result[2,0] := sin(angle);
  result[2,2] := cos(angle);
  result[3,3] := 1;
end;

function AdMatrix_RotationZ(angle:single):TAdMatrix;
begin
  result := AdMatrix_Clear;
  result[0,0] := cos(angle);
  result[0,1] := sin(angle);
  result[1,0] := -sin(angle);
  result[1,1] := cos(angle);
  result[2,2] := 1;
  result[3,3] := 1;
end;


end.
