//
// This unit is part of the GLScene Project, http://glscene.org
// Licensed under the Mozilla Public License
//
// You can get the original file here: 
//   http://glscene.cvs.sourceforge.net/viewvc/*checkout*/glscene/Source/Base/Spline.pas?revision=1.14
//

{AdSpline.pas
 Cubic spline interpolation functions

 History:
  o 12/10/08 - Andreas Stöckel - Migrated this unit into Andorra 2D:
                                  - Updated documentation to a PasDoc style
                                  - Removed unneeded functions
                                  - Removed W component
  o 08/07/04 - LR              - Removed ../ from the GLScene.inc
  o 16/07/02 - Egg             - Added methods to access slope per axis
	o 28/05/00 - Egg             - Javadocisation, minor changes & optimizations,
                                 Renamed TSpline to TCubicSpline, added W component
                                 and a bunch of helper methods
	o 20/05/00 - RoC             - Created, based on the C source code from Eric
}

{Cubic spline interpolation functions}
unit AdSpline;

interface

uses
  AdTypes;

type
  TAdSpline = class(TObject)
    public
      {Creates the spline and declares interpolation points.
       Time references go from 0 (first point) to nb-1 (last point), the
       first and last reference matrices respectively are used when T is
       used beyond this range.
       Note : "nil" single arrays are accepted, in this case the axis is
       disabled and calculus will return 0 (zero) for this component. }
      constructor Create(const X, Y, Z: TAdFloatArray; const nb: integer);virtual;

      procedure SplineXY(const t: single; var X, Y: Single);virtual;abstract;
      procedure SplineXYZ(const t: single; var X, Y, Z: Single);virtual;abstract;
  end;

  TAdCubicSplineMatrix = array of array [0..3] of Single;

  {TAdCubicSpline is a 3D cubic spline handler class.
   This class allows to describe and calculate values of a time-based,
   three-dimensionnal cubic spline.
   Cubic spline pass through all given points and tangent on point N is
   given by the (N-1) to (N+1) vector.
   Note : X, Y & Z are actually interpolated independently. }
  TAdCubicSpline = class (TAdSpline)
    private
      matX, matY, matZ: TAdCubicSplineMatrix;
      FNb : Integer;
    public
      {Creates the spline and declares interpolation points.
       Time references go from 0 (first point) to nb-1 (last point), the
       first and last reference matrices respectively are used when T is
       used beyond this range.
       Note : "nil" single arrays are accepted, in this case the axis is
       disabled and calculus will return 0 (zero) for this component. }
      constructor Create(const X, Y, Z: TAdFloatArray; const nb : Integer);override;
      destructor Destroy; override;

      procedure SplineXY(const t: single; var X, Y: Single);override;
      procedure SplineXYZ(const t: single; var X, Y, Z: Single);override;
  end;

  TAdBezierSpline = class(TAdSpline)
    private
      FX, FY, FZ: TAdFloatArray;
      Fnb: Integer;
      function BaseFunction(t: Single; n: Integer): Single;
    public
      constructor Create(const X, Y, Z: TAdFloatArray; const nb : Integer);override;

      procedure SplineXY(const t: single; var X, Y: Single);override;
      procedure SplineXYZ(const t: single; var X, Y, Z: Single);override;
   end;

implementation

// VECCholeskyTriDiagResol
procedure VECCholeskyTriDiagResol(const b : array of Single; const nb : Integer;
                                  var Result : array of Single);
var
   Y, LDiag, LssDiag : array of Single;
   i, k, Debut, Fin: Integer;
begin
   Debut:=0;
   Fin:=nb-1;
   Assert(Length(b)>0);
   SetLength(LDiag, nb);
   SetLength(LssDiag, nb-1);
   LDiag[Debut]:=1.4142135; // = sqrt(2)
   LssDiag[Debut]:=1.0/1.4142135;
   for K:=Debut+1 to Fin-1 do begin
      LDiag[K]:=Sqrt(4-LssDiag[K-1]*LssDiag[K-1]);
      LssDiag[K]:=1.0/LDiag[K];
   end;
   LDiag[Fin]:=Sqrt(2-LssDiag[Fin-1]*LssDiag[Fin-1]);
   SetLength(Y, nb);
   Y[Debut]:=B[Debut]/LDiag[Debut];
   for I:=Debut+1 to Fin do
      Y[I]:=(B[I]-Y[I-1]*LssDiag[I-1])/LDiag[I];
   Assert(Length(Result)=nb);
   Result[Fin]:=Y[Fin]/LDiag[Fin];
   for i:=Fin-1 downto Debut do
      Result[I]:=(Y[I]-Result[I+1]*LssDiag[I])/LDiag[I];
end;

// MATInterpolationHermite
procedure MATInterpolationHermite(const ordonnees : TAdFloatArray; const nb : Integer;
                                  var Result : TAdCubicSplineMatrix); {$ifdef CLR}unsafe;{$endif}
var
   a, b, c, d : Single;
   i, n : Integer;
   bb, deriv : array of Single;
begin
   Result:=nil;
   if Assigned(Ordonnees) and (nb>0) then begin
      n:=nb-1;
      SetLength(bb, nb);
      bb[0]:=3*(ordonnees[1]-ordonnees[0]);
      bb[n]:=3*(ordonnees[n]-ordonnees[n-1]);
      for i:=1 to n-1 do
         bb[I]:=3*(ordonnees[I+1]-ordonnees[I-1]);
      SetLength(deriv, nb);
      VECCholeskyTriDiagResol(bb, nb, deriv);
      SetLength(Result, n);
      for i:=0 to n-1 do begin
         a:=ordonnees[I];
         b:=deriv[I];
         c:=3*(ordonnees[I+1]-ordonnees[I])-2*deriv[I]-deriv[I+1];
         d:=-2*(ordonnees[I+1]-ordonnees[I])+deriv[I]+deriv[I+1];
         Result[I][3]:=a+I*(I*(c-I*d)-b);
         Result[I][2]:=b+I*(3*I*d-2*c);
         Result[I][1]:=c-3*I*d;
         Result[I][0]:=d;
      end;
   end;
end;

// MATValeurSpline
function MATValeurSpline(const spline : TAdCubicSplineMatrix; const x : Single;
                         const nb : Integer) : Single;
var
   i : Integer;
begin
   if Length(Spline)>0 then begin
      if x<=0 then
         i:=0
      else if x>nb-1 then
         i:=nb-1
      else i:=Integer(Trunc(x));
      { TODO : the following line looks like a bug... }
      if i=(nb-1) then Dec(i);
      Result:=((spline[i][0]*x+spline[i][1])*x+spline[i][2])*x+spline[i][3];
   end else Result:=0;
end;

{ TAdCubicSpline }

constructor TAdCubicSpline.Create(const X, Y, Z: TAdFloatArray; const nb : Integer); {$ifdef CLR}unsafe;{$endif}
begin
   inherited;
   MATInterpolationHermite(X, nb, matX);
   MATInterpolationHermite(Y, nb, matY);
   MATInterpolationHermite(Z, nb, matZ);
   FNb:=nb;
end;

destructor TAdCubicSpline.Destroy;
begin
   inherited Destroy;
end;

procedure TAdCubicSpline.SplineXY(const t: single; var X, Y: Single);
begin
   X := MATValeurSpline(MatX, T, FNb);
   Y := MATValeurSpline(MatY, T, FNb);
end;

procedure TAdCubicSpline.SplineXYZ(const t : single; var X, Y, Z: Single);
begin
   X := MATValeurSpline(MatX, T, FNb);
   Y := MATValeurSpline(MatY, T, FNb);
   Z := MATValeurSpline(MatZ, T, FNb);
end;


{ TAdSpline }

constructor TAdSpline.Create(const X, Y, Z: TAdFloatArray; const nb: integer);
begin
  inherited Create;
end;

{ TAdBezierSpline }

constructor TAdBezierSpline.Create(const X, Y, Z: TAdFloatArray;
  const nb: Integer);
begin
  inherited;

  if X <> nil then  
    FX := Copy(X, 0, nb);
  if Y <> nil then
    FY := Copy(Y, 0, nb);
  if z <> nil then
    FZ := Copy(Z, 0, nb);

  Fnb := nb;
end;

function TAdBezierSpline.baseFunction(t: Single; n: Integer): Single;
begin
  case n of
    0:
      result := (1 - t) * (1 - t) * (1 - t);
    1:
      result := 3 * t * (1 - t) * (1 - t);
    2:
      result := 3 * t * t * (1 - t);
    3:
      result := t * t * t;
  else
    result := 0;
  end;
end;

procedure TAdBezierSpline.SplineXY(const t: single; var X, Y: Single);
var
  bfv: Single;
  i: integer;
begin
  X := 0; Y := 0;
  for i := 0 to 3 do
  begin
    bfv := baseFunction(t / 3, i);
    X := X + FX[i] * bfv;
    Y := Y + FY[i] * bfv;
  end;
end;


procedure TAdBezierSpline.SplineXYZ(const t : single; var X, Y, Z: Single);
var
  bfv: Single;
  i: integer;
begin
  X := 0; Y := 0; Z := 0;
  for i := 0 to 3 do
  begin
    bfv := baseFunction(t / 3, i);
    X := X + FX[i] * bfv;
    Y := Y + FY[i] * bfv;
    Z := Z + FZ[i] * bfv;
  end;
end;

end.

