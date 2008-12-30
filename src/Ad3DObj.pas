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
* File: Ad3dObj.pas
* Comment: This unit contains the 3D-Object generation code
}

{This unit contains some 3D-Object classes.}
unit Ad3DObj;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, Math,
  AdTypes, AdClasses, AdMath, AdDraws, AdSpline;

type
  TAdBPatch = array[0..3,0..3] of Word;
  TAdVectorArray = array of TAdVector3;

  {TAdMesh is the base class for 3D objects in Andorra 2D. It wraps around the
   TAd2dMesh interface and allows classes derived from TAdMesh to load their own
   mesh data. It also provides properties that may be used to set the position,
   scaling and the rotation of the object. Textures and materials may be applied
   to the mesh.}
  TAdMesh = class(TAdRenderingObject)
    private
      FX, FY, FZ: Single;
      FScaleX, FScaleY, FScaleZ: Single;
      FRotX, FRotY, FRotZ: Single;       
      FParent: TAdDraw;
      FMesh: TAd2DMesh;
      FMatrix: TAdMatrix;
      FTexture: TAdCustomTexture;
      FDrawMode: TAd2dDrawMode;
      FMatrixChanged: boolean;
      FMaterial: TAd2dMaterial;
      FUseMaterial: boolean;

      procedure SetCoeff(AIndex: integer; AValue: single);
      procedure SetMatrix(AMatrix: TAdMatrix);
      procedure SetTexture(ATexture: TAdCustomTexture);
      procedure SetMaterial(AMaterial: TAd2dMaterial);
      procedure SetUseMaterial(AValue: boolean);
      procedure Notify(Sender: TObject; AEvent: TAdSurfaceEventState);
    protected
      procedure BuildMatrix;
      procedure Initialize;
      procedure Finalize;
      
      procedure LoadMeshData;virtual;abstract;

      property Mesh: TAd2DMesh read FMesh;
      property DrawMode: TAd2dDrawMode read FDrawMode write FDrawMode;
    public
      {Creates a new instance of TAdMesh. The underlying TAd2dMesh structure is
       created and filled with data by calling the "LoadMeshData" function that
       has to be implemented by classes derived from TAdMesh.}
      constructor Create(AParent: TAdDraw);virtual;
      {Destroys the instance of TAdMesh.}
      destructor Destroy;override;
      
      {Draws the mesh on the specified surface.
       @param(ASurface specifies the target surface. May be nil, if you want to
         draw on the surface that is currently active.)
       @param(ABlendMode specifies the blend mode the mesh should be drawn in.)}
      procedure Draw(ASurface: TAdSurface; ABlendMode: TAd2dBlendMode = bmAlpha);virtual;
      
      {Pointer on the parent TAdDraw.}
      property Parent: TAdDraw read FParent write FParent;
      {The texture the loaded mesh should be texurized with. Please keep in mind
       that TAdMesh is not capable of figuring out the texture coordinates that
       match to your texture.}
      property Texture: TAdCustomTexture read FTexture write SetTexture;
      {Material represents the material of the mesh. "UseMaterial" is set to true
       when storing data in "Material". Do not use the Pascal "with" operation when
       writing data to material. It won't be properly stored. Use a temporary variable
       instead.
       @seealso(Material)}
      property Material: TAd2dMaterial read FMaterial write SetMaterial;
      {Set this to true if you want to use materials with this mesh. "UseMaterial"
       is automatically set to true, when setting the "Material" property.
       @seealso(UseMaterial)}
      property UseMaterial: boolean read FUseMaterial write SetUseMaterial;

      {The X-value the mesh should be translated by.}
      property X: single index 0 read FX write SetCoeff;
      {The Y-value the mesh should be translated by.}
      property Y: single index 1 read FY write SetCoeff;
      {The Z-value the mesh should be translated by.}
      property Z: single index 2 read FZ write SetCoeff;
      {The scale in X direction. Relative values.}
      property ScaleX: single index 3 read FScaleX write SetCoeff;
      {The scale in Y direction. Relative values.}
      property ScaleY: single index 4 read FScaleY write SetCoeff;
      {The scale in Z direction. Relative values.}
      property ScaleZ: single index 5 read FScaleZ write SetCoeff;
      {Rotation around the X-axis of the coordinate system.}
      property RotationX: single index 6 read FRotX write SetCoeff;
      {Rotation around the Y-axis of the coordinate system.}
      property RotationY: single index 7 read FRotY write SetCoeff;
      {Rotation around the Z-axis of the coordinate system.}
      property RotationZ: single index 8 read FRotZ write SetCoeff;
      {Returns the current translation matrix.}
      property Matrix: TAdMatrix read FMatrix write SetMatrix;
  end;

  {A simple plane.}
  TAdPlaneMesh = class(TAdMesh)
    private
      FWidth, FHeight: Single;
      procedure SetSizeCoeff(AIndex: integer; AValue: single);
    protected
      procedure LoadMeshData;override;
    public
      {Creates an instance of TAdPlaneMesh.}
      constructor Create(AParent: TAdDraw);override;

      {Set this value to change the base width of TAdPlaneMesh. If you want to scale
       the cube, you should use scale x instead.}      
      property Width: Single index 0 read FWidth write SetSizeCoeff;
      {Set this value to change the base height of TAdPlaneMesh. If you want to scale
       the cube, you should use scale y instead.}      
      property Height: Single index 1 read FHeight write SetSizeCoeff;
  end;

  {A simple cube.}
  TAdCubeMesh = class(TAdMesh)
    private
      FWidth, FHeight, FDepth: single;
      procedure SetSizeCoeff(AIndex: integer; AValue: single);
    protected
      procedure LoadMeshData;override;
    public
      {Creates an instance of TAdCubeMesh.}
      constructor Create(AParent: TAdDraw);override;

      {Set this value to change the base width of TAdCubeMesh. If you want to scale
       the cube, you should use scale x instead.}      
      property Width: single index 0 read FWidth write SetSizeCoeff;
      {Set this value to change the base height of TAdCubeMesh. If you want to scale
       the cube, you should use scale y instead.}      
      property Height: single index 1 read FHeight write SetSizeCoeff;
      {Set this value to change the base depth of TAdCubeMesh. If you want to scale
       the cube, you should use scale z instead.}      
      property Depth: single index 2 read FDepth write SetSizeCoeff;
  end;

  TAdConstMesh = class(TAdMesh)
    private
      FDetails: integer;
      procedure SetDetails(AValue: integer);
    protected
      procedure LoadConstMesh(AVertices: array of Single; APatches: array of TAdBPatch);  
    public
      {Creates an instance of TAdCubeMesh.}
      constructor Create(AParent: TAdDraw);override;

      {Specifies how many times each patch should be subdivided. The default
       value is 16. A higher value may cause the teapot to be displayed incorrectly.
       A lower value reduces the polygon count.}
      property Details: integer read FDetails write SetDetails;
  end;

  {A simple teapot. The teapot data is stored inside the exe. The teapot
   is generated when it is created.}
  TAdTeapotMesh = class(TAdConstMesh)
    protected
      procedure LoadMeshData;override;
  end;

  TAdTeacupMesh = class(TAdConstMesh)
    protected
      procedure LoadMeshData;override;
  end;

  TAdTeaspoonMesh = class(TAdConstMesh)
    protected
      procedure LoadMeshData;override;
  end;


implementation 

{ TAdMesh }

constructor TAdMesh.Create(AParent: TAdDraw);
begin
  inherited Create;

  FParent := AParent;
  FParent.RegisterNotifyEvent(Notify);

  //Set some presets
  FX := 0; FY := 0; FZ := 0;
  FScaleX := 1; FScaleY := 1; FScaleZ := 1;
  FRotX := 0; FRotY := 0; FRotZ := 0;

  //Set the default draw mode
  FDrawMode := adTriangles;

  FUseMaterial := false;
  FMaterial.Diffuse := Ad_ARGB(255, 255, 255, 255);

  //Initialize the mesh
  Initialize;
end;

destructor TAdMesh.Destroy;
begin
  FParent.UnRegisterNotifyEvent(Notify);

  //Finalize the mesh
  Finalize;
  inherited;
end;

procedure TAdMesh.Initialize;
begin
  //Finalize the mesh object to be sure that no memory leaks will be created
  Finalize;

  //Create a new mesh object
  FMesh := FParent.AdAppl.CreateMesh;
  Texture := FTexture;

  //Create the translation matrix
  BuildMatrix;

  //Tell classes derived from this class, to load its mesh data
  LoadMeshData;
end;

procedure TAdMesh.Finalize;
begin
  //Free the mesh if it was already initialized
  if FMesh <> nil then
    FMesh.Free;

  FMesh := nil;
end;

procedure TAdMesh.Draw(ASurface: TAdSurface; ABlendMode: TAd2dBlendMode);
begin
  if FMatrixChanged then
    BuildMatrix;

  //Activate the surface on which the mesh should be drawn
  if ASurface <> nil then
    ASurface.Activate;
    
  //Draw the mesh with the given paremeters
  FMesh.Draw(ABlendMode, FDrawMode);
end;

procedure TAdMesh.Notify(Sender: TObject; AEvent: TAdSurfaceEventState);
begin
  case AEvent of
    //Use the initialized event, to be sure, that the texture has been properly
    //initialized
    seInitialized: Initialize;
    seFinalize: Finalize;
  end;
end;

procedure TAdMesh.SetCoeff(AIndex: integer; AValue: single);
begin
  //Set the mesh translation coefficient that has been changed
  case AIndex of
    0: FX := AValue;
    1: FY := AValue;
    2: FZ := AValue;
    3: FScaleX := AValue;
    4: FScaleY := AValue;
    5: FScaleZ := AValue;
    6: FRotX := AValue;
    7: FRotY := AValue;
    8: FRotZ := AValue;
  end;

  //Rebuild the model matrix of the object the next time the object is rendered
  FMatrixChanged := true;
end;

procedure TAdMesh.SetMaterial(AMaterial: TAd2dMaterial);
begin
  FMaterial := AMaterial;

  //Send the material to the mesh
  FMesh.SetMaterial(@FMaterial);

  //Set use material to true
  FUseMaterial := true;
end;

procedure TAdMesh.SetUseMaterial(AValue: boolean);
begin
  FUseMaterial := AValue;

  if not FUseMaterial then
    FMesh.SetMaterial(nil) //If the parameter of set material is "nil" no material will be used
  else
    FMesh.SetMaterial(@FMaterial); //Set the material to the material in the "FMaterial" variable
end;

procedure TAdMesh.SetMatrix(AMatrix: TAdMatrix);
begin
  FMatrix := AMatrix;
  FMesh.Matrix := AMatrix;
end;

procedure TAdMesh.SetTexture(ATexture: TAdCustomTexture);
begin
  FTexture := ATexture;

  //Deactivate texturing if textures are switched off
  if FTexture = nil then
    FMesh.Texture := nil
  else
    FMesh.Texture := ATexture.Texture;
end;

procedure TAdMesh.BuildMatrix;
var
  FMat1, FMat2: TAdMatrix;
begin
  //Calculate the rotation matrix
  FMat1 := AdMatrix_Rotation(FRotX, FRotY, FRotZ);

  //Calculate the scaling matrix
  FMat2 := AdMatrix_Scale(FScaleX, FScaleY, FScaleZ);
  
  //Calculate the rotation+scale matrix
  FMat1 := AdMatrix_Multiply(FMat1, FMat2);

  //Calculate the translation matrix
  FMat2 := AdMatrix_Translate(FX, FY, FZ);

  //Calculate the rotation+scale+translation matrix
  FMat1 := AdMatrix_Multiply(FMat1, FMat2);

  Matrix := FMat1;
  FMatrixChanged := false;
end;

{ TAdCubeMesh }

constructor TAdCubeMesh.Create(AParent: TAdDraw);
begin
  //Set the default size coefficients
  FWidth := 100;
  FHeight := 100;
  FDepth := 100;

  inherited;
end;

procedure TAdCubeMesh.LoadMeshData;
var
  vert: TAdVertexArray;
  hx, hy, hz: single;
  i: Integer;
begin
  SetLength(vert, 36);

  hx := FWidth / 2;
  hy := FHeight / 2;
  hz := FDepth / 2;

  //Front side of the cube
  vert[0].Position := AdVector3(-hx, -hy, -hz);
  vert[0].Texture := AdVector2(0, 0);
  vert[0].Normal := AdVector3(0, 0, -1);

  vert[1].Position := AdVector3( hx, -hy, -hz);
  vert[1].Texture := AdVector2(1, 0);
  vert[1].Normal := AdVector3(0, 0, -1);

  vert[2].Position := AdVector3(-hx,  hy, -hz);
  vert[2].Texture := AdVector2(0, 1);
  vert[2].Normal := AdVector3(0, 0, -1);

  vert[3].Position := AdVector3( hx, -hy, -hz);
  vert[3].Texture := AdVector2(1, 0);
  vert[3].Normal := AdVector3(0, 0, -1);

  vert[4].Position := AdVector3( hx,  hy, -hz);
  vert[4].Texture := AdVector2(1, 1);
  vert[4].Normal := AdVector3(0, 0, -1);

  vert[5].Position := AdVector3(-hx,  hy, -hz);
  vert[5].Texture := AdVector2(0, 1);
  vert[5].Normal := AdVector3(0, 0, -1);

  //Left side of the cube
  vert[6].Position := AdVector3(-hx, -hy,  hz);
  vert[6].Texture := AdVector2(1, 0);
  vert[6].Normal := AdVector3(-1, 0, 0);

  vert[7].Position := AdVector3(-hx, -hy, -hz);
  vert[7].Texture := AdVector2(0, 0);
  vert[7].Normal := AdVector3(-1, 0, 0);

  vert[8].Position := AdVector3(-hx,  hy, -hz);
  vert[8].Texture := AdVector2(0, 1);
  vert[8].Normal := AdVector3(-1, 0, 0);

  vert[9].Position := AdVector3(-hx,  hy,  hz);
  vert[9].Texture := AdVector2(1, 1);
  vert[9].Normal := AdVector3(-1, 0, 0);

  vert[10].Position := AdVector3(-hx, -hy,  hz);
  vert[10].Texture := AdVector2(1, 0);
  vert[10].Normal := AdVector3(-1, 0, 0);

  vert[11].Position := AdVector3(-hx,  hy, -hz);
  vert[11].Texture := AdVector2(0, 1);
  vert[11].Normal := AdVector3(-1, 0, 0);

  //Right side of the cube
  vert[12].Position := AdVector3(hx, -hy, -hz);
  vert[12].Texture := AdVector2(0, 0);
  vert[12].Normal := AdVector3(1, 0, 0);

  vert[13].Position := AdVector3(hx, -hy,  hz);
  vert[13].Texture := AdVector2(1, 0);
  vert[13].Normal := AdVector3(1, 0, 0);

  vert[14].Position := AdVector3(hx,  hy, -hz);
  vert[14].Texture := AdVector2(0, 1);
  vert[14].Normal := AdVector3(1, 0, 0);

  vert[15].Position := AdVector3(hx, -hy,  hz);
  vert[15].Texture := AdVector2(1, 0);
  vert[15].Normal := AdVector3(1, 0, 0);

  vert[16].Position := AdVector3(hx,  hy,  hz);
  vert[16].Texture := AdVector2(1, 1);
  vert[16].Normal := AdVector3(1, 0, 0);

  vert[17].Position := AdVector3(hx,  hy, -hz);
  vert[17].Texture := AdVector2(0, 1);
  vert[17].Normal := AdVector3(1, 0, 0);

  //Back side of the cube
  vert[18].Position := AdVector3( hx, -hy, hz);
  vert[18].Texture := AdVector2(1, 0);
  vert[18].Normal := AdVector3(0, 0, 1);

  vert[19].Position := AdVector3(-hx, -hy, hz);
  vert[19].Texture := AdVector2(0, 0);
  vert[19].Normal := AdVector3(0, 0, 1);

  vert[20].Position := AdVector3(-hx,  hy, hz);
  vert[20].Texture := AdVector2(0, 1);
  vert[20].Normal := AdVector3(0, 0, 1);

  vert[21].Position := AdVector3( hx,  hy, hz);
  vert[21].Texture := AdVector2(1, 1);
  vert[21].Normal := AdVector3(0, 0, 1);

  vert[22].Position := AdVector3( hx, -hy, hz);
  vert[22].Texture := AdVector2(1, 0);
  vert[22].Normal := AdVector3(0, 0, 1);

  vert[23].Position := AdVector3(-hx,  hy, hz);
  vert[23].Texture := AdVector2(0, 1);
  vert[23].Normal := AdVector3(0, 0, 1);

  //Top side of the cube
  vert[24].Position := AdVector3(-hx, -hy, -hz);
  vert[24].Texture := AdVector2(0, 0);
  vert[24].Normal := AdVector3(0, -1, 0);

  vert[25].Position := AdVector3(-hx, -hy,  hz);
  vert[25].Texture := AdVector2(0, 1);
  vert[25].Normal := AdVector3(0, -1, 0);

  vert[26].Position := AdVector3( hx, -hy, -hz);
  vert[26].Texture := AdVector2(1, 0);
  vert[26].Normal := AdVector3(0, -1, 0);

  vert[27].Position := AdVector3( hx, -hy, -hz);
  vert[27].Texture := AdVector2(1, 0);
  vert[27].Normal := AdVector3(0, -1, 0);

  vert[28].Position := AdVector3(-hx, -hy, hz);
  vert[28].Texture := AdVector2(0, 1);
  vert[28].Normal := AdVector3(0, -1, 0);

  vert[29].Position := AdVector3( hx, -hy, hz);
  vert[29].Texture := AdVector2(1, 1);
  vert[29].Normal := AdVector3(0, -1, 0);

  //Bottom side of the cube
  vert[30].Position := AdVector3(-hx, hy,  hz);
  vert[30].Texture := AdVector2(0, 1);
  vert[30].Normal := AdVector3(0, 1, 0);

  vert[31].Position := AdVector3(-hx, hy, -hz);
  vert[31].Texture := AdVector2(0, 0);
  vert[31].Normal := AdVector3(0, 1, 0);

  vert[32].Position := AdVector3( hx, hy, -hz);
  vert[32].Texture := AdVector2(1, 0);
  vert[32].Normal := AdVector3(0, 1, 0);

  vert[33].Position := AdVector3(-hx, hy, hz);
  vert[33].Texture := AdVector2(0, 1);
  vert[33].Normal := AdVector3(0, 1, 0);

  vert[34].Position := AdVector3( hx, hy, -hz);
  vert[34].Texture := AdVector2(1, 0);
  vert[34].Normal := AdVector3(0, 1, 0);

  vert[35].Position := AdVector3( hx, hy, hz);
  vert[35].Texture := AdVector2(1, 1);
  vert[35].Normal := AdVector3(0, 1, 0);

  for i := 0 to High(vert) do
    vert[i].Color := Ad_ARGB(255, 255, 255, 255);

  //Store the generated vertex/index data
  Mesh.Vertices := vert;
  Mesh.Indices := nil;
  Mesh.PrimitiveCount := 12;
  Mesh.Update;
end;

procedure TAdCubeMesh.SetSizeCoeff(AIndex: integer; AValue: single);
begin
  case AIndex of
    0: FWidth := AValue;
    1: FHeight := AValue;
    2: FDepth := AValue;  
  end;

  LoadMeshData;
end;

function VectorLength(const AVec: TAdVector3): Single;
begin
  result := sqrt(sqr(AVec.x) + sqr(AVec.y) + sqr(AVec.z));
end;

procedure NormalizeVector(var AVec: TAdVector3);
var
  len: Single;
begin
  len := VectorLength(AVec);

  with AVec do
  begin
    x := x / len;
    y := y / len;
    z := z / len;
  end;
end;

function CrossProduct(AVec1, AVec2: TAdVector3): TAdVector3;
begin    
  with result do
  begin
    x := -(AVec1.y * AVec2.z - AVec1.z * AVec2.y);
    y := -(AVec1.z * AVec2.x - AVec1.x * AVec2.z);
    z := -(AVec1.x * AVec2.y - AVec1.y * AVec2.x);
  end;
end;

function AddVectors(const AVec1, AVec2: TAdVector3): TAdVector3;
begin
  with result do
  begin
    x := (AVec1.x + AVec2.x);
    y := (AVec1.y + AVec2.y);
    z := (AVec1.z + AVec2.z);
  end;
end;

function SubstractVectors(const AVec1, AVec2: TAdVector3): TAdVector3;
begin
  with result do
  begin
    x := AVec1.x - AVec2.x;
    y := AVec1.y - AVec2.y;
    z := AVec1.z - AVec2.z;
  end;
end;

function TriangleNormal(const A, B, C: TAdVector3): TAdVector3;
var
  vec1, vec2: TAdVector3;
begin
  vec1 := SubstractVectors(B, A);
  vec2 := SubstractVectors(C, A);

  result := CrossProduct(vec1, vec2);
end;

procedure RecomputeNormals(var AInd: TAdIndexArray; var AVert: TAdVertexArray);
var
  i: Integer;
  a, b, c: PAdVertex;
  n: TAdVector3;
begin
  //Set all normal vectors to zero to prevent the following shading algorithm from
  //failing
  for i := 0 to High(AVert) do
    AVert[i].Normal := AdVector3(0, 0, 0);

  //Go through every triangle and set the normal vector
  for i := 0 to Length(AInd) div 3 - 1 do
  begin
    //Store the three vertices of
    a := @AVert[AInd[i*3+0]];
    b := @AVert[AInd[i*3+1]];
    c := @AVert[AInd[i*3+2]];

    n := TriangleNormal(a^.Position, b^.Position, c^.Position);
    a^.Normal := AddVectors(a^.Normal, n);
    b^.Normal := AddVectors(b^.Normal, n);
    c^.Normal := AddVectors(c^.Normal, n);
  end;

  //Normalize the resulting vectors
  for i := 0 to High(AVert) do
    NormalizeVector(AVert[i].Normal);  
end;

procedure Tesselate_BPatch(
  var AVertDst: TAdVectorArray;
  var AIndDst: TAdIndexArray;
  AVertSrc: TAdVectorArray;
  ABPatch: TAdBPatch;
  ADetails: integer);
var
  i, j, m: integer;
  hor_splines: array[0..3] of TAdBezierSpline;
  vert_splines: array of TAdBezierSpline;
  pntsx: array[0..3] of TAdFloatArray;
  pntsy: array[0..3] of TAdFloatArray;
  pntsz: array[0..3] of TAdFloatArray;
  pntx, pnty, pntz: TAdFloatArray;
begin
  //Get the vertices from the vertex buffer and store them in the "pnts"-Variable
  for i := 0 to 3 do
  begin
    //Reserve memory for the points
    SetLength(pntsx[i], 4);
    SetLength(pntsy[i], 4);
    SetLength(pntsz[i], 4);

    for j := 0 to 3 do
    begin
      pntsx[i,j] := AVertSrc[ABPatch[i,j] - 1].x;
      pntsy[i,j] := AVertSrc[ABPatch[i,j] - 1].y;
      pntsz[i,j] := AVertSrc[ABPatch[i,j] - 1].z;
    end;
  end;

  //Create four splines
  for i := 0 to 3 do
    hor_splines[i] := TAdBezierSpline.Create(pntsx[i], pntsy[i], pntsz[i], 4);

  //Calculate the vertical splines
  SetLength(vert_splines, ADetails + 1);

  SetLength(pntx, 4);
  SetLength(pnty, 4);
  SetLength(pntz, 4);

  for i := 0 to ADetails do
  begin
    for j := 0 to 3 do
      hor_splines[j].SplineXYZ(3 * (i / ADetails), pntx[j], pnty[j], pntz[j]);
      
    vert_splines[i] := TAdBezierSpline.Create(pntx, pnty, pntz, 4);
  end;

  //Calculate the points and add them to the position output buffer

  m := 0;
  for i := 0 to ADetails do
  begin
    for j := 0 to ADetails do
    begin
      vert_splines[i].SplineXYZ(3 * (j / ADetails),
        AVertDst[m].x, AVertDst[m].y, AVertDst[m].z);

      m := m + 1;
    end;      
  end;

  //Write the point numbers to the index output buffer
  m := 0;
  for j := 0 to ADetails - 1 do
  begin
    for i := 0 to ADetails - 1 do
    begin
      AIndDst[m] :=   j     * (ADetails + 1) + i + 1;
      AIndDst[m+1] := (j+1) * (ADetails + 1) + i;
      AIndDst[m+2] := j     * (ADetails + 1) + i;
      AIndDst[m+3] := j     * (ADetails + 1) + i + 1;
      AIndDst[m+4] := (j+1) * (ADetails + 1) + i + 1;
      AIndDst[m+5] := (j+1) * (ADetails + 1) + i;
      m := m + 6;
    end;
  end;

  //Free the vertical splines
  for i := 0 to High(hor_splines) do
    hor_splines[i].Free;

  //Free the four horizontal splines
  for i := 0 to High(vert_splines) do
    vert_splines[i].Free;
end;

{ TAdConstMesh }

constructor TAdConstMesh.Create(AParent: TAdDraw);
begin
  FDetails := 16;

  inherited;
end;

procedure TAdConstMesh.LoadConstMesh(AVertices: array of Single;
  APatches: array of TAdBPatch);
var
  indices: TAdIndexArray;
  vertices: TAdVertexArray;
  tmp_indices: TAdIndexArray;
  tmp_vertices: TAdVectorArray;
  src_vertices: TAdVectorArray;
  i, j: integer;
  indexpos, vertexpos: integer;
  patches: integer;
  vpp: integer; //vertices per patch
  ppp: integer; //primitives per patch
begin
  //Store the count of patches in the patches variable
  patches := Length(APatches);

  //Calculate the count of vertices per patch
  vpp := sqr(FDetails+1);
  //Calculate the count of primitives per patch
  ppp := sqr(FDetails) * 2;

  //Reserve memory for the temporary index buffer, that stores the index data
  //for a single patch
  SetLength(tmp_indices, ppp * 3);

  //Reserve memory for the temporary vertex buffer, that stores the vertex data
  //for a single patch
  SetLength(tmp_vertices, vpp);

  //Reserve memory for the source vertex buffer. All vertices are copied into this
  //buffer in the next step
  SetLength(src_vertices, Length(AVertices) div 3);

  //Copy all vertices to the source vertex buffer.
  for i := 0 to Length(AVertices) div 3 - 1 do
    src_vertices[i] := AdVector3(
      AVertices[i*3+0],
      AVertices[i*3+1],
      AVertices[i*3+2]
    );

  //Reserve memory for the output vertex buffer
  SetLength(vertices, patches * vpp);
  //Reserve memorx for the output index buffer
  SetLength(indices, patches * ppp * 3);

  indexpos := 0;
  vertexpos := 0;

  for i := 0 to patches - 1 do
  begin
    //Tesselate each patch and store the results in the "tmp_vertices" and "tmp_indices"
    //variables
    Tesselate_BPatch(tmp_vertices, tmp_indices, src_vertices, APatches[i], FDetails);

    //Add the results to the indexbuffer
    for j := 0 to High(tmp_indices) do
      indices[j + indexpos] := tmp_indices[j] + vertexpos;

    //Add the results to the vertex buffer
    for j := 0 to High(tmp_vertices) do
      vertices[j + vertexpos].Position := tmp_vertices[j];

    //Calculate the next position of the index/vertex pointer
    indexpos := indexpos + Length(tmp_indices);
    vertexpos := vertexpos + Length(tmp_vertices);
  end;  

  //Scale the resuilting modell and set its color
  for i := 0 to High(vertices) do
  begin
    vertices[i].Position := AdVector3(
        vertices[i].Position.x * 20,
        vertices[i].Position.y * 20,
        vertices[i].Position.z * 20);
    vertices[i].Color := Ad_ARGB(255, 255, 255, 255);
    vertices[i].Texture := AdVector2(0, 0);
  end;

  //Compute the normals of the generated mesh
  RecomputeNormals(indices, vertices);

  //Store the mesh data in the mesh class
  Mesh.Vertices := vertices;
  Mesh.Indices := indices;
  Mesh.PrimitiveCount := ppp * patches;

  //Copy the mesh data to the graphics board
  Mesh.Update;
end;

procedure TAdConstMesh.SetDetails(AValue: integer);
begin
  FDetails := AValue;

  LoadMeshData;
end;

{ TAdTeapotMesh }

{$I teapot.inc}

procedure TAdTeapotMesh.LoadMeshData;
begin
  LoadConstMesh(teapot_vertices, teapot_indices);
end;

{ TAdTeacupMesh }

{$I teacup.inc}

procedure TAdTeacupMesh.LoadMeshData;
begin
  LoadConstMesh(teacup_vertices, teacup_indices);
end;

{ TAdTeaspoonMesh }

{$I teaspoon.inc}

procedure TAdTeaspoonMesh.LoadMeshData;
begin
  LoadConstMesh(teaspoon_vertices, teaspoon_indices);
end;

{ TAdPlaneMesh }

constructor TAdPlaneMesh.Create(AParent: TAdDraw);
begin
  FWidth := 100;
  FHeight := 100;

  inherited;

  DrawMode := adTriangleStrips;
end;

procedure TAdPlaneMesh.LoadMeshData;
var
  vertices: TAdVertexArray;
  i: integer;
begin
  SetLength(vertices, 4);

  vertices[0].Position := AdVector3(- FWidth / 2, - FHeight / 2, 0);
  vertices[0].Texture := AdVector2(0, 0);
  vertices[1].Position := AdVector3(  FWidth / 2, - FHeight / 2, 0);
  vertices[1].Texture := AdVector2(1, 0);
  vertices[2].Position := AdVector3(- FWidth / 2,   FHeight / 2, 0);
  vertices[2].Texture := AdVector2(0, 1);
  vertices[3].Position := AdVector3(  FWidth / 2,   FHeight / 2, 0);
  vertices[3].Texture := AdVector2(1, 1);

  for i := 0 to 3 do
  begin
    vertices[i].Color := Ad_ARGB(255, 255, 255, 255);
    vertices[i].Normal := AdVector3(0, 0, -1);
  end;

  Mesh.Vertices := vertices;
  Mesh.Indices := nil;
  Mesh.PrimitiveCount := 2;
  Mesh.Update;
end;

procedure TAdPlaneMesh.SetSizeCoeff(AIndex: integer; AValue: single);
begin
  case AIndex of
    0: FWidth := AValue;
    1: FHeight := AValue;
  end;

  LoadMeshData;
end;

end.
