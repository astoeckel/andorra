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

{This unit contains some 3D-Object classes. Please notice, that those classes are not ready yet. This will be done for version 0.4}
unit Ad3DObj;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses Classes, AdTypes, AdClasses, AdDraws;

type
  {This class represents a main 3D-object, which can be rotated scaled and translated.}
  TAdMesh = class
    private
      FBuffer:TAd2DMesh;
      FParent:TAdDraw;
      FMatrix:TAdMatrix;
      FRotX,FRotY,FRotZ:single;
      FScaleX,FScaleY,FScaleZ:single;
      FX,FY,FZ:single;
      FWidth,FHeight,FDepth:single;
      FDrawMode:TAd2DDrawMode;
      FBlendMode:TAd2DBlendMode;
      FColor:TAndorraColor;
    protected
      procedure Notify(ASender:TObject;AEvent:TSurfaceEventState);
      procedure UpdateMatrix(Index:integer;Value:single);virtual;
      property Width:single index 9 read FWidth write UpdateMatrix;
      property Height:single index 10 read FHeight write UpdateMatrix;
      property Depth:single index 11 read FDepth write UpdateMatrix;
      property Matrix:TAdMatrix read FMatrix write FMatrix;
    public
      {The constructor of the TAdMesh class. AParent specifies the target AdDraw. TAdMesh will automaticly connect to the AdDraws surface events for finalization and initialization.}
      constructor Create(AParent:TAdDraw);
      {The destructor of the TAdMesh class. Destroys all used objects.}
      destructor Destroy;override;

      {Initializes the mesh. This means creating a TAd2dMesh and loading the mesh data by calling the virtual "CreateMesh" function.}
      procedure Initialize;
      {Destroys the instance of TAd2dMesh.}
      procedure Finalize;

      {This virtual function should be used to load the mesh data (so vertices and indices) into the TAd2dMesh}
      procedure CreateMesh;virtual;

      {This draws the TAd2dMesh buffer using the specified "BlendMode" and "DrawMode"}
      procedure Draw;virtual;

      {A link to the TAd2dMesh buffer}
      property Buffer:TAd2DMesh read FBuffer;
      {A link to the parent TAdDraw}
      property Parent:TAdDraw read FParent;

      {Setting this property will set the X-Coordinate of the object by updating the matrix of the mesh}
      property X:single index 0 read FX write UpdateMatrix;
      {Setting this property will set the Y-Coordinate of the object by updating the matrix of the mesh}
      property Y:single index 1 read FY write UpdateMatrix;
      {Setting this property will set the Z-Coordinate of the object by updating the matrix of the mesh}
      property Z:single index 2 read FZ write UpdateMatrix;
      {Setting this property will set the X-Roation of the object by updating the matrix of the mesh}      
      property RotationX:single index 3 read FRotX write UpdateMatrix;
      {Setting this property will set the Y-Roation of the object by updating the matrix of the mesh}      
      property RotationY:single index 4 read FRotY write UpdateMatrix;
      {Setting this property will set the Z-Roation of the object by updating the matrix of the mesh}      
      property RotationZ:single index 5 read FRotZ write UpdateMatrix;
      {Setting this property will set the X-Scale of the object by updating the matrix of the mesh}      
      property ScaleX:single index 6 read FScaleX write UpdateMatrix;
      {Setting this property will set the Y-Scale of the object by updating the matrix of the mesh}      
      property ScaleY:single index 7 read FScaleY write UpdateMatrix;
      {Setting this property will set the Z-Scale of the object by updating the matrix of the mesh}      
      property ScaleZ:single index 8 read FScaleZ write UpdateMatrix;

      {This property sets the color which should be used by the "CreateMesh" function}      
      property Color:TAndorraColor read FColor write FColor;

      {This property sets the blendmode the object is drawn with}      
      property BlendMode:TAd2DBlendMode read FBlendMode write FBlendMode;
      {This property sets the drawmode the object is drawn with}      
      property DrawMode:TAd2DDrawMode read FDrawMode write FDrawMode;

      {This virtual function stores the mesh data into a stream}      
      procedure SaveToStream(AStream:TStream);virtual;
      {This virtual function loads the mesh data from a stream}      
      procedure LoadFromStream(AStream:TStream);virtual;
      {This procedure uses SaveToStream using a "TMemoryStream" to store the mesh data in a file}      
      procedure SaveToFile(AFile:string);
      {This procedure uses LoadFromStream using a "TMemoryStream" to load the mesh data from a file}      
      procedure LoadFromFile(AFile:string);
  end;

  {This simple child class of TAdMesh creates a simple plane}
  TAdPlane = class(TAdMesh)
    private
    protected
    public
      {This base function creates the base mesh consisting of four vertices}
      procedure CreateMesh;override;
      {Use this property to set the width of the created plane}
      property Width;
      {Use this property to set the size of the created plane}
      property Height;
  end;

implementation

{ TAdMesh }

constructor TAdMesh.Create(AParent: TAdDraw);
begin
  inherited Create;
  FParent := AParent;
  FParent.RegisterNotifyEvent(Notify);
  Initialize;

  FDrawMode := adTriangleStrips;
  FBlendMode := bmAlpha;
  FColor := Ad_ARGB(255,255,255,255);

  FScaleX := 1;
  FScaleY := 1;
  FScaleZ := 1;
end;

destructor TAdMesh.Destroy;
begin         
  Finalize;
  FParent.UnRegisterNotifyEvent(Notify);
  inherited;
end;

procedure TAdMesh.CreateMesh;
begin
  //Nothig to do now
end;

procedure TAdMesh.Draw;
begin
  Buffer.SetMatrix(FMatrix);
  Buffer.Draw(FBlendMode,FDrawMode);
end;

procedure TAdMesh.Finalize;
begin
  if Buffer <> nil then
  begin
    Buffer.Free;
    FBuffer := nil;
  end;              
end;

procedure TAdMesh.Initialize;
begin
  Finalize;
  FBuffer := FParent.AdAppl.CreateMesh;
end;

procedure TAdMesh.LoadFromFile(AFile: string);
var ms:TMemoryStream;
begin
  ms := TMemoryStream.Create;
  ms.LoadFromFile(AFile);
  ms.Position := 0;
  LoadFromStream(ms);
  ms.Free;
end;

procedure TAdMesh.SaveToFile(AFile: string);
var ms:TMemoryStream;
begin
  ms := TMemoryStream.Create;
  SaveToStream(ms);
  ms.SaveToFile(AFile);
  ms.Free;
end;

procedure TAdMesh.LoadFromStream(AStream: TStream);
var c:integer;
begin
  c := 0;
  if Buffer.Vertices <> nil then
  begin
    c := Length(Buffer.Vertices);
  end;
  AStream.Write(c,SizeOf(c));
  if Buffer.Vertices <> nil then
  begin
    AStream.Write(Buffer.Vertices[0],SizeOf(TAdVertex)*c)
  end;
  c := 0;
  if Buffer.IndexBuffer <> nil then
  begin
    c := Length(Buffer.IndexBuffer);
  end;
  AStream.Write(c,SizeOf(c));
  if Buffer.IndexBuffer <> nil then
  begin
    AStream.Write(Buffer.IndexBuffer[0],SizeOf(TAdVertex)*c)
  end;
end;

procedure TAdMesh.SaveToStream(AStream: TStream);
var c:integer;
    vert:TAdVertexArray;
    inde:TAdIndexArray;
begin
  AStream.Read(c,SizeOf(c));
  SetLength(Buffer.Vertices,c);
  AStream.Read(Buffer.Vertices[0],SizeOf(TAdVertex)*c);
  SetLength(Buffer.IndexBuffer,c);
  AStream.Read(Buffer.IndexBuffer[0],SizeOf(TAdVertex)*c);
end;

procedure TAdMesh.Notify(ASender: TObject; AEvent: TSurfaceEventState);
begin
  case AEvent of
    seInitialize: Initialize;
    seFinalize: Finalize;
  end;
end;

procedure TAdMesh.UpdateMatrix(Index: integer; Value: single);
var Mat:TAdMatrix;
begin
  case Index of
    0:FX := Value;
    1:FY := Value;
    2:FZ := Value;
    3:FRotX := Value;
    4:FRotY := Value;
    5:FRotZ := Value;
    6:FScaleX := Value;
    7:FScaleY := Value;
    8:FScaleZ := Value;
    9:FWidth := Value;
   10:FHeight := Value;
   11:FDepth := Value;
  end;

  FMatrix := AdMatrix_Identity;
  Mat := AdMatrix_Scale(FScaleX,FScaleY,FScaleZ);
  FMatrix := AdMatrix_Multiply(FMatrix,Mat);
  Mat := AdMatrix_RotationX(FRotX);
  FMatrix := AdMatrix_Multiply(FMatrix,Mat);
  Mat := AdMatrix_RotationY(FRotY);
  FMatrix := AdMatrix_Multiply(FMatrix,Mat);
  Mat := AdMatrix_RotationZ(FRotZ);
  FMatrix := AdMatrix_Multiply(FMatrix,Mat);
  Mat := AdMatrix_Translate(FX+Width / 2,FY+Height / 2,FZ+Depth / 2);
  FMatrix := AdMatrix_Multiply(FMatrix,Mat);
end;

{ TAdPlane }

procedure TAdPlane.CreateMesh;
var
  vert:TAdVertexArray;
begin
  SetLength(vert,4);
  vert[0].Position := AdVector3(-Width / 2, Height / 2,0);
  vert[0].Color := Color;
  vert[0].Normal := AdVector3(0,0,-1);
  vert[0].Texture := AdVector2(0,1);

  vert[1].Position := AdVector3(-Width / 2, -Height / 2,0);
  vert[1].Color := Color;
  vert[1].Normal := AdVector3(0,0,-1);
  vert[1].Texture := AdVector2(0,0);

  vert[2].Position := AdVector3(Width / 2, Height / 2,0);
  vert[2].Color := Color;
  vert[2].Normal := AdVector3(0,0,-1);
  vert[2].Texture := AdVector2(1,1);

  vert[3].Position := AdVector3(Width / 2, -Height / 2,0);
  vert[3].Color := Color;
  vert[3].Normal := AdVector3(0,0,-1);
  vert[3].Texture := AdVector2(1,0);

  Buffer.Vertices := vert;
  Buffer.IndexBuffer := nil;
  Buffer.PrimitiveCount := 2;
  Buffer.Update;

  UpdateMatrix(-1,0);
end;

end.
