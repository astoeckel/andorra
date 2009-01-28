{
* EXCEPT AS EXPRESSLY SET FORTH IN THIS AGREEMENT, THE PROGRAM IS PROVIDED ON
* AN "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, EITHER EXPRESS
* OR IMPLIED INCLUDING, WITHOUT LIMITATION, ANY WARRANTIES OR CONDITIONS OF
* TITLE, NON-INFRINGEMENT, MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.
*
* This program is licensed under the Common Public License (CPL) Version 1.0
* You should have recieved a copy of the license with this file.
* If not, see http://www.opensource.org/licenses/cpl1.0.txt for more informations.
* You also should have recieved a copy of this license with this file.
* 
* Inspite of the incompatibility between the Common Public License (CPL) and the GNU General Public License (GPL) you're allowed to use this program 
* under the GPL. 
* If not, see http://www.gnu.org/licenses/gpl.txt for more informations.
*
* Project: Andorra 2D
* Author:  Andreas Stoeckel
* File: AdModelLoaderClass.pas
* Comment: This unit contains an abstract interface for loading models.
}

{This unit contains an abstract interface for loading models. Most classes in
 this unit are used internally.}
unit Ad3DModelLoaderClass;

interface

uses
  Classes, 
  AdPersistent, AdList, AdClasses, AdTypes, AdBitmap, AdBuffer;

type
  TAd3DModelTexture = class
    private
      FBitmap: TAdBitmap;
      FName: string;
    public
      constructor Create;
      destructor Destroy;override;
      
      property Bitmap: TAdBitmap read FBitmap;
      property Name: string read FName write FName;
  end;

  TAd3DModelMaterial = class
    private
      FName: string;
      FMaterial: TAd2dMaterial;
    public
      property Name: string read FName write FName;
      property Material: TAd2dMaterial read FMaterial write FMaterial;
  end;

  TAd3DModelSubmeshDataList = class;
  TAd3DModelSubmeshData = class
    private
      FMaterialName: string;
      FTextureName: string;
      FVertices: TAdVertexArray;
      FIndices: TAdIndexArray;
      FSubItems: TAd3DModelSubmeshDataList;
      FName: string;
    public
      constructor Create;
      destructor Destroy;override;

      property Name: string read FName write FName;      
      property MaterialName: string read FMaterialName write FMaterialName;
      property TextureName: string read FTextureName write FTextureName;
      property Vertices: TAdVertexArray read FVertices write FVertices;
      property Indices: TAdIndexArray read FIndices write FIndices;
      property SubItems: TAd3DModelSubmeshDataList read FSubItems;
  end;

  TAd3DModelTextureList = class(TAdList)
    private
      function GetItem(AIndex: integer): TAd3DModelTexture;
      procedure SetItem(AIndex: integer; AValue: TAd3DModelTexture);
    protected
      procedure Notify(ptr: Pointer; action: TListNotification);override;
    public
      function Find(AName: string): TAd3DModelTexture;
      property Items[Index: integer]: TAd3DModelTexture read GetItem write SetItem; default;
  end;

  TAd3DModelMaterialList = class(TAdList)
    private
      function GetItem(AIndex: integer): TAd3DModelMaterial;
      procedure SetItem(AIndex: integer; AValue: TAd3DModelMaterial);
    protected
      procedure Notify(ptr: Pointer; action: TListNotification);override;
    public
      function Find(AName: string): TAd3DModelMaterial;
      property Items[Index: integer]: TAd3DModelMaterial read GetItem write SetItem; default;
  end;

  TAd3DModelSubmeshDataList = class(TAdList)
    private
      function GetItem(AIndex: integer): TAd3DModelSubmeshData;
      procedure SetItem(AIndex: integer; AValue: TAd3DModelSubmeshData);
    protected
      procedure Notify(ptr: Pointer; action: TListNotification);override;
    public
      function Find(AName: string): TAd3DModelSubmeshData;
      property Items[Index: integer]: TAd3DModelSubmeshData read GetItem write SetItem; default;
  end;

  TAd3DModelLoader = class(TAdPersistent)
    private
      FMaterials: TAd3DModelMaterialList;
      FTextures: TAd3DModelTextureList;
      FSubmeshs: TAd3DModelSubmeshDataList;
    protected
      procedure Clear;
    public
      constructor Create;virtual;
      destructor Destroy;override;

      function LoadFromStream(AStream: TStream): boolean;virtual;abstract;
      function SupportsStream(AStream: TStream): boolean;virtual;abstract;
      
      property Materials: TAd3DModelMaterialList read FMaterials;
      property Textures: TAd3DModelTextureList read FTextures;
      property Submeshs: TAd3DModelSubmeshDataList read FSubmeshs;
    end;

  TAd3DModelLoaderClass = class of TAd3DModelLoader;

var
  Registered3DModelLoaders: TStringList;

procedure Register3DModelLoader(ALoaderClass: TAd3DModelLoaderClass);

implementation

procedure Register3DModelLoader(ALoaderClass: TAd3DModelLoaderClass);
begin
  Registered3DModelLoaders.Add(ALoaderClass.ClassName);
  AdRegisterClass(ALoaderClass);
end;

{ TAd3DModelTextureList }

function TAd3DModelTextureList.Find(AName: string): TAd3DModelTexture;
var
  i: Integer;
begin
  result := nil;
  for i := 0 to Count - 1 do
    if Items[i].Name = AName then
    begin
      result := Items[i];
      break;
    end;
end;

function TAd3DModelTextureList.GetItem(AIndex: integer): TAd3DModelTexture;
begin
  result := inherited Items[AIndex];
end;

procedure TAd3DModelTextureList.SetItem(AIndex: integer;
  AValue: TAd3DModelTexture);
begin
  inherited Items[AIndex] := AValue;
end;

procedure TAd3DModelTextureList.Notify(ptr: Pointer; action: TListNotification);
begin
  if action = lnDeleted then
    TAd3DModelTexture(ptr).Free;
end;

{ TAd3DModelSubMeshDataList }

function TAd3DModelSubmeshDataList.Find(AName: string): TAd3DModelSubmeshData;
var
  i: Integer;
begin
  result := nil;
  for i := 0 to Count - 1 do
    if Items[i].Name = AName then
    begin
      result := Items[i];
      break;
    end;
end;

function TAd3DModelSubMeshDataList.GetItem(AIndex: integer): TAd3DModelSubMeshData;
begin
  result := inherited Items[AIndex];
end;

procedure TAd3DModelSubMeshDataList.SetItem(AIndex: integer;
  AValue: TAd3DModelSubMeshData);
begin
  inherited Items[AIndex] := AValue;
end;

procedure TAd3DModelSubMeshDataList.Notify(ptr: Pointer; action: TListNotification);
begin
  if action = lnDeleted then
    TAd3DModelSubMeshData(ptr).Free;
end;

{ TAd3DModelMaterialList }

function TAd3DModelMaterialList.Find(AName: string): TAd3DModelMaterial;
var
  i: Integer;
begin
  result := nil;
  for i := 0 to Count - 1 do
    if Items[i].Name = AName then
    begin
      result := Items[i];
      break;
    end;
end;

function TAd3DModelMaterialList.GetItem(AIndex: integer): TAd3DModelMaterial;
begin
  result := inherited Items[AIndex];
end;

procedure TAd3DModelMaterialList.SetItem(AIndex: integer;
  AValue: TAd3DModelMaterial);
begin
  inherited Items[AIndex] := AValue;
end;

procedure TAd3DModelMaterialList.Notify(ptr: Pointer; action: TListNotification);
begin
  if action = lnDeleted then
    TAd3DModelMaterial(ptr).Free;
end;

{ TAd3DModelTexture }

constructor TAd3DModelTexture.Create;
begin
  inherited;

  FBitmap := TAdBitmap.Create;
end;

destructor TAd3DModelTexture.Destroy;
begin
  FBitmap.Free;

  inherited;
end;

{ TAd3DModelSubmeshData }

constructor TAd3DModelSubmeshData.Create;
begin
  inherited;                 
  FSubItems := TAd3DModelSubmeshDataList.Create;
end;

destructor TAd3DModelSubmeshData.Destroy;
begin
  FSubItems.Free;
  inherited;
end;

{ TAd3DModelLoader }

constructor TAd3DModelLoader.Create;
begin
  inherited;

  FMaterials := TAd3DModelMaterialList.Create;
  FTextures := TAd3DModelTextureList.Create;
  FSubmeshs := TAd3DModelSubmeshDataList.Create;
end;

destructor TAd3DModelLoader.Destroy;
begin
  FMaterials.Free;
  FTextures.Free;
  FSubmeshs.Free;
  
  inherited;
end;

procedure TAd3DModelLoader.Clear;
begin
  FMaterials.Clear;
  FTextures.Clear;
  FSubmeshs.Clear;
end;

initialization
  Registered3DModelLoaders := TStringList.Create; 

finalization
  Registered3DModelLoaders.Free;

end.
