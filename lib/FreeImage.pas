unit FreeImage;

// ==========================================================
// Delphi wrapper for FreeImage 3
//
// Design and implementation by
// - Simon Beavis
// - Peter Byström
// - Anatoliy Pulyaevskiy (xvel84@rambler.ru)
//
// This file is part of FreeImage 3
//
// COVERED CODE IS PROVIDED UNDER THIS LICENSE ON AN "AS IS" BASIS, WITHOUT WARRANTY
// OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, WITHOUT LIMITATION, WARRANTIES
// THAT THE COVERED CODE IS FREE OF DEFECTS, MERCHANTABLE, FIT FOR A PARTICULAR PURPOSE
// OR NON-INFRINGING. THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE COVERED
// CODE IS WITH YOU. SHOULD ANY COVERED CODE PROVE DEFECTIVE IN ANY RESPECT, YOU (NOT
// THE INITIAL DEVELOPER OR ANY OTHER CONTRIBUTOR) ASSUME THE COST OF ANY NECESSARY
// SERVICING, REPAIR OR CORRECTION. THIS DISCLAIMER OF WARRANTY CONSTITUTES AN ESSENTIAL
// PART OF THIS LICENSE. NO USE OF ANY COVERED CODE IS AUTHORIZED HEREUNDER EXCEPT UNDER
// THIS DISCLAIMER.
//
// Use at your own risk!
// ==========================================================
// Modified by Nikolai Wyderka 2008/06/19 for Linux compatibility

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

{$MINENUMSIZE 4} // Make sure enums are stored as an integer to be compatible with C/C++

const
{$IFDEF WIN32}
  FIDLL = 'FreeImage.dll';
{$ELSE}
  {$DEFINE FINoPrefix}
  {$DEFINE FIcdecl}
  FIDLL = 'libfreeimage.so';
{$ENDIF}
  
// --------------------------------------------------------------------------
// Bitmap types -------------------------------------------------------------
// --------------------------------------------------------------------------

type
  FIBITMAP = record
    data : Pointer;
  end;
  PFIBITMAP = ^FIBITMAP;

  FIMULTIBITMAP = record
    data : Pointer;
  end;
  PFIMULTIBITMAP = ^FIMULTIBITMAP;

  DWORD = type LongWord;
  PDWORD = ^DWORD;

  TRGBQuad = record
    Red, Green, Blue, Reserved : byte;
  end;
  PRGBQuad = ^TRGBQuad;

// --------------------------------------------------------------------------
// Indexes for byte arrays, masks and shifts for treating pixels as words ---
// These coincide with the order of RGBQUAD and RGBTRIPLE -------------------
// Little Endian (x86 / MS Windows, Linux) : BGR(A) order -------------------
// --------------------------------------------------------------------------

const
  FI_RGBA_RED         = 2;
  FI_RGBA_GREEN       = 1;
  FI_RGBA_BLUE        = 0;
  FI_RGBA_ALPHA       = 3;
  FI_RGBA_RED_MASK    = $00FF0000;
  FI_RGBA_GREEN_MASK  = $0000FF00;
  FI_RGBA_BLUE_MASK   = $000000FF;
  FI_RGBA_ALPHA_MASK  = $FF000000;
  FI_RGBA_RED_SHIFT   = 16;
  FI_RGBA_GREEN_SHIFT = 8;
  FI_RGBA_BLUE_SHIFT  = 0;
  FI_RGBA_ALPHA_SHIFT = 24;

// --------------------------------------------------------------------------
// The 16bit macros only include masks and shifts, --------------------------
// since each color element is not byte aligned -----------------------------
// --------------------------------------------------------------------------

const
  FI16_555_RED_MASK		 = $7C00;
  FI16_555_GREEN_MASK	 = $03E0;
  FI16_555_BLUE_MASK	 = $001F;
  FI16_555_RED_SHIFT	 = 10;
  FI16_555_GREEN_SHIFT = 5;
  FI16_555_BLUE_SHIFT	 = 0;
  FI16_565_RED_MASK		 = $F800;
  FI16_565_GREEN_MASK	 = $07E0;
  FI16_565_BLUE_MASK	 = $001F;
  FI16_565_RED_SHIFT	 = 11;
  FI16_565_GREEN_SHIFT = 5;
  FI16_565_BLUE_SHIFT	 = 0;

// --------------------------------------------------------------------------
// ICC profile support ------------------------------------------------------
// --------------------------------------------------------------------------

const
  FIICC_DEFAULT = $0;
  FIICC_COLOR_IS_CMYK	= $1;

type
  FIICCPROFILE = record
    flags : WORD;   // info flag
    size : DWORD;   // profile's size measured in bytes
    data : Pointer; // points to a block of contiguous memory containing the profile
  end;
  PFIICCPROFILE = ^FIICCPROFILE;

// --------------------------------------------------------------------------
// Important enums ----------------------------------------------------------
// --------------------------------------------------------------------------

type
  FREE_IMAGE_FORMAT         = type Integer;
  FREE_IMAGE_TYPE           = type Integer;
  FREE_IMAGE_COLOR_TYPE     = type Integer;
  FREE_IMAGE_QUANTIZE       = type Integer;
  FREE_IMAGE_DITHER         = type Integer;
  FREE_IMAGE_FILTER         = type Integer;
  FREE_IMAGE_COLOR_CHANNEL  = type Integer;
  FREE_IMAGE_MDTYPE         = type Integer;
  FREE_IMAGE_MDMODEL        = type Integer;
  FREE_IMAGE_JPEG_OPERATION = type Integer;
  FREE_IMAGE_TMO            = type Integer;

const
  // I/O image format identifiers.
  FIF_UNKNOWN = FREE_IMAGE_FORMAT(-1);
  FIF_BMP     = FREE_IMAGE_FORMAT(0);
  FIF_ICO     = FREE_IMAGE_FORMAT(1);
  FIF_JPEG    = FREE_IMAGE_FORMAT(2);
  FIF_JNG     = FREE_IMAGE_FORMAT(3);
  FIF_KOALA   = FREE_IMAGE_FORMAT(4);
  FIF_LBM     = FREE_IMAGE_FORMAT(5);
  FIF_IFF     = FIF_LBM;
  FIF_MNG     = FREE_IMAGE_FORMAT(6);
  FIF_PBM     = FREE_IMAGE_FORMAT(7);
  FIF_PBMRAW  = FREE_IMAGE_FORMAT(8);
  FIF_PCD     = FREE_IMAGE_FORMAT(9);
  FIF_PCX     = FREE_IMAGE_FORMAT(10);
  FIF_PGM     = FREE_IMAGE_FORMAT(11);
  FIF_PGMRAW  = FREE_IMAGE_FORMAT(12);
  FIF_PNG     = FREE_IMAGE_FORMAT(13);
  FIF_PPM     = FREE_IMAGE_FORMAT(14);
  FIF_PPMRAW  = FREE_IMAGE_FORMAT(15);
  FIF_RAS     = FREE_IMAGE_FORMAT(16);
  FIF_TARGA   = FREE_IMAGE_FORMAT(17);
  FIF_TIFF    = FREE_IMAGE_FORMAT(18);
  FIF_WBMP    = FREE_IMAGE_FORMAT(19);
  FIF_PSD     = FREE_IMAGE_FORMAT(20);
  FIF_CUT     = FREE_IMAGE_FORMAT(21);
  FIF_XBM     = FREE_IMAGE_FORMAT(22);
  FIF_XPM     = FREE_IMAGE_FORMAT(23);
  FIF_DDS     = FREE_IMAGE_FORMAT(24);
  FIF_GIF     = FREE_IMAGE_FORMAT(25);
  FIF_HDR     = FREE_IMAGE_FORMAT(26);
  FIF_FAXG3   = FREE_IMAGE_FORMAT(27);
  FIF_SGI     = FREE_IMAGE_FORMAT(28);  

  // Image type used in FreeImage.
  FIT_UNKNOWN = FREE_IMAGE_TYPE(0);  // unknown type
  FIT_BITMAP  = FREE_IMAGE_TYPE(1);	 // standard image: 1-, 4-, 8-, 16-, 24-, 32-bit
  FIT_UINT16  = FREE_IMAGE_TYPE(2);	 // array of unsigned short: unsigned 16-bit
  FIT_INT16   = FREE_IMAGE_TYPE(3);  // array of short: signed 16-bit
  FIT_UINT32  = FREE_IMAGE_TYPE(4);	 // array of unsigned long: unsigned 32-bit
  FIT_INT32   = FREE_IMAGE_TYPE(5);	 // array of long: signed 32-bit
  FIT_FLOAT   = FREE_IMAGE_TYPE(6);	 // array of float: 32-bit IEEE floating point
  FIT_DOUBLE  = FREE_IMAGE_TYPE(7);	 // array of double: 64-bit IEEE floating point
  FIT_COMPLEX = FREE_IMAGE_TYPE(8);	 // array of FICOMPLEX: 2 x 64-bit IEEE floating point
  FIT_RGB16	  = FREE_IMAGE_TYPE(9);	 // 48-bit RGB image: 3 x 16-bit
	FIT_RGBA16	= FREE_IMAGE_TYPE(10); // 64-bit RGBA image: 4 x 16-bit
	FIT_RGBF	  = FREE_IMAGE_TYPE(11); // 96-bit RGB float image: 3 x 32-bit IEEE floating point
	FIT_RGBAF	  = FREE_IMAGE_TYPE(12); // 128-bit RGBA float image: 4 x 32-bit IEEE floating point

  // Image color type used in FreeImage.
  FIC_MINISWHITE = FREE_IMAGE_COLOR_TYPE(0); // min value is white
  FIC_MINISBLACK = FREE_IMAGE_COLOR_TYPE(1); // min value is black
  FIC_RGB        = FREE_IMAGE_COLOR_TYPE(2); // RGB color model
  FIC_PALETTE    = FREE_IMAGE_COLOR_TYPE(3); // color map indexed
  FIC_RGBALPHA   = FREE_IMAGE_COLOR_TYPE(4); // RGB color model with alpha channel
  FIC_CMYK       = FREE_IMAGE_COLOR_TYPE(5); // CMYK color model

  // Color quantization algorithms. Constants used in FreeImage_ColorQuantize.
  FIQ_WUQUANT = FREE_IMAGE_QUANTIZE(0);	// Xiaolin Wu color quantization algorithm
  FIQ_NNQUANT = FREE_IMAGE_QUANTIZE(1);	// NeuQuant neural-net quantization algorithm by Anthony Dekker

  // Dithering algorithms. Constants used FreeImage_Dither.
  FID_FS            = FREE_IMAGE_DITHER(0);	// Floyd & Steinberg error diffusion
  FID_BAYER4x4      = FREE_IMAGE_DITHER(1);	// Bayer ordered dispersed dot dithering (order 2 dithering matrix)
  FID_BAYER8x8      = FREE_IMAGE_DITHER(2);	// Bayer ordered dispersed dot dithering (order 3 dithering matrix)
  FID_CLUSTER6x6    = FREE_IMAGE_DITHER(3);	// Ordered clustered dot dithering (order 3 - 6x6 matrix)
  FID_CLUSTER8x8    = FREE_IMAGE_DITHER(4);	// Ordered clustered dot dithering (order 4 - 8x8 matrix)
  FID_CLUSTER16x16  = FREE_IMAGE_DITHER(5); // Ordered clustered dot dithering (order 8 - 16x16 matrix)

  // Lossless JPEG transformations Constants used in FreeImage_JPEGTransform
	FIJPEG_OP_NONE			  = FREE_IMAGE_JPEG_OPERATION(0);	// no transformation
	FIJPEG_OP_FLIP_H		  = FREE_IMAGE_JPEG_OPERATION(1);	// horizontal flip
	FIJPEG_OP_FLIP_V		  = FREE_IMAGE_JPEG_OPERATION(2);	// vertical flip
	FIJPEG_OP_TRANSPOSE		= FREE_IMAGE_JPEG_OPERATION(3);	// transpose across UL-to-LR axis
	FIJPEG_OP_TRANSVERSE	= FREE_IMAGE_JPEG_OPERATION(4);	// transpose across UR-to-LL axis
	FIJPEG_OP_ROTATE_90		= FREE_IMAGE_JPEG_OPERATION(5);	// 90-degree clockwise rotation
	FIJPEG_OP_ROTATE_180	= FREE_IMAGE_JPEG_OPERATION(6);	// 180-degree rotation
	FIJPEG_OP_ROTATE_270	= FREE_IMAGE_JPEG_OPERATION(7); // 270-degree clockwise (or 90 ccw)

  // Tone mapping operators. Constants used in FreeImage_ToneMapping.
  FITMO_DRAGO03	   = FREE_IMAGE_TMO(0);	// Adaptive logarithmic mapping (F. Drago, 2003)
	FITMO_REINHARD05 = FREE_IMAGE_TMO(1);	// Dynamic range reduction inspired by photoreceptor physiology (E. Reinhard, 2005)

  // Upsampling / downsampling filters. Constants used in FreeImage_Rescale.
  FILTER_BOX	      = FREE_IMAGE_FILTER(0);	// Box, pulse, Fourier window, 1st order (constant) b-spline
  FILTER_BICUBIC    = FREE_IMAGE_FILTER(1);	// Mitchell & Netravali's two-param cubic filter
  FILTER_BILINEAR   = FREE_IMAGE_FILTER(2);	// Bilinear filter
  FILTER_BSPLINE    = FREE_IMAGE_FILTER(3);	// 4th order (cubic) b-spline
  FILTER_CATMULLROM = FREE_IMAGE_FILTER(4);	// Catmull-Rom spline, Overhauser spline
  FILTER_LANCZOS3   = FREE_IMAGE_FILTER(5);	// Lanczos3 filter

  // Color channels. Constants used in color manipulation routines.
  FICC_RGB   = FREE_IMAGE_COLOR_CHANNEL(0); // Use red, green and blue channels
  FICC_RED   = FREE_IMAGE_COLOR_CHANNEL(1); // Use red channel
  FICC_GREEN = FREE_IMAGE_COLOR_CHANNEL(2); // Use green channel
  FICC_BLUE  = FREE_IMAGE_COLOR_CHANNEL(3); // Use blue channel
  FICC_ALPHA = FREE_IMAGE_COLOR_CHANNEL(4); // Use alpha channel
  FICC_BLACK = FREE_IMAGE_COLOR_CHANNEL(5); // Use black channel
  FICC_REAL  = FREE_IMAGE_COLOR_CHANNEL(6); // Complex images: use real part
  FICC_IMAG  = FREE_IMAGE_COLOR_CHANNEL(7); // Complex images: use imaginary part
  FICC_MAG   = FREE_IMAGE_COLOR_CHANNEL(8); // Complex images: use magnitude
  FICC_PHASE = FREE_IMAGE_COLOR_CHANNEL(9);	// Complex images: use phase

  // Tag data type information (based on TIFF specifications)
  FIDT_NOTYPE	   = FREE_IMAGE_MDTYPE(0);	// placeholder
  FIDT_BYTE	     = FREE_IMAGE_MDTYPE(1);	// 8-bit unsigned integer
  FIDT_ASCII	   = FREE_IMAGE_MDTYPE(2);	// 8-bit bytes w/ last byte null
  FIDT_SHORT	   = FREE_IMAGE_MDTYPE(3);	// 16-bit unsigned integer
  FIDT_LONG	     = FREE_IMAGE_MDTYPE(4);	// 32-bit unsigned integer
  FIDT_RATIONAL  = FREE_IMAGE_MDTYPE(5);	// 64-bit unsigned fraction
  FIDT_SBYTE	   = FREE_IMAGE_MDTYPE(6);	// 8-bit signed integer
  FIDT_UNDEFINED = FREE_IMAGE_MDTYPE(7);	// 8-bit untyped data
  FIDT_SSHORT	   = FREE_IMAGE_MDTYPE(8);	// 16-bit signed integer
  FIDT_SLONG	   = FREE_IMAGE_MDTYPE(9);	// 32-bit signed integer
  FIDT_SRATIONAL = FREE_IMAGE_MDTYPE(10); // 64-bit signed fraction
  FIDT_FLOAT	   = FREE_IMAGE_MDTYPE(11); // 32-bit IEEE floating point
  FIDT_DOUBLE	   = FREE_IMAGE_MDTYPE(12); // 64-bit IEEE floating point
  FIDT_IFD	     = FREE_IMAGE_MDTYPE(13);	// 32-bit unsigned integer (offset)
  FIDT_PALETTE	 = FREE_IMAGE_MDTYPE(14);	// 32-bit RGBQUAD

  // Metadata models supported by FreeImage
  FIMD_NODATA	        = FREE_IMAGE_MDMODEL(-1);
  FIMD_COMMENTS	      = FREE_IMAGE_MDMODEL(0);  // single comment or keywords
  FIMD_EXIF_MAIN      = FREE_IMAGE_MDMODEL(1);  // Exif-TIFF metadata
  FIMD_EXIF_EXIF      = FREE_IMAGE_MDMODEL(2);  // Exif-specific metadata
  FIMD_EXIF_GPS	      = FREE_IMAGE_MDMODEL(3);  // Exif GPS metadata
  FIMD_EXIF_MAKERNOTE = FREE_IMAGE_MDMODEL(4);  // Exif maker note metadata
  FIMD_EXIF_INTEROP   = FREE_IMAGE_MDMODEL(5);  // Exif interoperability metadata
  FIMD_IPTC	          = FREE_IMAGE_MDMODEL(6);  // IPTC/NAA metadata
  FIMD_XMP	          = FREE_IMAGE_MDMODEL(7);  // Abobe XMP metadata
  FIMD_GEOTIFF	      = FREE_IMAGE_MDMODEL(8);  // GeoTIFF metadata (to be implemented)
  FIMD_ANIMATION		  = FREE_IMAGE_MDMODEL(9);  // Animation metadata
  FIMD_CUSTOM	        = FREE_IMAGE_MDMODEL(10); // Used to attach other metadata types to a dib

//{$endif}

type
  // Handle to a metadata model
  FIMETADATA = record
    data: Pointer;
  end;
  PFIMETADATA = ^FIMETADATA;

  // Handle to a metadata tag
  FITAG = record
    data: Pointer;
  end;
  PFITAG = ^FITAG;

// --------------------------------------------------------------------------
// File IO routines ---------------------------------------------------------
// --------------------------------------------------------------------------

type
  FI_Handle = Pointer;
  PCardinal = ^Cardinal;
  PInt = ^Integer;

  FI_ReadProc = function(buffer : pointer; size : Cardinal; count : Cardinal; handle : fi_handle) : PCardinal; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF};
  FI_WriteProc = function(buffer : pointer; size, count : Cardinal; handle : FI_Handle) : PCardinal; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF};
  FI_SeekProc = function(handle : fi_handle; offset : longint; origin : integer) : pint; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF};
  FI_TellProc = function(handle : fi_handle) : PCardinal; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF};

  FreeImageIO = packed record
    read_proc : FI_ReadProc;     // pointer to the function used to read data
    write_proc: FI_WriteProc;    // pointer to the function used to write data
    seek_proc : FI_SeekProc;     // pointer to the function used to seek
    tell_proc : FI_TellProc;     // pointer to the function used to aquire the current position
  end;
  PFreeImageIO = ^FreeImageIO;

  // Handle to a memory I/O stream
  FIMEMORY = record
    data: Pointer;
  end;
  PFIMEMORY = ^FIMEMORY;

const
  // constants used in FreeImage_Seek for Origin parameter
  SEEK_SET = 0;
  SEEK_CUR = 1;
  SEEK_END = 2;

// --------------------------------------------------------------------------
// Plugin routines ----------------------------------------------------------
// --------------------------------------------------------------------------

type
  PPluginStruct = ^PluginStruct;

  FI_InitProc = procedure(Plugin: PPluginStruct; Format_ID: Integer); {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF};
  FI_FormatProc = function: PChar; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF};
  FI_DescriptionProc = function: PChar; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF};
  FI_ExtensionListProc = function: PChar; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF};
  FI_RegExprProc = function: PChar; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF};
  FI_OpenProc = function(IO: PFreeImageIO; Handle: FI_Handle; Read: Boolean): Pointer; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF};
  FI_CloseProc = procedure(IO: PFreeImageIO; Handle: FI_Handle; Data: Pointer); {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF};
  FI_PageCountProc = function(IO: PFreeImageIO; Handle: FI_Handle; Data: Pointer): Integer; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF};
  FI_PageCapabilityProc = function(IO: PFreeImageIO; Handle: FI_Handle; Data: Pointer): integer; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF};
  FI_LoadProc = function(IO: PFreeImageIO; Handle: FI_Handle; Page, Flags: Integer; data: pointer): PFIBITMAP; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF};
  FI_SaveProc = function(IO: PFreeImageIO; Dib: PFIBITMAP; Handle: FI_Handle; Page, Flags: Integer; Data: Pointer): Boolean; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF};
  FI_ValidateProc = function(IO: PFreeImageIO; Handle: FI_Handle): Boolean; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF};
  FI_MimeProc = function: PChar; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF};
  FI_SupportsExportBPPProc = function(Bpp: integer): boolean; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF};
  FI_SupportsExportTypeProc = function(AType: FREE_IMAGE_TYPE): Boolean; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF};
  FI_SupportsICCProfilesProc = function: Boolean; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF};

  PluginStruct = record
    format_proc: FI_FormatProc;
    description_proc: FI_DescriptionProc;
    extension_proc: FI_ExtensionListProc;
    regexpr_proc: FI_RegExprProc;
    open_proc: FI_OpenProc;
    close_proc: FI_CloseProc;
    pagecount_proc: FI_PageCountProc;
    pagecapability_proc: FI_PageCapabilityProc;
    load_proc: FI_LoadProc;
    save_proc: FI_SaveProc;
    validate_proc: FI_ValidateProc;
    mime_proc: FI_MimeProc;
    supports_export_bpp_proc: FI_SupportsExportBPPProc;
    supports_export_type_proc: FI_SupportsExportTypeProc;
    supports_icc_profiles_proc: FI_SupportsICCProfilesProc;
  end;

// --------------------------------------------------------------------------
// Load/Save flag constants -------------------------------------------------
// --------------------------------------------------------------------------

const
  BMP_DEFAULT         = 0;
  BMP_SAVE_RLE        = 1;
  CUT_DEFAULT         = 0;
  DDS_DEFAULT         = 0;
  FAXG3_DEFAULT       = 0;
  GIF_DEFAULT         = 0;
  ICO_DEFAULT         = 0;
  ICO_MAKEALPHA       = 0;     // convert to 32bpp and create an alpha channel from the AND-mask when loading
  IFF_DEFAULT         = 0;
  JPEG_DEFAULT        = 0;
  JPEG_FAST           = 1;
  JPEG_ACCURATE       = 2;
  JPEG_QUALITYSUPERB  = $0080;
  JPEG_QUALITYGOOD    = $0100;
  JPEG_QUALITYNORMAL  = $0200;
  JPEG_QUALITYAVERAGE = $0400;
  JPEG_QUALITYBAD     = $0800;
  JPEG_CMYK           = $1000; // load separated CMYK "as is" (use | to combine with other flags)
  KOALA_DEFAULT       = 0;
  LBM_DEFAULT         = 0;
  MNG_DEFAULT         = 0;
  PCD_DEFAULT         = 0;
  PCD_BASE            = 1;     // load the bitmap sized 768 x 512
  PCD_BASEDIV4        = 2;     // load the bitmap sized 384 x 256
  PCD_BASEDIV16       = 3;     // load the bitmap sized 192 x 128
  PCX_DEFAULT         = 0;
  PNG_DEFAULT         = 0;
  PNG_IGNOREGAMMA     = 1;     // avoid gamma correction
  PNM_DEFAULT         = 0;
  PNM_SAVE_RAW        = 0;     // If set the writer saves in RAW format (i.e. P4, P5 or P6)
  PNM_SAVE_ASCII      = 1;     // If set the writer saves in ASCII format (i.e. P1, P2 or P3)
  PSD_DEFAULT         = 0;
  RAS_DEFAULT         = 0;
  SGI_DEFAULT         = 0;
  TARGA_DEFAULT       = 0;
  TARGA_LOAD_RGB888   = 1;     // If set the loader converts RGB555 and ARGB8888 -> RGB888.
  TIFF_DEFAULT        = 0;
  TIFF_CMYK	          = $0001;  // reads/stores tags for separated CMYK (use | to combine with compression flags)
  TIFF_PACKBITS       = $0100;  // save using PACKBITS compression
  TIFF_DEFLATE        = $0200;  // save using DEFLATE compression
  TIFF_ADOBE_DEFLATE  = $0400;  // save using ADOBE DEFLATE compression
  TIFF_NONE           = $0800;  // save without any compression
  TIFF_CCITTFAX3		  = $1000;  // save using CCITT Group 3 fax encoding
  TIFF_CCITTFAX4		  = $2000;  // save using CCITT Group 4 fax encoding
  TIFF_LZW			      = $4000; 	// save using LZW compression
  TIFF_JPEG			      = $8000;	// save using JPEG compression
  WBMP_DEFAULT        = 0;
  XBM_DEFAULT         = 0;
  XPM_DEFAULT         = 0;

// --------------------------------------------------------------------------
// Init/Error routines ------------------------------------------------------
// --------------------------------------------------------------------------

procedure FreeImage_Initialise(load_local_plugins_only : boolean = False); {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_Initialise'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};
procedure FreeImage_DeInitialise; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_DeInitialise'{$IFNDEF FINoPrefix}+'@0'{$ENDIF};

// --------------------------------------------------------------------------
// Version routines ---------------------------------------------------------
// --------------------------------------------------------------------------

function FreeImage_GetVersion : PChar; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_GetVersion'{$IFNDEF FINoPrefix}+'@0'{$ENDIF};
function FreeImage_GetCopyrightMessage : PChar; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_GetCopyrightMessage'{$IFNDEF FINoPrefix}+'@0'{$ENDIF};

// --------------------------------------------------------------------------
// Message output functions -------------------------------------------------
// --------------------------------------------------------------------------

procedure FreeImage_OutPutMessageProc(fif: Integer; fmt: PChar); {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name 'FreeImage_OutputMessageProc';
type FreeImage_OutputMessageFunction = function(fif: FREE_IMAGE_FORMAT; msg: PChar): pointer; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF};
procedure FreeImage_SetOutputMessage(omf: FreeImage_OutputMessageFunction); {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_SetOutputMessage'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};

// --------------------------------------------------------------------------
// Allocate/Unload routines -------------------------------------------------
// --------------------------------------------------------------------------

function FreeImage_Allocate(width, height, bpp: integer; red_mask: Cardinal = 0; green_mask: Cardinal = 0; blue_mask: Cardinal = 0): PFIBITMAP; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_Allocate'{$IFNDEF FINoPrefix}+'@24'{$ENDIF};
function FreeImage_AllocateT(Atype: FREE_IMAGE_TYPE; Width, Height: Integer; bpp: Integer = 8; red_mask: Cardinal = 0; green_mask: Cardinal = 0; blue_mask: Cardinal = 0): PFIBITMAP; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_AllocateT'{$IFNDEF FINoPrefix}+'@28'{$ENDIF};
function FreeImage_Clone(dib: PFIBITMAP): PFIBITMAP; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_Clone'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};
procedure FreeImage_Unload(dib: PFIBITMAP); {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_Unload'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};

// --------------------------------------------------------------------------
// Load / Save routines -----------------------------------------------------
// --------------------------------------------------------------------------

function FreeImage_Load(fif: FREE_IMAGE_FORMAT; const filename: PChar; flags: integer = 0): PFIBITMAP; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_Load'{$IFNDEF FINoPrefix}+'@12'{$ENDIF};
function FreeImage_LoadU(fif: FREE_IMAGE_FORMAT; const filename: PWideChar; flags: Integer = 0): PFIBITMAP; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_LoadU'{$IFNDEF FINoPrefix}+'@12'{$ENDIF};
function FreeImage_LoadFromHandle(fif: FREE_IMAGE_FORMAT; io: PFreeImageIO; handle: fi_handle; flags: integer = 0): PFIBITMAP; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_LoadFromHandle'{$IFNDEF FINoPrefix}+'@16'{$ENDIF};
function FreeImage_Save(fif: FREE_IMAGE_FORMAT; dib: PFIBITMAP; filename: PChar; flags: integer = 0): Boolean; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_Save'{$IFNDEF FINoPrefix}+'@16'{$ENDIF};
function FreeImage_SaveU(fif: FREE_IMAGE_FORMAT; dib: PFIBITMAP; const filename: PWideChar; flags: Integer = 0): Boolean; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_SaveU'{$IFNDEF FINoPrefix}+'@16'{$ENDIF};
function FreeImage_SaveToHandle(fif: FREE_IMAGE_FORMAT; dib: PFIBITMAP; io : PFreeImageIO; handle : fi_handle; flags : integer = 0) : Boolean; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_SaveToHandle'{$IFNDEF FINoPrefix}+'@20'{$ENDIF};

// --------------------------------------------------------------------------
// Memory I/O stream routines -----------------------------------------------
// --------------------------------------------------------------------------

function FreeImage_OpenMemory(data: PByte = nil; size_in_bytes: DWORD = 0): PFIMEMORY; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_OpenMemory'{$IFNDEF FINoPrefix}+'@8'{$ENDIF};
procedure FreeImage_CloseMemory(stream: PFIMEMORY); {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_CloseMemory'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};
function FreeImage_LoadFromMemory(fif: FREE_IMAGE_FORMAT; stream: PFIMEMORY; flags: Integer = 0): PFIBITMAP; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_LoadFromMemory'{$IFNDEF FINoPrefix}+'@12'{$ENDIF};
function FreeImage_SaveToMemory(fif: FREE_IMAGE_FORMAT; dib: PFIBITMAP; stream: PFIMEMORY; flags: Integer = 0): Boolean; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_SaveToMemory'{$IFNDEF FINoPrefix}+'@16'{$ENDIF};
function FreeImage_TellMemory(stream: PFIMEMORY): Longint; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_TellMemory'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};
function FreeImage_SeekMemory(stream: PFIMEMORY; offset: Longint; origin: Integer): Boolean; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_SeekMemory'{$IFNDEF FINoPrefix}+'@12'{$ENDIF};
function FreeImage_AcquireMemory(stream: PFIMEMORY; var data: PByte; var size_in_bytes: DWORD): Boolean; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_AcquireMemory'{$IFNDEF FINoPrefix}+'@12'{$ENDIF};

// --------------------------------------------------------------------------
// Plugin Interface ---------------------------------------------------------
// --------------------------------------------------------------------------

function FreeImage_RegisterLocalPlugin(proc_address: FI_InitProc; format, description, extension, regexpr: PChar): FREE_IMAGE_FORMAT; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_RegisterLocalPlugin'{$IFNDEF FINoPrefix}+'@20'{$ENDIF};
function FreeImage_RegisterExternalPlugin(path, format, description, extension, regexpr: PChar): FREE_IMAGE_FORMAT; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_RegisterExternalPlugin'{$IFNDEF FINoPrefix}+'@20'{$ENDIF};
function FreeImage_GetFIFCount: Integer; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_GetFIFCount'{$IFNDEF FINoPrefix}+'@0'{$ENDIF};
procedure FreeImage_SetPluginEnabled(fif: FREE_IMAGE_FORMAT; enable: Boolean); {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL Name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_SetPluginEnabled'{$IFNDEF FINoPrefix}+'@8'{$ENDIF};
function FreeImage_IsPluginEnabled(fif: FREE_IMAGE_FORMAT): Integer; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL Name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_IsPluginEnabled'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};
function FreeImage_GetFIFFromFormat(const format: PChar): FREE_IMAGE_FORMAT; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL Name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_GetFIFFromFormat'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};
function FreeImage_GetFIFFromMime(const format: PChar): FREE_IMAGE_FORMAT; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL Name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_GetFIFFromMime'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};
function FreeImage_GetFormatFromFIF(fif: FREE_IMAGE_FORMAT): PChar; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL Name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_GetFormatFromFIF'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};
function FreeImage_GetFIFExtensionList(fif: FREE_IMAGE_FORMAT): PChar; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL Name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_GetFIFExtensionList'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};
function FreeImage_GetFIFDescription(fif: FREE_IMAGE_FORMAT): PChar; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL Name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_GetFIFDescription'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};
function FreeImage_GetFIFRegExpr(fif: FREE_IMAGE_FORMAT): PChar; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL Name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_GetFIFRegExpr'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};
function FreeImage_GetFIFFromFilename(const fname: PChar): FREE_IMAGE_FORMAT; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL Name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_GetFIFFromFilename'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};
function FreeImage_GetFIFFromFilenameU(const fname:PWideChar): FREE_IMAGE_FORMAT; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL Name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_GetFIFFromFilenameU'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};
function FreeImage_FIFSupportsReading(fif: FREE_IMAGE_FORMAT): Boolean; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL Name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_FIFSupportsReading'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};
function FreeImage_FIFSupportsWriting(fif: FREE_IMAGE_FORMAT): Boolean; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL Name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_FIFSupportsWriting'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};
function FreeImage_FIFSupportsExportBPP(fif: FREE_IMAGE_FORMAT; bpp: Integer): Boolean; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL Name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_FIFSupportsExportBPP'{$IFNDEF FINoPrefix}+'@8'{$ENDIF};
function FreeImage_FIFSupportsICCProfiles(fif: FREE_IMAGE_FORMAT): Boolean; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL Name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_FIFSupportsICCProfiles'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};
function FreeImage_FIFSupportsExportType(fif: FREE_IMAGE_FORMAT; image_type: FREE_IMAGE_TYPE): Boolean; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_FIFSupportsExportType'{$IFNDEF FINoPrefix}+'@8'{$ENDIF};

// --------------------------------------------------------------------------
// Multipaging interface ----------------------------------------------------
// --------------------------------------------------------------------------

function FreeImage_OpenMultiBitmap(fif: FREE_IMAGE_FORMAT; filename: PChar; create_new, read_only, keep_cache_in_memory: Boolean; flags: integer = 0): PFIMULTIBITMAP; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL Name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_OpenMultiBitmap'{$IFNDEF FINoPrefix}+'@24'{$ENDIF};
function FreeImage_CloseMultiBitmap(bitmap: PFIMULTIBITMAP; flags: Integer = 0): Boolean; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL Name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_CloseMultiBitmap'{$IFNDEF FINoPrefix}+'@8'{$ENDIF};
function FreeImage_GetPageCount(bitmap: PFIMULTIBITMAP): Integer; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL Name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_GetPageCount'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};
procedure FreeImage_AppendPage(bitmap: PFIMULTIBITMAP; data: PFIBITMAP); {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL Name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_AppendPage'{$IFNDEF FINoPrefix}+'@8'{$ENDIF};
procedure FreeImage_InsertPage(bitmap: PFIMULTIBITMAP; page: Integer; data: PFIBITMAP); {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL Name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_InsertPage'{$IFNDEF FINoPrefix}+'@12'{$ENDIF};
procedure FreeImage_DeletePage(bitmap: PFIMULTIBITMAP; page: Integer); {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL Name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_DeletePage'{$IFNDEF FINoPrefix}+'@8'{$ENDIF};
function FreeImage_LockPage(bitmap: PFIMULTIBITMAP; page: Integer): PFIBITMAP; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL Name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_LockPage'{$IFNDEF FINoPrefix}+'@8'{$ENDIF};
procedure FreeImage_UnlockPage(bitmap: PFIMULTIBITMAP; page: PFIBITMAP; changed: boolean); {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL Name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_UnlockPage'{$IFNDEF FINoPrefix}+'@12'{$ENDIF};
function FreeImage_MovePage(bitmap: PFIMULTIBITMAP; target, source: Integer): Boolean; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL Name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_MovePage'{$IFNDEF FINoPrefix}+'@12'{$ENDIF};
function FreeImage_GetLockedPageNumbers(bitmap: PFIMULTIBITMAP; var pages: Integer; var count : integer): Boolean; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL Name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_GetLockedPageNumbers'{$IFNDEF FINoPrefix}+'@12'{$ENDIF};

// --------------------------------------------------------------------------
// Filetype request routines ------------------------------------------------
// --------------------------------------------------------------------------

function FreeImage_GetFileType(const filename: PChar; size: Integer): FREE_IMAGE_FORMAT; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_GetFileType'{$IFNDEF FINoPrefix}+'@8'{$ENDIF};
function FreeImage_GetFileTypeU(const filename: PWideChar; size: Integer): FREE_IMAGE_FORMAT; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_GetFileTypeU'{$IFNDEF FINoPrefix}+'@8'{$ENDIF};
function FreeImage_GetFileTypeFromHandle(io: PFreeImageIO; handle: FI_Handle; size: Integer = 0): FREE_IMAGE_FORMAT; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_GetFileTypeFromHandle'{$IFNDEF FINoPrefix}+'@12'{$ENDIF};
function FreeImage_GetFileTypeFromMemory(stream: PFIMEMORY; size: Integer = 0): FREE_IMAGE_FORMAT; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_GetFileTypeFromMemory'{$IFNDEF FINoPrefix}+'@8'{$ENDIF};

// --------------------------------------------------------------------------
// ImageType request routine ------------------------------------------------
// --------------------------------------------------------------------------

function FreeImage_GetImageType(dib: PFIBITMAP): FREE_IMAGE_TYPE; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_GetImageType'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};

// --------------------------------------------------------------------------
// FreeImage helper routines ------------------------------------------------
// --------------------------------------------------------------------------

function FreeImage_IsLittleEndian: Boolean; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_IsLittleEndian'{$IFNDEF FINoPrefix}+'@0'{$ENDIF};
function FreeImage_LookupX11Color(const szColor: PChar; var nRed, nGreen, nBlue: PByte): Boolean; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_LookupX11Color'{$IFNDEF FINoPrefix}+'@16'{$ENDIF};
function FreeImage_LookupSVGColor(const szColor: PChar; var nRed, nGreen, nBlue: PByte): Boolean; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_LookupSVGColor'{$IFNDEF FINoPrefix}+'@16'{$ENDIF};

// --------------------------------------------------------------------------
// Pixels access routines ---------------------------------------------------
// --------------------------------------------------------------------------

function FreeImage_GetBits(dib: PFIBITMAP): PByte; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_GetBits'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};
function FreeImage_GetScanLine(dib: PFIBITMAP; scanline: Integer): PByte; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_GetScanLine'{$IFNDEF FINoPrefix}+'@8'{$ENDIF};

function FreeImage_GetPixelIndex(dib: PFIBITMAP; X, Y: Longint; Value: PByte): Boolean; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_GetPixelIndex'{$IFNDEF FINoPrefix}+'@16'{$ENDIF};
function FreeImage_GetPixelColor(dib: PFIBITMAP; X, Y: Longint; Value: PRGBQuad): Boolean; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_GetPixelColor'{$IFNDEF FINoPrefix}+'@16'{$ENDIF};
function FreeImage_SetPixelIndex(dib: PFIBITMAP; X, Y: Longint; Value: PByte): Boolean; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_SetPixelIndex'{$IFNDEF FINoPrefix}+'@16'{$ENDIF};
function FreeImage_SetPixelColor(dib: PFIBITMAP; X, Y: Longint; Value: PRGBQuad): Boolean; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_SetPixelColor'{$IFNDEF FINoPrefix}+'@16'{$ENDIF};

// --------------------------------------------------------------------------
// DIB info routines --------------------------------------------------------
// --------------------------------------------------------------------------

function FreeImage_GetColorsUsed(dib: PFIBITMAP): Cardinal; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_GetColorsUsed'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};
function FreeImage_GetBPP(dib: PFIBITMAP): Cardinal; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_GetBPP'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};
function FreeImage_GetWidth(dib: PFIBITMAP): Cardinal; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_GetWidth'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};
function FreeImage_GetHeight(dib: PFIBITMAP): Cardinal; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_GetHeight'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};
function FreeImage_GetLine(dib: PFIBITMAP): Cardinal; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_GetLine'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};
function FreeImage_GetPitch(dib : PFIBITMAP) : Cardinal; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_GetPitch'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};
function FreeImage_GetDIBSize(dib: PFIBITMAP): Cardinal; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_GetDIBSize'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};
function FreeImage_GetPalette(dib: PFIBITMAP): PRGBQUAD; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_GetPalette'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};

function FreeImage_GetDotsPerMeterX(dib: PFIBITMAP): Cardinal; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_GetDotsPerMeterX'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};
function FreeImage_GetDotsPerMeterY(dib: PFIBITMAP): Cardinal; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_GetDotsPerMeterY'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};
procedure FreeImage_SetDotsPerMeterX(dib: PFIBITMAP; res: Cardinal); {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_SetDotsPerMeterX'{$IFNDEF FINoPrefix}+'@8'{$ENDIF};
procedure FreeImage_SetDotsPerMeterY(dib: PFIBITMAP; res: Cardinal); {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_SetDotsPerMeterY'{$IFNDEF FINoPrefix}+'@8'{$ENDIF};

//function FreeImage_GetInfoHeader(dib: PFIBITMAP): PBITMAPINFOHEADER; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_GetInfoHeader'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};
//function FreeImage_GetInfo(var dib: FIBITMAP): PBITMAPINFO; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_GetInfo'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};
function FreeImage_GetColorType(dib: PFIBITMAP): FREE_IMAGE_COLOR_TYPE; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_GetColorType'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};

function FreeImage_GetRedMask(dib: PFIBITMAP): Cardinal; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_GetRedMask'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};
function FreeImage_GetGreenMask(dib: PFIBITMAP): Cardinal; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_GetGreenMask'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};
function FreeImage_GetBlueMask(dib: PFIBITMAP): Cardinal; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_GetBlueMask'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};

function FreeImage_GetTransparencyCount(dib: PFIBITMAP): Cardinal; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_GetTransparencyCount'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};
function FreeImage_GetTransparencyTable(dib: PFIBITMAP): PByte; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_GetTransparencyTable'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};
procedure FreeImage_SetTransparent(dib: PFIBITMAP; enabled: boolean); {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_SetTransparent'{$IFNDEF FINoPrefix}+'@8'{$ENDIF};
procedure FreeImage_SetTransparencyTable(dib: PFIBITMAP; table: PByte; count: integer); {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_SetTransparencyTable'{$IFNDEF FINoPrefix}+'@12'{$ENDIF};
function FreeImage_IsTransparent(dib: PFIBITMAP): boolean; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_IsTransparent'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};

function FreeImage_HasBackgroundColor(dib: PFIBITMAP): Boolean; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_HasBackgroundColor'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};
function FreeImage_GetBackgroundColor(dib: PFIBITMAP; var bkcolor: PRGBQUAD): Boolean; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_GetBackgroundColor'{$IFNDEF FINoPrefix}+'@8'{$ENDIF};
function FreeImage_SetBackgroundColor(dib: PFIBITMAP; bkcolor: PRGBQUAD): Boolean; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_SetBackgroundColor'{$IFNDEF FINoPrefix}+'@8'{$ENDIF};

// --------------------------------------------------------------------------
// ICC profile routines -----------------------------------------------------
// --------------------------------------------------------------------------

function FreeImage_GetICCProfile(var dib: FIBITMAP): PFIICCPROFILE; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_GetICCProfile'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};
function FreeImage_CreateICCProfile(var dib: FIBITMAP; data: Pointer; size: Longint): PFIICCPROFILE; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name 'FreeImage_CreateICCProfile'{$IFNDEF FINoPrefix}+'@12'{$ENDIF};
procedure FreeImage_DestroyICCProfile(var dib : FIBITMAP); {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name 'FreeImage_DestroyICCProfile'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};

// --------------------------------------------------------------------------
// Line conversion routines -------------------------------------------------
// --------------------------------------------------------------------------

procedure FreeImage_ConvertLine1To4(target, source: PBYTE; width_in_pixels: Integer); {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_ConvertLine1To4'{$IFNDEF FINoPrefix}+'@12'{$ENDIF};
procedure FreeImage_ConvertLine8To4(target, source: PBYTE; width_in_pixels: Integer; palette: PRGBQuad);  {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_ConvertLine8To4'{$IFNDEF FINoPrefix}+'@16'{$ENDIF};
procedure FreeImage_ConvertLine16To4_555(target, source: PBYTE; width_in_pixels: Integer); {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_ConvertLine16To4_555'{$IFNDEF FINoPrefix}+'@12'{$ENDIF};
procedure FreeImage_ConvertLine16To4_565(target, source: PBYTE; width_in_pixels: Integer); {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_ConvertLine16To4_565'{$IFNDEF FINoPrefix}+'@12'{$ENDIF};
procedure FreeImage_ConvertLine24To4(target, source: PBYTE; width_in_pixels: Integer); {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_ConvertLine24To4'{$IFNDEF FINoPrefix}+'@12'{$ENDIF};
procedure FreeImage_ConvertLine32To4(target, source: PBYTE; width_in_pixels: Integer); {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_ConvertLine32To4'{$IFNDEF FINoPrefix}+'@12'{$ENDIF};

procedure FreeImage_ConvertLine1To8(target, source: PBYTE; width_in_pixels: Integer); {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_ConvertLine1To8'{$IFNDEF FINoPrefix}+'@12'{$ENDIF};
procedure FreeImage_ConvertLine4To8(target, source: PBYTE; width_in_pixels: Integer); {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_ConvertLine4To8'{$IFNDEF FINoPrefix}+'@12'{$ENDIF};
procedure FreeImage_ConvertLine16To8_555(target, source: PBYTE; width_in_pixels: Integer); {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_ConvertLine16To8_555'{$IFNDEF FINoPrefix}+'@12'{$ENDIF};
procedure FreeImage_ConvertLine16To8_565(target, source: PBYTE; width_in_pixels: Integer); {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_ConvertLine16To8_565'{$IFNDEF FINoPrefix}+'@12'{$ENDIF};
procedure FreeImage_ConvertLine24To8(target, source: PBYTE; width_in_pixels: Integer); {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_ConvertLine24To8'{$IFNDEF FINoPrefix}+'@12'{$ENDIF};
procedure FreeImage_ConvertLine32To8(target, source: PBYTE; width_in_pixels: Integer); {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_ConvertLine32To8'{$IFNDEF FINoPrefix}+'@12'{$ENDIF};

procedure FreeImage_ConvertLine1To16_555(target, source: PBYTE; width_in_pixels: Integer; palette: PRGBQUAD); {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_ConvertLine1To16_555'{$IFNDEF FINoPrefix}+'@16'{$ENDIF};
procedure FreeImage_ConvertLine4To16_555(target, source: PBYTE; width_in_pixels: Integer; palette: PRGBQUAD); {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_ConvertLine4To16_555'{$IFNDEF FINoPrefix}+'@16'{$ENDIF};
procedure FreeImage_ConvertLine8To16_555(target, source: PBYTE; width_in_pixels: Integer; palette: PRGBQUAD); {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_ConvertLine8To16_555'{$IFNDEF FINoPrefix}+'@16'{$ENDIF};
procedure FreeImage_ConvertLine16_565_To16_555(target, source: PBYTE; width_in_pixels: Integer); {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_ConvertLine16_565_To16_555'{$IFNDEF FINoPrefix}+'@12'{$ENDIF};
procedure FreeImage_ConvertLine24To16_555(target, source: PBYTE; width_in_pixels: Integer); {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_ConvertLine24To16_555'{$IFNDEF FINoPrefix}+'@12'{$ENDIF};
procedure FreeImage_ConvertLine32To16_555(target, source: PBYTE; width_in_pixels: Integer); {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_ConvertLine32To16_555'{$IFNDEF FINoPrefix}+'@12'{$ENDIF};

procedure FreeImage_ConvertLine1To16_565(target, source : PBYTE; width_in_pixels: Integer; palette: PRGBQUAD); {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_ConvertLine1To16_565'{$IFNDEF FINoPrefix}+'@16'{$ENDIF};
procedure FreeImage_ConvertLine4To16_565(target, source : PBYTE; width_in_pixels : Integer; palette : PRGBQUAD); {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_ConvertLine4To16_565'{$IFNDEF FINoPrefix}+'@16'{$ENDIF};
procedure FreeImage_ConvertLine8To16_565(target, source: PBYTE; width_in_pixels: Integer; palette: PRGBQUAD); {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_ConvertLine8To16_565'{$IFNDEF FINoPrefix}+'@16'{$ENDIF};
procedure FreeImage_ConvertLine16_555_To16_565(target, source: PBYTE; width_in_pixels: Integer); {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_ConvertLine16_555_To16_565'{$IFNDEF FINoPrefix}+'@12'{$ENDIF};
procedure FreeImage_ConvertLine24To16_565(target, source: PBYTE; width_in_pixels: Integer); {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_ConvertLine24To16_565'{$IFNDEF FINoPrefix}+'@12'{$ENDIF};
procedure FreeImage_ConvertLine32To16_565(target, source : PBYTE; width_in_pixels : Integer); {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_ConvertLine32To16_565'{$IFNDEF FINoPrefix}+'@12'{$ENDIF};

procedure FreeImage_ConvertLine1To24(target, source: PBYTE; width_in_pixels: Integer; palette: PRGBQUAD); {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_ConvertLine1To24'{$IFNDEF FINoPrefix}+'@16'{$ENDIF};
procedure FreeImage_ConvertLine4To24(target, source : PBYTE; width_in_pixels: Integer; palette: PRGBQUAD); {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_ConvertLine4To24'{$IFNDEF FINoPrefix}+'@16'{$ENDIF};
procedure FreeImage_ConvertLine8To24(target, source: PBYTE; width_in_pixels: Integer; palette: PRGBQUAD); {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_ConvertLine8To24'{$IFNDEF FINoPrefix}+'@16'{$ENDIF};
procedure FreeImage_ConvertLine16To24_555(target, source: PBYTE; width_in_pixels: Integer); {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_ConvertLine16To24_555'{$IFNDEF FINoPrefix}+'@12'{$ENDIF};
procedure FreeImage_ConvertLine16To24_565(target, source: PBYTE; width_in_pixels: Integer); {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_ConvertLine16To24_565'{$IFNDEF FINoPrefix}+'@12'{$ENDIF};
procedure FreeImage_ConvertLine32To24(target, source: PBYTE; width_in_pixels: Integer); {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_ConvertLine32To24'{$IFNDEF FINoPrefix}+'@12'{$ENDIF};

procedure FreeImage_ConvertLine1To32(target, source: PBYTE; width_in_pixels: Integer; palette: PRGBQUAD); {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_ConvertLine1To32'{$IFNDEF FINoPrefix}+'@16'{$ENDIF};
procedure FreeImage_ConvertLine4To32(target, source: PBYTE; width_in_pixels: Integer; palette: PRGBQUAD); {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_ConvertLine4To32'{$IFNDEF FINoPrefix}+'@16'{$ENDIF};
procedure FreeImage_ConvertLine8To32(target, source: PBYTE; width_in_pixels: Integer; palette: PRGBQUAD); {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_ConvertLine8To32'{$IFNDEF FINoPrefix}+'@16'{$ENDIF};
procedure FreeImage_ConvertLine16To32_555(target, source: PBYTE; width_in_pixels: Integer); {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_ConvertLine16To32_555'{$IFNDEF FINoPrefix}+'@12'{$ENDIF};
procedure FreeImage_ConvertLine16To32_565(target, source: PBYTE; width_in_pixels: Integer); {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_ConvertLine16To32_565'{$IFNDEF FINoPrefix}+'@12'{$ENDIF};
procedure FreeImage_ConvertLine24To32(target, source: PBYTE; width_in_pixels: Integer); {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_ConvertLine24To32'{$IFNDEF FINoPrefix}+'@12'{$ENDIF};

// --------------------------------------------------------------------------
// Smart conversion routines ------------------------------------------------
// --------------------------------------------------------------------------

function FreeImage_ConvertTo4Bits(dib: PFIBITMAP): PFIBITMAP; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_ConvertTo4Bits'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};
function FreeImage_ConvertTo8Bits(dib: PFIBITMAP): PFIBITMAP; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_ConvertTo8Bits'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};
function FreeImage_ConvertToGreyscale(dib: PFIBITMAP): PFIBITMAP; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_ConvertToGreyscale'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};
function FreeImage_ConvertTo16Bits555(dib: PFIBITMAP): PFIBITMAP; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_ConvertTo16Bits555'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};
function FreeImage_ConvertTo16Bits565(dib: PFIBITMAP): PFIBITMAP; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_ConvertTo16Bits565'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};
function FreeImage_ConvertTo24Bits(dib: PFIBITMAP): PFIBITMAP; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_ConvertTo24Bits'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};
function FreeImage_ConvertTo32Bits(dib: PFIBITMAP): PFIBITMAP; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_ConvertTo32Bits'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};
function FreeImage_ColorQuantize(dib: PFIBITMAP; quantize: FREE_IMAGE_QUANTIZE): PFIBITMAP; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_ColorQuantize'{$IFNDEF FINoPrefix}+'@8'{$ENDIF};
function FreeImage_ColorQuantizeEx(dib: PFIBITMAP; quantize: FREE_IMAGE_QUANTIZE = FIQ_WUQUANT; PaletteSize: Integer = 256; ReserveSize: Integer = 0; ReservePalette: PRGBQuad = nil): PFIBITMAP; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_ColorQuantizeEx'{$IFNDEF FINoPrefix}+'@20'{$ENDIF};
function FreeImage_Threshold(dib: PFIBITMAP; T: Byte): PFIBITMAP; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_Threshold'{$IFNDEF FINoPrefix}+'@8'{$ENDIF};
function FreeImage_Dither(dib: PFIBITMAP; algorithm: FREE_IMAGE_DITHER): PFIBITMAP; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_Dither'{$IFNDEF FINoPrefix}+'@8'{$ENDIF};

function FreeImage_ConvertFromRawBits(bits: PBYTE; width, height, pitch: Integer; bpp, red_mask, green_mask, blue_mask: LongWord; topdown: Boolean): PFIBITMAP; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_ConvertFromRawBits'{$IFNDEF FINoPrefix}+'@36'{$ENDIF};
procedure FreeImage_ConvertToRawBits(bits: PBYTE; dib: PFIBITMAP; pitch: Integer; bpp, red_mask, green_mask, blue_mask: LongWord; topdown: Boolean); {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_ConvertToRawBits'{$IFNDEF FINoPrefix}+'@32'{$ENDIF};

function FreeImage_ConvertToRGBF(dib: PFIBITMAP): PFIBITMAP; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_ConvertToRGBF'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};

function FreeImage_ConvertToStandardType(src: PFIBITMAP; scale_linear: Boolean = True): PFIBITMAP; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_ConvertToStandardType'{$IFNDEF FINoPrefix}+'@8'{$ENDIF};
function FreeImage_ConvertToType(src: PFIBITMAP; dst_type: FREE_IMAGE_TYPE; scale_linear: Boolean = True): PFIBITMAP; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_ConvertToType'{$IFNDEF FINoPrefix}+'@12'{$ENDIF};

// tone mapping operators
function FreeImage_ToneMapping(dib: PFIBITMAP; tmo: FREE_IMAGE_TMO; first_param: Double = 0; second_param: Double = 0): PFIBITMAP; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_ToneMapping'{$IFNDEF FINoPrefix}+'@24'{$ENDIF};
function FreeImage_TmoDrago03(src: PFIBITMAP; gamma: Double = 2.2; exposure: Double = 0): PFIBITMAP; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_TmoDrago03'{$IFNDEF FINoPrefix}+'@20'{$ENDIF};
function FreeImage_TmoReinhard05(src: PFIBITMAP; intensity: Double = 0; contrast: Double = 0): PFIBITMAP; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_TmoReinhard05'{$IFNDEF FINoPrefix}+'@20'{$ENDIF};

// --------------------------------------------------------------------------
// ZLib interface -----------------------------------------------------------
// --------------------------------------------------------------------------

function FreeImage_ZLibCompress(target: PBYTE; target_size: DWORD; source: PBYTE; source_size: DWORD): DWORD; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_ZLibCompress'{$IFNDEF FINoPrefix}+'@16'{$ENDIF};
function FreeImage_ZLibUncompress(target: PBYTE; target_size: DWORD; source: PBYTE; source_size: DWORD): DWORD; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_ZLibUncompress'{$IFNDEF FINoPrefix}+'@16'{$ENDIF};

function FreeImage_ZLibGZip(target: PBYTE; target_size: DWORD; source: PBYTE; source_size: DWORD): DWORD; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_ZLibGZip'{$IFNDEF FINoPrefix}+'@16'{$ENDIF};
function FreeImage_ZLibGUnzip(target: PBYTE; target_size: DWORD; source: PBYTE; source_size: DWORD): DWORD; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_ZLibGUnzip'{$IFNDEF FINoPrefix}+'@16'{$ENDIF};
function FreeImage_ZLibCRC32(crc: DWORD; source: PByte; source_size: DWORD): DWORD; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_ZLibCRC32'{$IFNDEF FINoPrefix}+'@12'{$ENDIF};

// --------------------------------------------------------------------------
// Metadata routines --------------------------------------------------------
// --------------------------------------------------------------------------

// tag creation / destruction
function FreeImage_CreateTag: PFITAG; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_CreateTag'{$IFNDEF FINoPrefix}+'@0'{$ENDIF};
procedure FreeImage_DeleteTag(tag: PFITAG); {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_DeleteTag'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};
function FreeImage_CloneTag(tag: PFITAG): PFITAG; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_CloneTag'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};

// tag getters and setters
function FreeImage_GetTagKey(tag: PFITAG): PChar; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_GetTagKey'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};
function FreeImage_GetTagDescription(tag: PFITAG): PChar; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_GetTagDescription'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};
function FreeImage_GetTagID(tag: PFITAG): Word; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_GetTagID'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};
function FreeImage_GetTagType(tag: PFITAG): FREE_IMAGE_MDTYPE; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_GetTagType'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};
function FreeImage_GetTagCount(tag: PFITAG): DWORD; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_GetTagCount'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};
function FreeImage_GetTagLength(tag: PFITAG): DWORD; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_GetTagLength'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};
function FreeImage_GetTagValue(tag: PFITAG): Pointer; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_GetTagValue'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};

function FreeImage_SetTagKey(tag: PFITAG; const key: PChar): Boolean; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_SetTagKey'{$IFNDEF FINoPrefix}+'@8'{$ENDIF};
function FreeImage_SetTagDescription(tag: PFITAG; const description: PChar): Boolean; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_SetTagDescription'{$IFNDEF FINoPrefix}+'@8'{$ENDIF};
function FreeImage_SetTagID(tag: PFITAG; id: Word): Boolean; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_SetTagID'{$IFNDEF FINoPrefix}+'@8'{$ENDIF};
function FreeImage_SetTagType(tag: PFITAG; atype: FREE_IMAGE_MDTYPE): Boolean; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_SetTagType'{$IFNDEF FINoPrefix}+'@8'{$ENDIF};
function FreeImage_SetTagCount(tag: PFITAG; count: DWORD): Boolean; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_SetTagCount'{$IFNDEF FINoPrefix}+'@8'{$ENDIF};
function FreeImage_SetTagLength(tag: PFITAG; length: DWORD): Boolean; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_SetTagLength'{$IFNDEF FINoPrefix}+'@8'{$ENDIF};
function FreeImage_SetTagValue(tag: PFITAG; const value: Pointer): Boolean; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_SetTagValue'{$IFNDEF FINoPrefix}+'@8'{$ENDIF};

// iterator
function FreeImage_FindFirstMetadata(model: FREE_IMAGE_MDMODEL; dib: PFIBITMAP; var tag: PFITAG): PFIMETADATA; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_FindFirstMetadata'{$IFNDEF FINoPrefix}+'@12'{$ENDIF};
function FreeImage_FindNextMetadata(mdhandle: PFIMETADATA; var tag: PFITAG): Boolean; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_FindNextMetadata'{$IFNDEF FINoPrefix}+'@8'{$ENDIF};
procedure FreeImage_FindCloseMetadata(mdhandle: PFIMETADATA); {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_FindCloseMetadata'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};

// metadata setter and getter
function FreeImage_SetMetadata(model: FREE_IMAGE_MDMODEL; dib: PFIBITMAP; const key: PChar; tag: PFITAG): Boolean; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_SetMetadata'{$IFNDEF FINoPrefix}+'@16'{$ENDIF};
function FreeImage_GetMetaData(model: FREE_IMAGE_MDMODEL; dib: PFIBITMAP; const key: PChar; var tag: PFITAG): Boolean; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_GetMetadata'{$IFNDEF FINoPrefix}+'@16'{$ENDIF};

// helpers
function FreeImage_GetMetadataCount(model: FREE_IMAGE_MDMODEL; dib: PFIBITMAP): Cardinal; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_GetMetadataCount'{$IFNDEF FINoPrefix}+'@8'{$ENDIF};

// tag to C string conversion
function FreeImage_TagToString(model: FREE_IMAGE_MDMODEL; tag: PFITAG; Make: PChar = nil): PChar; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_TagToString'{$IFNDEF FINoPrefix}+'@12'{$ENDIF};

// --------------------------------------------------------------------------
// Image manipulation toolkit -----------------------------------------------
// --------------------------------------------------------------------------

// rotation and flipping
function FreeImage_RotateClassic(dib: PFIBITMAP; angle: Double): PFIBITMAP; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_RotateClassic'{$IFNDEF FINoPrefix}+'@12'{$ENDIF};
function FreeImage_RotateEx(dib: PFIBITMAP; angle, x_shift, y_shift, x_origin, y_origin: Double; use_mask: Boolean): PFIBITMAP; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_RotateEx@48';
function FreeImage_FlipHorizontal(dib: PFIBITMAP): Boolean; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_FlipHorizontal'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};
function FreeImage_FlipVertical(dib: PFIBITMAP): Boolean; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_FlipVertical'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};
function FreeImage_JPEGTransform(const src_file: PChar; const dst_file: PChar; operation: FREE_IMAGE_JPEG_OPERATION; perfect: Boolean = False): Boolean; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_JPEGTransform'{$IFNDEF FINoPrefix}+'@16'{$ENDIF};

// upsampling / downsampling
function FreeImage_Rescale(dib: PFIBITMAP; dst_width, dst_height: Integer; filter: FREE_IMAGE_FILTER): PFIBITMAP; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_Rescale'{$IFNDEF FINoPrefix}+'@16'{$ENDIF};
function FreeImage_MakeThumbnail(dib: PFIBITMAP; max_pixel_size: Integer; convert:boolean = TRUE): PFIBITMAP; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_MakeThumbnail'{$IFNDEF FINoPrefix}+'@12'{$ENDIF};

// color manipulation routines (point operations)
function FreeImage_AdjustCurve(dib: PFIBITMAP; LUT: PBYTE; channel: FREE_IMAGE_COLOR_CHANNEL): Boolean; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_AdjustCurve'{$IFNDEF FINoPrefix}+'@12'{$ENDIF};
function FreeImage_AdjustGamma(dib: PFIBITMAP; gamma: Double): Boolean; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_AdjustGamma'{$IFNDEF FINoPrefix}+'@12'{$ENDIF};
function FreeImage_AdjustBrightness(dib: PFIBITMAP; percentage: Double): Boolean; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_AdjustBrightness'{$IFNDEF FINoPrefix}+'@12'{$ENDIF};
function FreeImage_AdjustContrast(dib: PFIBITMAP; percentage: Double): Boolean; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_AdjustContrast'{$IFNDEF FINoPrefix}+'@12'{$ENDIF};
function FreeImage_Invert(dib: PFIBITMAP): Boolean; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_Invert'{$IFNDEF FINoPrefix}+'@4'{$ENDIF};
function FreeImage_GetHistogram(dib: PFIBITMAP; histo: PDWORD; channel: FREE_IMAGE_COLOR_CHANNEL = FICC_BLACK): Boolean; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_GetHistogram'{$IFNDEF FINoPrefix}+'@12'{$ENDIF};

// channel processing routines
function FreeImage_GetChannel(dib: PFIBITMAP; channel: FREE_IMAGE_COLOR_CHANNEL): PFIBITMAP; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_GetChannel'{$IFNDEF FINoPrefix}+'@8'{$ENDIF};
function FreeImage_SetChannel(dib, dib8: PFIBITMAP; channel: FREE_IMAGE_COLOR_CHANNEL): Boolean; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_SetChannel'{$IFNDEF FINoPrefix}+'@12'{$ENDIF};
function FreeImage_GetComplexChannel(src: PFIBITMAP; channel: FREE_IMAGE_COLOR_CHANNEL): PFIBITMAP; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_GetComplexChannel'{$IFNDEF FINoPrefix}+'@8'{$ENDIF};
function FreeImage_SetComplexChannel(src: PFIBITMAP; channel: FREE_IMAGE_COLOR_CHANNEL): Boolean; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_SetComplexChannel'{$IFNDEF FINoPrefix}+'@12'{$ENDIF};

// copy / paste / composite routines

function FreeImage_Copy(dib: PFIBITMAP; left, top, right, bottom: Integer): PFIBITMAP; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_Copy'{$IFNDEF FINoPrefix}+'@20'{$ENDIF};
function FreeImage_Paste(dst, src: PFIBITMAP; left, top, alpha: Integer): Boolean; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_Paste'{$IFNDEF FINoPrefix}+'@20'{$ENDIF};
function FreeImage_Composite(fg: PFIBITMAP; useFileBkg: Boolean = False; appBkColor: PRGBQUAD = nil; bg: PFIBITMAP = nil): PFIBITMAP; {$IFDEF FIcdecl}cdecl{$ELSE}stdcall{$ENDIF}; external FIDLL name {$IFNDEF FINoPrefix}'_'+{$ENDIF}'FreeImage_Composite'{$IFNDEF FINoPrefix}+'@16'{$ENDIF};
  
{$MINENUMSIZE 1}
implementation

end.
