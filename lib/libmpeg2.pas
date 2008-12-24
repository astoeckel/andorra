{*
 * mpeg2.h
 * Copyright (C) 2000-2003 Michel Lespinasse <walken@zoy.org>
 * Copyright (C) 1999-2000 Aaron Holtzman <aholtzma@ess.engr.uvic.ca>
 *
 * This file is part of mpeg2dec, a free MPEG-2 video stream decoder.
 * See http://libmpeg2.sourceforge.net/ for updates.
 *
 * mpeg2dec is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * mpeg2dec is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *}
 
{Header conversion done by Andreas Stöckel in February 2008}

unit libmpeg2;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

{$DEFINE Mpeg2Convert} //Remove if you don't need the mpeg2convert library

uses
  Types;
  
const
  SEQ_FLAG_MPEG2 = 1;
  SEQ_FLAG_CONSTRAINED_PARAMETERS = 2;
  SEQ_FLAG_PROGRESSIVE_SEQUENCE = 4;
  SEQ_FLAG_LOW_DELAY = 8;
  SEQ_FLAG_COLOUR_DESCRIPTION = 16;

  SEQ_MASK_VIDEO_FORMAT = $E0;
  SEQ_VIDEO_FORMAT_COMPONENT = 0;
  SEQ_VIDEO_FORMAT_PAL = $20;
  SEQ_VIDEO_FORMAT_NTSC = $40;
  SEQ_VIDEO_FORMAT_SECAM = $60;
  SEQ_VIDEO_FORMAT_MAC = $80;
  SEQ_VIDEO_FORMAT_UNSPECIFIED = $A0;
  
  GOP_FLAG_DROP_FRAME = 1;
  GOP_FLAG_BROKEN_LINK = 2;
  GOP_FLAG_CLOSED_GOP = 4;
  
  PIC_MASK_CODING_TYPE = 7;
  PIC_FLAG_CODING_TYPE_I = 1;
  PIC_FLAG_CODING_TYPE_P = 2;
  PIC_FLAG_CODING_TYPE_B = 3;
  PIC_FLAG_CODING_TYPE_D = 4;

  PIC_FLAG_TOP_FIELD_FIRST = 8;
  PIC_FLAG_PROGRESSIVE_FRAME = 16;
  PIC_FLAG_COMPOSITE_DISPLAY = 32;
  PIC_FLAG_SKIP = 64;
  PIC_FLAG_TAGS = 128;
  PIC_MASK_COMPOSITE_DISPLAY = $FFFFF000;
  
  MPEG2_ACCEL_X86_MMX = 1;
  MPEG2_ACCEL_X86_3DNOW = 2;
  MPEG2_ACCEL_X86_MMXEXT = 4;
  MPEG2_ACCEL_PPC_ALTIVEC = 1;
  MPEG2_ACCEL_ALPHA = 1;
  MPEG2_ACCEL_ALPHA_MVI = 2;
  MPEG2_ACCEL_SPARC_VIS = 1;
  MPEG2_ACCEL_SPARC_VIS2 = 2;
  MPEG2_ACCEL_DETECT = $80000000;
  
const
  {$IFDEF WIN32}
    LibMpeg2Name = 'libmpeg2.dll';  
    LibMpeg2ConvertName = 'libmpeg2convert.dll';
  {$ELSE}
    LibMpeg2Name = 'libmpeg2.so';
    LibMpeg2ConvertName = 'libmpeg2convert.so';
  {$ENDIF}

type
  TMPEG2_State = (
    STATE_BUFFER, 
    STATE_SEQUENCE, 
    STATE_SEQUENCE_REPEATED, 
    STATE_GOP, 
    STATE_PICTURE, 
    STATE_SLICE_1ST, 
    STATE_PICTURE_2ND, 
    STATE_SLICE, 
    STATE_END,
    STATE_INVALID, 
    STATE_INVALID_END);
    
  TMPEG2_Convert_Stage = (
    MPEG2_CONVERT_SET, 
    MPEG2_CONVERT_STRIDE, 
    MPEG2_CONVERT_START);    
        
  TMPEG2_Alloc = (
    MPEG2_ALLOC_MPEG2DEC, 
    MPEG2_ALLOC_CHUNK, 
    MPEG2_ALLOC_YUV, 
    MPEG2_ALLOC_CONVERT_ID, 
    MPEG2_ALLOC_CONVERTED);
    
  PMPEG2dec = Pointer;
  PMPEG2_decoder = Pointer;
    
  TMPEG2_Sequence = packed record
    width, height: Cardinal;
    chroma_width, chroma_height: Cardinal;
    byte_rate: Cardinal;
    vbv_buffer_size: Cardinal;
    
    flags: Cardinal;
    
    picture_width, picture_height: Cardinal;
    display_width, display_height: Cardinal;
    pixel_width, pixel_height: Cardinal;
    frame_period: Cardinal;
    
    profile_level_id: byte;
    colour_primaries: byte;
    transfer_characteristics: byte;
    matrix_coefficients: byte;
  end;
  PMPEG2_Sequence = ^TMPEG2_Sequence;
  
  TMPEG2_GOP = packed record
    hours: byte;
    minutes: byte;
    seconds: byte;
    pictures: byte;
    flags: Cardinal;
  end;
  PMPEG2_GOP = ^TMPEG2_GOP;
  
  TMPEG2_Picture = packed record
    temporal_reference: Cardinal;    
    nb_fields: Cardinal;
    
    tag, tag2: Cardinal;
    flags: Cardinal;
    display_offset: array[0..2] of TPoint;
  end;
  PMPEG2_Picture = ^TMPEG2_Picture;
  
  
  TMPEG2_Buf = packed record
    buf: array[0..2] of PByte;
    id: Pointer;
  end;
  PMPEG2_Buf = ^TMPEG2_Buf;
  
  TMPEG2_Info = packed record
    sequence: PMPEG2_Sequence;
    gop: PMPEG2_GOP;
    
    current_picture: PMPEG2_Picture;    
    current_picture_2nd: PMPEG2_Picture;
    current_fbuf: PMPEG2_Buf;
    
    display_picture: PMPEG2_Picture;
    display_picture_2nd: PMPEG2_Picture;
    
    display_fbuf: PMPEG2_Buf;
    discard_fbuf: PMPEG2_Buf;
    
    user_data: PByte;
    user_data_len: Cardinal;
  end;
  PMPEG2_Info = ^TMPEG2_Info;
  
  TMPEG2_FBuf = array[0..2] of PByte;

function mpeg2_accel(accel: Cardinal):Cardinal; cdecl; external LibMpeg2Name;
function mpeg2_init:PMPEG2dec; cdecl; external LibMpeg2Name;
function mpeg2_info(mpeg2dec: PMPEG2dec):PMPEG2_Info; cdecl; external LibMpeg2Name;
procedure mpeg2_close(mpeg2dec: PMPEG2dec); cdecl; external LibMpeg2Name;

procedure mpeg2_buffer(mpeg2dec: PMPEG2dec; astart, aend: PByte); cdecl; external LibMpeg2Name;
function mpeg2_getpos(mpeg2dec: PMPEG2dec): integer; cdecl; external LibMpeg2Name;
function mpeg2_parse(mpeg2dec: PMPEG2dec): TMPEG2_State; cdecl; external LibMpeg2Name;

function mpeg2_convert (mpeg2dec: PMPEG2dec; Convert: Pointer; Arg: Pointer): integer; cdecl; external LibMpeg2Name;

procedure mpeg2_reset(mpeg2dec: PMPEG2dec; full_reset: integer); cdecl; external LibMpeg2Name;
procedure mpeg2_skip(mpeg2dec: PMPEG2dec; skip: integer); cdecl; external LibMpeg2Name;
procedure mpeg2_slice_region(mpeg2dec: PMPEG2dec; astart, aend: integer); cdecl; external LibMpeg2Name;

procedure mpeg2_tag_picture(mpeg2dec: PMPEG2dec; tag, tag2: Cardinal); cdecl; external LibMpeg2Name;

procedure mpeg2_init_fbuf(decoder: PMPEG2_decoder; current_fbuf: TMPEG2_FBuf;
		      forward_fbuf: TMPEG2_FBuf; backward_fbuf: TMPEG2_FBuf); cdecl; external LibMpeg2Name;
procedure mpeg2_slice (decoder: PMPEG2_decoder; code: integer; const buffer: PByte); cdecl; external LibMpeg2Name;

function mpeg2_malloc (size: Cardinal; reason: TMPEG2_Alloc): Pointer; cdecl; external LibMpeg2Name;
procedure mpeg2_free (buf: Pointer); cdecl; external LibMpeg2Name;

{$IFDEF Mpeg2Convert}
var
  mpeg2convert_rgb32: Pointer;
  mpeg2convert_rgb24: Pointer;
  mpeg2convert_rgb16: Pointer;
  mpeg2convert_rgb15: Pointer;
  mpeg2convert_rgb8: Pointer;
  mpeg2convert_rgb: Pointer;

  mpeg2convert_bgr32: Pointer;
  mpeg2convert_bgr24: Pointer;
  mpeg2convert_bgr16: Pointer;
  mpeg2convert_bgr15: Pointer;
  mpeg2convert_bgr8: Pointer;
{$ENDIF}

implementation

{$IFDEF Mpeg2Convert}
uses
  SysUtils, {$IFDEF WIN32}Windows{$ELSE}dynlibs{$ENDIF};

var
  Handle:Cardinal;

procedure InitConvertVariables;
begin
  Handle := 0;
  if FileExists(LibMpeg2ConvertName) then
  begin
    Handle := LoadLibrary(PChar(LibMpeg2ConvertName));
    mpeg2convert_rgb32 := GetProcAddress(Handle, 'mpeg2convert_rgb32');
    mpeg2convert_rgb24 := GetProcAddress(Handle, 'mpeg2convert_rgb24');
    mpeg2convert_rgb16 := GetProcAddress(Handle, 'mpeg2convert_rgb16');
    mpeg2convert_rgb15 := GetProcAddress(Handle, 'mpeg2convert_rgb15');
    mpeg2convert_rgb8  := GetProcAddress(Handle, 'mpeg2convert_rgb8');
    mpeg2convert_rgb   := GetProcAddress(Handle, 'mpeg2convert_rgb');

    mpeg2convert_bgr32 := GetProcAddress(Handle, 'mpeg2convert_bgr32');
    mpeg2convert_bgr24 := GetProcAddress(Handle, 'mpeg2convert_bgr24');
    mpeg2convert_bgr16 := GetProcAddress(Handle, 'mpeg2convert_bgr16');
    mpeg2convert_bgr15 := GetProcAddress(Handle, 'mpeg2convert_bgr15');
    mpeg2convert_bgr8  := GetProcAddress(Handle, 'mpeg2convert_bgr8');
  end;
end;

procedure CloseConvertVariables;
begin
  if Handle <> 0 then
  begin
    FreeLibrary(Handle);
  end;
end;

initialization
  InitConvertVariables;

finalization
  CloseConvertVariables;
{$ENDIF}

end.
