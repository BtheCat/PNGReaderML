(* bword = 1 byte, word = 2 bytes, lword = 3 bytes, qword = 4 bytes *)

type bword = int;;
type word = int;;
type lword = int;;
type qword = int;;

(* En-tête de fichier PNG : PNG File Header *)

type pngFileHeader = {
  pfHighBit     : bword;
  pfFormat      : string;
  pfLineEnding  : word;
  pfEndOfFile   : bword;
  pfEndingConv  : bword;
};;

(* En-tête de fichier PNG : PNG Info Header *)

type color_type = GrayScale | RGB | PaletteIndex | GrayScaleAlpha | RGBAlpha

type pngIHDRChunk = {
  piLength      : qword;
  piChunkId     : qword;
  piWidth       : qword;
  piHeight      : qword;
  piBitDepth    : bword;
  piColorType   : color_type;
  piCompression : bword;
  piFilter      : bword;
  piInterface   : bword;
  piCRC         : qword;
}

type brut_data = bword array

type zlib_compressed_data = bword array

type zlib_data = {
  zlibCompressionMethod : bword;
  zlibCompressionInfo   : bword;
  zlibFCheck            : bword;
  zlibFDict             : bool;
  zlibZlibDict          : bword array option;
  zlibFLevel            : bword;
  zlibData              : zlib_compressed_data;
  zlibAdler             : bword array;
  }
  
type deflate_compressed_data = bword array

type deflate_data = deflate_compressed_data 
 
type size = { w : int ; h : int ; }

type gray_pixel = bword 

type rgb = {
  red   : bword;
  green : bword;
  blue  : bword;
}

type gray_alpha_pixel = {
  value : bword;
  alpha : bword;
}

type rgba = {
  red   : bword;
  green : bword;
  blue  : bword;
  alpha : bword;
}

type uncompressed_pixel =
  | UncompPix_GrayScale of gray_pixel
  | UncompPix_RGB of rgb 
  | UncompPix_PaletteIndex of rgba 
  | UncompPix_GrayScaleAlpha of gray_alpha_pixel
  | UncompPix_RGBAlpha of rgba  

type uncompressed_data = uncompressed_pixel array array

type compressed_data = 
  BrutData of brut_data
  | ZlibData of zlib_data
  | DeflateData of deflate_data
  | UncompressedData of uncompressed_data

type pngIDATChunk = compressed_data

type pngTEXTChunk = {
  textKeyword       : string;
  textNullSeparator : char;
  textContent       : string;
}

type pngZTXTChunk = {
  ztxtKeyword           : string;
  ztxtNullSeparator     : char;
  ztxtCompressionMethod : bword;
  ztxtContent           : compressed_data;
}

type pngITXTChunk = {
  itxtKeyword           : string;
  itxtNullSeparator     : char;
  itxtCompressionFlag   : bword;
  itxtCompressionMethod : bword;
  itxtLanguageTag       : string option;
  itxtTranslatedKeyword : string option;
  itxtContent           : compressed_data;
}

type pngTRNSChunk = 
  GrayLevel of word
  | RGBValue of rgba
  | Alphas of bword array

type pngGAMAChunk = float

type point = { x : float ; y : float ; }

type pngCHRMChunk = {
  chrmWhitePoint  : point;
  chrmRedPoint    : point;
  chrmGreenPoint  : point;
  chrmBluePoint   : point;
}

type pngPLTEChunk = rgba array

type pngPHYSChunk = {
  physPixByXAxis    : qword;
  physPixByYAxis    : qword;
  physUnitSpecifier : bword;
}

type pngTIMEChunk = {
  timeYear    : word;
  timeMonth   : bword;
  timeDay     : bword;
  timeHour    : bword;
  timeMinute  : bword;
  timeSecond  : bword;
}

type pngSRGBChunk = bword

type pngICCPChunk = {
  iccpProfileName       : string;
  iccpNullSeparator     : char;
  iccpCompressionMethod : bword;
  iccpCompressedProfile : compressed_data;
}

type pngBKGDChunk = 
  BKGD_IndexedColor of bword
  | BKGD_GrayScale of word
  | BKGD_RGBValue of rgba

type pngSBITChunk =
  SBIT_GrayScale of bword
  | SBIT_TrueColor of lword
  | SBIT_IndexedColor of lword
  | SBIT_GrayScaleAlpha of word
  | SBIT_TrueColorAlpha of qword

type splt_word = BWord of bword | Word of word

let splt_word_empty = BWord 0

type suggested_PLT = {
  red       : splt_word;
  green     : splt_word;
  blue      : splt_word;
  alpha     : splt_word;
  frequency : word;
}

let sugg_plt_empty = { red = splt_word_empty ; green = splt_word_empty ; blue = splt_word_empty ; alpha = splt_word_empty ; frequency = 0 ; }

type pngSPLTChunk = {
  spltPaletteName   : string;
  spltNullSeparator : char;
  spltSampleDepth   : bword;
  spltDatas         : suggested_PLT array;
}

type pngHISTChunk = word array

type chunkType = 
  PLTE of pngPLTEChunk
  | IDAT of pngIDATChunk
  | IEND

  | Unknown of brut_data
  
  | TRNS of pngTRNSChunk
  | GAMA of pngGAMAChunk
  | CHRM of pngCHRMChunk
  | SRGB of pngSRGBChunk
  | ICCP of pngICCPChunk

  | TEXT of pngTEXTChunk
  | ZTXT of pngZTXTChunk
  | ITXT of pngITXTChunk

  | BKGD of pngBKGDChunk
  | PHYS of pngPHYSChunk
  | SBIT of pngSBITChunk
  | SPLT of pngSPLTChunk
  | HIST of pngHISTChunk
  | TIME of pngTIMEChunk

(* Contenu du fichier PNG : Chunks *)

type pngChunk = {
  pcLengthChunk : qword;
  pcChunkId     : qword;
  pcChunkName   : string;
  pcData        : chunkType;
  pcCRC         : qword;
};;

(* Définition des différentes exceptions possibles *)

exception IHDR_BadBitDepth

exception PLTE_NotDefined
exception PLTE_NotDividedBy3

exception TRNS_WrongLength
exception TRNS_BadColorType

exception SPLT_BadLengthChunk
exception SPLT_BadSampleDepth

exception BadCompressionMethodValue

exception Deflate_BadCompressionMethod
exception Deflate_BadDistanceCode
exception Deflate_BadCodeSymbol
exception Deflate_BadCodeTableLen

exception BitReader_EnsureBits

exception FilterType_BadFilterMethod
exception ColorType_BadColorType

exception DecodeData_PLTEAbsent

type setOption = {
  srgb : pngSRGBChunk option;
  trns : pngTRNSChunk option;
  phys : pngPHYSChunk option;
  gama : pngGAMAChunk option;
  chrm : pngCHRMChunk option;
  iccp : pngICCPChunk option;
  bkgd : pngBKGDChunk option;
  sbit : pngSBITChunk option;
  splt : pngSPLTChunk list;
  hist : pngHISTChunk option;
}

let setOpt_empty = { srgb = None ; trns = None ; phys = None ; gama = None ; chrm = None ; iccp = None ; bkgd = None ; sbit = None ; splt = [] ; hist = None ; }

type status_png = Parsing | ZlibParsing | DeflateParsing | Uncompressed

type pngFile = {
  pngFH               : pngFileHeader;
  pngIH               : pngIHDRChunk;
  pngPLTE             : pngChunk option;
  pngStatus           : status_png;
  pngData             : pngChunk list;
  pngSetOption        : setOption;
  pngInfoOption       : pngChunk list;
  pngUnknownChunkList : pngChunk list;
  pngIENDPresent      : bool;
}

type deflate_comp_method = Fixed | Dynamic

type huffmanTree = {
  table_len   : word array ;
  table_value : word array ;
}

module type BitReaderSig = 
  sig 
    type bitReader

    val init : zlib_compressed_data -> int -> bitReader 

    val ensureBits9 : bitReader -> int -> bool 
    val ensureBits17 : bitReader -> int -> bool 
    val ensureBits25 : bitReader -> int -> bool
    val ensureBits32 : bitReader -> int -> bool

    val peekBits : bitReader -> int -> qword 
    val advanceBits : bitReader -> int -> unit 
    val readBits : bitReader -> int -> qword

    (* val getBuffer : bitReader -> qword *)
  end

module BitReader : BitReaderSig =
  struct 

    type bitReader = {
      data            : zlib_compressed_data;
      size            : int;
      bitsize         : int;
      mutable bp      : int;
      mutable buffer  : qword;
    }

    let init data size =
      {
        data = data;
        size = size;
        bitsize = size * 8;
        bp = 0;
        buffer = 0;
      }

    let ensureBits9 reader nbits =
      let start = reader.bp lsr 3 in 
      let size = reader.size in 

      if start + 1 < size then (
        reader.buffer <- reader.data.(start + 0) lor (reader.data.(start + 1) lsl 8);
        reader.buffer <- reader.buffer lsr (reader.bp land 7);
        true 
      ) else (
        reader.buffer <- 0;
        ( if start < size + 0 then reader.buffer <- reader.buffer lor (reader.data.(start + 0)) );
        reader.buffer <- reader.buffer lsr (reader.bp land 7);
        (reader.bp + nbits <= reader.bitsize)
      )

    let ensureBits17 reader nbits =
      let start = reader.bp lsr 3 in 
      let size = reader.size in 

      if start + 2 < size then (
        reader.buffer <- reader.data.(start + 0)  lor ( reader.data.(start + 1) lsl 8 )
                                                  lor ( reader.data.(start + 2) lsl 16 );
        reader.buffer <- reader.buffer lsr (reader.bp land 7);
        true 
      ) else (
        reader.buffer <- 0;
        ( if start + 0 < size then reader.buffer <- reader.buffer lor reader.data.(start + 0) );
        ( if start + 1 < size then reader.buffer <- reader.buffer lor (reader.data.(start + 1) lsl 8) );
        reader.buffer <- reader.buffer lsr (reader.bp land 7);
        (reader.bp + nbits <= reader.bitsize)
      )

    let ensureBits25 reader nbits =
      let start = reader.bp lsr 3 in 
      let size = reader.size in 

      if start + 3 < size then (
        reader.buffer <- reader.data.(start + 0)  lor ( reader.data.(start + 1) lsl 8 )
                                                  lor ( reader.data.(start + 2) lsl 16 )
                                                  lor ( reader.data.(start + 3) lsl 24 );
        reader.buffer <- reader.buffer lsr (reader.bp land 7);
        true 
      ) else (
        reader.buffer <- 0;
        ( if start + 0 < size then reader.buffer <- reader.buffer lor reader.data.(start + 0) );
        ( if start + 1 < size then reader.buffer <- reader.buffer lor (reader.data.(start + 1) lsl 8) );
        ( if start + 2 < size then reader.buffer <- reader.buffer lor (reader.data.(start + 2) lsl 16) );
        reader.buffer <- reader.buffer lsr (reader.bp land 7);
        (reader.bp + nbits <= reader.bitsize)
      )

    let ensureBits32 reader nbits =
      let start = reader.bp lsr 3 in 
      let size = reader.size in 

      if start + 4 < size then (
        reader.buffer <- reader.data.(start + 0)  lor ( reader.data.(start + 1) lsl 8 )
                                                  lor ( reader.data.(start + 2) lsl 16 )
                                                  lor ( reader.data.(start + 3) lsl 24 );
        reader.buffer <- reader.buffer lsr (reader.bp land 7);
        reader.buffer <- reader.buffer lor ( (reader.data.(start + 4) lsl 24) lsl (8 - (reader.bp land 7)) );
        true                                                   
      ) else (
        reader.buffer <- 0;
        ( if start + 0 < size then reader.buffer <- reader.buffer lor reader.data.(start + 0) );
        ( if start + 1 < size then reader.buffer <- reader.buffer lor (reader.data.(start + 1) lsl 8) );
        ( if start + 2 < size then reader.buffer <- reader.buffer lor (reader.data.(start + 2) lsl 16) );
        ( if start + 3 < size then reader.buffer <- reader.buffer lor (reader.data.(start + 3) lsl 24) );
        reader.buffer <- reader.buffer lsr (reader.bp land 7);
        (reader.bp + nbits <= reader.bitsize)        
      )

    let peekBits reader nbits = 
      reader.buffer land ( (1 lsl nbits) - 1 )

    let advanceBits reader nbits =
      reader.buffer <- reader.buffer lsr nbits;
      reader.bp <- reader.bp + nbits 

    let readBits reader nbits = 
      let result = peekBits reader nbits in 
      advanceBits reader nbits;
      result

    (* let getBuffer reader = reader.buffer *)

  end

module type UcVectorSig =
  sig 

    type ucvector 

    exception UcVector_BadSrcPos

    val resize : ucvector -> int -> unit
    val init : deflate_compressed_data -> int -> ucvector
    val insert_data : ucvector -> bword array -> unit
    val memcpy : ucvector -> int -> int -> int -> unit 

    val get_size : ucvector -> int 
    val get_data : ucvector -> deflate_compressed_data
  end

module UcVector : UcVectorSig = 
  struct 

    type ucvector = {
      mutable data      : deflate_compressed_data;
      mutable size      : int;
      mutable allocsize : int;
    }

    exception UcVector_BadSrcPos

    let resize ucv size =
      if size > ucv.allocsize then (
        let newsize = size + (ucv.allocsize lsl 1) in 
        let data = Array.make newsize 0 in 

        for i = 0 to ucv.size - 1 do 
          data.(i) <- ucv.data.(i)
        done;

        ucv.allocsize <- newsize;
        ucv.data <- data
      );

      ucv.size <- size
    
    let init buffer size =
      {
        data = buffer;
        size = size;
        allocsize = size;
      }

    let insert_data ucv data =
      let size_data = Array.length data in 
      let old_size = ucv.size in 
      resize ucv (ucv.size + size_data);

      for i = 0 to size_data - 1 do 
        ucv.data.(old_size + i) <- data.(i)
      done

    let memcpy ucv pos_dst pos_src len =
      for i = 0 to len - 1 do 
        ucv.data.(pos_dst + i) <- ucv.data.(pos_src + i)
      done

    let get_size ucv = ucv.size
    let get_data ucv = ucv.data 
  end

type filter_type = NoFilter | Sub | Up | Average | Paeth 