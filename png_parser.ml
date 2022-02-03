#use "png_tools.ml"

let idat_id = idChunk_of_strChunk "IDAT";;
let iend_id = idChunk_of_strChunk "IEND";;
let plte_id = idChunk_of_strChunk "PLTE";;
let text_id = idChunk_of_strChunk "tEXt";;
let ztxt_id = idChunk_of_strChunk "zTXt";;
let itxt_id = idChunk_of_strChunk "iTXt";; 
let phys_id = idChunk_of_strChunk "pHYs";;
let time_id = idChunk_of_strChunk "tIME";;
let srgb_id = idChunk_of_strChunk "sRGB";;
let trns_id = idChunk_of_strChunk "tRNS";;
let gama_id = idChunk_of_strChunk "gAMA";;
let chrm_id = idChunk_of_strChunk "cHRM";;
let iccp_id = idChunk_of_strChunk "iCCP";;
let bkgd_id = idChunk_of_strChunk "bKGD";;
let sbit_id = idChunk_of_strChunk "sBIT";;
let splt_id = idChunk_of_strChunk "sPLT";;
let hist_id = idChunk_of_strChunk "hIST";;

let null_separator = '\000';;
let value_compression_method = 0;;

(* Lecture du PNGFileHeader *)

let read_file_header channel =
  let hb = read_bword channel in 
  let f = read_lword channel in 
  let le = read_word channel in 
  let eof = read_bword channel in
  let ec = read_bword channel in 
  {
    pfHighBit = hb;
    pfFormat = str_of_id f 3;
    pfLineEnding = le;
    pfEndOfFile = eof;
    pfEndingConv = ec;
  };;

(* Lecture du PNGInfoHeader *)

let read_info_header channel =
  let lc = read_qword channel in 
  let cid = read_qword channel in 
  let w = read_qword channel in 
  let h = read_qword channel in 
  let bd = read_bword channel in 
  let ct = read_bword channel in 
  let c = read_bword channel in 
  let f = read_bword channel in 
  let i = read_bword channel in 
  let crc = read_qword channel in 

  let ct_val = color_of_int ct in 

  ( if not (check_bitdepth bd ct_val) then raise IHDR_BadBitDepth );

  {
    piLength = lc;
    piChunkId = cid;
    piWidth = w;
    piHeight = h;
    piBitDepth = bd;
    piColorType = ct_val;
    piCompression = c;
    piFilter = f;
    piInterface = i;
    piCRC = crc;
  };;

(* Lecture d'un pngChunk *)

let read_plte_chunk channel len_chunk =
  (if len_chunk mod 3 <> 0 then raise PLTE_NotDividedBy3);

  let plte_chunk = Array.make (len_chunk / 3) rgba_empty in 

  for i = 0 to (len_chunk / 3) - 1 do 
    let r = read_bword channel in 
    let g = read_bword channel in 
    let b = read_bword channel in 
    plte_chunk.(i) <- { red = r; green = g; blue = b ; alpha = 255 ; }
  done;

  plte_chunk

let read_trns_chunk channel len_chunk color_type bitdepth =
  match color_type with
    | GrayScale -> GrayLevel (truncate (read_word channel) bitdepth)

    | RGB -> let r = truncate (read_word channel) bitdepth in 
        let g = truncate (read_word channel) bitdepth in 
        let b = truncate (read_word channel) bitdepth in
        RGBValue { red = r ; green = g ; blue = b ; alpha = 255 ; } 

    | PaletteIndex -> let alphas = Array.make len_chunk 0 in 
        for i = 0 to len_chunk - 1 do 
          alphas.(i) <- read_bword channel
        done;
        Alphas alphas

    | _ -> raise TRNS_BadColorType

let read_gama_chunk channel =
  let g = read_lword channel in (conversion g : pngGAMAChunk)

let read_brut_data channel len_data =
  let d = Array.make len_data (-1) in 

  for i = 0 to len_data - 1 do 
    d.(i) <- read_bword channel
  done;
  (d : brut_data) 

let read_idat_chunk channel len_chunk = 
  let d = read_brut_data channel len_chunk in 
  (BrutData d : pngIDATChunk)

let read_chrm_chunk channel =
  let wx = read_lword channel in 
  let wy = read_lword channel in
  let rx = read_lword channel in 
  let ry = read_lword channel in 
  let gx = read_lword channel in 
  let gy = read_lword channel in 
  let bx = read_lword channel in 
  let by = read_lword channel in 
  {
    chrmWhitePoint = { x = conversion wx ; y = conversion wy ; } ;
    chrmRedPoint = { x = conversion rx ; y = conversion ry ; } ;
    chrmGreenPoint = { x = conversion gx ; y = conversion gy ; } ;
    chrmBluePoint = { x = conversion bx ; y = conversion by ; } ;
  }

let read_keyword_txt_chunk channel =
  let actual_byte = ref (-1) 
  and k = ref ""
  and i = ref 0 in 

  while !actual_byte <> 0 do 
    actual_byte := read_bword channel;
    
    if !actual_byte = 0 then incr i
    else (
      k := !k ^ (String.make 1 (Char.chr !actual_byte));
      incr i 
    )
  done;
  (!k, !i)

let read_text_chunk channel len_chunk =
  let k, ind_endKey = read_keyword_txt_chunk channel in 
  let c = ref "" in 

  for i = ind_endKey to len_chunk - 1 do 
    let actual_char = Char.chr (read_bword channel) in 
    c := !c ^ (String.make 1 actual_char)
  done;
  {
    textKeyword = k;
    textNullSeparator = null_separator;
    textContent = !c;
  }

let read_ztxt_chunk channel len_chunk =
  let k, ind_endKey = read_keyword_txt_chunk channel in 
  let cm = read_bword channel in 
  {
    ztxtKeyword = k;
    ztxtNullSeparator = null_separator;
    ztxtCompressionMethod = cm;
    ztxtContent = BrutData ( read_brut_data channel (len_chunk - (ind_endKey + 1)) );
  }

let read_itxt_chunk channel len_chunk =
  let k, ind_endKey = read_keyword_txt_chunk channel in 
  let cf = read_bword channel in 
  let cm = read_bword channel in 
  let lt_opt = ref None 
  and tk_opt = ref None in 

  let (lt, ind_endLt) = read_keyword_txt_chunk channel in 
  let (tk, ind_endTk) = read_keyword_txt_chunk channel in 

  let len_lt = String.length lt 
  and len_tk = String.length tk in 

  (if len_lt <> 0 then lt_opt := Some lt);
  (if len_tk <> 0 then tk_opt := Some tk);

  let n = len_chunk - (ind_endKey + 1 + 1 + 1 + len_lt + 1 + len_tk + 1) in 
  {
    itxtKeyword = k;
    itxtNullSeparator = null_separator; 
    itxtCompressionFlag = cf;
    itxtCompressionMethod = cm;
    itxtLanguageTag = !lt_opt;
    itxtTranslatedKeyword = !tk_opt;
    itxtContent = BrutData (read_brut_data channel n);
  }

let read_phys_chunk channel = 
  let pxa = read_qword channel in 
  let pya = read_qword channel in 
  let us = read_bword channel in 
  {
    physPixByXAxis = pxa;
    physPixByYAxis = pya;
    physUnitSpecifier = us;
  }

let read_time_chunk channel =
  let y = read_word channel in 
  let m = read_bword channel in 
  let d = read_bword channel in 
  let h = read_bword channel in 
  let mn = read_bword channel in 
  let s = read_bword channel in 
  {
    timeYear = y;
    timeMonth = m;
    timeDay = d;
    timeHour = h;
    timeMinute = mn;
    timeSecond = s;
  }

let read_iccp_chunk channel len_chunk =
  let pn, ind_endKey = read_keyword_txt_chunk channel in 
  let cm = read_bword channel in 
  {
    iccpProfileName = pn;
    iccpNullSeparator = null_separator;
    iccpCompressionMethod = cm;
    iccpCompressedProfile = BrutData ( read_brut_data channel (len_chunk - (ind_endKey + 1)) );
  }

let read_bkgd_chunk channel color_type bitdepth =
  match color_type with 
    | GrayScale | GrayScaleAlpha 
        -> BKGD_GrayScale (truncate (read_word channel) bitdepth)

    | RGB | RGBAlpha 
        ->  let r = truncate (read_word channel) bitdepth in
            let g = truncate (read_word channel) bitdepth in
            let b = truncate (read_word channel) bitdepth in
            BKGD_RGBValue { red = r ; green = g ; blue = b ; alpha = 255 ; } 

    | PaletteIndex
        -> BKGD_IndexedColor (read_bword channel)
     
let read_sbit_chunk channel color_type =
  match color_type with 
    | GrayScale       -> SBIT_GrayScale (read_bword channel)
    | RGB             -> SBIT_TrueColor (read_lword channel)
    | PaletteIndex    -> SBIT_IndexedColor (read_lword channel)
    | GrayScaleAlpha  -> SBIT_GrayScaleAlpha (read_word channel)
    | RGBAlpha        -> SBIT_TrueColorAlpha (read_qword channel)

let read_spltWord channel depth =
  match depth with 
    | 8   -> BWord (read_bword channel)
    | 16  -> Word (read_word channel)
    | _   -> raise SPLT_BadSampleDepth

let read_splt_chunk channel len_chunk =
  let (pn, ind_endPN) = read_keyword_txt_chunk channel in 
  let sd = read_bword channel in 

  let len_datas = len_chunk - (ind_endPN + 1) in 

  (
    if not ( ( (sd = 8) && (len_datas mod 6 = 0) ) || ( (sd = 16) && (len_datas mod 10 = 0) ) ) then raise SPLT_BadLengthChunk
  );

  let n = (if sd = 8 then (len_datas / 6) else (len_datas / 10)) in 

  let d = Array.make n sugg_plt_empty in 

  for i = 0 to n - 1 do 
    let r = read_spltWord channel sd in 
    let g = read_spltWord channel sd in 
    let b = read_spltWord channel sd in 
    let a = read_spltWord channel sd in 
    let f = read_word channel in 
    d.(i) <- { red = r ; green = g ; blue = b ; alpha = a ; frequency = f ; }
  done;
  {
    spltPaletteName = pn;
    spltNullSeparator = null_separator;
    spltSampleDepth = sd;
    spltDatas = d;
  }

let read_hist_chunk channel len_chunk =
  let d = Array.make (len_chunk / 2) 0 in 

  for i = 0 to (len_chunk / 2) - 1 do 
    d.(i) <- read_word channel
  done; (d : pngHISTChunk)

let read_info_chunk channel chunk_id len_chunk ih =
  match chunk_id with 
    | n when n = plte_id -> PLTE (read_plte_chunk channel len_chunk)
    | n when n = idat_id -> IDAT (read_idat_chunk channel len_chunk)
    | n when n = iend_id -> IEND
    
    | n when n = trns_id -> TRNS (read_trns_chunk channel len_chunk ih.piColorType ih.piBitDepth)
    | n when n = gama_id -> GAMA (read_gama_chunk channel)
    | n when n = chrm_id -> CHRM (read_chrm_chunk channel)
    | n when n = srgb_id -> SRGB (read_bword channel)
    | n when n = iccp_id -> ICCP (read_iccp_chunk channel len_chunk)

    | n when n = text_id -> TEXT (read_text_chunk channel len_chunk)
    | n when n = ztxt_id -> ZTXT (read_ztxt_chunk channel len_chunk)
    | n when n = itxt_id -> ITXT (read_itxt_chunk channel len_chunk)
    
    | n when n = bkgd_id -> BKGD (read_bkgd_chunk channel ih.piColorType ih.piBitDepth)
    | n when n = phys_id -> PHYS (read_phys_chunk channel)
    | n when n = sbit_id -> SBIT (read_sbit_chunk channel ih.piColorType)
    | n when n = splt_id -> SPLT (read_splt_chunk channel len_chunk)
    | n when n = hist_id -> HIST (read_hist_chunk channel len_chunk)
    | n when n = time_id -> TIME (read_time_chunk channel)

    | _ -> Unknown (read_brut_data channel len_chunk)

let read_chunk channel ih =
  let lc = read_qword channel in 
  let cid = read_qword channel in 
  let d = read_info_chunk channel cid lc ih in 
  let crc = read_qword channel in 
  {
    pcLengthChunk = lc;
    pcChunkId = cid;
    pcChunkName = str_of_id cid 4;
    pcData = d;
    pcCRC = crc;
  }

(* Parsing d'un fichier PNG *)

let parse_png_file filename =
  let channel = open_in_bin filename in 
  let fh = read_file_header channel in 
  let ih = read_info_header channel in 

  let idat_list = ref [] 
  and set_option = ref setOpt_empty 
  and info_list = ref [] 
  and plte_opt = ref None 
  and unknown_list = ref [] in 

  let eof = ref false 
  and iend_present = ref false in 

  while (not !eof) || (not !iend_present) do
    try (
      let pChunk = read_chunk channel ih in 
      (
        match pChunk.pcChunkId with 
          | n when n = idat_id  
                            -> idat_list := !idat_list @ [pChunk]

          | n when n = plte_id  
                            -> plte_opt := Some pChunk

          | n when n = trns_id
                            -> (if !plte_opt = None then raise PLTE_NotDefined);
                            (
                              match (pChunk.pcData, !plte_opt) with 
                                | (TRNS Alphas trns_chunk_values, Some plte_chunk) ->
                                      let new_plte_chunk = {
                                        pcLengthChunk = plte_chunk.pcLengthChunk;
                                        pcChunkId = plte_chunk.pcChunkId;
                                        pcChunkName = plte_chunk.pcChunkName;
                                        pcData = rgbaPalette_of_rgbPalette trns_chunk_values plte_chunk.pcData;
                                        pcCRC = plte_chunk.pcCRC
                                      } in 
                                      plte_opt := Some new_plte_chunk
                                | (TRNS trns_chunk, _) -> 
                                      set_option := {
                                        srgb = !set_option.srgb ;
                                        trns = Some trns_chunk ;
                                        phys = !set_option.phys ;
                                        gama = !set_option.gama ;
                                        chrm = !set_option.chrm ;
                                        iccp = !set_option.iccp ;
                                        bkgd = !set_option.bkgd ;
                                        sbit = !set_option.sbit ;
                                        splt = !set_option.splt ;
                                        hist = !set_option.hist ;
                                      }
                                | _ -> failwith "parse_file_png"
                            )
                            
          | n when n = phys_id
                            -> let phys_chunk = (match pChunk.pcData with PHYS phys_chunk -> phys_chunk | _ -> failwith "parse_png_file") in 
                              set_option := { 
                                srgb = !set_option.srgb ; 
                                trns = !set_option.trns ;
                                phys = Some phys_chunk ;
                                gama = !set_option.gama ;
                                chrm = !set_option.chrm ;
                                iccp = !set_option.iccp ;
                                bkgd = !set_option.bkgd ;
                                sbit = !set_option.sbit ;
                                splt = !set_option.splt ;
                                hist = !set_option.hist ;
                              }

          | n when n = srgb_id
                            -> let srgb_chunk = (match pChunk.pcData with SRGB srgb_chunk -> srgb_chunk | _ -> failwith "parse_png_file") in 
                              set_option := { 
                                srgb = Some srgb_chunk ; 
                                trns = !set_option.trns ;
                                phys = !set_option.phys ;
                                gama = !set_option.gama ;
                                chrm = !set_option.chrm ;
                                iccp = !set_option.iccp ;
                                bkgd = !set_option.bkgd ;
                                sbit = !set_option.sbit ;
                                splt = !set_option.splt ;
                                hist = !set_option.hist ;
                              }

          | n when n = gama_id
                            -> let gama_chunk = (match pChunk.pcData with GAMA srgb_chunk -> srgb_chunk | _ -> failwith "parse_png_file") in 
                              set_option := { 
                                srgb = !set_option.srgb ; 
                                trns = !set_option.trns ;
                                phys = !set_option.phys ;
                                gama = Some gama_chunk ;
                                chrm = !set_option.chrm ;
                                iccp = !set_option.iccp ;
                                bkgd = !set_option.bkgd ;
                                sbit = !set_option.sbit ;
                                splt = !set_option.splt ;
                                hist = !set_option.hist ;
                              } 

          | n when n = chrm_id
                            -> let chrm_chunk = (match pChunk.pcData with CHRM srgb_chunk -> srgb_chunk | _ -> failwith "parse_png_file") in 
                              set_option := { 
                                srgb = !set_option.srgb ; 
                                trns = !set_option.trns ;
                                phys = !set_option.phys ;
                                gama = !set_option.gama ;
                                chrm = Some chrm_chunk ;
                                iccp = !set_option.iccp ;
                                bkgd = !set_option.bkgd ;
                                sbit = !set_option.sbit ;
                                splt = !set_option.splt ;
                                hist = !set_option.hist ;
                              }

          | n when n = iccp_id
                            -> let iccp_chunk = (match pChunk.pcData with ICCP iccp_chunk -> iccp_chunk | _ -> failwith "parse_png_file") in 
                              set_option := {
                                srgb = !set_option.srgb ;
                                trns = !set_option.trns ;
                                phys = !set_option.phys ;
                                gama = !set_option.gama ;
                                chrm = !set_option.chrm ;
                                iccp = Some iccp_chunk ;
                                bkgd = !set_option.bkgd ;
                                sbit = !set_option.sbit ;
                                splt = !set_option.splt ;
                                hist = !set_option.hist ;
                              }

          | n when n = bkgd_id
                            -> let bkgd_chunk = (match pChunk.pcData with BKGD bkgd_chunk -> bkgd_chunk | _ -> failwith "parse_png_file") in 
                              set_option := {
                                srgb = !set_option.srgb ;
                                trns = !set_option.trns ;
                                phys = !set_option.phys ;
                                gama = !set_option.gama ;
                                chrm = !set_option.chrm ;
                                iccp = !set_option.iccp ;
                                bkgd = Some bkgd_chunk ;
                                sbit = !set_option.sbit ;
                                splt = !set_option.splt ;
                                hist = !set_option.hist ;
                              }

          | n when n = sbit_id
                            -> let sbit_chunk = (match pChunk.pcData with SBIT sbit_chunk -> sbit_chunk | _ -> failwith "parse_png_file") in 
                              set_option := {
                                srgb = !set_option.srgb ;
                                trns = !set_option.trns ;
                                phys = !set_option.phys ;
                                gama = !set_option.gama ;
                                chrm = !set_option.chrm ;
                                iccp = !set_option.iccp ;
                                bkgd = !set_option.bkgd ;
                                sbit = Some sbit_chunk ;
                                splt = !set_option.splt ;
                                hist = !set_option.hist ;
                              }

          | n when n = splt_id
                            -> let splt_chunk = (match pChunk.pcData with SPLT splt_chunk -> splt_chunk | _ -> failwith "parse_png_file") in 
                              set_option := {
                                srgb = !set_option.srgb ;
                                trns = !set_option.trns ;
                                phys = !set_option.phys ;
                                gama = !set_option.gama ;
                                chrm = !set_option.chrm ;
                                iccp = !set_option.iccp ;
                                bkgd = !set_option.bkgd ;
                                sbit = !set_option.sbit ;
                                splt = !set_option.splt @ [splt_chunk] ;
                                hist = !set_option.hist ;
                              }

          | n when n = hist_id
                            -> let hist_chunk = (match pChunk.pcData with HIST hist_chunk -> hist_chunk | _ -> failwith "parse_png_file") in 
                              set_option := {
                                srgb = !set_option.srgb ;
                                trns = !set_option.trns ;
                                phys = !set_option.phys ;
                                gama = !set_option.gama ;
                                chrm = !set_option.chrm ;
                                iccp = !set_option.iccp ;
                                bkgd = !set_option.bkgd ;
                                sbit = !set_option.sbit ;
                                splt = !set_option.splt ;
                                hist = Some hist_chunk ;
                              }

          | n when (n = text_id || n = time_id || n = ztxt_id || n = itxt_id)
                            -> info_list := !info_list @ [pChunk]

          | n when n = iend_id
                            -> iend_present := true

          | n -> print_string ("Unknown chunk : " ^ (str_of_id n 4) ^ "\n") ; unknown_list := !unknown_list @ [pChunk]
      )      
    ) with | _ -> eof := true 
  done;

  close_in channel;

  {
    pngFH = fh;
    pngIH = ih;
    pngPLTE = !plte_opt;
    pngStatus = Parsing;
    pngData = !idat_list;
    pngSetOption = !set_option;
    pngInfoOption = !info_list;
    pngUnknownChunkList = !unknown_list;
    pngIENDPresent = !iend_present;
  }

let parse_zlib_data png_file =
  let check_compression_method () =
    let rec check_compression_method_info_option = function 
      | []    -> true 
      | pc::l -> (
        match pc.pcData with 
          | ZTXT ztxt_chunk -> (ztxt_chunk.ztxtCompressionMethod = value_compression_method) && (check_compression_method_info_option l)
          | ITXT itxt_chunk -> (itxt_chunk.itxtCompressionMethod = value_compression_method) && (check_compression_method_info_option l)
          | TIME _ | TEXT _ -> check_compression_method_info_option l 
          | _ -> failwith "check_compression_method_info_option"
      )
    in 
    let check_compression_method_set_option set_opt =
      match set_opt.iccp with 
        | None            -> true 
        | Some iccp_chunk -> (iccp_chunk.iccpCompressionMethod = value_compression_method)
    in 
    let check_compression_method_idat_chunk ih = 
      ih.piCompression = value_compression_method
    in 

    ( check_compression_method_idat_chunk png_file.pngIH )
    && ( check_compression_method_set_option png_file.pngSetOption )
    && ( check_compression_method_info_option png_file.pngInfoOption )
  in 

  ( if not (check_compression_method ()) then raise BadCompressionMethodValue );

  let parse_zlib_data_list pc = 
    match pc.pcData with
      | IDAT idat_chunk -> 
            let new_idat_chunk = {
              pcLengthChunk = pc.pcLengthChunk ;
              pcChunkId = pc.pcChunkId ;
              pcChunkName = pc.pcChunkName ;
              pcData = IDAT ( ZlibData (zlib_data_of_comp_data idat_chunk) ) ;
              pcCRC = pc.pcCRC ;
            } in new_idat_chunk

      | TEXT _          -> pc

      | ZTXT ztxt_chunk ->
            let new_ztxt_chunk = {
              pcLengthChunk = pc.pcLengthChunk ;
              pcChunkId = pc.pcChunkId ;
              pcChunkName = pc.pcChunkName ;
              pcData = ZTXT {
                  ztxtKeyword = ztxt_chunk.ztxtKeyword ;
                  ztxtNullSeparator = ztxt_chunk.ztxtNullSeparator ;
                  ztxtCompressionMethod = ztxt_chunk.ztxtCompressionMethod ;
                  ztxtContent = ZlibData (zlib_data_of_comp_data ztxt_chunk.ztxtContent) ;
                } ;
              pcCRC = pc.pcCRC ;
            } in new_ztxt_chunk

      | ITXT itxt_chunk ->
            let new_itxt_chunk = {
              pcLengthChunk = pc.pcLengthChunk ;
              pcChunkId = pc.pcChunkId ;
              pcChunkName = pc.pcChunkName ;
              pcData = ITXT {
                  itxtKeyword = itxt_chunk.itxtKeyword ;
                  itxtNullSeparator = itxt_chunk.itxtNullSeparator ;
                  itxtCompressionFlag = itxt_chunk.itxtCompressionFlag ;
                  itxtCompressionMethod = itxt_chunk.itxtCompressionMethod ;
                  itxtLanguageTag = itxt_chunk.itxtLanguageTag ;
                  itxtTranslatedKeyword = itxt_chunk.itxtTranslatedKeyword ;
                  itxtContent = (
                      if itxt_chunk.itxtCompressionFlag = 0 then ( 
                        match itxt_chunk.itxtContent with 
                          | BrutData bd -> BrutData bd
                          | _ -> failwith "parse_zlib_data_list" 
                      ) else ( ZlibData (zlib_data_of_comp_data itxt_chunk.itxtContent) )
                    ) ;
                } ;
              pcCRC = pc.pcCRC ;
            } in new_itxt_chunk

      | TIME _          -> pc

      | _               -> failwith "parse_zlib_data_list"
    in 

    let new_iccp_chunk = 
      match png_file.pngSetOption.iccp with 
        | None            -> None
        | Some iccp_chunk -> 
                Some {
                  iccpProfileName = iccp_chunk.iccpProfileName ;
                  iccpNullSeparator = iccp_chunk.iccpNullSeparator ;
                  iccpCompressionMethod = iccp_chunk.iccpCompressionMethod ;
                  iccpCompressedProfile = ZlibData (zlib_data_of_comp_data iccp_chunk.iccpCompressedProfile) ;
                }
    in 

    {
      pngFH = png_file.pngFH ;
      pngIH = png_file.pngIH ;
      pngPLTE = png_file.pngPLTE ;
      pngStatus = ZlibParsing ;
      pngData = List.map parse_zlib_data_list png_file.pngData ;
      pngSetOption = {
          srgb = png_file.pngSetOption.srgb ;
          trns = png_file.pngSetOption.trns ;
          phys = png_file.pngSetOption.phys ;
          gama = png_file.pngSetOption.gama ;
          chrm = png_file.pngSetOption.chrm ;
          iccp = new_iccp_chunk ;
          bkgd = png_file.pngSetOption.bkgd ;
          sbit = png_file.pngSetOption.sbit ;
          splt = png_file.pngSetOption.splt ;
          hist = png_file.pngSetOption.hist ;
        } ;
      pngInfoOption = List.map parse_zlib_data_list png_file.pngInfoOption ;
      pngUnknownChunkList = png_file.pngUnknownChunkList ;
      pngIENDPresent = png_file.pngIENDPresent ;
    }
