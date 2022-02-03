#use "png_parser.ml";;

(* Base de données des longueurs représentés par les codes 257-285 *)
let lengthbase = [| 3 ; 4 ; 5 ; 6 ; 7 ; 8 ; 9 ; 10 ; 11 ; 13 ; 15 ; 17 ; 19 ; 23 ; 27 ;
  31 ; 35 ; 43 ; 51 ; 59 ; 67 ; 83 ; 99 ; 115 ; 131 ; 163 ; 195 ; 227 ; 258 |]
(* Base de données des extra bits utilisés par les codes 257-285 (ajoutés à la base des longueurs) *)
let lengthextra = [| 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 1 ; 1 ; 1 ; 1 ; 2 ; 2 ; 2 ; 2 ;
  3 ; 3 ; 3 ; 3 ; 4 ; 4 ; 4 ; 4 ; 5 ; 5 ; 5 ; 5 ; 0 |]

(* Base de données des distances avec une précédente occurence *)
let distancebase = [| 1 ; 2 ; 3 ; 4 ; 5 ; 7 ; 9 ; 13 ; 17 ; 25 ; 33 ; 49 ; 65 ; 97 ; 129 ;
  193 ; 257 ; 385 ; 513 ; 769 ; 1025 ; 1537 ; 2049 ; 3073 ; 4097 ; 6145 ; 8193 ; 12289 ; 16385 ; 24577 |]
(* Base de données des extra bits utilisés par les distances *)
let distanceextra = [| 0 ; 0 ; 0 ; 0 ; 1 ; 1 ; 2 ; 2 ; 3 ; 3 ; 4 ; 4 ; 5 ; 5 ; 6 ; 6 ; 7 ; 7 ; 8 ;
  8 ; 9 ; 9 ; 10 ; 10 ; 11 ; 11 ; 12 ; 12 ; 13 ; 13 |]

(* L'ordre dans lequel "code length alphabet code lengths" sont stockés comme spécifié par l'algorithme deflate *)
let num_code_length_codes = 19
let codeLength_order = [| 16 ; 17 ; 18 ; 0 ; 8 ; 7 ; 9 ; 6 ; 10 ; 5 ; 11 ; 4 ; 12 ; 3 ; 13 ; 2 ; 14 ; 1 ; 15 |]

let firstbits = 9
let invalidsymbol = 65535
let first_length_code_index = 257
let last_length_code_index = 285
let num_deflate_code_symbols = 288
let num_distance_symbols = 32

let huff_tree_empty = { table_len = [||] ; table_value = [||] ; }

let huffmanTree_make_table codes lengths num_codes max_bit_len =
  let headsize = 1 lsl firstbits 
  and mask = (1 lsl firstbits) - 1 in 

  let maxlens = Array.make headsize 0 in 

  for i = 0 to num_codes - 1 do 
    let symbol = codes.(i) in 
    let l = lengths.(i) in 

    if l > firstbits then (
      let index = reverse_bits firstbits (symbol lsr (l - firstbits)) in 
      maxlens.(index) <- max maxlens.(index) l 
    )
  done;

  let size = ref headsize in 
  for i = 0 to headsize - 1 do 
    let l = maxlens.(i) in 
    if l > firstbits then size := !size + (1 lsl (l - firstbits))
  done;

  let table_len = Array.make !size 16
  and table_value = Array.make !size 0 in 

  let pointer = ref headsize in 
  for i = 0 to headsize - 1 do 
    let l = maxlens.(i) in 
    if l > firstbits then (
      table_len.(i) <- l;
      table_value.(i) <- !pointer;
      pointer := !pointer + (1 lsl (l - firstbits))
    )
  done;

  let num_present = ref 0 in 
  for i = 0 to num_codes - 1 do 
    let l = lengths.(i) in 
    let symbol = codes.(i) in 
    let reverse = reverse_bits l symbol in 
    if l <> 0 then (
      incr num_present;

      if l <= firstbits then (
        let num = 1 lsl (firstbits - l) in 
        for j = 0 to num - 1 do 
          let index = reverse lor (j lsl l) in 
          table_len.(index) <- l;
          table_value.(index) <- i 
        done
      ) else (
        let index = reverse land mask in 
        let maxlen = table_len.(index) in 
        let tablelen = maxlen - firstbits in 
        let start = table_value.(index) in 
        let num = 1 lsl (tablelen - (l - firstbits)) in 

        for j = 0 to num - 1 do 
          let reverse2 = reverse lsr firstbits in 
          let index2 = start + ( reverse2 lor (j lsl (l - firstbits)) ) in 
          table_len.(index2) <- l;
          table_value.(index2) <- i 
        done
      )
    )
  done;

  if !num_present < 2 then (
    for i = 0 to !size - 1 do 
      if table_len.(i) = 16 then (
        table_len.(i) <- ( if i < headsize then 1 else (firstbits + 1) );
        table_value.(i) <- invalidsymbol
      )
    done
  ) else (
    for i = 0 to !size - 1 do 
      if table_len.(i) = 16 then raise Deflate_BadCodeTableLen
    done
  );

  {
    table_len = table_len;
    table_value = table_value;
  }

let huffmanTree_make_from_lengths bitlen num_codes max_bit_len =
  let lengths = Array.make num_codes 0 in 

  for i = 0 to num_codes - 1 do lengths.(i) <- bitlen.(i) done;

  let codes = Array.make num_codes 0 in 
  let blcount = Array.make (max_bit_len + 1) 0 in 
  let next_code = Array.make (max_bit_len + 1) 0 in 

  for bits = 0 to num_codes - 1 do 
    blcount.(lengths.(bits)) <- blcount.(lengths.(bits)) + 1
  done;

  for bits = 1 to max_bit_len do 
    next_code.(bits) <- (next_code.(bits - 1) + blcount.(bits - 1)) lsl 1
  done;

  for n = 0 to num_codes - 1 do (
    if lengths.(n) <> 0 then (
      (*codes.(n) <- next_code.(lengths.(n));*)
      codes.(n) <- next_code.(lengths.(n));
      next_code.(lengths.(n)) <- next_code.(lengths.(n)) + 1;
      codes.(n) <- codes.(n) land ( (1 lsl lengths.(n)) - 1 )
    )
  ) done;

  huffmanTree_make_table codes lengths num_codes max_bit_len

let generate_fixed_litlen_tree () = 
  let bitlen = Array.make num_deflate_code_symbols 0 in 

  for i = 0 to 143 do bitlen.(i) <- 8 done;
  for i = 144 to 255 do bitlen.(i) <- 9 done;
  for i = 256 to 279 do bitlen.(i) <- 7 done;
  for i = 280 to 287 do bitlen.(i) <- 8 done;

  huffmanTree_make_from_lengths bitlen num_deflate_code_symbols 15 

let generate_fixed_dist_tree () = 
  let bitlen = Array.make num_distance_symbols 5 in 
  huffmanTree_make_from_lengths bitlen num_distance_symbols 15

let get_huffTree_fixed () =
  let huff_tree_lexlen = generate_fixed_litlen_tree () in 
  let huff_tree_d = generate_fixed_dist_tree () in 
  (huff_tree_lexlen, huff_tree_d)

let huffman_decode_symbol reader huff_tree_code = 
  let code = BitReader.peekBits reader firstbits in 
  let l = huff_tree_code.table_len.(code) 
  and value = huff_tree_code.table_value.(code) in

  if l <= firstbits then (
    BitReader.advanceBits reader l;
    value
  ) else (
    BitReader.advanceBits reader firstbits;
    let index2 = value + ( BitReader.peekBits reader (l - firstbits) ) in 
    BitReader.advanceBits reader (huff_tree_code.table_len.(index2) - firstbits);
    huff_tree_code.table_value.(index2)
  )

let get_huffTree_dynamic reader = 
  ( if not (BitReader.ensureBits17 reader 14) then raise BitReader_EnsureBits );

  let hlit = ( BitReader.readBits reader 5 ) + 257 in 
  let hdist = ( BitReader.readBits reader 5 ) + 1 in 
  let hclen = ( BitReader.readBits reader 4 ) + 4 in 
  
  let bitlen_cl = Array.make num_code_length_codes 0 in 

  for i = 0 to hclen - 1 do 
    let _ = BitReader.ensureBits9 reader 3 in 
    bitlen_cl.(codeLength_order.(i)) <- BitReader.readBits reader 3
  done;

  let huff_tree_codelen = huffmanTree_make_from_lengths bitlen_cl num_code_length_codes 7 in 

  let bitlen_ll = Array.make num_deflate_code_symbols 0
  and bitlen_d = Array.make num_distance_symbols 0 in 

  let i = ref 0 in 

  while !i < hlit + hdist do
    let _ = BitReader.ensureBits25 reader 22 in 
    let code = huffman_decode_symbol reader huff_tree_codelen in 

    if code <= 15 then (
      (
        if !i < hlit then bitlen_ll.(!i) <- code
        else bitlen_d.(!i - hlit) <- code
      );
      incr i 
    ) else if code = 16 then (
      let replength = 3 + ( BitReader.readBits reader 2 ) in 
      let value = ( if !i < hlit + 1 then bitlen_ll.(!i - 1) else bitlen_d.(!i - hlit - 1) ) in 

      for n = 0 to replength - 1 do 
        (
          if !i < hlit then bitlen_ll.(!i) <- value
          else bitlen_d.(!i - hlit) <- value
        );
        incr i 
      done
    ) else if code = 17 then (
        let replength = 3 + ( BitReader.readBits reader 3 ) in 

        for n = 0 to replength - 1 do
          (
            if !i < hlit then bitlen_ll.(!i) <- 0
            else bitlen_d.(!i - hlit) <- 0
          );
          incr i 
        done
    ) else if code = 18 then (
      let replength = 11 + ( BitReader.readBits reader 7 ) in 

      for n = 0 to replength - 1 do 
        (
          if !i < hlit then bitlen_ll.(!i) <- 0
          else bitlen_d.(!i) <- 0
        );
        incr i 
      done
    ) else raise Deflate_BadCodeSymbol
  done;

  let huff_tree_lexlen = huffmanTree_make_from_lengths bitlen_ll num_deflate_code_symbols 15 in 

  let huff_tree_dist = huffmanTree_make_from_lengths bitlen_d num_distance_symbols 15 in 

  (huff_tree_lexlen, huff_tree_dist)

let inflateHuffmanBlock reader out huff_type =
  let (huff_tree_lexlen, huff_tree_dist) = ( match huff_type with Fixed -> get_huffTree_fixed () | Dynamic -> get_huffTree_dynamic reader ) in
  let end_code_found = ref false in 

  while not !end_code_found do 
    let _ = BitReader.ensureBits25 reader 20 in
    let code_ll = huffman_decode_symbol reader huff_tree_lexlen in 

    if code_ll <= 255 then ( 
      UcVector.insert_data out [| code_ll |]
    ) else if code_ll >= first_length_code_index && code_ll <= last_length_code_index then (

      let len = ref lengthbase.(code_ll - first_length_code_index)
      and numextrabits_l = lengthextra.(code_ll - first_length_code_index) in 

      ( if numextrabits_l <> 0 then len := !len + (BitReader.readBits reader numextrabits_l) );

      let _ = BitReader.ensureBits32 reader 28 in 
      let code_d = huffman_decode_symbol reader huff_tree_dist in 

      (if code_d > 29 then raise Deflate_BadDistanceCode);

      let dist = ref distancebase.(code_d)
      and numextrabits_d = distanceextra.(code_d) in 

      ( if numextrabits_d <> 0 then dist := !dist + (BitReader.readBits reader numextrabits_d) );

      let start = ref ( UcVector.get_size out ) in 
      let backward = !start - !dist in 

      (if !dist > !start then failwith "here");

      UcVector.resize out (!start + !len);

      if !dist < !len then (
        UcVector.memcpy out !start backward !dist;
        start := !start + !dist;
        UcVector.memcpy out !start backward (!len - !dist) 
      ) else (
        UcVector.memcpy out !start backward !len 
      )

    ) else end_code_found := true
  done

let inflateNoCompression reader out = 
  BitReader.advanceBits reader 5;

  ( if not (BitReader.ensureBits32 reader 32) then raise BitReader_EnsureBits );

  let len = BitReader.readBits reader 16 in 
  let _ = BitReader.readBits reader 16 in 

  for i = 0 to len - 1 do 
    let _ = BitReader.ensureBits9 reader 8 in 
    UcVector.insert_data out [| BitReader.readBits reader 8 |]
  done

let deflate_data_of_comp_data (zlib_data : compressed_data) =
  let zlib_data = ( match zlib_data with | ZlibData zd -> zd.zlibData | _ -> assert false ) in 
  let reader = BitReader.init zlib_data (Array.length zlib_data) in 
  let out = UcVector.init [||] 0 in
  let final = ref false in 

  while not !final do 
    ( if not (BitReader.ensureBits9 reader 3) then raise BitReader_EnsureBits );
    let bfinal = BitReader.readBits reader 1 in 
    let btype = BitReader.readBits reader 2 in 

    (
      match btype with 
      | 0 ->  inflateNoCompression reader out

      | 1 -> inflateHuffmanBlock reader out Fixed

      | 2 -> inflateHuffmanBlock reader out Dynamic 

      | _ -> raise Deflate_BadCompressionMethod
    ); 

    final := (bfinal = 1) 
  done;

  UcVector.get_data out

let inflate_zlib_data png_file =

  let inflate_zlib_data_list pc = 
    match pc.pcData with
      | IDAT idat_chunk -> 
            let new_idat_chunk = {
              pcLengthChunk = pc.pcLengthChunk ;
              pcChunkId = pc.pcChunkId ;
              pcChunkName = pc.pcChunkName ;
              pcData = IDAT ( DeflateData (deflate_data_of_comp_data idat_chunk) ) ;
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
                  ztxtContent = DeflateData (deflate_data_of_comp_data ztxt_chunk.ztxtContent) ;
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
                      if itxt_chunk.itxtCompressionFlag <> 0 then ( 
                        DeflateData (deflate_data_of_comp_data itxt_chunk.itxtContent) 
                      ) else ( itxt_chunk.itxtContent )
                    ) ;
                } ;
              pcCRC = pc.pcCRC ;
            } in new_itxt_chunk

      | TIME _          -> pc

      | _               -> failwith "inflate_zlib_data_list"
    in 

    let new_iccp_chunk = 
      match png_file.pngSetOption.iccp with 
        | None            -> None
        | Some iccp_chunk -> 
                Some {
                  iccpProfileName = iccp_chunk.iccpProfileName ;
                  iccpNullSeparator = iccp_chunk.iccpNullSeparator ;
                  iccpCompressionMethod = iccp_chunk.iccpCompressionMethod ;
                  iccpCompressedProfile = DeflateData (deflate_data_of_comp_data iccp_chunk.iccpCompressedProfile) ;
                }
    in 
  {
    pngFH = png_file.pngFH;
    pngIH = png_file.pngIH;
    pngPLTE = png_file.pngPLTE;
    pngStatus = DeflateParsing;
    pngData = List.map inflate_zlib_data_list png_file.pngData ; 
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
    pngInfoOption = List.map inflate_zlib_data_list png_file.pngInfoOption ;
    pngUnknownChunkList = png_file.pngUnknownChunkList;
    pngIENDPresent = png_file.pngIENDPresent;
  }