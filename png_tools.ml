#use "png_types.ml"

(* Lecture des xword pour x dans {'b', '', 'l', 'q'} *)

let read_bword channel =
  ( input_byte channel : bword )

let read_word channel =
  let a = input_byte channel in 
  let b = input_byte channel in 
  ( (a lsl 8) lor b : word )

let read_lword channel =
  let a = input_byte channel in 
  let b = input_byte channel in 
  let c = input_byte channel in 
  ( (a lsl 16) lor (b lsl 8) lor c : lword )

let read_qword channel = 
  let a = input_byte channel in 
  let b = input_byte channel in 
  let c = input_byte channel in 
  let d = input_byte channel in 
  ( (a lsl 24) lor (b lsl 16) lor (c lsl 8) lor d : qword )

(* Initialisation des différents types de pixels *)

let gray_empty = -1
let rgb_empty = { red = -1 ; green = -1 ; blue = -1 ; }
let gray_alpha_empty = { value = -1 ; alpha = -1 ; }
let rgba_empty = { red = -1 ; green = -1 ; blue = -1 ; alpha = -1 ; }

let init_uncompressed_data size = function 
  | GrayScale       -> ( Array.make_matrix size.h size.w (UncompPix_GrayScale gray_empty) : uncompressed_data )
  | RGB             -> ( Array.make_matrix size.h size.w (UncompPix_RGB rgb_empty) : uncompressed_data )
  | PaletteIndex    -> ( Array.make_matrix size.h size.w (UncompPix_PaletteIndex rgba_empty) : uncompressed_data )
  | GrayScaleAlpha  -> ( Array.make_matrix size.h size.w (UncompPix_GrayScaleAlpha gray_alpha_empty) : uncompressed_data )
  | RGBAlpha        -> ( Array.make_matrix size.h size.w (UncompPix_RGBAlpha rgba_empty) : uncompressed_data )

(* Typage spécifique à OCaml pour les chunks *)

let idChunk_of_strChunk str =
  let id = ref 0 and n = String.length str in 
  for i = 0 to n - 1 do 
    id := ( (Char.code str.[n - 1 - i]) lsl (8 * i) ) lor !id 
  done;
  !id 

let str_of_id cid len_cstr =
  let id = ref cid in 
  let cStr = ref "" in 

  for i = 0 to len_cstr - 1 do
    let v = !id lsr (8 * (len_cstr - 1 - i)) in
    cStr := !cStr ^ (String.make 1 (Char.chr v));
    id := !id - (v lsl (8 * (len_cstr - 1 - i)))
  done;

  !cStr 

let truncate byte bitdepth =
  let mask = (1 lsl bitdepth) - 1 in 
  byte land mask

let conversion v = (float_of_int v) /. 100000.

let show_bin_of_bword bword =
  let rec show_bin_of_bword_rec bword pow_2 str_bin i =
    if i = 8 then str_bin
    else if bword < pow_2 then show_bin_of_bword_rec bword (pow_2 / 2) ("0" ^ str_bin) (i + 1)
    else show_bin_of_bword_rec (bword - pow_2) (pow_2 / 2) ("1" ^ str_bin) (i + 1)
  in 
  show_bin_of_bword_rec bword 128 "" 0

exception Not_a_digit

let ioc c = 
  match c with
  | '0'..'9'  -> (int_of_char c) - 48
  | _         -> raise Not_a_digit

let int_of_strBin str_bin =
  let n = String.length str_bin in 

  let rec int_of_strBin_aux ind int_bin pow_2 =
    if ind = n then int_bin 
    else int_of_strBin_aux (ind + 1) (int_bin + pow_2 * (ioc str_bin.[ind])) (pow_2 * 2)
  in 

  int_of_strBin_aux 0 0 1

let reverse_bits num bits =
  let result = ref 0 in 

  for i = 0 to num - 1 do 
    result := !result lor ( ( (bits lsr (num - i - 1)) land 1 ) lsl i)
  done;

  !result

let rgbaPalette_of_rgbPalette trns_chunk_values plte_chunk_data =
  let rgb_plte_chunk = (match plte_chunk_data with | PLTE rgb_array -> rgb_array | _ -> failwith "read_trns_chunk" ) in 
  let n = Array.length rgb_plte_chunk 
  and m = Array.length trns_chunk_values in 

  ( if n < m then raise TRNS_WrongLength );

  let plte_chunk_alpha = Array.make n rgba_empty in 

  for i = 0 to n - 1 do 
    plte_chunk_alpha.(i) <- {
      red = rgb_plte_chunk.(i).red;
      green = rgb_plte_chunk.(i).green;
      blue = rgb_plte_chunk.(i).blue;
      alpha = (if i < m then trns_chunk_values.(i) else 255);
    }
  done;

  PLTE plte_chunk_alpha

let zlib_data_of_comp_data (comp_data : compressed_data) =
  let ind_comp = ref 0 in
  
  let comp_data = (match comp_data with | BrutData db -> db | _ -> failwith "zlib_data_of_comp_data") in 
  let len_chunk = Array.length comp_data in 

  let c = comp_data.(!ind_comp) in
  incr ind_comp; 

  let f = comp_data.(!ind_comp) in 
  incr ind_comp;
  
  let cm = truncate c 4
  and ci = c lsr 4 in 

  let fc = truncate f 5
  and fd = ( ( (truncate f 6) lsr 5 ) = 1 )
  and fl = f lsr 6 in 

  let dict = ref None in 

  (
    if fd then (
      let len_dict = 4 in 
      let dict_data = Array.make len_dict (-1) in 
      for i = 0 to len_dict - 1 do 
        dict_data.(i) <- comp_data.(!ind_comp);
        incr ind_comp
      done;
      dict := Some dict_data
    )
  );

  let d = Array.sub comp_data !ind_comp (len_chunk - (2 + (if fd then 4 else 0) + 4)) in 
  let a = Array.sub comp_data (len_chunk - 5) 4 in 
  {
    zlibCompressionMethod = cm;
    zlibCompressionInfo = ci;
    zlibFCheck = fc;
    zlibFDict = fd;
    zlibZlibDict = !dict;
    zlibFLevel = fl;
    zlibData = d;
    zlibAdler = a;
  }

let ceil_int value div = 
  int_of_float (ceil ( (float_of_int value) /. (float_of_int div) ))

let filter_of_int = function 
  | 0 -> NoFilter 
  | 1 -> Sub 
  | 2 -> Up 
  | 3 -> Average 
  | 4 -> Paeth 
  | _ -> raise FilterType_BadFilterMethod

let color_of_int = function 
  | 0 -> GrayScale 
  | 2 -> RGB 
  | 3 -> PaletteIndex
  | 4 -> GrayScaleAlpha
  | 6 -> RGBAlpha
  | _ -> raise ColorType_BadColorType

let rec depth_in_set depth = function 
  | [] -> false 
  | bd::_ when bd = depth -> true 
  | _::l -> depth_in_set depth l  

let check_bitdepth depth = function 
  | GrayScale       -> depth_in_set depth [1 ; 2 ; 4 ; 8 ; 16]
  | RGB             -> depth_in_set depth [8 ; 16]
  | PaletteIndex    -> depth_in_set depth [1 ; 2 ; 4 ; 8]
  | GrayScaleAlpha  -> depth_in_set depth [8 ; 16]
  | RGBAlpha        -> depth_in_set depth [8 ; 16]