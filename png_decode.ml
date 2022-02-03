#use "png_inflate.ml"
#load "graphics.cma"
open Graphics

let get_sample = function 
  | GrayScale       -> 1
  | RGB             -> 3
  | PaletteIndex    -> 1
  | GrayScaleAlpha  -> 2
  | RGBAlpha        -> 4

let split_scanline (scanline : deflate_compressed_data) pixels depth = 
  let channel_scanline = Array.make pixels 0 in 
  let reader = BitReader.init (Array.map (reverse_bits 8) scanline) (Array.length scanline) in 

  for i = 0 to pixels - 1 do 
    let _ = BitReader.ensureBits9 reader depth in 
    channel_scanline.(i) <- reverse_bits depth (BitReader.readBits reader depth)
  done;
  (channel_scanline : deflate_compressed_data)

let paeth_predictor a b c =
  let p = a + b + c in 
  let pa = abs (p - a) in 
  let pb = abs (p - b) in 
  let pc = abs (p - c) in 

  if pa <= pb && pa <= pc then a 
  else if pb <= pc then b 
  else c 

let apply_filter_reverse (scanline : deflate_compressed_data) bpp filter_fun (previous_scanline_opt : deflate_compressed_data option) = 
  let len_scan = Array.length scanline in 

  match filter_fun with 
    | NoFilter    -> ()

    | Sub     -> 
          for i = bpp to len_scan - 1 do 
            scanline.(i) <- ( scanline.(i) + scanline.(i - bpp) ) mod 256
          done

    | Up      -> 
        begin
          match previous_scanline_opt with 
            | None -> ()
            | Some previous_scanline ->
                for i = 0 to len_scan - 1 do 
                  scanline.(i) <- ( scanline.(i) + previous_scanline.(i) ) mod 256
                done
        end
            
    | Average ->
        begin 
          match previous_scanline_opt with 
            | None -> ()
            | Some previous_scanline ->
                for i = 0 to bpp - 1 do 
                  scanline.(i) <- ( scanline.(i) + (previous_scanline.(i) lsr 1) ) mod 256
                done;
                for i = bpp to len_scan - 1 do 
                  scanline.(i) <- ( scanline.(i) + ( (scanline.(i - bpp) + previous_scanline.(i)) lsr 1 ) ) mod 256
                done
        end

    | Paeth   -> 
        begin
          match previous_scanline_opt with 
            | None -> 
                for i = bpp to len_scan - 1 do 
                  scanline.(i) <- ( scanline.(i) + scanline.(i - bpp) ) mod 256
                done

            | Some previous_scanline ->
                for i = 0 to bpp - 1 do 
                  scanline.(i) <- ( scanline.(i) + previous_scanline.(i) ) mod 256
                done;

                let length = (
                  if len_scan mod bpp = 0 then (len_scan / bpp) - 1 
                  else len_scan / bpp
                ) in 

                for i = 1 to length do 
                  let j = i - 1 in 
                  let s0 = scanline.(bpp * i + 0) and r0 = scanline.(bpp * j + 0) and p0 = previous_scanline.(bpp * i + 0) and q0 = previous_scanline.(bpp * j + 0) in 
                  if bpp > 1 then (
                    let s1 = scanline.(bpp * i + 1) and r1 = scanline.(bpp * j + 1) and p1 = previous_scanline.(bpp * i + 1) and q1 = previous_scanline.(bpp * j + 1) in 
                    if bpp > 2 then (
                      let s2 = scanline.(bpp * i + 2) and r2 = scanline.(bpp * j + 2) and p2 = previous_scanline.(bpp * i + 2) and q2 = previous_scanline.(bpp * j + 2) in 
                      if bpp > 3 then (
                        let s3 = scanline.(bpp * i + 3) and r3 = scanline.(bpp * j + 3) and p3 = previous_scanline.(bpp * i + 3) and q3 = previous_scanline.(bpp * j + 3) in 
                        scanline.(bpp * i + 3) <- ( s3 + (paeth_predictor r3 p3 q3) ) mod 256
                      );
                      scanline.(bpp * i + 2) <- ( s2 + (paeth_predictor r2 p2 q2) ) mod 256
                    );
                    scanline.(bpp * i + 1) <- ( s1 + (paeth_predictor r1 p1 q1) ) mod 256
                  );
                  scanline.(bpp * i + 0) <- ( s0 + (paeth_predictor r0 p0 q0) ) mod 256
                done
        end

let decode_deflate_idat_data (palette_opt : pngPLTEChunk option) colorType depth size (deflate_data : deflate_compressed_data) =
  (*let len_data = Array.length deflate_data in *)
  let i = ref 0 in 
  let line = ref 0 in 
  let data = init_uncompressed_data size colorType in
  let previous_scanline_opt = ref None in 

  while !line < size.h do 
    let filter_method = filter_of_int deflate_data.(!i) in 
    incr i;
    let sample = get_sample colorType in 
    let len_bytes_line = ceil_int (depth * size.w * sample) 8 in 
    let scanline = Array.sub deflate_data !i len_bytes_line in
    i := !i + len_bytes_line;

    let channel_scanline = split_scanline scanline (size.w * sample) depth in 
    
    apply_filter_reverse channel_scanline (ceil_int (sample * depth) 8) filter_method !previous_scanline_opt;
    previous_scanline_opt := Some channel_scanline;
    

    if colorType = PaletteIndex then (
      match palette_opt with 
        | Some palette -> 
            for j = 0 to size.w - 1 do 
              data.(!line).(j) <- UncompPix_PaletteIndex palette.(channel_scanline.(j))
            done
        | None -> raise DecodeData_PLTEAbsent
    ) else (
      for j = 0 to size.w - 1 do
        match colorType with 
          | GrayScale       -> data.(!line).(j) <- UncompPix_GrayScale channel_scanline.(j)
          | RGB             -> data.(!line).(j) <- UncompPix_RGB { 
                                                                  red = channel_scanline.(sample * j + 0);
                                                                  green = channel_scanline.(sample * j + 1);
                                                                  blue = channel_scanline.(sample * j + 2);
                                                                }
          | PaletteIndex    -> assert false
          | GrayScaleAlpha  -> data.(!line).(j) <- UncompPix_GrayScaleAlpha  {
                                                                              value = channel_scanline.(sample * j + 0);
                                                                              alpha = channel_scanline.(sample * j + 1);
                                                                            }
          | RGBAlpha        -> data.(!line).(j) <- UncompPix_RGBAlpha  {
                                                                        red = channel_scanline.(sample * j + 0);
                                                                        green = channel_scanline.(sample * j + 1);
                                                                        blue = channel_scanline.(sample * j + 2);
                                                                        alpha = channel_scanline.(sample * j + 3)
                                                                      }
      done
    );
    incr line
  done;

  (data : uncompressed_data)

let decode_inflate_data png_file =

  let get_deflate_idat_data idat_chunk =
    match idat_chunk.pcData with 
      | IDAT (DeflateData data) -> data 
      | _ -> assert false
  in

  let palette_opt = (
    match png_file.pngPLTE with 
      | Some plte -> ( match plte.pcData with PLTE palette -> Some palette | _ -> assert false )
      | None -> None
  ) in 

  let decode_idat_data = 
    png_file.pngData  |> List.map get_deflate_idat_data 
                      |> Array.concat 
                      |> decode_deflate_idat_data palette_opt png_file.pngIH.piColorType png_file.pngIH.piBitDepth { w = png_file.pngIH.piWidth ; h = png_file.pngIH.piHeight }
  in

  {
    pngFH = png_file.pngFH;
    pngIH = png_file.pngIH;
    pngPLTE = png_file.pngPLTE;
    pngStatus = Uncompressed;
    pngData = [{
      pcLengthChunk = png_file.pngIH.piWidth * png_file.pngIH.piHeight;
      pcChunkId = idat_id;
      pcChunkName = "IDAT";
      pcData = IDAT (UncompressedData decode_idat_data);
      pcCRC = 0;
    }];
    pngSetOption = png_file.pngSetOption;
    pngInfoOption = png_file.pngInfoOption;
    pngUnknownChunkList = png_file.pngUnknownChunkList;
    pngIENDPresent = png_file.pngIENDPresent;
  }

let show_me_file png_file = 
  let matrix = ( match (List.hd png_file.pngData).pcData with IDAT (UncompressedData m) -> m | _ -> assert false ) in 
  let h = Array.length matrix in 
  let w = Array.length matrix.(0) in 

  let file = open_out "test" in 
  for i = 0 to h - 1 do 
    for j = 0 to w - 1 do 
      let str_pixel = ref "" in 
      (
        match matrix.(i).(j) with 
          | UncompPix_RGBAlpha pixel -> str_pixel := "(" ^ (string_of_int pixel.red) 
                                                      ^ ", " ^ (string_of_int pixel.green)
                                                      ^ ", " ^ (string_of_int pixel.blue) ^ ") "
          | UncompPix_RGB pixel -> str_pixel := "(" ^ (string_of_int pixel.red) 
                                                ^ ", " ^ (string_of_int pixel.green)
                                                ^ ", " ^ (string_of_int pixel.blue) ^ ") "
          | UncompPix_PaletteIndex pixel -> str_pixel := "(" ^ (string_of_int pixel.red) 
                                              ^ ", " ^ (string_of_int pixel.green)
                                              ^ ", " ^ (string_of_int pixel.blue) ^ ") "
          | _ -> assert false 
      );
      output_string file !str_pixel
    done;
    output_string file "\n"
  done;
  close_out file

let show_me png_file =
  let matrix = ( match (List.hd png_file.pngData).pcData with IDAT (UncompressedData m) -> m | _ -> assert false ) in 
  let h = Array.length matrix in 
  let w = Array.length matrix.(0) in 

  let s = " " ^ (string_of_int (w + 10)) ^ "x" ^ (string_of_int (h + 10)) ^ "+0+0" in 

  open_graph s;
  let window_title = "Pixel matrix Viewer (" ^
                        (string_of_int w) ^ "x" ^
                        (string_of_int h) ^ ")" in 
  set_window_title (window_title);

  for i = 0 to h - 1 do 
    for j = 0 to w - 1 do 
      (
        match matrix.(i).(j) with 
          | UncompPix_RGBAlpha pixel -> set_color (rgb pixel.red pixel.green pixel.blue)
          | UncompPix_RGB pixel -> set_color (rgb pixel.red pixel.green pixel.blue)
          | UncompPix_PaletteIndex pixel -> set_color (rgb pixel.red pixel.green pixel.blue)
          | _ -> print_string "yes" ; assert false 
      );
      plot (j + 5) (h - i + 5)
    done
  done;

  let _ = read_key () in close_graph ()