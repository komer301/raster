open Core

(* This should look familiar by now! *)
let transform image = 
  let grayed_out = Grayscale.transform image in
  let _ = Image.mapi grayed_out ~f:(fun ~x ~y value ->
    let pixel_original = Pixel in 
    if value >. 0.5 then value = 1.0 else Pixel.zero in
    let calc_error = pixel_original - Pixel in 
    if x + 1 <= Image.width - 1
      then Image.set grayed_out ~x:(x+1) ~y:y Pixel = Pixel + (calc_error * 7/16) in
    if x -1 >= 0 && y -1 <= Image.height - 1
      then Image.set grayed_out ~x:(x-1) ~y:(y+1) Pixel = Pixel + (calc_error * 3/16) in
    if y - 1 <= Image.height -1 
      then Image.set grayed_out ~x:(x) ~y:(y+1) Pixel = Pixel + (calc_error * 5/16) in
    if x + 1 <= Image.width -1 && y -1 <= Image.height - 1
      then Image.set grayed_out ~x:(x+1) ~y:(y+1) Pixel = Pixel + (calc_error * 1/16) in
    ) in
    grayed_out
;;

let command =
  Command.basic
    ~summary:"Dither an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      in
      fun () ->
        let image = Image.load_ppm ~filename |> transform in
        Image.save_ppm
          image
          ~filename:(String.chop_suffix_exn filename ~suffix:".ppm" ^ "_dither.ppm")]
;;
