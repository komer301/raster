open Core

let check_bounds image x y =
  not (x < 0 || x >= Image.width image || y < 0 || y >= Image.height image)
;;

let set_adjacent image x y error =
  (* Checking east (right) *)
  if check_bounds image (x + 1) y
  then
    Image.set
      image
      ~x:(x + 1)
      ~y
      (Pixel.( + )
         (Image.get image ~x:(x + 1) ~y)
         (Pixel.of_int (Float.to_int (error *. 7.0) / 16)));
  (* Checking southwest (bottom-left) *)
  if check_bounds image (x - 1) (y + 1)
  then
    Image.set
      image
      ~x:(x - 1)
      ~y:(y + 1)
      (Pixel.( + )
         (Image.get image ~x:(x - 1) ~y:(y + 1))
         (Pixel.of_int (Float.to_int (error *. 3.0) / 16)));
  (* Checking south (bottom) *)
  if check_bounds image x (y + 1)
  then
    Image.set
      image
      ~x
      ~y:(y + 1)
      (Pixel.( + )
         (Image.get image ~x ~y:(y + 1))
         (Pixel.of_int (Float.to_int (error *. 5.0) / 16)));
  (* Checking southeast (bottom right) *)
  if check_bounds image (x + 1) (y + 1)
  then
    Image.set
      image
      ~x:(x + 1)
      ~y:(y + 1)
      (Pixel.( + )
         (Image.get image ~x:(x + 1) ~y:(y + 1))
         (Pixel.of_int (Float.to_int (error *. 1.0) / 16)))
;;

let set_pixel_and_error
  image
  pixel
  x
  y
  max_threshold
  (white_out : Pixel.t)
  blacked
  =
  let pixel_value = Pixel.red pixel in
  if pixel_value > max_threshold / 2
  then (
    set_adjacent image x y (Int.to_float (pixel_value - max_threshold));
    white_out)
  else (
    set_adjacent image x y (Int.to_float pixel_value);
    blacked)
;;

(* This should look familiar by now! *)
let transform image =
  let grayed_out = Grayscale.transform image in
  let max_threshold = Image.max_val image in
  let white_out = Pixel.of_int max_threshold in
  let blacked = Pixel.zero in
  Image.mapi grayed_out ~f:(fun ~x ~y pixel ->
    set_pixel_and_error grayed_out pixel x y max_threshold white_out blacked)
;;

(* let _ = Image.mapi grayed_out ~f:(fun ~x ~y value -> let pixel_original =
   Pixel in if value >. 0.5 then value = 1.0 else Pixel.zero in let
   calc_error = pixel_original - Pixel in if x + 1 <= Image.width - 1 then
   Image.set grayed_out ~x:(x+1) ~y:y Pixel = Pixel + (calc_error * 7/16) in
   if x -1 >= 0 && y -1 <= Image.height - 1 then Image.set grayed_out
   ~x:(x-1) ~y:(y+1) Pixel = Pixel + (calc_error * 3/16) in if y - 1 <=
   Image.height -1 then Image.set grayed_out ~x:(x) ~y:(y+1) Pixel = Pixel +
   (calc_error * 5/16) in if x + 1 <= Image.width -1 && y -1 <= Image.height
   - 1 then Image.set grayed_out ~x:(x+1) ~y:(y+1) Pixel = Pixel +
   (calc_error * 1/16) in ) in grayed_out *)

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
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_dither.ppm")]
;;
