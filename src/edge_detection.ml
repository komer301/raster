open Core

let off_sets image x y =
  [ (* Top Row *)
    Image.get image ~x:(x - 1) ~y:(y - 1)
  ; Image.get image ~x:(x - 1) ~y
  ; Image.get image ~x:(x - 1) ~y:(y + 1)
  ; (* Middle Row *)
    Image.get image ~x ~y:(y - 1)
  ; Image.get image ~x ~y
  ; Image.get image ~x ~y:(y + 1)
  ; (* Bottom Row *)
    Image.get image ~x:(x + 1) ~y:(y - 1)
  ; Image.get image ~x:(x + 1) ~y
  ; Image.get image ~x:(x + 1) ~y:(y + 1)
  ]
;;

let check_borders image x y =
  not
    (x = 0
     || x = Image.width image - 1
     || y = 0
     || y = Image.height image - 1)
;;

let g_x_offset image x y =
  let x_offset_values = [ -1; 0; 1; -2; 0; 2; -1; 0; 1 ] in
  let positions = off_sets image x y in
  let final_values =
    List.map2_exn x_offset_values positions ~f:(fun off_set_value pixel ->
      Pixel.red pixel * off_set_value)
  in
  List.fold final_values ~init:0 ~f:(fun first second -> first + second)
;;

let g_y_offset image x y =
  let y_offset_values = [ -1; -2; -1; 0; 0; 0; 1; 2; 1 ] in
  let positions = off_sets image x y in
  let final_values =
    List.map2_exn y_offset_values positions ~f:(fun off_set_value pixel ->
      Pixel.red pixel * off_set_value)
  in
  List.fold final_values ~init:0 ~f:(fun first second -> first + second)
;;

let g_calculator
  image
  x
  y
  user_threshold
  (white_out : Pixel.t)
  (blacked : Pixel.t)
  =
  if check_borders image x y
  then (
    let g_x = g_x_offset image x y in
    let g_y = g_y_offset image x y in
    let final_g_value =
      sqrt ((Int.to_float g_x ** 2.0) +. (Int.to_float g_y ** 2.0))
    in
    if Float.( >. ) final_g_value (Int.to_float user_threshold *. 0.4)
    then white_out
    else blacked)
  else white_out
;;

let transform image =
  let grayed_out = Grayscale.transform image in
  let main_image = Blur.transform grayed_out ~radius:2 in
  let max_threshold = Image.max_val image in
  let white_out = Pixel.of_int max_threshold in
  let blacked = Pixel.zero in
  Image.mapi main_image ~f:(fun ~x ~y (_ : Pixel.t) ->
    g_calculator main_image x y max_threshold white_out blacked)
;;

let command =
  Command.basic
    ~summary:"Convert an image to edge detection"
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
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_edge.ppm")]
;;
