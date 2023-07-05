open Core

(* You need to modify this function to blur the input image based on the
   provided radius instead of ignoring it. *)
let transform image ~radius =
  let blurred = Image.copy image in
  let _ =
    Image.mapi image ~f:(fun ~x ~y pixel ->
      let start_x = Int.max (x - radius) 0 in
      let end_x =
        if x + radius > Image.width image - 1
        then Image.width image - 1
        else x + radius
      in
      let start_y = Int.max (y - radius) 0 in
      let end_y =
        if y + radius > Image.height image - 1
        then Image.height image - 1
        else y + radius
      in
      let sliced =
        Image.slice
          image
          ~x_start:start_x
          ~x_end:end_x
          ~y_start:start_y
          ~y_end:end_y
      in
      let average = Image.mean_pixel sliced in
      Image.set blurred ~x ~y average;
      pixel)
  in
  blurred
;;

let command =
  Command.basic
    ~summary:"Blur an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      and radius =
        flag
          "radius"
          (required Command.Param.int)
          ~doc:"N the radius to use when blurring (higher = more blurred)"
      in
      fun () ->
        let image = Image.load_ppm ~filename in
        let image' = transform image ~radius in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_blur.ppm")]
;;
