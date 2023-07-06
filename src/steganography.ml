open Core

let transform image =
  Image.map image ~f:(fun (r, g, b) ->
    let red = r % 4 * 64 in
    let green = g % 4 * 64 in
    let blue = b % 4 * 64 in
    red, green, blue)
;;

let command =
  Command.basic
    ~summary:"Find hidden immage"
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
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_hidden.ppm")]
;;
