open Core

let inverter number max_value threshold =
  if number > threshold then max_value - number else number
;;

let converter r g b max_threshold =
  let red = inverter r max_threshold (max_threshold * 3 / 10) in
  let blue = inverter b max_threshold (max_threshold * 3 / 10) in
  let green = inverter g max_threshold (max_threshold * 3 / 10) in
  red, green, blue
;;

let transform image =
  let max_threshold = Image.max_val image in
  Image.map image ~f:(fun (r, g, b) -> converter r g b max_threshold)
;;

let command =
  Command.basic
    ~summary:"Convert an image to a solarized version"
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
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_solar.ppm")]
;;
