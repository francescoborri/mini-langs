module ColorText = ColorText

let ( >> ) f g x = g (f x)
let id = fun x -> x

let write_to_file filename content =
  let oc = open_out filename in
  output_string oc content;
  close_out oc

let expand_speclist speclist =
  List.concat_map
    (fun (keys, spec, doc) -> List.map (fun flag -> (flag, spec, doc)) keys)
    speclist
