(** ANSI escape code for red text *)
let red = "\027[31m"

(** ANSI escape code for green text *)
let green = "\027[32m"

(** ANSI escape code to reset text formatting *)
let reset = "\027[0m"

(** Colors the given text with the specified ANSI color code and resets
    formatting afterward *)
let color_text color text = color ^ text ^ reset
