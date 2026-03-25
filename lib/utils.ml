module ColorText = ColorText

let ( >> ) f g x = g (f x)
let id = fun x -> x
