let soi = string_of_int

(*
let s2e = ReasonReact.stringToElement

let value_of_event event = (ReactDOMRe.domElementToObj (ReactEventRe.Form.target event))##value

let name_of_event event = (ReactDOMRe.domElementToObj (ReactEventRe.Form.target event))##name

let checked_of_event event = (ReactDOMRe.domElementToObj (ReactEventRe.Form.target event))##checked
*)

let string_of_list f l = match l with
  | [] ->
    "[]"
  | x :: xs ->
    let e = List.fold_left (fun s a -> s ^ "; " ^ (f a)) (f x) xs in
    "[" ^ e ^ "]"

