let soi = string_of_int

(*
let s2e = ReasonReact.stringToElement

let value_of_event event = (ReactDOMRe.domElementToObj (ReactEventRe.Form.target event))##value

let name_of_event event = (ReactDOMRe.domElementToObj (ReactEventRe.Form.target event))##name

let checked_of_event event = (ReactDOMRe.domElementToObj (ReactEventRe.Form.target event))##checked
*)

let rec zip l1 l2 = match (l1, l2) with
  | ([], []) -> []
  | (x :: xs, y :: ys) -> (x, y) :: (zip xs ys)
  | _ -> failwith "zip"

let join_ sep f l if_emp = match l with
  | [] ->
    if_emp
  | x :: xs ->
    let e = List.fold_left (fun s a -> s ^ sep ^ (f a)) (f x) xs in
    e

let string_of_list f l = join_ "; " f l "[]"
let join sep f l = join_ sep f l ""
