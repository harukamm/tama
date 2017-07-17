open Types

(*

type ast_t =
  | Int of int * loc_info
  | Var of string * loc_info
  | Plus of ast_t * ast_t * loc_info
  | Minus of ast_t * ast_t * loc_info
  | Times of ast_t * ast_t * loc_info
  | Divide of ast_t * ast_t * loc_info
  | If of ast_t * ast_t * ast_t * loc_info
  | Let of string * string list * ast_t * ast_t * bool * loc_info
  | Declare of string * string list * ast_t * bool * loc_info
  | Block of ast_t list * loc_info
  | True of loc_info
  | False of loc_info
  | GreaterThan of ast_t * ast_t * bool * loc_info
  | LessThan of ast_t * ast_t * bool * loc_info
  | Equal of ast_t * ast_t * loc_info
  | App of ast_t * ast_t list * loc_info

*)

let count = ref 0

let inc_count () = count := !count + 1

let add (k, v) maps = (k, v) :: maps

let global_m : (string * string) list ref = ref []

let add_global (k, v) = global_m := (k, v) :: !global_m

let get_ k maps =
  try
    Some (List.assoc k maps)
  with Not_found -> None

let get k maps =
  match get_ k maps with
  | None -> get_ k !global_m
  | Some x -> Some x

let gensym s =
  let cnt = !count in
  let name = s ^ "_" ^ (string_of_int cnt) in
  let () = inc_count () in
  name

let rec alpha t m = match t with
  | Int (n, l) ->
    Int (n, l)
  | Var (v, l) ->
    begin
      match (get v m) with
      | None -> failwith ("Unbound value " ^ v)
      | Some v' -> Var (v', l)
    end
  | Plus (e1, e2, l) ->
    Plus (alpha e1 m, alpha e2 m, l)
  | Minus (e1, e2, l) ->
    Minus (alpha e1 m, alpha e2 m, l)
  | Times (e1, e2, l) ->
    Times (alpha e1 m, alpha e2 m, l)
  | Divide (e1, e2, l) ->
    Divide (alpha e1 m, alpha e2 m, l)
  | If (e1, e2, e3, l) ->
    If (alpha e1 m, alpha e2 m, alpha e3 m, l)
  | Let (x, xs, e1, e2, is_rec, l) ->
    let x' = gensym x in
    let xs' = List.map gensym xs in
    let kvs = Util.zip (x :: xs) (x' :: xs') in
    let m' = if is_rec then add (x, x') m else m in
    let m1 = List.fold_left (fun m kv -> add kv m) m' kvs in
    let m2 = add (x, x') m in
    Let (x', xs', alpha e1 m1, alpha e2 m2, is_rec, l)
  | Declare (x, xs, e1, is_rec, l) ->
    let x' = gensym x in
    let _ = add_global (x, x') in
    let xs' = List.map gensym xs in
    let kvs = Util.zip (x :: xs) (x' :: xs') in
    let m' = if is_rec then add (x, x') m else m in
    let m1 = List.fold_left (fun m kv -> add kv m) m' kvs in
    Declare (x', xs', alpha e1 m1, is_rec, l)
  | Block (es, l) ->
    begin
      let h (m, ts) e =
        let e' = alpha e m in
        let m' = match (e, e') with
          | (Let (x, _, _, _, _, _), Let (x', _, _, _, _, _))
          | (Declare (x, _, _, _, _), Declare (x', _, _, _, _)) -> add (x, x') m
          | _ -> m
        in
        (m', e' :: ts)
      in
      let (_, rts) = List.fold_left h (m, []) es in
      Block (List.rev rts, l)
    end
  | GreaterThan (e1, e2, eq, l) ->
    GreaterThan (alpha e1 m, alpha e2 m, eq, l)
  | LessThan (e1, e2, eq, l) ->
    LessThan (alpha e1 m, alpha e2 m, eq, l)
  | Equal (e1, e2, l) ->
    Equal (alpha e1 m, alpha e2 m, l)
  | App (x, xs, l) ->
    App (alpha x m, List.map (fun x -> alpha x m) xs, l)
  | True (l) ->
    True (l)
  | False (l) ->
    False (l)

let alpha_main t =
  global_m := [];
  count := 0;
  alpha t []
(*
let rec declares t = match t with
  | Int (n, l) ->
  | Var (v, l) ->
  | Plus (e1, e2, l) ->
  | Minus (e1, e2, l) ->
  | Times (e1, e2, l) ->
  | Divide (e1, e2, l) ->
  | If (e1, e2, e3, l) ->
  | Let (x, xs, e1, e2, is_rec, l) ->
  | Declare (x, xs, e1, is_rec, l) ->
  | Block (es, l) ->
  | GreaterThan (e1, e2, eq, l) ->
  | LessThan (e1, e2, eq, l) ->
  | Equal (e1, e2, l) ->
  | App (x, xs, l) ->
  | True (l) ->
  | False (l) ->
*)

let rec fvars t = match t with
  | Int (n, l) -> []
  | Var (v, l) -> [v]
  | Plus (e1, e2, l) -> (fvars e1) @ (fvars e2)
  | Minus (e1, e2, l) -> (fvars e1) @ (fvars e2)
  | Times (e1, e2, l) -> (fvars e1) @ (fvars e2)
  | Divide (e1, e2, l) -> (fvars e1) @ (fvars e2)
  | If (e1, e2, e3, l) -> (fvars e1) @ (fvars e2) @ (fvars e3)
  | Let (x, xs, e1, e2, is_rec, l) ->
    
  | Declare (x, xs, e1, is_rec, l) ->
  | Block (es, l) ->
  | GreaterThan (e1, e2, eq, l) ->
  | LessThan (e1, e2, eq, l) ->
  | Equal (e1, e2, l) ->
  | App (x, xs, l) ->
  | True (l) ->
  | False (l) ->

