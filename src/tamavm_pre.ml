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

exception Not_Supported of (string * loc_info)

let access_fvars_error = "not supported accessing free variables"

let application_error = "not supported expression"

let rem_included l =
  List.filter (fun (x, _) -> not (List.mem x l))

let only_included l =
  List.filter (fun (x, _) -> List.mem x l)

let rec check_boudings t m = match t with
  | Int (n, l) ->
    []
  | Var (v, l) ->
    [(v, l)]
  | Plus (e1, e2, l) ->
    (check_boudings e1 m) @ (check_boudings e2 m)
  | Minus (e1, e2, l) ->
    (check_boudings e1 m) @ (check_boudings e2 m)
  | Times (e1, e2, l) ->
    (check_boudings e1 m) @ (check_boudings e2 m)
  | Divide (e1, e2, l) ->
    (check_boudings e1 m) @ (check_boudings e2 m)
  | If (e1, e2, e3, l) ->
    (check_boudings e1 m) @ (check_boudings e2 m) @ (check_boudings e3 m)
  | Let (x, xs, e1, e2, is_rec, l) ->
    let pairs1 = check_boudings e1 m in
    let pairs2 = check_boudings e2 m in
    let bvs = if is_rec then (x :: xs) else xs in
    let vs1 = rem_included (m @ bvs) pairs1 in
    let vs2 = rem_included (x :: m) pairs2 in
    begin
      match (vs1, vs2) with
      | ((v, l) :: vs, _)
      | ([], ((v, l) :: vs)) -> raise (Not_Supported (access_fvars_error, l))
      | _ -> vs1 @ vs2
    end
  | Declare (x, xs, e1, is_rec, l) ->
    let pairs1 = check_boudings e1 m in
    let bvs = if is_rec then (x :: xs) else xs in
    let vs1 = rem_included (m @ bvs) pairs1 in
    begin
      match vs1 with
      | [] -> []
      | (v, l) :: vs -> raise (Not_Supported (access_fvars_error, l))
    end
  | Block (es, l) ->
    begin
      let h m' e = match e with
        | Let (x, _, _, _, _, _)
        | Declare (x, _, _, _, _) -> x :: m'
        | _ -> m'
      in
      let m1 = List.fold_left h m es in
      let vss = List.map (fun e -> check_boudings e m1) es in
      List.concat vss
    end
  | GreaterThan (e1, e2, _, l) | LessThan (e1, e2, _, l)
  | Equal (e1, e2, l) ->
    (check_boudings e1 m) @ (check_boudings e2 m)
  | App (x, xs, l) ->
    let vss = List.map (fun e -> check_boudings e m) (x :: xs) in
    List.concat vss
  | True (l) ->
    []
  | False (l) ->
    []

type ast_sym_t =
  | SInt | SVar | SPlus | SMinus | STimes | SDivide
  | SIf | SLet | SDeclare | SBlock | SGreaterThan | SLessThan
  | SEqual | SApp | STrue | SFalse

let rec heads t = match t with
  | Int (_, l) -> [(SInt, l)]
  | Var (_, l) -> [(SVar, l)]
  | Plus (e1, e2, l) -> (SPlus, l) :: (heads e1) @ (heads e2)
  | Minus (e1, e2, l) -> (SMinus, l) :: (heads e1) @ (heads e2)
  | Times (e1, e2, l) -> (STimes, l) :: (heads e1) @ (heads e2)
  | Divide (e1, e2, l) -> (SDivide, l) :: (heads e1) @ (heads e2)
  | If (e1, e2, e3, l) -> (SIf, l) :: (heads e1) @ (heads e2) @ (heads e3)
  | Let (_, _, e1, e2, _, l) -> (SLet, l) :: (heads e1) @ (heads e2)
  | Declare (_, _, e1, _, l) -> (SDeclare, l) :: (heads e1)
  | Block (es, l) -> (SBlock, l) :: (List.concat (List.map heads es))
  | GreaterThan (e1, e2, _, l) -> (SGreaterThan, l) :: (heads e1) @ (heads e2)
  | LessThan (e1, e2, _, l) -> (SLessThan, l) :: (heads e1) @ (heads e2)
  | Equal (e1, e2, l) -> (SEqual, l) :: (heads e1) @ (heads e2)
  | App (x, xs, l) -> (SApp, l) :: (List.concat (List.map heads (x :: xs)))
  | True (l) -> [(STrue, l)]
  | False (l) -> [(SFalse, l)]

let occur_other_sym syms e =
  let slst = heads e in
  let slst' = rem_included syms slst in
  match slst' with
  | [] -> None
  | x :: xs -> Some (x)

let occur_sym syms e =
  let slst = heads e in
  let slst' = only_included syms slst in
  match slst' with
  | [] -> None
  | x :: xs -> Some (x)

let rec check t = match t with
  | Int _ ->
    ()
  | Var _ ->
    ()
  | Plus (e1, e2, l) | Minus (e1, e2, l)
  | Times (e1, e2, l) | Divide (e1, e2, l) ->
    let _ = check e1 in
    let _ = check e2 in
    ()
  | If (e1, e2, e3, l) ->
    let _ = check e1 in
    let _ = check e2 in
    let _ = check e3 in
    ()
  | Let (x, xs, e1, e2, is_rec, l) ->
    let _ = check e1 in
    let _ = check e2 in
    ()
  | Declare (x, xs, e1, is_rec, l) ->
    let _ = check e1 in
    ()
  | Block (es, _) ->
    List.iter check es
  | GreaterThan (e1, e2, _, _) | LessThan (e1, e2, _, _)
  | Equal (e1, e2, _) ->
    begin
      let invalids = [SLet; SApp] in
      let (x1, x2) = (occur_sym invalids e1, occur_sym invalids e2) in
      let h x = match x with
        | None -> ()
        | Some (_, l) -> raise (Not_Supported (application_error, l))
      in
      let _ = h x1 in
      let _ = h x2 in
      ()
    end
  | App (x, xs, _) ->
    let ex = List.map (occur_sym [SLet; SApp]) (x :: xs) in
    begin
      let h x = match x with
        | None -> ()
        | Some (_, l) -> raise (Not_Supported (application_error, l))
      in
      List.iter h ex
    end
  | True (l) ->
    ()
  | False (l) ->
    ()

let main t =
  let t' = alpha_main t in
  let _ = check_boudings t' [] in
  let _ = check t' in
  t'

