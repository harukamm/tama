open Types

exception Out_of_Index

exception Unexpected of token_t

exception Not_Found_Match of string

exception Has_No_Token

let ptr = ref 0

let tarr : token_t array ref = ref [||]

let set_tkn arr = tarr := arr

let is_safe index = index < Array.length !tarr

let index () = !ptr

let ahead_with n = ptr := !ptr + n

let ahead () = ahead_with 1

let get_with index =
  if is_safe index then !tarr.(index) else (raise Out_of_Index)

let get () =
  get_with (index ())

let nearest_index () =
  let rec h ind' =
    let t = get_with ind' in
    if skip_tkn t then h (ind' + 1)
    else Some (ind')
  in
  try
    h (index ())
  with Out_of_Index -> None

let is_end () = nearest_index () = None

let rec consume () =
  let t = get () in
  let () = ahead () in
  if skip_tkn t then consume ()
  else t

let set pt = ptr := pt

let init ts =
  set 0;
  set_tkn (Array.of_list ts)

(* accept: (token_t -> bool) -> bool *)
let accept p =
  try
    let ind = index () in
    if p (consume ()) then true else (set ind; false)
  with Out_of_Index ->
    false

(* expect: (token_t -> bool) -> token_t *)
let expect p =
  let ind = index () in
  let x = consume () in
  if p x then x else (set ind; raise (Unexpected x))

(* consume_mult: int -> token_t list *)
let consume_mult n =
  let rec h cnt =
    if n <= cnt then []
    else
      let t = consume () in
      t :: (h (cnt + 1))
  in
  try
    h 0
  with _ -> raise (Not_Found_Match "consume_mult")

(* forsee: int -> int * token_t list *)
let forsee n =
  let ind = index () in
  let lst = consume_mult n in
  let ind' = index () in
  set ind; (ind', lst)

(* accept_tkn: sym_t -> bool *)
let accept_tkn sym = accept (tkn_eq sym)

(* expect_mult: int -> (token_t list -> bool) -> token_t list *)
let expect_mult n p =
  let ind = index () in
  let (ind', lst) = forsee n in
  if p lst then (ahead_with (ind' - ind); lst)
  else (raise (Not_Found_Match "expect_mult")) 

(* expect_tkn: sym_t -> token *)
let expect_tkn sym = expect (tkn_eq sym)

(* expect_tkns: sym_t list -> token_t list *)
let expect_tkns syms =
  expect_mult (List.length syms) (tkns_eq syms)

(* expect_tkn_or: sym_t list -> token_t *)
let rec expect_tkn_or syms = match syms with
  | [] ->
    raise (Not_Found_Match "expect_mult")
  | x :: xs ->
    try
      expect_tkn x
    with Unexpected _ -> expect_tkn_or xs

(* try_f: (unit -> 'a) -> 'a *)
let try_f f =
  let pt = index () in
  try f ()
  with e -> set pt; raise e

(* many: (unit -> 'a) -> 'a list *)
let rec many f =
  try
    let e = try_f f in
    e :: (many f)
  with _ -> []

(* many1: (unit -> 'a) -> 'a list *)
let many1 f =
  let ind = index () in
  let xs = many f in
  if xs = [] then (set ind; raise (Not_Found_Match "many1"))
  else xs

(* or_: string ->  (unit -> 'a) list -> 'a *)
let rec or_ name fs = match fs with
  | [] ->
    raise (Not_Found_Match ("or_ " ^ name))
  | f :: rest ->
    try
      try_f f
    with _ -> or_ name rest

(* and_: (unit -> 'a) list -> 'a list *)
let and_ fs =
  let ind = index () in
  let rec h lst = match lst with
  | [] ->
    []
  | f :: rest ->
    let e = f () in
    e :: (h rest)
  in try
       h fs
     with e -> set ind; raise e

let merge_info info =
  let _ = if info = [] then failwith "Cannot merge invalid location-info" in
  let len = List.length info in
  let (loc1, _) = List.hd info in
  let (_, loc2) = List.nth info (len - 1) in
  (loc1, loc2)

let get_and_merge_tkn_info ts =
  let info_lst = List.map get_tkn_info ts in
  merge_info info_lst

let get_and_merge_ast_info ts =
  let info_lst = List.map get_ast_info ts in
  merge_info info_lst

(*
 * l := LET VAR+ EQUAL l [IN] l
 *    | s
 *
 * s := IF s THEN s ELSE s
 *    | e
 *
 * e := e PLUS t
 *    | e MINUS t
 *    | t
 *
 * t := t TIMES f
 *    | t DIVIDE f
 *    | f
 *
 * f := LPAREN l RPAREN
 *    | v
 *
 * v := INT
 *    | MINUS INT
 *    | VAR
 *    | TRUE
 *    | FALSE
 *)

exception SNH (* should not happen *)
let get_name_from_var v = match v with
  | Var (s, _) -> s
  | _ -> raise SNH

let num () = match expect_tkn SINT with
  | INT (n, l) -> Int (n, l)
  | _ -> raise SNH

let negative_num () = match expect_tkns [SMINUS; SINT] with
  | [MINUS (l1); INT (n, l2)] -> Int (-n, merge_info [l1; l2])
  | _ -> raise SNH

let var () = match expect_tkn SVAR with
  | VAR (s, l) -> Var (s, l)
  | _ -> raise SNH

let bool_ () = match expect_tkn_or [STRUE; SFALSE] with
  | TRUE (l) -> True (l)
  | FALSE (l) -> False (l)
  | _ -> raise SNH

let int_ () = or_ "int_" [num; negative_num]
let value () = or_ "value" [int_; var; bool_]

let rec tops () =
  let f_1 () =
    let hd = expect_tkn SLET in
    let f = var () in
    let name = get_name_from_var f in
    let xs = many (fun () -> let v = var () in get_name_from_var v) in
    let _ = expect_tkn SEQUAL in
    let e1 = ifs () in
    let tl = expect_tkn SDOUBLE_SEMICOLON in
    let (i1, i2) = (get_tkn_info hd, get_tkn_info tl) in
    Declare (name, xs, e1, merge_info [i1; i2])
  in
  or_ "tops" [f_1; ifs]

and ifs () =
  let f_1 () =
    let tif = expect_tkn SIF in
    let e1 = ifs () in
    let _ = expect_tkn STHEN in
    let e2 = ifs () in
    let _ = expect_tkn SELSE in
    let e3 = ifs () in
    let i1 = get_tkn_info tif in
    let i2 = get_ast_info e3 in
    If (e1, e2, e3, merge_info [i1; i2])
  in
  let f_2 () =
    let hd = expect_tkn SLET in
    let f = var () in
    let name = get_name_from_var f in
    let xs = many (fun () -> let v = var () in get_name_from_var v) in
    let _ = expect_tkn SEQUAL in
    let e1 = ifs () in
    let _ = accept_tkn SIN in
    let e2 = ifs () in
    let (i1, i2) = (get_tkn_info hd, get_ast_info e2) in
    Let (name, xs, e1, e2, merge_info [i1; i2])
  in
  or_ "ifs" [f_1; f_2; expr]

and expr () =
  let t1 = term () in
  let f_1 () =
    let ts = many1 (fun () ->
                     let op = expect_tkn_or [SPLUS; SMINUS] in
                     let t = term () in (op, t))
  in
    List.fold_left (fun tr (op, t) ->
                     if tkn_eq SPLUS op then Plus (tr, t, get_and_merge_ast_info [tr; t])
                     else Minus (tr, t, get_and_merge_ast_info [tr; t])) t1 ts
  in
  or_ "expr" [f_1; (fun () -> t1)]

and term () =
  let f1 = factor () in
  let f_1 () =
    let fs = many1 (fun () ->
                     let op = expect_tkn_or [STIMES; SDIVIDE] in
                     let f = factor () in (op, f))
  in
    List.fold_left (fun tr (op, t) ->
                     if tkn_eq STIMES op then Times (tr, t, get_and_merge_ast_info [tr; t])
                     else Divide (tr, t, get_and_merge_ast_info [tr; t])) f1 fs
  in
  or_ "term" [f_1; (fun () -> f1)]

and factor () =
  let f_1 () =
    let lprn = expect_tkn SLPAREN in
    let e = ifs () in
    let rprn = expect_tkn SRPAREN in
    let info' = get_and_merge_tkn_info [lprn; rprn] in
    set_ast_info e info'
  in
  or_ "factor" [value; f_1]

(* main : token_t list -> ast_t *)
let main ts =
  init ts;
  try
    if is_end () then (raise Has_No_Token)
      else
        let e = tops () in
        if is_end () then e
        else failwith "remain"
  with e -> raise e
