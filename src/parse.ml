(*
open Types

exception Out_of_Index

exception Unexpected of token_t

exception Not_Found_Match of string

let ptr = ref 0

let tarr : token_t array ref = ref [||]

let set_tkn arr = tarr := arr

let is_safe index = index < Array.length !tarr

let index () = !ptr

let ahead_with n = ptr := !ptr + n

let ahead () = ahead_with 1

let is_end () = not (is_safe (index ()))

let get_with index =
  if is_safe index then !tarr.(index) else (raise Out_of_Index)

let get () =
  get_with (index ())

let set pt = ptr := pt

(* accept: (token_t -> bool) -> bool *)
let accept p =
  try
    if p (get ()) then (ahead (); true) else false
  with Out_of_Index ->
    false

(* expect: (token_t -> bool) -> token_t *)
let expect p =
  let x = get () in
  if p x then (ahead (); x) else (raise (Unexpected x))

(* forsee: int -> token_t list *)
let forsee n =
  let pt = index () in
  let rec h dif =
    let ind = pt + dif in
    if n <= dif then []
    else (get_with ind) :: (h (dif + 1))
  in
  let max_dif = n - 1 in
  if is_safe max_dif then h 0
  else (raise (Not_Found_Match "forsee"))

(* expect_mult: int -> (token_t list -> bool) -> token_t list *)
let expect_mult n p =
  let lst = forsee n in
  if p lst then (ahead_with n; lst)
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

let valid_info info =
  let rec h pl pce lst = match lst with
    | [] ->
      true
    | (l, (c1, c2)) :: rest ->
      ((pl = l && pce <= c1) || pl < l) && (h l c2 rest)
  in
  info != [] && (h (-1) (-1) info)

let valid_ast_info info =
  let rec h pl pce lst = match lst with
    | [] ->
      true
    | ((ls, cs), (le, ce)) :: rest ->
      (((pl = ls) && pce < cs) || pl < ls) && (h le ce rest)
  in
  info != [] && (h (-1) (-1) info)

let merge_tkn_info info : loc_info2 =
  let is_valid = valid_info info in
  let _ = if not is_valid then failwith "Cannot merge invalid location-info" in
  let len = List.length info in
  let (l1, (cs1, _)) = List.hd info in
  let (l2, (_, ce2)) = List.nth info (len - 1) in
  ((l1, cs1), (l2, ce2))

let merge_ast_info info : loc_info2 =
  let is_valid = valid_ast_info info in
  let _ = if not is_valid then failwith "Cannot merge invalid location-info2" in
  let len = List.length info in
  let ((ls1, cs1), (_, _)) = List.hd info in
  let ((_, _), (le2, ce2)) = List.nth info (len - 1) in
  ((ls1, cs1), (le2, ce2))

let conv_info info : loc_info2 = match info with
  (l, (cs, ce)) -> ((l, cs), (l, ce))

let get_and_merge_tkn_info ts =
  let info_lst = List.map get_tkn_info ts in
  merge_tkn_info info_lst

let get_and_merge_ast_info ts =
  let info_lst = List.map get_ast_info ts in
  merge_ast_info info_lst

(*
 * e := e PLUS t
 *    | e MINUS t
 *    | t
 *
 * t := t TIMES f
 *    | t DIVIDE f
 *    | f
 *
 * f := LPAREN e RPAREN
 *    | v
 *
 * v := INT
 *    | MINUS INT
 *    | VAR
 *)

exception SNH (* should not happen *)
let num () = match expect_tkn SINT with
  | INT (n, l) -> Int (n, conv_info l)
  | _ -> raise SNH

let negative_num () = match expect_tkns [SMINUS; SINT] with
  | [MINUS (l1); INT (n, l2)] -> Int (-n, merge_tkn_info [l1; l2])
  | _ -> raise SNH

let var () = match expect_tkn SVAR with
  | VAR (s, l) -> Var (s, conv_info l)
  | _ -> raise SNH

let int_ () = or_ "int_" [num; negative_num]
let value () = or_ "value" [int_; var]

let rec expr () =
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
    let e = expr () in
    let rprn = expect_tkn SRPAREN in
    let info' = get_and_merge_tkn_info [lprn; rprn] in
    set_ast_info e info'
  in
  or_ "factor" [value; f_1]

(* main : token_t list -> ast_t *)
let main ts = ts
*)
