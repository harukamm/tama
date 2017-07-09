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

(* expects: (token_t -> bool) list -> token_t *)
let rec expects ps = match ps with
  | [] ->
    raise (Not_Found_Match "expectes")
  | p :: rest ->
    try
      expect p
    with Unexpected _ -> expects rest

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
  with e -> []

(* or_: (unit -> 'a) list -> 'a *)
let rec or_ fs = match fs with
  | [] ->
    raise (Not_Found_Match "or_")
  | f :: rest ->
    try
      try_f f
    with Unexpected _ -> or_ rest

let valid_info info =
  let rec h pl pce lst = match lst with
    | [] ->
      true
    | (l, (c1, c2)) :: rest ->
      ((pl = l && pce <= c1) || pl < l) && (h l c2 rest)
  in
  info != [] && (h (-1) (-1) info)

let merge_info info : loc_info2 =
  let is_valid = valid_info info in
  let _ = if not is_valid then failwith "Cannot merge invalid location-info" in
  let len = List.length info in
  let (l1, (cs1, ce1)) = List.hd info in
  let (l2, (cs2, ce2)) = List.nth info (len - 1) in
  ((l1, cs1), (l2, ce2))

let conv_info info : loc_info2 = match info with
  (l, (cs, ce)) -> ((l, cs), (l, ce))

(*
 * e := t PLUS e
 *    | t MINUS e
 *    | t
 *
 * t := f TIMES t
 *    | t DIVIDE f
 *    | f
 *
 * f := LPAREN e RPAREN
 *    | INT
 *    | MINUS INT
 *    | VAR
 *
 * v := INT
 *    | MINUS INT
 *    | VAR
 *)

(* main : token_t list -> ast_t *)
let main ts = ts
