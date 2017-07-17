open Types

(*
 * stack;
 *
 * op := ADD
 *     | SUB
 *     | MUL
 *     | DIV
 *     | JZ
 *     | JNZ
 *     | GTEQ
 *     | GT
 *     | LSEQ
 *     | LS
 *     | CALL
 *     | RETURN
 *)

type stack_loc = int * int

type op_t = ADD
          | SUB
          | MUL
          | DIV
          | PUSH of int
          | JZ of int
          | JNZ
          | GTEQ
          | GT
          | LSEQ
          | EQ
          | LS
          | CALL of int
          | RETURN
          | LABEL of int
          | MOV of int
          | WithInfo of op_t list * loc_info

let label_cnt = ref 0

let inc_label () = label_cnt := !label_cnt + 1

let label () =
  let l = !label_cnt in
  let _ = inc_label () in
  l

let stk_cnt = ref 0

let inc_stk () = stk_cnt := !stk_cnt + 1

let make_stk () =
  let c = !stk_cnt in
  let _ = inc_stk () in
  c

let rec stack_maps t = match t with
  | Int (n, l) ->
    []
  | Var (v, l) ->
    []
  | Plus (e1, e2, l) | Minus (e1, e2, l)
  | Times (e1, e2, l) | Divide (e1, e2, l) ->
    let m1 = stack_maps e1 in
    let m2 = stack_maps e2 in
    m1 @ m2
  | If (e1, e2, e3, l) ->
    let m1 = stack_maps e1 in
    let m2 = stack_maps e2 in
    let m3 = stack_maps e3 in
    m1 @ m2 @ m3
  | Let (x, xs, e1, e2, is_rec, l) ->
    let stk = make_stk () in
    let m1 = stack_maps e1 in
    let m2 = stack_maps e2 in
    (x, stk) :: m1 @ m2
  | Declare (x, xs, e1, is_rec, l) ->
    let stk = make_stk () in
    let m1 = stack_maps e1 in
    (x, stk) :: m1
  | Block (es, l) ->
    let lst = List.map stack_maps es in
    List.concat lst
  | GreaterThan (e1, e2, eq, l) | LessThan (e1, e2, eq, l) ->
    let m1 = stack_maps e1 in
    let m2 = stack_maps e2 in
    m1 @ m2
  | Equal (e1, e2, l) ->
    let m1 = stack_maps e1 in
    let m2 = stack_maps e2 in
    m1 @ m2
  | App (x, xs, l) ->
    []
  | True (l) ->
    []
  | False (l) ->
    []

let maps = ref []

let func = ref []

let add_func (k, c) = func := (k, c) :: !func

let get_stkp x =
  try
    Some (List.assoc x !maps)
  with Not_found -> None

let rec emit t = match t with
  | Int (n, l) ->
    WithInfo ([PUSH n], l)
  | Var (v, l) ->
    begin
      match get_stkp v with
      | None -> failwith "Unbound value"
      | Some n -> WithInfo ([MOV n], l)
    end
  | Plus (e1, e2, l) ->
    let c1 = emit e1 in
    let c2 = emit e2 in
    let lst = c1 :: c2 :: [ADD] in
    WithInfo (lst, l)
  | Minus (e1, e2, l) ->
    let c1 = emit e1 in
    let c2 = emit e2 in
    let lst = c1 :: c2 :: [SUB] in
    WithInfo (lst, l)
  | Times (e1, e2, l) ->
    let c1 = emit e1 in
    let c2 = emit e2 in
    let lst = c1 :: c2 :: [MUL] in
    WithInfo (lst, l)
  | Divide (e1, e2, l) ->
    let c1 = emit e1 in
    let c2 = emit e2 in
    let lst = c1 :: c2 :: [DIV] in
    WithInfo (lst, l)
  | If (e1, e2, e3, l) ->
    let c1 = emit e1 in
    let ln = label () in
    let j = JZ ln in
    let c2 = emit e2 in
    let lb = LABEL ln in
    let c3 = emit e3 in
    let lst = c1 :: j :: c2 :: lb :: [c3] in
    WithInfo (lst, l)
  | Let (x, [], e1, e2, is_rec, l) ->
    let c1 = emit e1 in
    let c2 = emit e2 in
    WithInfo (c1 :: [c2], l)
  | Declare (x, [], e1, is_rec, l) ->
    let c1 = emit e1 in
    WithInfo ([c1], l)
  | Let (x, xs, e1, e2, is_rec, l) ->
    let c1 = emit e1 in
    let info = get_ast_info e1 in
    let _ = add_func (x, WithInfo ([c1], info)) in
    let c2 = emit e2 in
    WithInfo ([c2], l)
  | Declare (x, xs, e1, is_rec, l) ->
    let c1 = emit e1 in
    let () = add_func (x, WithInfo ([c1], l)) in
    WithInfo ([PUSH 0], l)
  | Block (es, l) ->
    let cs = List.map emit es in
    WithInfo (cs, l)
  | GreaterThan (e1, e2, eq, l) ->
    let c1 = emit e1 in
    let c2 = emit e2 in
    let op = if eq then GTEQ else GT in
    let lst = c1 :: c2 :: [op] in
    WithInfo (lst, l)
  | LessThan (e1, e2, eq, l) ->
    let c1 = emit e1 in
    let c2 = emit e2 in
    let op = if eq then LSEQ else LS in
    let lst = c1 :: c2 :: [op] in
    WithInfo (lst, l)
  | Equal (e1, e2, l) ->
    let c1 = emit e1 in
    let c2 = emit e2 in
    let lst = c1 :: c2 :: [EQ] in
    WithInfo (lst, l)
  | True (l) ->
    WithInfo ([PUSH 1], l)
  | False (l) ->
    WithInfo ([PUSH 0], l)
  | App (x, xs, l) ->
    let cs = List.map emit xs in
    let c1 = emit x in
    let c2 = CALL (List.length xs) in
    WithInfo (cs @ c1 :: [c2], l)

let main t =
  let () = maps := stack_maps t in
  ()
