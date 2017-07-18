open Types

let label_cnt = ref 0

let inc_label () = label_cnt := !label_cnt + 1

let label () =
  let l = !label_cnt in
  let _ = inc_label () in
  l

(* stack_maps : ast_t -> int -> (string * int) list *)
let rec stack_maps t n = match t with
  | Int (n, l) ->
    []
  | Var (v, l) ->
    []
  | Plus (e1, e2, l) | Minus (e1, e2, l)
  | Times (e1, e2, l) | Divide (e1, e2, l) ->
    let m1 = stack_maps e1 n in
    let m2 = stack_maps e2 (n + 1) in
    m1 @ m2
  | If (e1, e2, e3, l) ->
    let m1 = stack_maps e1 n in
    let m2 = stack_maps e2 n in
    let m3 = stack_maps e3 n in
    m1 @ m2 @ m3
  | Let (x, xs, e1, e2, is_rec, l) ->
    let (m, n1) = ([(x, n)], n + 1) in
    let (m', n2) =
      List.fold_left (fun (l, i) a -> ((a, i) :: l), i + 1) (m, n1) xs in
    let m1 = stack_maps e1 n2 in
    let m2 = stack_maps e2 (n + 1) in
    m' @ m1 @ m2
  | Declare (x, xs, e1, is_rec, l) ->
    let (m, n1) = ([(x, n)], n + 1) in
    let (m', n2) =
      List.fold_left (fun (l, i) a -> ((a, i) :: l, i + 1)) (m, n1) xs in
    let m1 = stack_maps e1 n2 in
    m' @ m1
  | Block (es, l) ->
    begin
      let h (l, i) e = match e with
        | Let (x, _, _, _, _, _) -> (l, i + 1)
        | Declare (x, _, _, _, _) -> (l, i + 1)
        | _ -> (l @ (stack_maps e i), i)
      in
      let (m, _) = List.fold_left h ([], n) es in
      m
    end
  | GreaterThan (e1, e2, eq, l) | LessThan (e1, e2, eq, l) ->
    let m1 = stack_maps e1 n in
    let m2 = stack_maps e2 (n + 1) in
    m1 @ m2
  | Equal (e1, e2, l) ->
    let m1 = stack_maps e1 n in
    let m2 = stack_maps e2 (n + 1) in
    m1 @ m2
  | App (x, xs, l) ->
    []
  | True (l) ->
    []
  | False (l) ->
    []

(* flatten_ops_h : op_t list -> loc_info -> (op_t * loc_info) list *)
let rec flatten_ops_h op_lst l = match op_lst with
  | [] ->
    []
  | WithInfo (ops, l') :: rest ->
    let ops' = flatten_ops_h ops l' in
    let rest' = flatten_ops_h rest l in
    ops' @ rest'
  | x :: rest ->
    let rest' = flatten_ops_h rest l in
    (x, l) :: rest'

(* flatten_ops : op_t -> oploc_t list *)
let flatten_ops op : oploc_t list = match op with
  | WithInfo (xs, l) -> flatten_ops_h xs l
  | _ -> failwith "flatten_ops only works when the arg is `WithInfo _`"

let maps = ref []

let func = ref []

let add_func (k, c) =
  let c' = flatten_ops c in
  func := (k, c') :: !func

let get_stkp x =
  try
    Some (List.assoc x !maps)
  with Not_found -> None

(* emit_h : ast_t -> op_t *)
let rec emit_h t = match t with
  | Int (n, l) ->
    WithInfo ([PUSH n], l)
  | Var (v, l) ->
    begin
      match get_stkp v with
      | None -> failwith ("Unbound value " ^ v)
      | Some n -> WithInfo ([MOV n], l)
    end
  | Plus (e1, e2, l) ->
    let c1 = emit_h e1 in
    let c2 = emit_h e2 in
    let lst = c1 :: c2 :: [ADD] in
    WithInfo (lst, l)
  | Minus (e1, e2, l) ->
    let c1 = emit_h e1 in
    let c2 = emit_h e2 in
    let lst = c1 :: c2 :: [SUB] in
    WithInfo (lst, l)
  | Times (e1, e2, l) ->
    let c1 = emit_h e1 in
    let c2 = emit_h e2 in
    let lst = c1 :: c2 :: [MUL] in
    WithInfo (lst, l)
  | Divide (e1, e2, l) ->
    let c1 = emit_h e1 in
    let c2 = emit_h e2 in
    let lst = c1 :: c2 :: [DIV] in
    WithInfo (lst, l)
  | If (e1, e2, e3, l) ->
    let c1 = emit_h e1 in
    let ln = label () in
    let j = JZ ln in
    let c2 = emit_h e2 in
    let lb = LABEL ln in
    let c3 = emit_h e3 in
    let lst = c1 :: j :: c2 :: lb :: [c3] in
    WithInfo (lst, l)
  | Let (x, [], e1, e2, is_rec, l) ->
    let c1 = emit_h e1 in
    let c2 = emit_h e2 in
    WithInfo (c1 :: [c2], l)
  | Declare (x, [], e1, is_rec, l) ->
    let c1 = emit_h e1 in
    WithInfo ([c1], l)
  | Let (x, xs, e1, e2, is_rec, l) ->
    let c1 = emit_h e1 in
    let info = get_ast_info e1 in
    let _ = add_func (x, WithInfo ([c1], info)) in
    let c2 = emit_h e2 in
    WithInfo ([c2], l)
  | Declare (x, xs, e1, is_rec, l) ->
    let c1 = emit_h e1 in
    let () = add_func (x, WithInfo ([c1], l)) in
    WithInfo ([PUSH 0], l)
  | Block (es, l) ->
    let cs = List.map emit_h es in
    WithInfo (cs, l)
  | GreaterThan (e1, e2, eq, l) ->
    let c1 = emit_h e1 in
    let c2 = emit_h e2 in
    let op = if eq then GTEQ else GT in
    let lst = c1 :: c2 :: [op] in
    WithInfo (lst, l)
  | LessThan (e1, e2, eq, l) ->
    let c1 = emit_h e1 in
    let c2 = emit_h e2 in
    let op = if eq then LSEQ else LS in
    let lst = c1 :: c2 :: [op] in
    WithInfo (lst, l)
  | Equal (e1, e2, l) ->
    let c1 = emit_h e1 in
    let c2 = emit_h e2 in
    let lst = c1 :: c2 :: [EQ] in
    WithInfo (lst, l)
  | True (l) ->
    WithInfo ([PUSH 1], l)
  | False (l) ->
    WithInfo ([PUSH 0], l)
  | App (x, xs, l) ->
    let cs = List.map emit_h xs in
    let c1 = emit_h x in
    let c2 = CALL (List.length xs) in
    WithInfo (cs @ c1 :: [c2], l)

(* emit: ast_t -> opcode_t *)
let emit t =
  let _ = maps := stack_maps t 0 in
  let op = emit_h t in
  let c1 = !func in
  let c2 = flatten_ops op in
  let result = { funcs = c1; main = c2 } in
  let () = print_endline (display_opcode result) in
  result

let main t =
  ()
