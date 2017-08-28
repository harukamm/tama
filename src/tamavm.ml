open Types

let label_cnt = ref 0

let clear_label () = label_cnt := 0

let inc_label () = label_cnt := !label_cnt + 1

let label () =
  let l = !label_cnt in
  let _ = inc_label () in
  l

(* stack_maps : ast_t -> bool -> int -> (string * stack_pointer) list *)
let rec stack_maps inside t n = match t with
  | Int _ ->
    []
  | Var _ ->
    []
  | Plus (e1, e2, _) | Minus (e1, e2, _)
  | Times (e1, e2, _) | Divide (e1, e2, _) ->
    let m1 = stack_maps inside e1 n in
    let m2 = stack_maps inside e2 (n + 1) in
    m1 @ m2
  | If (e1, e2, e3, _) ->
    let m1 = stack_maps inside e1 n in
    let m2 = stack_maps inside e2 n in
    let m3 = stack_maps inside e3 n in
    m1 @ m2 @ m3
  | Let (x, [], e1, e2, _, _) ->
    let m' = (x, if inside then Offset n else Index n) in
    let m1 = stack_maps inside e1 n in
    let m2 = stack_maps inside e2 (n + 1) in
    m' :: m1 @ m2
  | Declare (x, [], e1, _, _) ->
    let m' = (x , if inside then Offset n else Index n) in
    let m1 = stack_maps inside e1 n in
    m' :: m1
  | Let (_, xs, e1, e2, _, _) ->
    let (m', n2) =
      List.fold_left (fun (l, i) a -> ((a, Offset i) :: l), i + 1) ([], 0) xs in
    let m1 = stack_maps true e1 n2 in
    let m2 = stack_maps inside e2 n in
    m' @ m1 @ m2
  | Declare (_, xs, e1, _, _) ->
    let (m', _) =
      List.fold_left (fun (l, i) a -> ((a, Offset i) :: l, i + 1)) ([], 0) xs in
    let m1 = stack_maps true e1 n in
    m' @ m1
  | Block (es, _) ->
    begin
      let h (m, n1) e = match e with
        | Let (_, [], _, _, _, _)
        | Declare (_, [], _, _, _) ->
          let m1 = stack_maps inside e n1 in
          (m1 @ m, n1 + 1)
        | _ ->
          let m1 = stack_maps inside e n1 in
          (m1 @ m, n1)
      in
      let (m, _) = List.fold_left h ([], n) es in
      m
    end
  | GreaterThan (e1, e2, _, _)
  | LessThan (e1, e2, _, _)
  | Equal (e1, e2, _) ->
    let m1 = stack_maps inside e1 n in
    let m2 = stack_maps inside e2 (n + 1) in
    m1 @ m2
  | App (x, xs, _) ->
    let x' = stack_maps inside x n in
    let xs' = List.map (fun e -> stack_maps inside e n) xs in
    x' @ (List.concat xs')
  | True _ ->
    []
  | False _ ->
    []

(* collect_funcs: ast_t -> string list *)
let rec collect_funcs t = match t with
  | Int _ ->
    []
  | Var _ ->
    []
  | Plus (e1, e2, _) | Minus (e1, e2, _)
  | Times (e1, e2, _) | Divide (e1, e2, _) ->
    (collect_funcs e1) @ (collect_funcs e2)
  | If (e1, e2, e3, _) ->
    (collect_funcs e1) @ (collect_funcs e2) @ (collect_funcs e3)
  | Let (x, xs, e1, e2, _, _) ->
    let fs = (collect_funcs e1) @ (collect_funcs e2) in
    if xs = [] then fs
    else x :: fs
  | Declare (x, xs, e1, _, _) ->
    let fs = collect_funcs e1 in
    if xs = [] then fs
    else x :: fs
  | Block (es, _) ->
    let fss = List.map collect_funcs es in
    List.concat fss
  | GreaterThan (e1, e2, _, _)
  | LessThan (e1, e2, _, _) | Equal (e1, e2, _) ->
    (collect_funcs e1) @ (collect_funcs e2)
  | App (x, xs, _) ->
    let fs = collect_funcs x in
    let fss = List.map collect_funcs xs in
    fs @ (List.concat fss)
  | True _ ->
    []
  | False _ ->
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

let func_names = ref []

let func_def = ref []

let add_func (k, c) =
  let c' = flatten_ops c in
  func_def := (k, c') :: !func_def

let get_stkp x =
  try
    Some (List.assoc x !maps)
  with Not_found -> None

let exists_func x =
  if List.mem x !func_names then Some x else None

(* emit_h : ast_t -> op_t *)
let rec emit_h t = match t with
  | Int (n, l) ->
    WithInfo ([PUSH n], l)
  | Var (v, l) ->
    begin
      match get_stkp v with
      | None ->
        begin
          match exists_func v with
          | None -> raise (Unbound_Variable (l))
          | Some p -> WithInfo ([PUSHP p], l)
        end
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
    let ln1 = label () in
    let ln2 = label () in
    let lb1 = LABEL ln1 in
    let lb2 = LABEL ln2 in
    let j1 = JZ ln1 in
    let j2 = JMP ln2 in
    let c2 = emit_h e2 in
    let c3 = emit_h e3 in
    let lst = c1 :: j1 :: c2 :: j2 :: lb1 :: [c3; lb2] in
    WithInfo (lst, l)
  | Let (_, [], e1, e2, _, l) ->
    let c1 = emit_h e1 in
    let c2 = emit_h e2 in
    WithInfo (c1 :: [c2], l)
  | Declare (_, [], e1, _, l) ->
    let c1 = emit_h e1 in
    WithInfo ([c1], l)
  | Let (x, _::_, e1, e2, _, l) ->
    let c1 = emit_h e1 in
    let info = get_ast_info e1 in
    let _ = add_func (x, WithInfo ([c1], info)) in
    let c2 = emit_h e2 in
    WithInfo ([c2], l)
  | Declare (x, _::_, e1, _, l) ->
    let c1 = emit_h e1 in
    let () = add_func (x, WithInfo ([c1], l)) in
    WithInfo ([], l)
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
    let c1 = emit_h x in
    let cs = List.map emit_h xs in
    let len = List.length xs in
    let c2 = CALL len in
    let c3 = POPE (len + 1) in
    WithInfo (c1 :: cs @ [c2; c3], l)

(* emit: ast_t -> opcode_t *)
let emit t =
  let _ = clear_label () in
  let _ = func_def := [] in
  let map = stack_maps false t 0 in
  let _ = maps := map in
  let fnames = collect_funcs t in
  let _ = func_names := fnames in
  let op = emit_h t in
  let c1 = !func_def in
  let c2 = flatten_ops op in
  let result = { funcs = c1; main = c2 } in
  result


(* step execution *)

type elem_t =
  | Pointer of string
  | Int of int

type state_t =
  | Main of int * (oploc_t list)
  | Func of string * int * (oploc_t list)

let ready = ref false

let x_opcode = ref { funcs = []; main = [] }

let x_bsp : int list ref = ref []

let x_stk : elem_t list ref = ref []

let chain : state_t list ref = ref []

let current_state = ref (Main (-1, []))

let get_bsp () = match !x_bsp with
  | [] -> raise (Runtime_Error "bsp is not registered")
  | x :: _ -> x

let add_bsp x = x_bsp := x :: !x_bsp

let remove_bsp () = match !x_bsp with
  | [] -> raise (Runtime_Error "Cannot pop a bsp from empty list")
  | _ :: xs -> x_bsp := xs

let stk_len () = List.length !x_stk

let start_invoke f n =
  let len = stk_len () in
  add_bsp (len - n);
  chain := !current_state :: !chain;
  let ops = (!x_opcode).funcs in
  let fop = List.assoc f ops in
  current_state := Func (f, 0, fop)

let end_invoke () = match !chain with
  | [] -> raise (Runtime_Error "Empty returning point")
  | x :: xs ->
    remove_bsp ();
    chain := xs;
    current_state := x

let pop_stk () = match !x_stk with
  | [] -> raise (Runtime_Error "Try to pop from empty stack")
  | x :: xs -> x_stk := xs; x

let getv_stk i =
  let len = stk_len () in
  if i < len then List.nth !x_stk (len - i - 1)
  else raise (Runtime_Error "Access the stack out of bounds")

let push_stk e = x_stk := e :: !x_stk
let pushi_stk x = push_stk (Int x)
let pushp_stk s = push_stk (Pointer s)
let pushb_stk b = if b then pushi_stk 1 else pushi_stk 0

let index_of_label target ops =
  let rec h ops1 ind = match ops1 with
    | [] -> raise (Label_Not_Found target)
    | (LABEL x, _) :: xs when x = target -> ind
    | _ :: xs -> h xs (ind + 1)
  in
  h ops 0

let until_label x = match !current_state with
  | Main (_, op) ->
    index_of_label x op
  | Func (s, _, op) ->
    index_of_label x op

let get_pc () = match !current_state with
  | Main (i, _)
  | Func (_, i, _) -> i

let set_pc n = match !current_state with
  | Main (i, op) -> current_state := Main (n, op)
  | Func (s, i, op) -> current_state := Func (s, n, op)

let ahead () = match !current_state with
  | Main (i, op) -> current_state := Main (i + 1, op)
  | Func (s, i, op) -> current_state := Func (s, i + 1, op)

let arith = [(ADD, (+)); (SUB, (-)); (MUL, ( * )); (DIV, (/))]

let comp = [(GTEQ, (>=)); (GT, (>)); (LSEQ, (<=)); (LS, (<)); (EQ, (=))]

let execute_one_h (op : op_t) = match op with
  | ADD | SUB | MUL | DIV ->
    let x2 = pop_stk () in
    let x1 = pop_stk () in
    let f = List.assoc op arith in
    begin
      match (x1, x2) with
      | (Int i1, Int i2) -> pushi_stk (f i1 i2); ahead ()
      | _ -> raise (Invalid_Operand_Type ("int", "pointer"))
    end
  | PUSHP x ->
    begin
      try
        let _ = List.assoc x (!x_opcode).funcs in
        pushp_stk x; ahead ()
      with Not_found -> raise (Runtime_Error "Pointer not found")
    end
  | PUSH i ->
    pushi_stk i; ahead ()
  | JMP x ->
    let i = until_label x in
    set_pc i
  | JZ x ->
    let x1 = pop_stk () in
    begin
      match x1 with
      | Int i1 when i1 = 0 -> set_pc (until_label x)
      | Int i1 when i1 <> 0 -> ahead ()
      | _ -> raise (Invalid_Operand_Type ("int", "pointer"))
    end
  | GTEQ | GT | LSEQ | LS | EQ ->
    let x2 = pop_stk () in
    let x1 = pop_stk () in
    let f = List.assoc op comp in
    begin
      match (x1, x2) with
      | (Int i1, Int i2) -> pushb_stk (f i1 i2); ahead ()
      | _ -> raise (Invalid_Operand_Type ("int", "pointer"))
    end
  | CALL n ->
    let len = stk_len () in
    let f = getv_stk (len - 1 - n) in
    begin
      match f with
      | Pointer (p) ->
        start_invoke p n
      | _ -> raise (Invalid_Operand_Type ("pointer", "int"))
    end
  | RETURN ->
    ahead ()
  | LABEL _ ->
    ahead ()
  | MOV (Index (i)) ->
    let v = getv_stk (i) in
    push_stk v; ahead ()
  | MOV (Offset (i)) ->
    let p = get_bsp () in
    let v = getv_stk (i + p) in
    push_stk v; ahead ()
  | POPE (n) ->
    let x1 = pop_stk () in
    let rec h i =
      if i <= 0 then () else let _ = pop_stk () in h (i - 1)
    in
    let _ = h n in
    push_stk x1; ahead ()
  | WithInfo _ ->
    failwith "not supported"

(* is_end: unit -> bool *)
let is_end () =
  match !current_state with
  | Main (i, op) ->
    let oplen = List.length op in
    i = oplen
  | Func _ -> false

(* is_end_of_invocation: unit -> bool *)
let is_end_of_invocation () =
  match !current_state with
  | Func (s, i, op) ->
    let oplen = List.length op in
    i = oplen
  | Main _ -> false

(* next_op: unit -> oploc_t *)
let next_op () = match !current_state with
  | Main (i, op)
  | Func (_, i, op) ->
    List.nth op i

let just_called = ref false

(* execute_one_main: op_t -> unit *)
let execute_one_main op =
  if not (!ready) then
    raise Not_Ready_State
  else if !just_called then
   (just_called := false;
    ahead ())
  else
   (print_endline ("step:" ^ (display_op op));
    execute_one_h (op);
    if is_end_of_invocation () then
     (end_invoke ();
      print_endline "end invoke";
      just_called := true))

(* init: unit -> unit *)
let init () =
  x_bsp := [];
  x_stk := [];
  chain := [];
  current_state := (Main (0, (!x_opcode).main));
  ready := true

(* clear: unit -> unit *)
let clear () =
 (init ();
  current_state := (Main (-1, (!x_opcode).main)))

let string_of_state x = match x with
  | Main (i, _) -> "Main (" ^ (string_of_int i) ^ ")"
  | Func (s, i, _) -> "Func (" ^ s ^ ", " ^ (string_of_int i) ^ ")"

(* display_current_op : string -> int -> oploc_t list -> string *)
let display_current_op prefix target ops =
  let rec h cnt lst = match lst with
    | [] ->
      ""
    | (o, _) :: xs ->
      let s = prefix ^ (display_op o) ^ "\n" in
      let s = if cnt = target then "<mark>" ^ s ^ "</mark>" else s in
      s ^ (h (cnt + 1) xs)
  in
  h 0 ops

(* display_opcode_x: opcode_t -> string *)
let display_opcode_x { funcs = c1; main = c2 } =
  let h (name, ops) =
    let ops_s =
      match !current_state with
        | Main (i, _) when name = "main" -> display_current_op "  " i ops
        | Func (s, i, _) when name = s -> display_current_op "  " i ops
        | _ -> display_current_op "  " (-1) ops
    in
    name ^ ":\n" ^ ops_s
  in
  let func' = List.fold_left (fun s x -> s ^ (h x)) "" c1 in
  let main' = h ("main", c2) in
  func' ^ "\n" ^ main'

(* highlighted_opcode : unit -> string *)
let highlighted_opcode () =
  if !ready then display_opcode_x !x_opcode
  else ""

exception End_of_Step

(* step_exe: unit -> unit *)
let step_exe () =
  let _ = if is_end () then raise End_of_Step in
  let (op, l) = next_op () in
  try
    execute_one_main op
  with Not_Ready_State ->
       raise (Failed (Runtime, "VM seems not to be initialized"))
     | Label_Not_Found (n) ->
       raise (FailedWith (Runtime, "Label" ^ (Util.soi n) ^ " Not Found", l))
     | Invalid_Operand_Type (expected, actual) ->
       let disc = "(Expected: " ^ expected ^ ", " ^ "Actual: " ^ actual ^ ")" in
       raise (FailedWith (Runtime, "Invalid operand type" ^ disc, l))
     | Runtime_Error (msg) ->
       raise (FailedWith (Runtime, msg, l))
     | _ ->
       raise (Failed (Runtime, "Unknown error"))

(* step_from_reason : unit -> result_t *)
let step_from_reason s =
  try
    let _ = step_exe () in
    let highlighted = highlighted_opcode () in
    RSuccess (highlighted)
  with End_of_Step ->
        REnd
     | FailedWith (typ, s, info) ->
        RError (typ, s, Some info)
     | Failed (typ, s) ->
        RError (typ, s, None)

(* main: string -> opcode_t *)
let main s =
  let ts =
    try
      Tokenize.main s
    with Tokenize_Error (s, info) ->
          raise (FailedWith (Tokenizing, "Invalid token: `" ^ s ^ "'", info))
       | Comment_Not_Terminated info ->
          raise (FailedWith (Tokenizing, "Comment not terminated", info))
       | _ ->
          raise (Failed (Tokenizing, "Please report ('-' +)"))
  in
  let ast =
    try
      Parse.main ts
    with Has_No_Token ->
          raise (Failed (Parsing, "Empty token"))
       | Unexpected (t) ->
          raise (FailedWith (Parsing, "Unexpected token", get_tkn_info t))
       | _ ->
          raise (Failed (Parsing, "I don't know ( '-')"))
  in
  let ast' =
    try
      Tamavm_pre.main ast
    with Unbound_Variable (info) ->
          raise (FailedWith (Prechecking, "Unbound variable", info))
       | Not_Supported (msg, info) ->
          raise (FailedWith (Prechecking, msg, info))
       | _ ->
          raise (Failed (Prechecking, "I don't know (;; '-') Please report"))
  in
  let ops =
    try
      emit ast'
    with Unbound_Variable (info) ->
          raise (FailedWith (Emitting, "This is a bug! (> o <);;", info))
       | _ ->
          raise (Failed (Emitting, "I don't know ('x' )/))"))
  in
  let _ = x_opcode := ops in
  let _ = init () in
  ops

(* compile_from_reason : string -> result_t *)
let compile_from_reason s =
  try
    let ops = main s in
    RSuccess (display_opcode ops)
  with FailedWith (typ, s, info) ->
        RError (typ, s, Some info)
     | Failed (typ, s) ->
        RError (typ, s, None)

