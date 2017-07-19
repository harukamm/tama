type loc_info =
  (int           (* start           *)
    * int        (* start of lineno *)
    * int)       (* start of colno  *)
  *
  (int           (* end             *)
    * int        (* end of lineno   *)
    * int)       (* end of colno    *)

type stack_pointer =
  | Index of int
  | Offset of int

type op_t = ADD
          | SUB
          | MUL
          | DIV
          | PUSHP of string
          | PUSH of int
          | JMP of int
          | JZ of int
          | GTEQ
          | GT
          | LSEQ
          | EQ
          | LS
          | CALL of int
          | RETURN
          | LABEL of int
          | MOV of stack_pointer
          | POPE of int
          | WithInfo of op_t list * loc_info

type oploc_t = op_t * loc_info

type opcode_t = {
  funcs: (string * oploc_t list) list;
  main: oploc_t list
}

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

type token_t =
  | INT of int * loc_info
  | VAR of string * loc_info
  | PLUS of loc_info
  | MINUS of loc_info
  | TIMES of loc_info
  | DIVIDE of loc_info
  | LPAREN of loc_info
  | RPAREN of loc_info
  | DOUBLE_SEMICOLON of loc_info
  | COMMENT of string * loc_info
  | LET of loc_info
  | IF of loc_info
  | THEN of loc_info
  | ELSE of loc_info
  | EQUAL of loc_info
  | REC of loc_info
  | IN of loc_info
  | TRUE of loc_info
  | FALSE of loc_info
  | GREATER_THAN_EQ of loc_info
  | GREATER_THAN of loc_info
  | LESS_THAN_EQ of loc_info
  | LESS_THAN of loc_info

type app_states_t = {
  is_executing: bool;
  text: string
}

type sym_t =
  | SINT
  | SVAR
  | SPLUS
  | SMINUS
  | STIMES
  | SDIVIDE
  | SLPAREN
  | SRPAREN
  | SDOUBLE_SEMICOLON
  | SCOMMENT
  | SLET
  | SIF
  | STHEN
  | SELSE
  | SEQUAL
  | SREC
  | SIN
  | STRUE
  | SFALSE
  | SGREATER_THAN_EQ
  | SGREATER_THAN
  | SLESS_THAN_EQ
  | SLESS_THAN

let sym_of_token t = match t with
  | INT _ -> SINT
  | VAR _ -> SVAR
  | PLUS _ -> SPLUS
  | MINUS _ -> SMINUS
  | TIMES _ -> STIMES
  | DIVIDE _ -> SDIVIDE
  | LPAREN _ -> SLPAREN
  | RPAREN _ -> SRPAREN
  | COMMENT _ -> SCOMMENT
  | DOUBLE_SEMICOLON _ -> SDOUBLE_SEMICOLON
  | LET _ -> SLET
  | IF _ -> SIF
  | THEN _ -> STHEN
  | ELSE _ -> SELSE
  | EQUAL _ -> SEQUAL
  | REC _ -> SREC
  | IN _ -> SIN
  | TRUE _ -> STRUE
  | FALSE _ -> SFALSE
  | GREATER_THAN_EQ _ -> SGREATER_THAN_EQ
  | GREATER_THAN _ -> SGREATER_THAN
  | LESS_THAN_EQ _ -> SLESS_THAN_EQ
  | LESS_THAN _ -> SLESS_THAN

let tkn_eq sym t =
  let sym' = sym_of_token t in
  sym = sym'

let rec tkn_mem syms t = match syms with
  | [] -> false
  | x :: xs -> tkn_eq x t || tkn_mem xs t 

let rec tkns_eq syms ts = match (syms,  ts) with
  | ([], []) -> true
  | (_ :: _, []) -> false
  | ([], _ :: _) -> false
  | (x :: xs, y :: ys) -> (tkn_eq x y) && (tkns_eq xs ys)

let rec ast_eq e1 e2 = match (e1, e2) with
  | (Int (t1, _), Int (t2, _)) ->
    t1 = t2
  | (Var (t1, _), Var (t2, _)) ->
    t1 = t2
  | (Plus (t1, t2, _), Plus (t3, t4, _)) ->
    (ast_eq t1 t3) && (ast_eq t2 t4)
  | (Minus (t1, t2, _), Minus (t3, t4, _)) ->
    (ast_eq t1 t3) && (ast_eq t2 t4)
  | (Times (t1, t2, _), Times (t3, t4, _)) ->
    (ast_eq t1 t3) && (ast_eq t2 t4)
  | (Divide (t1, t2, _), Divide (t3, t4, _)) ->
    (ast_eq t1 t3) && (ast_eq t2 t4)
  | (If (e1, e2, e3, _), If (e4, e5, e6, _)) ->
    (ast_eq e1 e4) && (ast_eq e2 e5) && (ast_eq e3 e6)
  | (Let (x, xs, e1, e2, r1, _), Let (y, ys, e3, e4, r2, _)) ->
    (r1 = r2) && (x = y) && (xs = ys) && (ast_eq e1 e3) && (ast_eq e2 e4)
  | (Declare (x, xs, e1, r1, _), Declare (y, ys, e2, r2, _)) ->
    (r1 = r2) && (x = y) && (xs = ys) && (ast_eq e1 e2)
  | (Block (es1, _), Block (es2, _)) when List.length es1 = List.length es2 ->
    let pairs = Util.zip es1 es2 in
    List.for_all (fun (e1, e2) -> ast_eq e1 e2) pairs
  | (True _, True _)
  | (False _, False _) ->
    true
  | (GreaterThan (e1, e2, b1, _), GreaterThan (e3, e4, b2, _))
  | (LessThan (e1, e2, b1, _), LessThan (e3, e4, b2, _)) ->
    (b1 = b2) && (ast_eq e1 e3) && (ast_eq e2 e4)
  | (Equal (e1, e2, _), Equal (e3, e4, _)) ->
    (ast_eq e1 e3) && (ast_eq e2 e4)
  | (App (e1, es1, _), App (e2, es2, _)) ->
    let pairs = Util.zip es1 es2 in
    (ast_eq e1 e2) && (List.for_all (fun (e1, e2) -> ast_eq e1 e2) pairs)
  | _ ->
    false

let skip_tkn t = match t with
  | COMMENT _ -> true
  | _ -> false

let soli (l : loc_info) = match l with
  ((sp, sl, sc), (ep, el, ec)) ->
    (Util.soi sp) ^ "," ^ (Util.soi sl) ^ "," ^ (Util.soi sc) ^ ":" ^
      (Util.soi ep) ^ "," ^ (Util.soi el) ^ "," ^ (Util.soi ec)

let get_tkn_info t = match t with
  | INT (_, l) -> l
  | VAR (_, l) -> l
  | PLUS (l) -> l
  | MINUS (l) -> l
  | TIMES (l) -> l
  | DIVIDE (l) -> l
  | LPAREN (l) -> l
  | RPAREN (l) -> l
  | COMMENT (_, l) -> l
  | DOUBLE_SEMICOLON (l) -> l
  | LET (l) -> l
  | IF (l) -> l
  | THEN (l) -> l
  | ELSE (l) -> l
  | EQUAL (l) -> l
  | REC (l) -> l
  | IN (l) -> l
  | TRUE (l) -> l
  | FALSE (l) -> l
  | GREATER_THAN_EQ (l) -> l
  | GREATER_THAN (l) -> l
  | LESS_THAN_EQ (l) -> l
  | LESS_THAN (l) -> l



let get_ast_info t = match t with
  | Int (_, l) -> l
  | Var (_, l) -> l
  | Plus (_, _, l) -> l
  | Minus (_, _, l) -> l
  | Times (_, _, l) -> l
  | Divide (_, _, l) -> l
  | If (_, _, _, l) -> l
  | Let (_, _, _, _, _, l) -> l
  | Declare (_, _, _, _, l) -> l
  | Block (_, l) -> l
  | True (l) -> l
  | False (l) -> l
  | GreaterThan (_, _, _, l) -> l
  | LessThan (_, _, _, l) -> l
  | Equal (_, _, l) -> l
  | App (_, _, l) -> l

let set_ast_info t l = match t with
  | Int (t, _) -> Int (t, l)
  | Var (t, _) -> Var (t, l)
  | Plus (t1, t2, _) -> Plus (t1, t2, l)
  | Minus (t1, t2, _) -> Minus (t1, t2, l)
  | Times (t1, t2, _) -> Times (t1, t2, l)
  | Divide (t1, t2, _) -> Divide (t1, t2, l)
  | If (e1, e2, e3, _) -> If (e1, e2, e3, l)
  | Let (x, xs, e1, e2, r, _) -> Let (x, xs, e1, e2, r, l)
  | Declare (x, xs, e, r, _) -> Declare (x, xs, e, r, l)
  | Block (es, _) -> Block (es, l)
  | True _ -> True (l)
  | False _ -> False (l)
  | GreaterThan (e1, e2, b, _) -> GreaterThan (e1, e2, b, l)
  | LessThan (e1, e2, b, _) -> LessThan (e1, e2, b, l)
  | Equal (e1, e2, _) -> Equal (e1, e2, l)
  | App (e1, es, _) -> App (e1, es, l)

let rec soa t = match t with
  | Int (i, l) ->
    "Int (" ^ (string_of_int i) ^ ", " ^ (soli l) ^ ")"
  | Var (s, l) ->
    "Var (" ^ s ^ ", " ^ (soli l) ^ ")"
  | Plus (e1, e2, l) ->
    "Plus (" ^ (soa e1) ^ ", " ^ (soa e2) ^ ", " ^ (soli l) ^ ")"
  | Minus (e1, e2, l) ->
    "Minus (" ^ (soa e1) ^ ", " ^ (soa e2) ^ ", " ^ (soli l) ^ ")"
  | Times (e1, e2, l) ->
    "Times (" ^ (soa e1) ^ ", " ^ (soa e2) ^ ", " ^ (soli l) ^ ")"
  | Divide (e1, e2, l) ->
    "Divide (" ^ (soa e1) ^ ", " ^ (soa e2) ^ ", " ^ (soli l) ^ ")"
  | If (e1, e2, e3, l) ->
    "If (" ^ (soa e1) ^ ", " ^ (soa e2) ^ ", " ^ (soa e3) ^ ", " ^ (soli l) ^ ")"
  | _ -> failwith "not implemented yet"

let sot t = match t with
  | INT (i, l) ->
    "INT (" ^ (string_of_int i) ^ ", " ^ (soli l) ^ ")"
  | VAR (s, l) ->
    "VAR (" ^ s ^ ", " ^ (soli l) ^ ")"
  | PLUS (l) ->
    "PLUS (" ^ (soli l) ^ ")"
  | MINUS (l) ->
    "MINUS (" ^ (soli l) ^ ")"
  | TIMES (l) ->
    "TIMES (" ^ (soli l) ^ ")"
  | DIVIDE (l) ->
    "DIVIDE (" ^ (soli l) ^ ")"
  | LPAREN (l) ->
    "LRAREN (" ^ (soli l) ^ ")"
  | RPAREN (l) ->
    "RPAREN (" ^ (soli l) ^ ")"
  | DOUBLE_SEMICOLON (l) ->
    "DOUBLE_SEMICOLON (" ^ (soli l) ^ ")"
  | COMMENT (s, l) ->
    "COMMENT (" ^ s ^ "," ^ (soli l) ^ ")"
  | LET (l) ->
    "LET (" ^ (soli l) ^ ")"
  | IF (l) ->
    "IF (" ^ (soli l) ^ ")"
  | THEN (l) ->
    "THEN (" ^ (soli l) ^ ")"
  | ELSE (l) ->
    "ELSE (" ^ (soli l) ^ ")"
  | EQUAL (l) ->
    "EQUAL (" ^ (soli l) ^ ")"
  | REC (l) ->
    "REC (" ^ (soli l) ^ ")"
  | IN (l) ->
    "IN (" ^ (soli l) ^ ")"
  | TRUE (l) ->
    "TRUE (" ^ (soli l) ^ ")"
  | FALSE (l) ->
    "FALSE (" ^ (soli l) ^ ")"
  | _ -> failwith "not implemented yet"

let rec disp_ast e = match e with
  | Int (t, _) ->
    string_of_int t
  | Var (t, _) ->
    t
  | Plus (t1, t2, _) ->
    "(" ^ (disp_ast t1) ^ "+" ^ (disp_ast t2) ^ ")"
  | Minus (t1, t2, _) ->
    "(" ^ (disp_ast t1) ^ "-" ^ (disp_ast t2) ^ ")"
  | Times (t1, t2, _) ->
    "(" ^ (disp_ast t1) ^ "*" ^ (disp_ast t2) ^ ")"
  | Divide (t1, t2, _) ->
    "(" ^ (disp_ast t1) ^ "/" ^ (disp_ast t2) ^ ")"
  | If (e1, e2, e3, _) ->
    "if " ^ (disp_ast e1) ^ " then " ^ (disp_ast e2) ^ " else " ^ (disp_ast e3)
  | Let (x, xs, e1, e2, r, _) ->
    "let " ^ (if r then "rec " else "") ^ x ^ " " ^
      (Util.string_of_list (fun x -> x) xs) ^ " = " ^
      (disp_ast e1) ^ "\nin " ^ (disp_ast e2)
  | Declare (x, xs, e, r, _) ->
    "let " ^ (if r then "rec " else "") ^ x ^ " " ^
      (Util.string_of_list (fun x -> x) xs) ^ " = " ^
      (disp_ast e)
  | Block (es, _) ->
    Util.join ";;\n" disp_ast es
  | True _ ->
    "true"
  | False _ ->
    "false"
  | GreaterThan (e1, e2, b, _) ->
    (disp_ast e1) ^ " >" ^ (if b then "=" else "") ^ " " ^ (disp_ast e2)
  | LessThan (e1, e2, b, _) ->
    (disp_ast e1) ^ " <" ^ (if b then "=" else "") ^ " " ^ (disp_ast e2)
  | Equal (e1, e2, _) ->
    (disp_ast e1) ^ " = " ^ (disp_ast e2)
  | App (e1, es, _) ->
    (disp_ast e1) ^ " " ^ (Util.join " " disp_ast es)

let string_of_linfo = soli

let string_of_ast = soa

let string_of_token = sot

let string_of_tokens = Util.string_of_list sot

(* display_op : op_t -> string *)
let display_op op = match op with
  | ADD -> "ADD"
  | SUB -> "SUB"
  | MUL -> "MUL"
  | DIV -> "DIV"
  | PUSHP (f) -> "PUSHP " ^ f
  | PUSH (i) -> "PUSH " ^ (string_of_int i)
  | JMP (i) -> "JMP " ^ (string_of_int i)
  | JZ (i) -> "JZ " ^ (string_of_int i)
  | GTEQ -> "GTEQ"
  | GT -> "GT"
  | LSEQ -> "LSEQ"
  | EQ -> "EQ"
  | LS -> "LS"
  | CALL (i) -> "CALL " ^ (string_of_int i)
  | RETURN -> "RETURN"
  | LABEL (i) -> "LABEL " ^ (string_of_int i)
  | MOV (Index (i)) -> "MOV " ^ (string_of_int i)
  | MOV (Offset (i)) -> "MOV " ^ (string_of_int i) ^ "(BSP)"
  | POPE (i) -> "POPE " ^ (string_of_int i)
  | WithInfo _ -> failwith "not supported"

(* display_oplocs_h : string -> oploc_t list -> string *)
let rec display_aplocs_h prefix ops = match ops with
  | [] ->
    ""
  | (o, _) :: xs ->
    (prefix ^ (display_op o) ^ "\n") ^ (display_aplocs_h prefix xs)

(* display_opcode: opcode_t -> string *)
let display_opcode { funcs = c1; main = c2 } =
  let h s (name, ops) =
    s ^ name ^ ":\n" ^ (display_aplocs_h "  " ops)
  in
  let func' = List.fold_left h "" c1 in
  let main' = display_aplocs_h "  " c2 in
  func' ^ "\n" ^ "main:\n" ^ main'

(* tokenizing exceptions *)

exception Tokenize_Error of (string * loc_info)

exception Comment_Not_Terminated of loc_info


(* parsing exceptions *)

exception SNH (* should not happen *)

exception Out_of_Index

exception Unexpected of token_t

exception Not_Found_Match of string

exception Has_No_Token


(* TamaVM-pre exceptions *)

exception Not_Supported of (string * loc_info)

exception Unbound_Variable of loc_info


(* TamaVM exceptions *)

type error_typ =
  | Tokenizing | Parsing | Prechecking | Emitting

exception FailedWith of (error_typ * string * loc_info)

exception Failed of (error_typ * string)

