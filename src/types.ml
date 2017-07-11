type loc_info =
  int            (* start *)
  * int          (* end   *)

type ast_t =
  | Int of int * loc_info
  | Var of string * loc_info
  | Plus of ast_t * ast_t * loc_info
  | Minus of ast_t * ast_t * loc_info
  | Times of ast_t * ast_t * loc_info
  | Divide of ast_t * ast_t * loc_info
  | Comment of string * loc_info

exception Tokenize_Error of (string * loc_info)

exception Comment_Not_Terminated of loc_info

type token_t =
  | INT of int * loc_info
  | VAR of string * loc_info
  | PLUS of loc_info
  | MINUS of loc_info
  | TIMES of loc_info
  | DIVIDE of loc_info
  | LPAREN of loc_info
  | RPAREN of loc_info
  | COMMENT of string * loc_info

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
  | SCOMMENT

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

let soli (l : loc_info) = match l with
  (s, e) ->
    (string_of_int s) ^ ":" ^ (string_of_int e)

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

let get_ast_info t = match t with
  | Int (_, l) -> l
  | Var (_, l) -> l
  | Plus (_, _, l) -> l
  | Minus (_, _, l) -> l
  | Times (_, _, l) -> l
  | Divide (_, _, l) -> l
  | Comment (_, l) -> l

let set_ast_info t l = match t with
  | Int (t, _) -> Int (t, l)
  | Var (t, _) -> Var (t, l)
  | Plus (t1, t2, _) -> Plus (t1, t2, l)
  | Minus (t1, t2, _) -> Minus (t1, t2, l)
  | Times (t1, t2, _) -> Times (t1, t2, l)
  | Divide (t1, t2, _) -> Divide (t1, t2, l)
  | Comment (s, _) -> Comment (s, l)

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
  | Comment (s, l) ->
    "Comemnt (" ^ s ^ ", " ^ (soli l) ^ ")"

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
  | COMMENT (s, l) ->
    "COMMENT (" ^ s ^ "," ^ (soli l) ^ ")"

let string_of_linfo = soli

let string_of_ast = soa

let string_of_token = sot

let string_of_tokens = Util.string_of_list sot
