type loc_info =
  int            (* lineno *)
  * (int         (* colno of start *)
   * int)        (* colno of end *)

type loc_info2 =
  (int           (* lineno of start *)
    * int)       (* colno of start *)
  * (int         (* lineno of end *)
      * int)     (* colno of end *)

type ast_t =
  | Int of int * loc_info2
  | Var of string * loc_info2
  | Plus of ast_t * ast_t * loc_info2
  | Minus of ast_t * ast_t * loc_info2
  | Times of ast_t * ast_t * loc_info2
  | Divide of ast_t * ast_t * loc_info2

exception Tokenize_Error of (string * loc_info)

type token_t =
  | INT of int * loc_info
  | VAR of string * loc_info
  | PLUS of loc_info
  | MINUS of loc_info
  | TIMES of loc_info
  | DIVIDE of loc_info

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

let tkn_eq sym t = match (sym, t) with
  | (SINT, INT _) | (SVAR, VAR _)
  | (SPLUS, PLUS _) | (SMINUS, MINUS _)
  | (STIMES, TIMES _) | (SDIVIDE, DIVIDE _) -> true
  | _ -> false

let rec tkn_mem syms t = match syms with
  | [] -> false
  | x :: xs -> tkn_eq x t || tkn_mem xs t 

let rec tkns_eq syms ts = match (syms,  ts) with
  | ([], []) -> true
  | (x :: xs, []) -> false
  | ([], y :: ys) -> false
  | (x :: xs, y :: ys) -> (tkn_eq x y) && (tkns_eq xs ys)

let soli (l : loc_info) = match l with
  (ln, (s, e)) ->
    "line:" ^ (string_of_int ln) ^ " col:" ^ (string_of_int s) ^ ":" ^ (string_of_int e)

let soli2 (l : loc_info2) = match l with
  ((ls, cs), (le, ce)) ->
    "start:" ^ (string_of_int ls) ^ "," ^ (string_of_int cs) ^ ":" ^
      (string_of_int le) ^ "," ^ (string_of_int ce)

let rec soa t = match t with
  | Int (i, l) ->
    "Int (" ^ (string_of_int i) ^ ", " ^ (soli2 l) ^ ")"
  | Var (s, l) ->
    "Var (" ^ s ^ ", " ^ (soli2 l) ^ ")"
  | Plus (e1, e2, l) ->
    "Plus (" ^ (soa e1) ^ ", " ^ (soa e2) ^ ", " ^ (soli2 l) ^ ")"
  | Minus (e1, e2, l) ->
    "Minus (" ^ (soa e1) ^ ", " ^ (soa e2) ^ ", " ^ (soli2 l) ^ ")"
  | Times (e1, e2, l) ->
    "Times (" ^ (soa e1) ^ ", " ^ (soa e2) ^ ", " ^ (soli2 l) ^ ")"
  | Divide (e1, e2, l) ->
    "Divide (" ^ (soa e1) ^ ", " ^ (soa e2) ^ ", " ^ (soli2 l) ^ ")"

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

let string_of_linfo = soli

let string_of_ast = soa

let string_of_token = sot

let string_of_tokens = Util.string_of_list sot
