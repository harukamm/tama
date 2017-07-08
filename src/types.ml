type loc_info =
  int            (* lineno *)
  * int          (* colno of start *)
  * int          (* colno of end *)

type ast_t =
  | Int of int * loc_info
  | Var of string * loc_info
  | Plus of ast_t * ast_t * loc_info
  | Minus of ast_t * ast_t * loc_info
  | Times of ast_t * ast_t * loc_info
  | Divide of ast_t * ast_t * loc_info

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

let soli (l : loc_info) = match l with
  (ln, s, e) ->
    "line:" ^ (string_of_int ln) ^ " col:" ^ (string_of_int s) ^ ":" ^ (string_of_int e)

let rec soa t = match t with
  | Int (i, l) ->
    "Int (" ^ (string_of_int i) ^ ", " ^ (soli l) ^")"
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
