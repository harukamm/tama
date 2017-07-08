open Types

type 'a state_t =
  int       (* start index of remaining string *)
  * int     (* lineno *)
  * 'a      (* string to read, or result buffer *)

(* is_lb *)
let is_lb c = c = '\n'

(* clst_to_string: char list -> string *)
let clst_to_string =
  List.fold_left (fun s c -> s ^ (String.make 1 c)) ""

(* string_to_clst: string -> char list *)
let rec string_to_clst s =
  let len = String.length s in
  if len = 0 then
    []
  else
    let c = String.get s 0 in
    let rest = String.sub s 1 (len - 1) in
    c :: (string_to_clst rest)

(* skip : (char -> bool) -> string state_t -> string state_t *)
let skip p (ptr, lineno, ss) =
  let len = String.length ss in
  let rec main pt lno value =
    if len <= pt then
      (pt, lno, value)
    else
      let c = String.get ss pt in
      let lno' = if is_lb c then lno + 1 else lno in 
      if p c then main (pt + 1) lno' (c :: value)
      else (pt, lno', value)
  in
  let (npr, nlno, rvalue) = main ptr lineno [] in
  let value = List.rev rvalue in
  (npr, nlno, clst_to_string value)

(* skip_mem: char list -> string state_t -> string state_t *)
let skip_mem cs = skip (fun c -> List.mem c cs)

(* spaces: char list *)
let spaces = ['\n'; '\r'; ' ']

(* skip_spaces: string state_t -> string state_t *)
let skip_spaces = skip (fun c -> List.mem c spaces)

(* ascii: int -> char *)
let ascii = Char.chr

(* aciis: int -> int -> char list *)
let asciis st en =
  let rec h i = if en < i then [] else (ascii i) :: h (i + 1) in h st

(* num: char list *)
let num = asciis 48 57
(* lowers: char list *)
let lowers = asciis 97 122
(* uppers: char list *)
let uppers = asciis 65 90
(* words: char list *)
let words = '_' :: lowers @ uppers @ num

(* is_num: char -> bool *)
let is_num c = List.mem c num
(* skip_num: string state_t -> string state_t *)
let skip_num = skip_mem num

(* consist_of: string -> char list -> bool *)
let all_mem s cs = List.for_all (fun c -> List.mem c cs) (string_to_clst s)

(* starts_with: string -> char -> bool *)
let starts_with s c = 0 < String.length s && String.get s 0 = c

(* starts_with_mem: string -> char list -> bool *)
let rec starts_with_mem s cs = match cs with
  | [] -> false
  | x :: xs -> starts_with s x || starts_with_mem s xs

(* skip_word: string state_t -> string state_t *)
let skip_word (pt, lno, ss) =
  let len = String.length ss in
  if len <= pt then
    (pt, lno, "")
  else
    let get_chr = String.get ss in
    let c = get_chr pt in
    let has_next = pt + 1 < len in
    if is_num c then
      skip (fun c -> List.mem c num) (pt, lno, ss)
    else if c = '-' && has_next && is_num (get_chr (pt + 1)) then
      let (pt1, lno1, v) = skip_num (pt + 1, lno, ss) in
      (pt1, lno1, "-" ^ v)
    else
      skip_mem words (pt, lno, ss)

exception End_of_input

(* one_token: string state_t -> token_t state_t *)
let one_token (pt, lno, ss) =
  let len = String.length ss in
  let (pt, lno, _) = skip_spaces (pt, lno, ss) in
  let (pt1, lno1, word) = skip_word (pt, lno, ss) in
  let starts_with_num = starts_with_mem word ('-' :: num) in
  match word with
    | "" -> (* is not a word *)
      begin
        if len <= pt1 then raise End_of_input
        else
          let c = String.get ss pt1 in
          let info = (lno1, pt1, pt1 + 1) in
          match c with
            | '+' -> (pt1 + 1, lno1, PLUS (info))
            | '-' -> (pt1 + 1, lno1, MINUS (info))
            | '*' -> (pt1 + 1, lno1, TIMES (info))
            | '/' -> (pt1 + 1, lno1, DIVIDE (info))
            | _ -> failwith (String.make 1 c)
       end
    | _ when starts_with_num ->
      let info = (lno1, pt, pt1) in
      (pt1, lno1, INT (int_of_string word, info))
    | _ ->
      let info = (lno1, pt, pt1) in
      (pt1, lno1, VAR (word, info))

(* token_loop: string state_t -> token_t list *)
let rec token_loop (pt, lno, ss) =
  let len = String.length ss in
  try
    begin
      if pt < len then
        let (pt1, lno1, t) = one_token (pt, lno, ss) in
        t :: (token_loop (pt1, lno1, ss))
      else []
    end
  with End_of_input -> []
     | e -> raise e

(* tokenize_main : string -> token_t list *)
let main input =
  let ts = token_loop (0, 0, input) in
(*  let _ = print_endline (Util.string_of_tokens ts) in *)
  ts

(* tokenize : string -> Types.token_t list *)
let tokenize s = main s
