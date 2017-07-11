open Types

type 'a state_t =
  int       (* start index of remaining string *)
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
let skip p (ptr, ss) =
  let len = String.length ss in
  let rec main pt value =
    if len <= pt then
      (pt, value)
    else
      let c = String.get ss pt in
      if p c then main (pt + 1) (c :: value)
      else (pt, value)
  in
  let (npr, rvalue) = main ptr [] in
  let value = List.rev rvalue in
  (npr, clst_to_string value)

(* skip_mem: char list -> string state_t -> string state_t *)
let skip_mem cs = skip (fun c -> List.mem c cs)

(* skip_not_mem: char list -> string state_t -> string state_t *)
let skip_not_mem cs = skip (fun c -> not (List.mem c cs))

(* spaces: char list *)
let spaces = ['\n'; '\r'; ' '; '\t']

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

(* until_spaces: string state_t -> string state_t *)
let until_spaces = skip_not_mem spaces

(* consist_of: string -> char list -> bool *)
let all_mem s cs = List.for_all (fun c -> List.mem c cs) (string_to_clst s)

(* look_ahead: int -> int -> string *)
let look_ahead n pt s =
  let npt = pt + n in
  let len = String.length s in
  if pt <= 0 || len <= npt then ""
  else String.sub s pt n

(* look_back: int -> int -> string *)
let look_back n pt s =
  let bpt = pt - n + 1 in
  let len = String.length s in
  if bpt <= 0 || len <= pt then ""
  else String.sub s bpt n

(* skip_comment: string state_t -> string state_t *)
let skip_comment (pt, s) =
  let len = String.length s in
  let rec h pt_ nested text =
    begin
      let (pt_1, cmt) =
        skip_not_mem ['('; ')'] (pt_, s) in
      let text = text ^ cmt in
      let nt = look_ahead 2 pt_1 s in
      let bk = look_back 2 pt_1 s in
      if len <= pt_1 then
        raise (Comment_Not_Terminated (pt, pt_1))
      else if bk = "*)" && nested = 1 then
        (pt_1 + 1, text ^ ")")
      else if nt = "(*" || bk = "*)" then
        let is_open = nt = "(*" in
        let (tail, diff, nested') =
          if is_open then (nt, 2, nested + 1) else (")", 1, nested - 1) in
        h (pt_1 + diff) nested' (text ^ tail)
      else
        let c = String.sub s pt_1 1 in
        h (pt_1 + 1) nested (text ^ c)
    end
  in
  let hds = look_ahead 2 pt s in
  if hds <> "(*" then (pt, "")
  else h (pt + 2) 1 "(*"

(* starts_with: string -> char -> bool *)
let starts_with s c = 0 < String.length s && String.get s 0 = c

(* starts_with_mem: string -> char list -> bool *)
let rec starts_with_mem s cs = match cs with
  | [] -> false
  | x :: xs -> starts_with s x || starts_with_mem s xs

(* skip_word: string state_t -> string state_t *)
let skip_word (pt, ss) =
  let len = String.length ss in
  if len <= pt then
    (pt, "")
  else
    let get_chr = String.get ss in
    let c = get_chr pt in
    if is_num c then
      skip (fun c -> List.mem c num) (pt, ss)
    else
      skip_mem words (pt, ss)

exception End_of_input

(* one_token: string state_t -> token_t state_t *)
let one_token (pt, ss) =
  let len = String.length ss in
  let (pt, _) = skip_spaces (pt, ss) in
  let (pt1, word) = skip_word (pt, ss) in
  let starts_with_num = starts_with_mem word num in
  match word with
    | "" -> (* is not a word *)
      begin
        if len <= pt1 then raise End_of_input
        else
          let c = String.get ss pt1 in
          let nxt = pt1 + 1 in
          let myb_cmt = nxt < len && String.get ss nxt = '*' in
          let info = (pt1, pt1 + 1) in
          match c with
            | '+' -> (pt1 + 1, PLUS (info))
            | '-' -> (pt1 + 1, MINUS (info))
            | '*' -> (pt1 + 1, TIMES (info))
            | '/' -> (pt1 + 1, DIVIDE (info))
            | ')' -> (pt1 + 1, RPAREN (info))
            | '(' ->
              if not myb_cmt then (pt1 + 1, LPAREN (info))
              else
                let (pt2, cmt) = skip_comment (pt1, ss) in
                let info = (pt1, pt2) in
                (pt2, COMMENT (cmt, info))
            | _ ->
              let (pt2, str) = until_spaces (pt1, ss) in
              let info = (pt1, pt2) in
              (raise (Tokenize_Error (str, info)))
       end
    | _ when starts_with_num ->
      let info = (pt, pt1) in
      (pt1, INT (int_of_string word, info))
    | _ ->
      let info = (pt, pt1) in
      (pt1, VAR (word, info))

(* token_loop: string state_t -> token_t list *)
let rec token_loop (pt, ss) =
  let len = String.length ss in
  try
    begin
      if pt < len then
        let (pt1, t) = one_token (pt, ss) in
        t :: (token_loop (pt1, ss))
      else []
    end
  with End_of_input -> []
     | e -> raise e

(* tokenize_main : string -> token_t list *)
let main input =
  let ts = token_loop (0, input) in
(*  let _ = print_endline (Util.string_of_tokens ts) in *)
  ts
