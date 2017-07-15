open Types

type 'a state_t =
  (int      (* start index of remaining string  *)
    * int   (* lineno                           *)
    * int)  (* colno                            *)
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
let skip p ((ptr, lineno, colno), ss) =
  let len = String.length ss in
  let rec main pt lno cno value =
    if len <= pt then
      ((pt, lno, cno), value)
    else
      let c = String.get ss pt in
      let lno' = if is_lb c then lno + 1 else lno in 
      let cno' = if is_lb c then 0 else cno + 1 in
      if p c then main (pt + 1) lno' cno' (c :: value)
      else ((pt, lno, cno), value)
  in
  let (loc, rvalue) = main ptr lineno colno [] in
  let value = List.rev rvalue in
  (loc, clst_to_string value)

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

(* is_upper: char -> bool *)
let is_upper c = List.mem c uppers

(* until_spaces: string state_t -> string state_t *)
let until_spaces = skip_not_mem spaces

(* consist_of: string -> char list -> bool *)
let all_mem s cs = List.for_all (fun c -> List.mem c cs) (string_to_clst s)

(* look_ahead: int -> int -> string *)
let look_ahead n pt s =
  let npt = pt + n - 1 in
  let len = String.length s in
  if pt < 0 || len <= npt then ""
  else String.sub s pt n

(* look_back: int -> int -> string *)
let look_back n pt s =
  let bpt = pt - n + 1 in
  let len = String.length s in
  if bpt < 0 || len <= pt then ""
  else String.sub s bpt n

(* skip_comment: string state_t -> string state_t *)
let skip_comment ((pt, lno, cno), s) =
  let len = String.length s in
  let rec h pt_ lno_ cno_ nested text =
    begin
      let ((pt_1, lno_1, cno_1), cmt) =
        skip_not_mem ['('; ')'] ((pt_, lno_, cno_), s) in
      let text = text ^ cmt in
      let nt = look_ahead 2 pt_1 s in
      let bk = look_back 2 pt_1 s in
      if len <= pt_1 then
        raise (Comment_Not_Terminated ((pt, lno, cno), (pt_1, lno_1, cno_1)))
      else if bk = "*)" && nested = 1 then
        ((pt_1 + 1, lno_1, cno_1 + 1), text ^ ")")
      else if nt = "(*" || bk = "*)" then
        let is_open = nt = "(*" in
        let (tail, diff, nested') =
          if is_open then (nt, 2, nested + 1) else (")", 1, nested - 1) in
        h (pt_1 + diff) lno_1 (cno_1 + diff) nested' (text ^ tail)
      else
        let c = String.sub s pt_1 1 in
        h (pt_1 + 1) lno_1 (cno_1 + 1) nested (text ^ c)
    end
  in
  let hds = look_ahead 2 pt s in
  if hds <> "(*" then ((pt, lno, cno), "")
  else h (pt + 2) lno (cno + 2) 1 "(*"

(* starts_with: string -> char -> bool *)
let starts_with s c = 0 < String.length s && String.get s 0 = c

(* starts_with_mem: string -> char list -> bool *)
let rec starts_with_mem s cs = match cs with
  | [] -> false
  | x :: xs -> starts_with s x || starts_with_mem s xs

(* skip_word: string state_t -> string state_t *)
let skip_word ((pt, lno, cno) as loc, ss) =
  let len = String.length ss in
  if len <= pt then
    (loc, "")
  else
    let get_chr = String.get ss in
    let c = get_chr pt in
    if is_num c then
      skip (fun c -> List.mem c num) (loc, ss)
    else
      skip_mem words (loc, ss)

exception End_of_input

(* one_token: string state_t -> token_t state_t *)
let one_token (loc, ss) =
  let len = String.length ss in
  let ((pt, lno, cno) as loc, _) = skip_spaces (loc, ss) in
  let ((pt1, lno1, cno1) as loc1, word) = skip_word (loc, ss) in
  let starts_with_num = starts_with_mem word num in
  match word with
    | "" -> (* is not a word *)
      begin
        if len <= pt1 then raise End_of_input
        else
          let c = String.get ss pt1 in
          let nxt = pt1 + 1 in
          let myb_cmt = nxt < len && String.get ss nxt = '*' in
          let loc2 = (pt1 + 1, lno1, cno1 + 1) in
          let info = (loc1, loc2) in
          match c with
            | '+' -> (loc2, PLUS (info))
            | '-' -> (loc2, MINUS (info))
            | '*' -> (loc2, TIMES (info))
            | '/' -> (loc2, DIVIDE (info))
            | '=' -> (loc2, EQUAL (info))
            | ')' -> (loc2, RPAREN (info))
            | '(' ->
              if not myb_cmt then (loc2, LPAREN (info))
              else
                let (loc2, cmt) = skip_comment (loc1, ss) in
                let info = (loc1, loc2) in
                (loc2, COMMENT (cmt, info))
            | _ ->
              let (loc2, str) = until_spaces (loc1, ss) in
              let info = (loc1, loc2) in
              (raise (Tokenize_Error (str, info)))
       end
    | _ when starts_with_num ->
      let info = (loc, loc1) in
      (loc1, INT (int_of_string word, info))
    | _ when is_upper (String.get word 0) ->
      let info = (loc, loc1) in
      (raise (Tokenize_Error (word, info)))
    | _ ->
      let info = (loc, loc1) in
      let tkn =
        match word with
        | "let" -> LET (info)
        | "if" -> IF (info)
        | "then" -> THEN (info)
        | "else" -> ELSE (info)
        | "equal" -> EQUAL (info)
        | "rec" -> REC (info)
        | "in" -> IN (info)
        | "true" -> TRUE (info)
        | "false" -> FALSE (info)
        | _ -> VAR (word, info)
      in
      (loc1, tkn)

(* token_loop: string state_t -> token_t list *)
let rec token_loop ((pt, _, _) as loc, ss) =
  let len = String.length ss in
  try
    begin
      if pt < len then
        let (loc1, t) = one_token (loc, ss) in
        t :: (token_loop (loc1, ss))
      else []
    end
  with End_of_input -> []
     | e -> raise e

(* tokenize_main : string -> token_t list *)
let main input =
  let ts = token_loop ((0, 0, 0), input) in
(*  let _ = print_endline (Util.string_of_tokens ts) in *)
  ts
