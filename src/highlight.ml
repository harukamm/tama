open Types

(* class_of_token: token_t -> string *)
let class_of_token tkn = match tkn with
  | INT (n, _) -> "_tint"
  | VAR _ -> "_ivar"
  | PLUS _ -> "_tap"
  | MINUS _ -> "_tam"
  | TIMES _ -> "_tat"
  | DIVIDE _ -> "_tad"
  | LPAREN _ -> "_tlp"
  | RPAREN _ -> "_trp"
  | COMMENT _ -> "_tcmt"

(* sub: string -> int -> int -> string *)
let sub s in1 in2 =
  String.sub s in1 (in2 - in1)

(* span: string -> string -> string *)
let span cls content =
  "<span class='" ^ cls ^ "'>" ^ content ^ "</span>"

(* span2: token_t -> string -> string *)
let span2 tkn = span (class_of_token tkn)

(* loop: string state_t -> string -> string *)
let rec loop ((pt, ss) as st) acc =
  try
    begin
      let (_, tkn) = Tokenize.one_token st in
      let (pt1, pt2) = get_tkn_info tkn in
      let not_token = sub ss pt pt1 in
      let token = sub ss pt1 pt2 in
      let token' = span2 tkn token in
      let acc' = acc ^ not_token ^ token' in
      loop (pt2, ss) acc'
    end
  with
  | Tokenize.End_of_input
  | Tokenize_Error _ ->
    let len = String.length ss in
    let rest = if pt < len then sub ss pt len else "" in
    acc ^ rest
  | Comment_Not_Terminated (pt1, pt2) ->
    let btn = sub ss pt pt1 in
    let rest = sub ss pt1 pt2 in
    acc ^ btn ^ (span "_tcmt" rest)
  | _ ->
    acc

(* main: string -> string *)
let main s = loop (0, s) ""
