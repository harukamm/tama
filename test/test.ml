#load "util.cmo";;
#load "types.cmo";;
#load "tokenize.cmo";;
#load "parse.cmo";;

open Types;;

let () = print_endline ">>>>>>>>>>>>>>>"
let () = print_endline " Testing..."
let () = print_endline ">>>>>>>>>>>>>>>"

let cnt = ref 0

let soi = string_of_int

(* Util test *)
let s1 = Util.string_of_list Util.soi [1; 2; 3]
let s2 = Util.string_of_list (fun x -> x) ["a"; "b"; "c"]

let () = assert ("[1; 2; 3]" = s1)
let () = assert ("[a; b; c]" = s2)


(* Tokenize test *)
let e1 = Tokenize.skip (fun x -> x = '\n') (0, 0, 0, "\n\n\nAA")
let e2 = Tokenize.skip (fun x -> x = 'c') (0, 0, 0, "ccc\n XX")
let e3 = Tokenize.skip_spaces (0, 0, 0, "  \r \n   XX")
let e4 = Tokenize.skip_num (3, 2, 3, "abc123\n  ")
let s1 = " \n\
\t xx "
let e5 = Tokenize.skip_spaces (0, 0, 0, s1)

let () = assert ((3, 3, 0, "\n\n\n") = e1)
let () = assert ((3, 0, 3, "ccc") = e2)
let () = assert ((8, 1, 3, "  \r \n   ") = e3)
let () = assert ((6, 2, 6, "123") = e4)
let () = assert ((4, 1, 2, " \n\t ") = e5)

let e1 = Tokenize.skip_word (2, 0, 2, "xx44xx")
let e2 = Tokenize.skip_word (3, 0, 3, "xxx-xxx")
let e3 = Tokenize.skip_word (0, 0, 0, "abc123 dko")

let () = assert ((4, 0, 4, "44") = e1)
let () = assert ((3, 0, 3, "") = e2)
let () = assert ((6, 0, 6, "abc123") = e3)

let e1 = Tokenize.one_token (2, 0, 2, "12aBCxxx =")
let i1 = (0, (2, 8))
let e2 = Tokenize.one_token (2, 0, 2, "12+")
let i2 = (0, (2, 3))
let s1 = " \n\
\t\txx "
let e3 = Tokenize.one_token (0, 0, 0, s1)
let i3 = (1, (2, 4))
let s2 = "    \n\
\t   \n\
\t    50000 \n"
let e4 = Tokenize.one_token (0, 0, 0, s2)
let i4 = (2, (5, 10))

let () = assert ((8, 0, 8, VAR ("aBCxxx", i1)) = e1)
let () = assert ((3, 0, 3, PLUS (i2)) = e2)
let () = assert ((6, 1, 4, VAR ("xx", i3)) = e3)
let () = assert ((20, 2, 10, INT (50000, i4)) = e4)

let s = "5 +  10\n\
\t-1\n"
let e = Tokenize.main s
let t1 = INT (5, (0, (0, 1)))
let t2 = PLUS (0, (2, 3))
let t3 = INT (10, (0, (5, 7)))
let t4 = MINUS (1, (1, 2))
let t5 = INT (1, (1, (2, 3)))
let () = assert ([t1; t2; t3; t4; t5] = e)

let s = "  xx - 1 \n\
\t xb_x \t  *  "
let e = Tokenize.main s
let t1 = VAR ("xx", (0, (2, 4)))
let t2 = MINUS (0, (5, 6))
let t3 = INT (1, (0, (7, 8)))
let t4 = VAR ("xb_x", (1, (2, 6)))
let t5 = TIMES (1, (10, 11))
let () = assert ([t1; t2; t3; t4; t5] = e)

let s = " / \n\
\t  @xx@@  "
let info = (1, (3, 8))
let e =
  try
    let _ = Tokenize.main s in
    false
  with Tokenize_Error (str, inf) -> str = "@xx@@" && inf = info
let () = assert e

let s = " (* A(*ABC*)D  \n\
*) x (*x*)"
let e = Tokenize.main s
let t1 = COMMENT ("(* A(*ABC*)D  \n*)", ((0, 1), (1, 2)))
let t2 = VAR ("x", (1, (3, 4)))
let t3 = COMMENT ("(*x*)", ((1, 5), (1, 10)))
let () = assert ([t1; t2; t3] = e)

let s = " (* (* (*\n\
ABC*)"
let e =
  try
    let _ = Tokenize.main s in
    false
  with Comment_Not_Terminated info ->
    info = ((0, 1), (1, 5))
let () = assert e

(* Parse test *)
let init_ptr () = Parse.set 0
let init_tkn ts = Parse.set_tkn (Array.of_list ts)
let init_status ts =
  let _ = init_ptr () in
  let _ = init_tkn ts in ()

let is_int = tkn_eq SINT
let is_var = tkn_eq SVAR
let is_plus = tkn_eq SPLUS
let is_minus = tkn_eq SMINUS
let is_times = tkn_eq STIMES
let is_divide = tkn_eq SDIVIDE

let ts = Tokenize.main " 5 + 10 "
let () = init_status ts
let () = assert (is_int (Parse.expect is_int))
let () = assert (is_plus (Parse.expect is_plus))
let () = assert (is_int (Parse.expect is_int))
let b =
  try
    let _ = Parse.expect is_plus in
    false
  with Parse.Out_of_Index -> true
let () = assert b

let () = init_ptr ()
let lst = Parse.forsee 3
let () = assert (tkns_eq [SINT; SPLUS; SINT] lst)
let () = assert (Parse.index () = 0)
let () = assert (let _ = Parse.expect_mult 3 (tkns_eq [SINT; SPLUS; SINT]) in true)
let () = init_ptr ()
let () = assert (let _ = Parse.expect_tkns [SINT; SPLUS] in true)
let () = assert (let _ = Parse.expect_tkns [SINT] in true)

let s = "   - \n\
\t   50"
let ts = Tokenize.main s
let () = init_status ts
let e = Parse.negative_num ()
let info = ((0, 3), (1, 6))
let () = assert (Int (-50, info)  = e)
let () = init_ptr ()
let e = Parse.int_ ()
let () = assert (Int (-50, info) = e)
let () = init_ptr ()
let e = Parse.value ()
let () = assert (Int (-50, info) = e)

let s = " 5 - 1 - 2"
let ts = Tokenize.main s
let () = init_status ts
let e = Parse.expr ()
let x1 = Int (5, ((0, 1), (0, 2)))
let x2 = Int (1, ((0, 5), (0, 6)))
let x3 = Int (2, ((0, 9), (0, 10)))
let x = Minus (Minus (x1, x2, ((0, 1), (0, 6))), x3, ((0, 1), (0, 10)))
let () = assert (x = e)

let s = " 5 + 4 * - 1 "
let ts = Tokenize.main s
let () = init_status ts
let e = Parse.expr ()
let x1 = Int (4, ((0, 5), (0, 6)))
let x2 = Int (-1, ((0, 9), (0, 12)))
let x3 = Times (x1, x2, ((0, 5), (0, 12)))
let x4 = Int (5, ((0, 1), (0, 2)))
let x5 = Plus (x4, x3, ((0, 1), (0, 12)))
let () = assert (x5 = e)

let s = " 10 / 2 * 3 \n\
+ 5"
let ts = Tokenize.main s
let () = init_status ts
let e = Parse.expr ()
let x1 = Int (10, ((0, 1), (0, 3)))
let x2 = Int (2, ((0, 6), (0, 7)))
let x3 = Int (3, ((0, 10), (0, 11)))
let x4 = Int (5, ((1, 2), (1, 3)))
let x5 = Divide (x1, x2, ((0, 1), (0, 7)))
let x6 = Times (x5, x3, ((0, 1), (0, 11)))
let x7 = Plus (x6, x4, ((0, 1), (1, 3)))
let () = assert (x7 = e)

let s = " 10 / 5 / (1 - (2))"
let ts = Tokenize.main s
let () = init_status ts
let e = Parse.expr ()
let x1 = Int (10, ((0, 1), (0, 3)))
let x2 = Int (5, ((0, 6), (0, 7)))
let x3 = Int (1, ((0, 11), (0, 12)))
let x4 = Int (2, ((0, 15), (0, 18)))
let x5 = Divide (x1, x2, ((0, 1), (0, 7)))
let x6 = Minus (x3, x4, ((0, 11), (0, 18)))
let x7 = Divide (x5, x6, ((0, 1), (0, 19)))

let () = print_endline "<<<<<<<<<<<<<<"
let () = print_endline "Success"
let () = print_endline "<<<<<<<<<<<<<<"
let () = exit 0

