#load "util.cmo";;
#load "types.cmo";;
#load "parse.cmo";;

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


(* Parse test *)
let e1 = Parse.skip (fun x -> x = '\n') (0, 0, 0, "\n\n\nAA")
let e2 = Parse.skip (fun x -> x = 'c') (0, 0, 0, "ccc\n XX")
let e3 = Parse.skip_spaces (0, 0, 0, "  \r \n   XX")
let e4 = Parse.skip_num (3, 2, 3, "abc123\n  ")
let s1 = " \n\
\t xx "
let e5 = Parse.skip_spaces (0, 0, 0, s1)

let () = assert ((3, 3, 0, "\n\n\n") = e1)
let () = assert ((3, 0, 3, "ccc") = e2)
let () = assert ((8, 1, 3, "  \r \n   ") = e3)
let () = assert ((6, 2, 6, "123") = e4)
let () = assert ((4, 1, 2, " \n\t ") = e5)

let e1 = Parse.skip_word (2, 0, 2, "xx44xx")
let e2 = Parse.skip_word (3, 0, 3, "xxx-xxx")
let e3 = Parse.skip_word (0, 0, 0, "abc123 dko")

let () = assert ((4, 0, 4, "44") = e1)
let () = assert ((3, 0, 3, "") = e2)
let () = assert ((6, 0, 6, "abc123") = e3)

let e1 = Parse.one_token (2, 0, 2, "12aBCxxx =")
let i1 = (0, (2, 8))
let e2 = Parse.one_token (2, 0, 2, "12+")
let i2 = (0, (2, 3))
let s1 = " \n\
\t\txx "
let e3 = Parse.one_token (0, 0, 0, s1)
let i3 = (1, (2, 4))
let s2 = "    \n\
\t   \n\
\t    50000 \n"
let e4 = Parse.one_token (0, 0, 0, s2)
let i4 = (2, (5, 10))

let () = assert ((8, 0, 8, Types.VAR ("aBCxxx", i1)) = e1)
let () = assert ((3, 0, 3, Types.PLUS (i2)) = e2)
let () = assert ((6, 1, 4, Types.VAR ("xx", i3)) = e3)
let () = assert ((20, 2, 10, Types.INT (50000, i4)) = e4)

let s = "5 +  10\n\
\t-1\n"
let e = Parse.tokenize s
let t1 = Types.INT (5, (0, (0, 1)))
let t2 = Types.PLUS (0, (2, 3))
let t3 = Types.INT (10, (0, (5, 7)))
let t4 = Types.MINUS (1, (1, 2))
let t5 = Types.INT (1, (1, (2, 3)))
let () = assert ([t1; t2; t3; t4; t5] = e)

let s = "  xx - 1 \n\
\t xb_x \t  *  "
let e = Parse.tokenize s
let t1 = Types.VAR ("xx", (0, (2, 4)))
let t2 = Types.MINUS (0, (5, 6))
let t3 = Types.INT (1, (0, (7, 8)))
let t4 = Types.VAR ("xb_x", (1, (2, 6)))
let t5 = Types.TIMES (1, (10, 11))
let () = assert ([t1; t2; t3; t4; t5] = e)

let () = print_endline "<<<<<<<<<<<<<<"
let () = print_endline "Success"
let () = print_endline "<<<<<<<<<<<<<<"
let () = exit 0
