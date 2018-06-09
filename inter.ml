open Main;;
open Eval;;
open Syntax;;


let () = print_value (eval (parse Sys.argv.(1)) [])
