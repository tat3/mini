let emptyenv () = []

let ext env x v = (x, v) :: env

let rec lookup x env =
    match env with
    | [] -> failwith ("unbound variable: " ^ x)
    | (y,v) :: tl -> if x = y then v 
                  else lookup x tl 


type exp =
    | IntLit of int
    | Plus of exp * exp
    | Times of exp * exp
    | BoolLit of bool
    | If of exp * exp * exp
    | Eq of exp * exp
    | Var of string
    | Let of string * exp * exp


type value =
    | IntVal of int
    | BoolVal of bool
    
let rec eval3 e env =
    let binop f e1 e2 env =
        match (eval3 e1 env, eval3 e2 env) with
        | (IntVal (n1), IntVal (n2)) -> IntVal (f n1 n2)
        | _ -> failwith "integer values expected"
    in
    match e with
    | Var (x) -> lookup x env
    | IntLit (n) -> IntVal (n)
    | Plus (e1, e2) -> binop (+) e1 e2 env
    | Times (e1, e2) -> binop (+) e1 e2 env
    | Eq (e1, e2) ->
        begin
            match (eval3 e1 env, eval3 e2 env) with
            | (IntVal (n1), IntVal (n2)) -> BoolVal (n1 = n2)
            | (BoolVal (b1), BoolVal (b2)) -> BoolVal (b1 = b2)
            | _ -> failwith "wrong value"
        end
    | BoolLit (b) -> BoolVal(b)
    | If (e1, e2, e3) -> 
        begin
            match (eval3 e1 env) with
            | BoolVal(true) -> eval3 e2 env
            | BoolVal(false) -> eval3 e3 env
            | _ -> failwith "wrong value"
        end
    | Let (x, e1, e2) ->
        let env1 = ext env x (eval3 e1 env)
        in eval3 e2 env1

let print_value = function
    IntVal (x) -> print_string (string_of_int x ^ "\n")
    | BoolVal (x) -> print_string (string_of_bool x ^ "\n")
    
(* テスト *)
let _ = eval3 (IntLit 1)
let _ = eval3 (IntLit 11)
let _ = eval3 (Plus (IntLit 1, Plus (IntLit 2, IntLit 11)))
let _ = eval3 (Times (IntLit 1, Plus (IntLit 2, IntLit 11)))
let _ = eval3 (If (Eq(IntLit 2, IntLit 11),
                   Times(IntLit 1, IntLit 2),
                   Times(IntLit 1, Plus(IntLit 2,IntLit 3))))
let _ = eval3 (Eq (IntLit 1, IntLit 1))
let _ = eval3 (Eq (IntLit 1, IntLit 2))
let () = print_value (eval3 (Eq (BoolLit true, BoolLit true)) [])
let () = print_value (eval3 (Eq (BoolLit true, BoolLit false)) [])
let () = print_value (eval3 (Let ("x", IntLit 3, Var "x")) [])

