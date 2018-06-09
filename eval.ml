open Syntax;;

let emptyenv () = []

let ext env x v = (x, v) :: env

let rec lookup x env =
    match env with
    | [] -> failwith ("unbound variable: " ^ x)
    | (y,v) :: tl -> if x = y then v 
                  else lookup x tl 


(* type exp =
    | IntLit of int
    | Plus of exp * exp
    | Times of exp * exp
    | BoolLit of bool
    | If of exp * exp * exp
    | Eq of exp * exp
    | Var of string
    | Let of string * exp * exp
    | Fun of string * exp
    | App of exp * exp *)

(* type value =
    | IntVal of int
    | BoolVal of bool
    | FunVal of string * exp * ((string * value) list) *)
        
let rec eval e env =
    let binop f e1 e2 env =
        match (eval e1 env, eval e2 env) with
        | (IntVal (n1), IntVal (n2)) -> IntVal (f n1 n2)
        | _ -> failwith "integer values expected"
    in
    match e with
    | Var (x) -> lookup x env
    | IntLit (n) -> IntVal (n)
    | Plus (e1, e2) -> binop (+) e1 e2 env
    | Times (e1, e2) -> binop ( * ) e1 e2 env
    | Eq (e1, e2) ->
        begin
            match (eval e1 env, eval e2 env) with
            | (IntVal (n1), IntVal (n2)) -> BoolVal (n1 = n2)
            | (BoolVal (b1), BoolVal (b2)) -> BoolVal (b1 = b2)
            | _ -> failwith "wrong value"
        end
    | BoolLit (b) -> BoolVal(b)
    | If (e1, e2, e3) -> 
        begin
            match (eval e1 env) with
            | BoolVal(true) -> eval e2 env
            | BoolVal(false) -> eval e3 env
            | _ -> failwith "wrong value"
        end
    | Let (x, e1, e2) ->
        let env1 = ext env x (eval e1 env)
        in eval e2 env1
    | Fun (x, e1) -> FunVal (x, e1, env)
    | App (e1, e2) ->
        begin
            match (eval e1 env) with
            | FunVal (x, body, env1) ->
                let arg = (eval e2 env)
                in eval body (ext env1 x arg)
            | _ -> failwith "function value expected"
        end

let print_value = function
    IntVal (x) -> print_string (string_of_int x ^ "\n")
    | BoolVal (x) -> print_string (string_of_bool x ^ "\n")
    | _ -> failwith "print failed"
(* テスト *)
let _ = eval (IntLit 1)
let _ = eval (IntLit 11)
let _ = eval (Plus (IntLit 1, Plus (IntLit 2, IntLit 11)))
let _ = eval (Times (IntLit 1, Plus (IntLit 2, IntLit 11)))
let _ = eval (If (Eq(IntLit 2, IntLit 11),
                   Times(IntLit 1, IntLit 2),
                   Times(IntLit 1, Plus(IntLit 2,IntLit 3))))
let _ = eval (Eq (IntLit 1, IntLit 1))
let _ = eval (Eq (IntLit 1, IntLit 2))

let _ = eval (Eq (BoolLit true, BoolLit true)) []
let _ = eval (Eq (BoolLit true, BoolLit false)) []
let _ = eval (Let ("x", IntLit 3, Var "x")) []
let _ = eval (Let ("f", Fun ("x", Plus (Var "x", IntLit 1)), (App (Var "f", IntLit 1)))) []

