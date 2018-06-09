type exp =
    | IntLit of int
    | Plus of exp * exp
    | Times of exp * exp
    | BoolLit of bool
    | If of exp * exp * exp
    | Eq of exp * exp

type value =
    | IntVal of int
    | BoolVal of bool
    
let rec eval2 e =
    let binop f e1 e2 =
        match (eval2 e1, eval2 e2) with
        | (IntVal (n1), IntVal (n2)) -> IntVal (f n1 n2)
        | _ -> failwith "integer values expected"
    in
    match e with
    | IntLit (n) -> IntVal (n)
    | Plus (e1, e2) -> binop (+) e1 e2
    | Times (e1, e2) -> binop (+) e1 e2
    | Eq (e1, e2) ->
        begin
            match (eval2 e1, eval2 e2) with
            | (IntVal (n1), IntVal (n2)) -> BoolVal (n1 = n2)
            | (BoolVal (b1), BoolVal (b2)) -> BoolVal (b1 = b2)
            | _ -> failwith "wrong value"
        end
    | BoolLit (b) -> BoolVal(b)
    | If (e1, e2, e3) -> 
        begin
            match (eval2 e1) with
            | BoolVal(true) -> eval2 e2
            | BoolVal(false) -> eval2 e3
            | _ -> failwith "wrong value"
        end



(* テスト *)
let _ = eval2 (IntLit 1)
let _ = eval2 (IntLit 11)
let _ = eval2 (Plus (IntLit 1, Plus (IntLit 2, IntLit 11)))
let _ = eval2 (Times (IntLit 1, Plus (IntLit 2, IntLit 11)))
let _ = eval2 (If (Eq(IntLit 2, IntLit 11),
                   Times(IntLit 1, IntLit 2),
                   Times(IntLit 1, Plus(IntLit 2,IntLit 3))))
let _ = eval2 (Eq (IntLit 1, IntLit 1))
let _ = eval2 (Eq (IntLit 1, IntLit 2))
let _ = eval2 (Eq (BoolLit true, BoolLit true))
let _ = eval2 (Eq (BoolLit true, BoolLit false))    
