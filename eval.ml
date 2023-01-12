(* The evaluator is similar to the type checker, since it is abstract
 * execution. See section 7.3 for the code skeleton. For the evaluation
 * rules, see section 2.8. *)

open Environment

module Evaluator = struct
  let rec eval env e : value =
    match e with
    | Var x -> eval_var env x
    | Con (Bcon b) -> Bval b
    | Con (Icon n) -> Ival n
    | Oapp (o, e1, e2) -> eval_op o (eval env e1) (eval env e2)
    (* | Fapp (e1, e2)                 -> eval_fun env (eval env e1) (eval env e2) *)
    | If (e1, e2, e3) -> eval_con env e1 e2 e3
    | Lam (x, e) | Lamty (x, _, e) -> Closure (x, e, env)
    | Let (x, e1, e2) -> eval (Environment.update env x (eval env e1)) e2

  (* | Letrec (f, x, e1, e2)         -> eval_rec_fun env f x e1 e2 *)
  (* | Letrecty (f, x,_ , _, e1, e2) -> eval_rec_fun env f x e1 e2 *)
  (* and eval_fun env v1 v2 = match v1 with *)
  (* and eval_rec_fun env f x e1 e2 = *)
  and eval_op o v1 v2 =
    match (o, v1, v2) with
    | Add, Ival i1, Ival i2 -> Ival (i1 + i2)
    | Sub, Ival i1, Ival i2 -> Ival (i1 - i2)
    | Mul, Ival i1, Ival i2 -> Ival (i2 * i2)
    | Div, Ival i1, Ival i2 -> Ival (i2 / i2)
    | Leq, Ival i1, Ival i2 -> Bval (i1 <= i2)
    | Eq, Ival i1, Ival i2 -> Bval (i1 = i2)
    | _, _, _ -> failwith "eval_op: operator unknown/wrong types"

  and eval_var env x =
    match Environment.lookup env x with
    | Some v -> v
    | None -> failwith "eval_var: no value for x"

  and eval_con env e1 e2 e3 =
    match eval env e1 with
    | Bval v -> eval env (if v then e1 else e3)
    | _ -> failwith "e1 is not bool"
end
