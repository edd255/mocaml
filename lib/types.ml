(* The implementation of check_op is very naive, since you could define  
 * other operators as well which are complex. For that, you would need to
 * check, whether e1: t1 and e2: t2 satisfy o: t1 -> t2 -> t3. This would
 * not be a problem but then, you would need to know the type of the     
 * operator, which is not always known.                                  
 * For the typing rules, see Figure 2.2. *)

open Environment
open Tokens

(* see section 7.2 *)
module TypeChecker = struct
  let rec check env e : ty =
    match e with
    | Var x -> check_var env x
    | Con (Bcon _) -> Bool
    | Con (Icon _) -> Int
    | Oapp (o, e1, e2) -> check_op o (check env e1) (check env e2)
    | Fapp (e1, e2) -> check_fun (check env e1) (check env e2)
    | If (e1, e2, e3) -> check_con env e1 e2 e3
    | Lam (_, _) -> failwith "check: Lam not allowed"
    | Lamty (x, t, e) -> Arrow (t, check (update env x t) e)
    | Let (x, e1, e2) -> check_let env x e1 e2
    | Letrec (_, _, _, _) -> failwith "check: Letrec not allowed"
    | Letrecty (f, x, t1, t2, e1, e2) -> check_rec_let env f x t1 t2 e1 e2

  and check_var env x =
    match lookup env x with
    | Some t -> t
    | _ -> failwith "check_var: type expected"

  and check_op o (t1 : ty) (t2 : ty) =
    match (o, t1, t2) with
    | Add, Int, Int -> Int
    | Sub, Int, Int -> Int
    | Mul, Int, Int -> Int
    | Div, Int, Int -> Int
    | Leq, Int, Int -> Bool
    | Eq, Int, Int -> Bool
    | _, _, _ -> failwith "check_op: operator unkown/wrong types"

  and check_fun t1 t2 =
    match t1 with
    | Arrow (t1', _) -> if t1 = t1' then t2 else failwith "check_fun: type"
    | _ -> failwith "check_fun: e1 is not a function"

  and check_con env e1 e2 e3 =
    if check env e1 = Bool then
      if check env e2 = check env e3 then check env e2
      else failwith "check_con: e2 and e3 need to have the same type"
    else failwith "check_con: e1 must be bool"

  and check_let env x e1 e2 =
    let envf = check env e1 in
    check (update env x envf) e2

  and check_rec_let env f x t1 t2 e1 e2 =
    let envf = update env f (Arrow (t1, t2)) in
    if check (update envf x t1) e1 = t2 then check envf e2
    else failwith "let rec: type did not match"
end
