open Util

module Parser = struct
  (* the grammar for types, see section 7.5
   * ty  ::= pty ty'
   * ty' ::= "->" ty | []
   * pty ::= "bool" | "int" | "(" ty ")" *)

  let rec tygr l =
    let t, l = ptygr l in
    tygr' t l

  and tygr' t l =
    match l with
    | ARR :: l ->
        let t', l = tygr l in
        (Arrow (t, t'), l)
    | _ -> (t, l)

  and ptygr l =
    match l with
    | BOOL :: l -> (Bool, l)
    | INT :: l -> (Int, l)
    | LP :: l ->
        let t, l = tygr l in
        (t, Util.verify RP l)
    | _ -> failwith "ptygr"

  (* I can't explain why, but the parentheses in the third line are needed. *)

  (* the linearization grammar, section 6.6
   * cexp  ::= sexp cexp′
   * cexp′ ::= "<=" sexp | "=" sexp | []
   * sexp  ::= mexp sexp′
   * sexp′ ::= "+" mexp sexp′ | "-" mexp sexp′ | []
   * mexp  ::= aexp mexp′
   * mexp′ ::= "*" aexp mexp′ | "/" aexp mexp | []
   * aexp  ::= pexp aexp′
   * aexp′ ::= pexp aexp′ | []
   * pexp  ::= var | con | "(" exp ")" *)

  (* parsing grammar
   * see section 7.5, page 116
   * exp      if, fun, let        top level
   * cexp     <=, =               comparisons
   * sexp     +, -                additive operators
   * mexp     *, /                multiplicative operators
   * aexp                         function applications
   * pexp                         bottom level *)

  (* see section 7.5 *)
  let rec exp l : exp * token list =
    match l with
    | IF :: l ->
        let e1, l = exp l in
        let e2, l = exp (Util.verify THEN l) in
        let e3, l = exp (Util.verify ELSE l) in
        (If (e1, e2, e3), l)
    | LAM :: VAR x :: ARR :: l ->
        let e1, l = exp l in
        (Lam (x, e1), l)
    | LAM :: LP :: VAR x :: COL :: l ->
        (* this caused a major pain... *)
        let ty1, l = tygr l in
        let e1, l = exp (Util.verify ARR (Util.verify RP l)) in
        (Lamty (x, ty1, e1), l)
    | LET :: VAR x :: EQ :: l ->
        let e1, l = exp l in
        let e2, l = exp (Util.verify IN l) in
        (Let (x, e1, e2), l)
    | LET :: REC :: VAR f :: VAR x :: EQ :: l ->
        let e1, l = exp l in
        let e2, l = exp (Util.verify IN l) in
        (Letrec (f, x, e1, e2), l)
    | LET :: REC :: VAR f :: LP :: VAR x :: COL :: l ->
        (* this caused a major pain... *)
        let ty1, l = tygr l in
        let ty2, l = tygr (Util.verify COL (Util.verify RP l)) in
        let e1, l = exp (Util.verify EQ l) in
        let e2, l = exp (Util.verify IN l) in
        (Letrecty (f, x, ty1, ty2, e1, e2), l)
    | l -> cexp l

  and cexp l =
    let e, l = sexp l in
    cexp' e l

  and cexp' e l =
    match l with
    | LEQ :: l ->
        let e', l = sexp l in
        (Oapp (Leq, e, e'), l)
    | EQ :: l ->
        let e', l = sexp l in
        (Oapp (Eq, e, e'), l)
        (* needed for = *)
    | _ -> (e, l)

  and sexp l =
    let e, l = mexp l in
    sexp' e l

  and sexp' e l =
    match l with
    | ADD :: l ->
        let e', l = mexp l in
        sexp' (Oapp (Add, e, e')) l
    | SUB :: l ->
        let e', l = mexp l in
        sexp' (Oapp (Sub, e, e')) l
    | _ -> (e, l)

  and mexp l =
    let e, l = aexp l in
    mexp' e l

  and mexp' e l =
    match l with
    | MUL :: l ->
        let e', l = mexp l in
        mexp' (Oapp (Mul, e, e')) l
    | DIV :: l ->
        let e', l = mexp l in
        mexp' (Oapp (Div, e, e')) l
    | _ -> (e, l)

  and aexp l =
    let e, l = pexp l in
    aexp' e l

  and aexp' e l =
    match l with
    | VAR x :: _ ->
        let e', l = pexp l in
        aexp' (Fapp (e, e')) l
    | CON c :: _ ->
        let e', l = pexp l in
        aexp' (Fapp (e, e')) l
    | LP :: _ ->
        let e', l = pexp l in
        aexp' (Fapp (e, e')) l
    | _ -> (e, l)

  and pexp l =
    match l with
    | CON (BCON c) :: l -> (Con (Bcon c), l)
    | CON (ICON c) :: l -> (Con (Icon c), l)
    | VAR x :: l -> (Var x, l)
    | LP :: l ->
        let e, l = exp l in
        (e, Util.verify RP l)
    | _ -> failwith "pexp"
end
