(**==========================================================================**)
(** The project implements two procedures                                    **)
(**      checkStr: string -> type                                            **)
(**      evalStr: string -> value                                            **)
(** parsing strings as Mini-OCaml expressions and either type checking or    **)
(** evaluation the expressions. Programming languages are structured in four **)
(** layers, according to the lecture notes:                                  **)
(**     Lexical Syntax: This layer concerns the translation of sequences of  **)
(**     characters into a sequences of words.                                **)
(**     Phrasal Syntax: This layer concerns the conditions syntax trees must **)
(**     satisfy to be well-formed.                                           **)
(**     Static Semantics: This layer concerns the conditions syntax trees    **)
(**     must satisfy to be well-formed.                                      **)
(**     Dynamic Semantics: This layer concerns the evaluation of syntax      **)
(**     trees.                                                               **)
(*============================================================================*)

(* checkStr: string -> type             *)
let checkStr (s: string) = 
    let exps = fst (exp (lex s)) in
    check empty exps ;; 

let evalStr (s: string) = 
    let exps = fst (exp (lex s)) 
    in eval empty exps

let evalStrChecked (s : string) = match checkStr s with
    | ty        -> evalStr s
    |         _ -> failwith "Type Error. Abort"
