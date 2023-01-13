(*
 * The project implements two procedures                                    
 *      checkStr: string -> type                                            
 *      evalStr: string -> value                                            
 * parsing strings as Mini-OCaml expressions and either type checking or    
 * evaluation the expressions. Programming languages are structured in four 
 * layers, according to the lecture notes:                                  
 *     Lexical Syntax: This layer concerns the translation of sequences of  
 *     characters into a sequences of words.                                
 *     Phrasal Syntax: This layer concerns the conditions syntax trees must 
 *     satisfy to be well-formed.                                           
 *     Static Semantics: This layer concerns the conditions syntax trees    
 *     must satisfy to be well-formed.                                      
 *     Dynamic Semantics: This layer concerns the evaluation of syntax      
 *     trees.                                                               
 *)

open Environment
open Evaluator
open Lexer
open Parser
open Types

(* checkStr: string -> type *)
module Main = struct
  let parse (s: string) = 
    let (e, l) = Parser.exp (Lexer.lex s) in
    if l = [] then e else failwith "parsing failed"

  let check (s : string) =
    let exps = parse s in
    TypeChecker.check empty exps

  let eval (s : string) =
    match check s with
      | _ -> Evaluator.eval [] (parse s)
end
