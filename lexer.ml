(* Constructor types, see section 7.1 *)
type ty    = Bool | Int | Arrow of ty * ty
type con   = Bcon of bool | Icon of int
type op    = Add | Sub | Mul | Div | Leq | Eq
type var   = string 
type exp   = Var of var | Con of con
           | Oapp of op * exp * exp
           | Fapp of exp * exp
           | If of exp * exp * exp
           | Lam of var * exp
           | Lamty of var * ty * exp
           | Let of var * exp * exp
           | Letrec of var * var * exp * exp
           | Letrecty of var * var * ty * ty * exp * exp
type value = Bval of bool | Ival of int
           | Closure of var * exp * (var, value) env
           | Rclosure of var * var * exp * (var, value) env
           | Letrecty of var * var * ty * ty * exp * exp 
type const = BCON of bool | ICON of int
type token = LP | RP | EQ | COL | ARR | ADD | SUB | MUL | DIV | LEQ | IF | THEN
           | ELSE | LAM | LET | IN | REC | CON of const | VAR of string | BOOL
           | INT | US | QUOTE | DBQUOTE | TY of ty

(** The basic design of the lexer (see section 6.6)           **)
(**     1. Finish if the string exhausted.                    **)
(**     2. If first character is                              **)
(**        a. white space, skip and recurse.                  **)
(**        b. '+', '*', '(', or ')', add corresponding token  **)
(**            and recurse.                                   **)
(**        c. '<', verify next character is '=', add token    **)
(**            Leq, and recurse.                              **)
(**        d. '-' and next character '>', add token Arr.      **)
(**            Otherwise, add token sub. In both cases        **)
(**            recurse.                                       **)
(**        e. digit, read number, add number token, and       **)
(**           recurse.                                        **)
(**        f. lower-case letter, read identifier, and recurse **)
(**           after adding token as follows:                  **)
(**           i.   If identifier is "if", "then", "else",     **)
(**                "fun", "let", "in", or "rec",              **)
(**                 corresponding token.                      **)
(**           ii.  If identifier is "true" or "false", add    **)
(**                corresponding constant token.              **)
(**           iii. Otherwise add variable token.              **)

(* lexer, see section 7.4 *)
let lex (s : string) : token list =
    let get i = String.get s i in
    let getstr i n = String.sub s (i-n) n in
    let exhausted i = i >= String.length s in
    let verify i c = not (exhausted i) && get i = c in
    let rec lex i l = if exhausted i then List.rev l
    else match get i with
        | '(' -> lex (i+1) (LP::l) | ')' -> lex (i+1) (RP::l)
        | '=' -> lex (i+1) (EQ::l)
        | ':' -> lex (i+1) (COL::l)
        | '/' -> lex (i+1) (DIV::l)
        | '-' -> begin match verify (i+1) '>' with
            | true  -> lex (i+2) (ARR::l)
            | false -> lex (i+1) (SUB::l)
        end
        | '+' -> lex (i+1) (ADD::l)
        | '*' -> lex (i+1) (MUL::l)
        | '<' -> begin match verify (i+1) '=' with
            | true  -> lex (i+2) (LEQ::l)
            | false -> failwith "lex: < not defined"
        end
        | '_'  -> lex (i+1) (US::l)
        | '\'' -> lex (i+1) (QUOTE::l)
        | '\"' -> lex (i+1) (DBQUOTE::l) 
        | c when whitespace c -> lex (i+1) l
        | c when digit c      -> lex_num (i+1) (num c) l
        | c when lowercase c  -> lex_id (i+1) 1 l
        | c                   -> failwith "lex: illegal character"
    and lex_num i n l =
        if exhausted i
        then lex_num' i n l
        else let c = get i in
            if digit c
            then lex_num (i+1) (10*n+num c) l
            else
                if lowercase c
                then failwith "lex_num: expression not allowed"
                else lex_num' i n l
    and lex_num' i n l = lex i (CON (ICON n)::l)
    and lex_id i n l =
        if exhausted i
        then lex_id' i n l
        else let c = get i in
        match c with
            | c' when underscore c -> lex_id (i+1) (n+1) l 
            | c' when lowercase c || digit c || prime c -> lex_id (i+1) (n+1) l
            | _ -> lex_id' i n l
    and lex_id' i n l = match getstr i n with
        | "if"    -> lex i (IF::l)
        | "then"  -> lex i (THEN::l)
        | "else"  -> lex i (ELSE::l)
        | "fun"   -> lex i (LAM::l)
        | "let"   -> lex i (LET::l)
        | "in"    -> lex i (IN::l)
        | "rec"   -> lex i (REC::l)
        | "true"  -> lex i (CON (BCON true)::l)
        | "false" -> lex i (CON (BCON false)::l)
        | "bool"  -> lex i (BOOL::l)
        | "int"   -> lex i (INT::l)
        | s       -> lex i (VAR s::l)
    in lex 0 [] 
