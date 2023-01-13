open Tokens

(* Constructor types, see section 7.1 *)
module Lexer = struct
  (* The basic design of the lexer (see section 6.6)
   *     1. Finish if the string exhausted.
   *     2. If first character is
   *        a. white space, skip and recurse.
   *        b. '+', '*', '(', or ')', add corresponding token
   *            and recurse.
   *        c. '<', verify next character is '=', add token
   *            Leq, and recurse.
   *        d. '-' and next character '>', add token Arr.
   *            Otherwise, add token sub. In both cases
   *            recurse.
   *        e. digit, read number, add number token, and
   *           recurse.
   *        f. lower-case letter, read identifier, and recurse
   *           after adding token as follows:
   *           i.   If identifier is "if", "then", "else",
   *                "fun", "let", "in", or "rec",
   *                 corresponding token.
   *           ii.  If identifier is "true" or "false", add
   *                corresponding constant token.
   *           iii. Otherwise add variable token. *)

  let whitespace (c : char) =
    match c with ' ' | '\n' | '\t' -> true | _ -> false

  let num (c : char) =
    match c with
    | '0' -> 0
    | '1' -> 1
    | '2' -> 2
    | '3' -> 3
    | '4' -> 4
    | '5' -> 5
    | '6' -> 6
    | '7' -> 7
    | '8' -> 8
    | '9' -> 9
    | _ -> failwith "num: no number"

  let digit (c : char) = 48 <= Char.code c && Char.code c <= 57
  let lowercase (c : char) = 97 <= Char.code c && Char.code c <= 122
  let underscore (c : char) = Char.code c = 95
  let prime (c : char) = Char.code c = 39
  
  (* lexer, see section 7.4 *)
  let lex (s : string) : token list =
    let get i = String.get s i in
    let getstr i n = String.sub s (i - n) n in
    let exhausted i = i >= String.length s in
    let verify i c = (not (exhausted i)) && get i = c in
    let rec lex i l =
      if exhausted i then List.rev l
      else
        match get i with
        | '(' -> lex (i + 1) (LP :: l)
        | ')' -> lex (i + 1) (RP :: l)
        | '=' -> lex (i + 1) (EQ :: l)
        | ':' -> lex (i + 1) (COL :: l)
        | '/' -> lex (i + 1) (DIV :: l)
        | '-' -> (
            match verify (i + 1) '>' with
            | true -> lex (i + 2) (ARR :: l)
            | false -> lex (i + 1) (SUB :: l))
        | '+' -> lex (i + 1) (ADD :: l)
        | '*' -> lex (i + 1) (MUL :: l)
        | '<' -> (
            match verify (i + 1) '=' with
            | true -> lex (i + 2) (LEQ :: l)
            | false -> failwith "lex: < not defined")
        | '_' -> lex (i + 1) (US :: l)
        | '\'' -> lex (i + 1) (QUOTE :: l)
        | '\"' -> lex (i + 1) (DBQUOTE :: l)
        | c when whitespace c -> lex (i + 1) l
        | c when digit c -> lex_num (i + 1) (num c) l
        | c when lowercase c -> lex_id (i + 1) 1 l
        | _ -> failwith "lex: illegal character"
    and lex_num i n l =
      if exhausted i then lex_num' i n l
      else
        let c = get i in
        if digit c then lex_num (i + 1) ((10 * n) + num c) l
        else if lowercase c then failwith "lex_num: expression not allowed"
        else lex_num' i n l
    and lex_num' i n l = lex i (CON (ICON n) :: l)
    and lex_id i n l =
      if exhausted i then lex_id' i n l
      else
        let c = get i in
        match c with
        | _ when underscore c -> lex_id (i + 1) (n + 1) l
        | _ when lowercase c || digit c || prime c -> lex_id (i + 1) (n + 1) l
        | _ -> lex_id' i n l
    and lex_id' i n l =
      match getstr i n with
      | "if" -> lex i (IF :: l)
      | "then" -> lex i (THEN :: l)
      | "else" -> lex i (ELSE :: l)
      | "fun" -> lex i (LAM :: l)
      | "let" -> lex i (LET :: l)
      | "in" -> lex i (IN :: l)
      | "rec" -> lex i (REC :: l)
      | "true" -> lex i (CON (BCON true) :: l)
      | "false" -> lex i (CON (BCON false) :: l)
      | "bool" -> lex i (BOOL :: l)
      | "int" -> lex i (INT :: l)
      | s -> lex i (VAR s :: l)
    in
    lex 0 []
end
