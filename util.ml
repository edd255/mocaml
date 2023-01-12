(* see section 6.2 *)
module Util = struct
  let verify c l =
    match l with
    | [] -> failwith "verify: no token"
    | c' :: l -> if c' = c then l else failwith "verify: wrong token"

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
end
