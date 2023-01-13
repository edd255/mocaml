open Mocaml

let _ = 
  match read_line() with
    | line -> Mocaml.eval line
