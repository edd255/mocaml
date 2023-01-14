open Mocaml

let _ = 
  Printf.printf "Mini-OCaml\n";
  while (true) do
    Printf.printf ">>> ";
    let _ = match read_line() with
      | line -> match Mocaml.eval line with
        | Bval b -> Printf.printf "%b\n" b
        | Ival i -> Printf.printf "%d\n" i
        | _      -> Printf.printf "\n"
    in ()
  done
