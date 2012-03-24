(* 
 * ocamlc -c perceptron.ml && ocaml perceptron.cmo sample.ml
 *)
let ($) f g = f g

let vec_of_assoc assoc =
  List.fold_left (fun w (k, v) -> Hashtbl.replace w k v; w) (Hashtbl.create 4) assoc 

let train_src =
  List.rev $
    List.fold_left
      (fun train_src (assoc, y) ->
        let x = vec_of_assoc assoc in
        (x, y)::train_src) [] [
          ([("R", 255); ("G",   0); ("B",   0); ("bias", 1)], 1);
          ([("R",   0); ("G", 255); ("B", 255); ("bias", 1)], -1);
          ([("R",   0); ("G", 255); ("B",   0); ("bias", 1)], -1);
          ([("R", 255); ("G",   0); ("B", 255); ("bias", 1)], 1);
          ([("R",   0); ("G",   0); ("B", 255); ("bias", 1)], -1);
          ([("R", 255); ("G", 255); ("B",   0); ("bias", 1)], 1);
        ]

let print_result x =
  let s = if x = 1 then "warm" else "cool" in
  print_endline s

let _ =
  let init = vec_of_assoc [("R", 0); ("G", 0); ("B", 0); ("bias", 1)] in
  let w = Perceptron.learn_all 10 init train_src in
  Perceptron.print w;
  let x = vec_of_assoc [("R", 200); ("G", 100); ("B", 100); ("bias", 1)] in
  print_result $ Perceptron.classify w x;
  let x = vec_of_assoc [("R", 100); ("G", 200); ("B", 200); ("bias", 1)] in
  print_result $ Perceptron.classify w x

