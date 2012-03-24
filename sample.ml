module Percepton =
struct
  let ($) f g = f g

  let print x =
    Hashtbl.iter (fun k v -> Printf.printf "%s => %d    " k v) x;
  print_newline ()

  let inner_prod w x =
    Hashtbl.fold (fun wk wv a -> a + wv * (Hashtbl.find x wk)) w 0

  let classify w x =
    if inner_prod w x >= 0 then 1 else -1

  let learn w x y =
    if classify w x != y then
      Hashtbl.iter
        (fun xk xv ->
          let wv = Hashtbl.find w xk in
          Hashtbl.replace w xk $ wv + xv * y) x

  let learn_all n w set_of_x_y =
    for i = 1 to n do
      List.iter (fun (x, y) -> learn w x y) set_of_x_y 
    done;
    w
end

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
  let w = Percepton.learn_all 10 init train_src in
  Percepton.print w;
  let x = vec_of_assoc [("R", 200); ("G", 100); ("B", 100); ("bias", 1)] in
  print_result $ Percepton.classify w x;
  let x = vec_of_assoc [("R", 100); ("G", 200); ("B", 200); ("bias", 1)] in
  print_result $ Percepton.classify w x

