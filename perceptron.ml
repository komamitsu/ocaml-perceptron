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
