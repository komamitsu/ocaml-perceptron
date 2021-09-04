(*
init:
  w={R:0, G:0, B:0}

----
x={R:0, G:255, B:255}, y=-1

check:
  w={R:0, G:0, B:0},
  x={R:0, G:255, B:255},
  y=-1
    R: w.R=0, x.R=0, w*x=0
    G: w.G=0, x.G=255, w*x=0
    B: w.B=0, x.B=255, w*x=0
  result => 1 (1 != 1: need to update)

update:
  w={R:0, G:0, B:0},
  x={R:0, G:255, B:255},
  y=-1
    R: w.R=0, x.R=0, w+(x*y)=0
      w={R:0, G:0, B:0}
    G: w.G=0, x.G=255, w+(x*y)=-255
      w={R:0, G:-255, B:0}
    B: w.B=0, x.B=255, w+(x*y)=-255
      w={R:0, G:-255, B:-255}

----
x={R:255, G:0, B:255}, y=1

check:
  w={R:0, G:-255, B:-255},
  x={R:255, G:0, B:255},
  y=1
    R: w.R=0, x.R=255, w*x=0
    G: w.G=-255, x.G=0, w*x=0
    B: w.B=-255, x.B=255, w*x=-65025
  result => -1 (-1 != 1: need to update)

update:
  w={R:0, G:-255, B:-255},
  x={R:255, G:0, B:255},
  y=1
    R: w.R=0, x.R=255, w+(x*y)=255
      w={R:255, G:-255, B:-255}
    G: w.G=-255, x.G=0, w+(x*y)=-255
      w={R:255, G:-255, B:-255}
    B: w.B=-255, x.B=255, w+(x*y)=0
      w={R:255, G:-255, B:0}
*)
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

