(* val przelewanka : (int * int) array -> int *)

(*
let f arr =
  let rec helper a =
    a.(2) <- 5
  in helper arr
  zmienie na wyjscu *)

open Queue
open Printf

let drukuj tab =
  printf "[|";
  Array.iter (fun x -> printf "%d; " x) tab;
  printf "|]\n"


exception Not_finished
exception Found

(* maks / oczekiwana *)
let przelewanka (naczynia: (int * int) array) =

  let zrobione stan =
    try
      Array.iteri (fun i (_, oczekiwana) ->
        if stan.(i) <> oczekiwana then raise Not_finished) naczynia;
      true
    with Not_finished -> false
  in

  let n = Array.length naczynia in
  (* kolejka stanow do rozpatrzenia *)
  let q = create () in
  let out = ref 0 in
  let pom = ref 0 in
  let pom2 = ref 0 in
  let visited = Hashtbl.create (n * n * n) in

  let dodaj_stan stan =
    if not (Hashtbl.mem visited stan) then begin
      if zrobione stan then begin incr out; raise Found end else begin
        Hashtbl.add visited (Array.copy stan) (!out + 1);
        add (Array.copy stan) q end end
  in

  (* add (Array.make n 0) q; *)
  Hashtbl.add visited (Array.make n 0) 0;
  add (Array.make n 0) q;

  try
    if zrobione (top q) then raise Found;
    while not (is_empty q) do
      let akt = pop q in
      out := Hashtbl.find visited akt;
      (* drukuj akt;
      printf "ile elementow = %d\n" (length q);
      printf "wynik = %d\n\n" !out; *)
      for i = 0 to n - 1 do
        for j = 0 to n - 1 do
          (* mozliwe czynnosci kubka z samym soba *)
          if i = j then begin
            (* gdy jakis stan nie odwiedzony *)
            pom := akt.(i);
            (* wylej calosc *)
            akt.(i) <- 0;
            dodaj_stan akt;
            (* nalej do pelna *)
            akt.(i) <- fst naczynia.(i);
            dodaj_stan akt;

            (* wracam do stanu przed zmianami *)
            akt.(i) <- !pom end
          else
            (* przelewam ile moge z i do j *)
            pom := akt.(i);
            pom2 := akt.(j);
            (* kiedy sie przeleje && nie dolewac jak jest pelny *)
            if !pom2 <> fst naczynia.(j) && !pom <> 0 then begin
              if !pom + !pom2 > fst naczynia.(j) && !pom2 <> fst naczynia.(j) then begin
                akt.(i) <- akt.(i) - fst naczynia.(j) + akt.(j);
                akt.(j) <- fst naczynia.(j);
                dodaj_stan akt end
              else if !pom2 <> fst naczynia.(j) then begin
                akt.(j) <- akt.(j) + akt.(i);
                akt.(i) <- 0;
                dodaj_stan akt end
            end;

            akt.(i) <- !pom;
            akt.(j) <- !pom2;

        done
      done
    done; -1
  with Found -> !out
