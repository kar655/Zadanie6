open Queue
open Printf

let drukuj tab =
  printf "[|";
  Array.iter (fun x -> printf "%d; " x) tab;
  printf "|]\n"


exception Not_finished
exception Found

(* val przelewanka : (int * int) array -> int *)
(* maks / oczekiwana *)
let przelewanka (naczynia: (int * int) array) =

  (* sprawdza czy oczekiwany stan zostal osiagniety *)
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
  (* stany odwidzone *)
  let visited = Hashtbl.create n in

  (* dodaje stan do odwiedzonych sprawdzajac czy jest dobry *)
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
    if zrobione (top q) then raise Found; (* czy same 0 sa ok *)
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
            (* kiedy sie przeleje *)
            if !pom2 <> fst naczynia.(j) && !pom <> 0 then begin
              if !pom + !pom2 > fst naczynia.(j) then begin
                akt.(i) <- akt.(i) - fst naczynia.(j) + akt.(j);
                akt.(j) <- fst naczynia.(j);
                dodaj_stan akt end
              else begin
                akt.(j) <- akt.(j) + akt.(i);
                akt.(i) <- 0;
                dodaj_stan akt end
            end;

            akt.(i) <- !pom;
            akt.(j) <- !pom2;

        done
      done
    done; -1 (* gdy nie ma juz mozliwych stanow do odwiedzenia *)
  with Found -> !out



(*
let c = [|(10,2);(20,20);(10,0);(1000,1000)|];;
assert ( przelewanka c = -1 );;
let c = [|(3,2);(5,4);(5,2);(6,1)|];;
assert (przelewanka c = -1);;
let c = [|(40,1);(10,4);(23,2);(40,1)|];;
assert (przelewanka c = -1);;
let c = [|(12,2);(6,3);(4,4);(10,2)|];;
assert (przelewanka c = -1);;
let c = [|(14,3);(3,1)|];;
assert (przelewanka c = -1);;

(*Testy różne*)
let c = [|(3,2);(3,3);(1,0);(12,1)|];;
assert ( przelewanka c = 4 );;
let c = [|(1,1);(100,99)|];;
assert ( przelewanka c = 2 );;
let c = [|(3,3);(5,4);(5,2);(6,1)|];;
assert (przelewanka c = 6);;
let c = [|(100,3);(2,1);(1,0);(6,1)|];;
assert (przelewanka c = 7);;
let c = [|(3,3);(5,5);(5,5);(6,6)|];;
assert (przelewanka c = 4);;
let c = [|(40,20);(20,10);(10,5);(5,0)|];;
przelewanka c ;;
let c = [|(19,3);(1,1);(2,2)|];;
assert (przelewanka c = 6);;
let c = [|(14,3);(3,1);(3,0)|];;
assert (przelewanka c = 13);;
let c = [|(3,3);(4,0);(1,1);(6,6)|];;
assert (przelewanka c = 3);;
let c = [|(46,20);(23,10);(13,5);(5,0)|];;
assert (przelewanka c = 10);;
let c = [|(18,3);(3,1);(2,2)|];;
assert (przelewanka c = 4);;
let c = [|(14,3);(5,1)|];;
assert (przelewanka c = -1);;
let c = [|(14,3);(5,1);(5,0)|];;
assert (przelewanka c = 16);;

(* Przelewanie ciągle z jednego do drugiego*)
let c = [|(10000,5000);(1,0)|];;
assert (przelewanka c = 10000);;
let c = [|(50000,450);(3,1);(3,0)|];;
assert (przelewanka c = 33635);;
let c = [|(100000,25252);(2,2)|];;
assert (przelewanka c = 25253);;
*)
