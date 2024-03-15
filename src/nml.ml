(* nml.ml *)

(* pride-and-prejudice.txt stop_words.txt *)

open Core

let () = 
    In_channel.read_all "./stop_words.txt" |> print_endline
    


(* end *)
