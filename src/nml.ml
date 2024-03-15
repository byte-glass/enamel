(* nml.ml *)

(* pride-and-prejudice.txt stop_words.txt *)

open Core
open Printf

let stop_words filename =
    String.split ~on:',' (In_channel.read_all filename)

let _ = 
    List.iter ~f:(printf "%s ") (stop_words "./stop_words.txt")
    


(* end *)
