(* nml.ml *)

(* 
   usage: in top level directory

    #use "topfind";;
    #require "pcre";;

    #use "src/nml.ml";;
*)

open Core
open Printf

let stop_words filename =
    String.split ~on:',' (In_channel.read_all filename)

let z = List.map ~f:char_of_int (List.range (97) (97 + 26))

let words filename =
    let w = In_channel.read_all filename and
        r = Pcre.regexp "[\\W_]+" in
    String.lowercase (Pcre.replace ~rex:r ~templ:" " w) |> String.split ~on:' '

let _ =
    List.iter ~f:(printf "%s ") (stop_words "./stop_words.txt");
    print_endline "";
    List.iter ~f:(printf "%s ") (words "./pride-and-prejudice.txt");
    print_endline ""

    


(* end *)
