(* nml.ml *)

(* 
   usage: in top level directory

    #use "topfind";;
    #require "pcre";;

    #use "src/nml.ml";;
*)

open Core

let stop_words filename =
    let s = String.split ~on:',' (In_channel.read_all filename) and
        a_to_z = List.map ~f:(fun n -> String.make 1 (char_of_int n)) (List.range (97) (97 + 26)) in
    Set.of_list (module String) (a_to_z @ s)

let words filename =
    let w = In_channel.read_all filename and
        r = Pcre.regexp "[\\W_]+" in
    String.lowercase (Pcre.replace ~rex:r ~templ:" " w) |> String.split ~on:' '

let bump t k =
    let find_or_0 t k = 
        match Hashtbl.find t k with
    | Some v -> v
    | None -> 0 in
    Hashtbl.set t ~key:k ~data:((find_or_0 t k) + 1); t

let counts w s =
    let c = List.fold w ~init:(Hashtbl.create (module String)) ~f:(fun t w -> if Set.mem s w then t else bump t w) in
    Hashtbl.fold c ~init:[] ~f:(fun ~key ~data acc -> (key, data) :: acc)

let sort s = List.sort s ~compare:(fun (_, a) (_, b) -> Int.compare b a)

let _ =
    let s = sort (counts (words "./pride-and-prejudice.txt") (stop_words "./stop_words.txt")) in
    List.iter (List.take s 25) ~f:(fun (k, v) -> print_endline (k ^ " - " ^ string_of_int(v)))

(* end *)
