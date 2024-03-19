(* pipeline.ml *)

(* 
   usage: in the top level directory

    #use "topfind";;
    #require "pcre";;

    #use "src/pipeline.ml";;

*)

open Core

let read_file path =
    In_channel.read_all path

let filter_chars_and_normalize str_data =
    let rex = Pcre.regexp "[\\W_]+" in
        String.lowercase (Pcre.replace ~rex ~templ:" " str_data)

let scan str_data =
    String.split ~on:' ' str_data

(*
let s = read_file "./pride-and-prejudice.txt"
let s = filter_chars_and_normalize (read_file "./pride-and-prejudice.txt")
let words = scan (filter_chars_and_normalize (read_file "./pride-and-prejudice.txt"))
*)

let remove_stop_words words =
    let stop_words = String.split ~on:',' (read_file "./stop_words.txt") and
        a_to_z = List.map ~f:(fun n -> String.make 1 (char_of_int n)) (List.range (97) (97 + 26)) in
    let stop = Set.of_list (module String) (a_to_z @ stop_words) in
    List.filter words ~f:(fun w -> not (Set.mem stop w))

(*
let stop_words = String.split ~on:',' (read_file "./stop_words.txt")
let a_to_z = List.map ~f:(fun n -> String.make 1 (char_of_int n)) (List.range 97 (97 + 26))
let stop = Set.of_list (module String) (a_to_z @ stop_words) 
let w = List.filter words ~f:(fun w -> not (Set.mem stop w))

let w = remove_stop_words (scan (filter_chars_and_normalize (read_file "./pride-and-prejudice.txt")))
*)


let bump t k =
    let find_or_0 t k = 
        match Hashtbl.find t k with
    | Some v -> v
    | None -> 0 in
    Hashtbl.set t ~key:k ~data:((find_or_0 t k) + 1)

let frequencies words =
    let t = Hashtbl.create (module String) in
    List.iter words ~f:(fun w -> bump t w); t 


let sort f = 
    let s = Hashtbl.fold f ~init:[] ~f:(fun ~key ~data acc -> (key, data) :: acc) in
    List.sort s ~compare:(fun (_, a) (_, b) -> Int.compare b a)

let _ = 
    let s = sort (frequencies (remove_stop_words (scan (filter_chars_and_normalize (read_file "./pride-and-prejudice.txt"))))) in
    List.iter (List.take s 25) ~f:(fun (k, v) -> print_endline (k ^ " - " ^ string_of_int(v)))

    (*
let w = remove_stop_words (scan (filter_chars_and_normalize (read_file "./pride-and-prejudice.txt")))
let f = frequencies w

let s = Hashtbl.fold f ~init:[] ~f:(fun ~key ~data acc -> (key, data) :: acc)
let t = List.sort s ~compare:(fun (_, a) (_, b) -> Int.compare b a)


let s = sort (frequencies (remove_stop_words (scan (filter_chars_and_normalize (read_file "./pride-and-prejudice.txt")))))


let _ =
    print_endline (filter_chars_and_normalize (read_file "../pride-and-prejudice.txt"))

*)



(* end *)
