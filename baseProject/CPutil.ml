
(* ------------------------ *)
(*        listes            *)
(* ------------------------ *)



let len(l : 'a list) : int = List.length l ;;

let fst(l : 'a list) : 'a =
  match l with
    [] -> failwith "error fst : list is empty"
  | hd::_ -> hd
;;

let rec lst(l : 'a list) : 'a =
  match l with
    [] -> failwith "error lst : list is empty"
    | hd::[] -> hd
    | _::tail -> lst(tail)
;;

let nth(l, k : 'a list * int) : 'a = 
  let rec nth1(l, k) =
    match l with
      []->  failwith "error nth : index out of bounds"
    | hd::tail -> if k = 0 then hd else nth1(tail,k-1)
  in
    if k < 0
    then failwith "error  nth : index must be positive"
    else nth1(l,k)
;;

let add_fst(l, e : 'a list * 'a) : 'a list = e::l ;;

let rec add_lst(l, e : 'a list * 'a) : 'a list =
  match l with
    [] -> [e]
  | hd::tail -> hd::add_lst(tail,e)
;;

let add_nth(l, e, k  : 'a list * 'a * int) : 'a list =
  let rec add_nth1(l, e, k) =
    match l with
      [] -> [e]
    | hd ::tail -> if k = 0 then e::l else hd::add_nth1(tail, e, k-1)
  in 
    if k < 0
    then failwith "error add_nth : index must be positive"
    else
      if k > len(l)
      then failwith "error add_nth : index out of bounds"
      else add_nth1(l,e,k)
;;

let rem_fst(l : 'a list) : 'a list = 
  match l with
    [] -> failwith "error rem_fst : list is empty"
    | _::tail -> tail
;;

let rec rem_lst(l : 'a list) : 'a list =
  match l with
    [] -> failwith "error rem_lst : list is empty"
    | [x] -> []
    | x::tail -> x::rem_lst(tail)
 ;;

let rem_nth(l, k : 'a list * int) : 'a list =
  let rec rem_nth1(l, k) =
    match l with
    | [] -> failwith "error rem_nth : index out of bounds"
    | hd:: tail -> if k = 0 then tail else hd::rem_nth1(tail, k-1)
  in
    if k < 0 
    then failwith "error rem_nth : index must be positive"
    else rem_nth1(l,k)
;;

let concat(l1, l2 : 'a list * 'a list) = l1 @ l2 ;;


(* ------------------------ *)
(*        tableaux          *)
(* ------------------------ *)

let arr_len(t : 'a array) : int = Array.length t ;;

let arr_make(n, v : int * 'a) : 'a array = 
  if n < 0
  then failwith("erreur arr_make ; parametre invalide")
  else Array.make n v 
;;

type 'a matrix = 'a array array ;;

let mat_make(n, m, v : int * int * 'a) : 'a matrix = 
  if n < 0 || m < 0
  then failwith("erreur mat_make ; parametre invalide")
  else Array.make_matrix n m v 
;;


(* ------------------- *)
(*      aleatoire      *)
(* ------------------- *)

let rand_init() : unit = Random.self_init() ;;

let rand_init_expl(n : int) : unit = Random.init(n) ;;

let rand_int_0(n : int) : int = Random.int(n+1) ;;

let rand_int(n, p : int * int) : int = Random.int(p-n + 1) + n ;;


(* ------------------------ *)
(*    lecture caractere     *)
(* ------------------------ *)

let read_char() : char =
  let s : string ref = ref "" and the_end : bool ref = ref false in
    (
    while not(!the_end) 
    do
      s:= read_line() ; 
      if String.length(!s) = 0
      then 
        (
        print_string("erreur read_char : aucun caractere saisi") ;
        print_newline() ;
        )
      else the_end := true;
    done ;
    (!s).[0] ;
    )
;;

(* ------------------------- *)
(* conversion char -> string *)
(* ------------------------- *)

let string_of_char(c : char) : string = Char.escaped c ;;


(* ------------------------ *)
(*    longueur string       *)
(* ------------------------ *)

let string_length(s : string) : int = String.length s ;;


(* ------------------------ *)
(* ------------------------ *)
(* ------------------------ *)
(*  variante avec graphics  *)
(* ------------------------ *)
(* ------------------------ *)
(* ------------------------ *)

(* ------------------------ *)
(*  pause durant execution  *)
(* ------------------------ *)
(* ------------------------ *)
let wait(n : int) : unit =
 Unix.sleep(n)
;;



(* ------------------------ *)
(*        graphique         *)
(* ------------------------ *)

let open_graph(dx, dy : int * int) : unit = 
  if Sys.os_type = "Unix" then  
    let s = ":0 "^string_of_int(dx)^"x"^string_of_int(dy) in
      Graphics.open_graph s
  else
    let s = string_of_int(dx)^"x"^string_of_int(dy) in
      Graphics.open_graph s
;;

let close_graph() : unit = Graphics.close_graph() ;;

let clear_graph() : unit = Graphics.clear_graph() ;;

let resize_window(x, y : int * int) : unit = Graphics.resize_window x y ;;


let moveto(x, y : int * int) : unit = Graphics.moveto x y ;;

let lineto(x, y : int * int) : unit = Graphics.lineto x y ;;

let plot(x, y : int * int) : unit = Graphics.plot x y ;;

let current_point() : int * int = Graphics.current_point() ;;

let draw_poly_line(t : (int * int) array) : unit = Graphics.draw_poly_line t ;;

let draw_circle(x, y, r : int * int * int) : unit = Graphics.draw_circle x y r ;;

let draw_ellipse(x, y, dx, dy : int * int * int * int) : unit = 
          Graphics.draw_ellipse x y dx dy 
;;

let draw_rect(x, y, dx, dy : int * int * int * int) : unit = 
  if Sys.os_type = "Unix" then  
    Graphics.draw_rect x y (dx- 1) (dy - 1)
  else
    Graphics.draw_rect x (y+1) (dx-1) (dy-1)
;;

let fill_rect(x, y, dx, dy : int * int * int * int) : unit = 
  if Sys.os_type = "Unix" then  
    Graphics.fill_rect x y (dx- 1) (dy - 1)
  else
    Graphics.fill_rect x y dx dy
;;

let fill_poly(t : (int * int) array) : unit = Graphics.fill_poly t ;;

let fill_circle(x, y, r : int * int * int) : unit = Graphics.fill_circle x y r ;;


let fill_ellipse(x, y, dx, dy : int * int * int * int) : unit = 
          Graphics.fill_ellipse x y dx dy 
;;

let set_line_width(e : int) : unit = Graphics.set_line_width e ;;

let draw_string(s : string) : unit = Graphics.draw_string s ;;

let set_text_size(n : int) : unit = 
  let s = "-*-courier-medium-r-*-*-"^string_of_int(n)^"-*"
  in Graphics.set_font s ;;



type t_color = Graphics.color ;;

let black : t_color = Graphics.black ;;
let blue : t_color = Graphics.blue ;;
let red : t_color = Graphics.red ;;
let green : t_color = Graphics.green ;;
let white : t_color = Graphics.white ;;
let yellow : t_color = Graphics.yellow ;;
let cyan : t_color = Graphics.cyan ;;
let magenta : t_color = Graphics.magenta ;;
let grey : t_color = 128 * 256 * 256 + 128 * 256 + 128 ;;

let color_of_rgb(r, g, b : int * int * int) : t_color =
  let valid(x : int) : bool = ((0 <= x) && x <= 255) in
    if not(valid(r)) ||  not(valid(g)) || not(valid(b))
    then failwith("erreur color_of_rgb : valeurs invalides")
    else Graphics.rgb r g b
;;



let set_color(color : t_color) : unit = Graphics.set_color color ;;

(* ------------------------ *)
(*   controle evenements    *)
(* ------------------------ *)


let key_pressed() : bool =
  Graphics.key_pressed()
;;

let read_key() : char =
  Graphics.read_key()
;;

let mouse_pos() : int * int =
  Graphics.mouse_pos()
;;

let button_down() : bool = 
  Graphics.button_down()
;;



(* ------------------------ *)
(* ------------------------ *)
(* ------------------------ *)
(*    utilitaires de tests  *)
(* ------------------------ *)
(* ------------------------ *)
(* ------------------------ *)


(* --------------------------------------------------- *)
(* --------------------------------------------------- *)
(* types et fonctions de manipulation de tests         *)
(* --------------------------------------------------- *)
(* --------------------------------------------------- *)


(* ------------- *)
(* types         *)
(* ------------- *)

type t_test_callresult = 
    Test_exec_success
  | Test_exec_failure of string
  | Test_exec_error of exn
  | Test_fail_success of string
  | Test_fail_failure
  | Test_fail_error of exn
  | Test_assert_violation of string
;;

type 'a t_test_result = 'a option * t_test_callresult ;;

type t_test_step = 
  {
  fname : string;
  cassert : int ref;
  cassert_ignored : int ref;
  error : t_test_callresult option ref
  }
;;

type t_test_status =
  {
  seq : t_test_step list ref;
  }
;;


(* --------------------------------------------------- *)
(* fonctions utilitaires pour le rendu des resultats   *)
(* --------------------------------------------------- *)

let count_all_tests(status : t_test_status) : int =
  len(!(status.seq))
;;

let count_all_ok_tests(status : t_test_status) : int =
  let rec aux(steps : t_test_step list) : int =
    match steps with
      [] -> 0
      | { fname = _; cassert = _; cassert_ignored = _; error = {contents = None} }::sl -> 1 + aux(sl)
      | _::sl -> aux(sl)
  in
    aux(!(status.seq))
;;

let count_all_ko_tests(status : t_test_status) : int =
  let rec aux(steps : t_test_step list) : int =
    match steps with
      [] -> 0
      | { fname = _; cassert = _; cassert_ignored = _; error = {contents = Some(_)} }::sl -> 1 + aux(sl)
      | _::sl -> aux(sl)
  in
    aux(!(status.seq))
;;

let print_test_report_step(step : t_test_step) : unit =
  let err : t_test_callresult option = !(step.error) in
  let str : string = 
    (
    match err with
      None -> "OK"
      | Some(e) -> 
        (
        match e with
          Test_exec_success -> "OK"
          | Test_exec_failure(serr) -> Printf.sprintf "KO failwith = '%s'" serr
          | Test_exec_error(ex) -> Printf.sprintf "KO error = '%s'" (Printexc.to_string ex)
          | Test_fail_success(serr) -> "OK"
          | Test_fail_failure -> "KO no failwith detected !"
          | Test_fail_error(ex) -> Printf.sprintf "KO error = '%s'" (Printexc.to_string ex)
          | Test_assert_violation(serr) -> Printf.sprintf "KO assert violation on '%s'" serr
        )
    ) 
  in
  Printf.printf "\tTest of %s: status = %s; \t(assertion stats: executed = %3d; ignored = %3d)\n" 
               step.fname str !(step.cassert) !(step.cassert_ignored)
;;


(* ---------------------------------------- *)
(* ---------------------------------------- *)
(* fonctions pour la definition de tests    *)
(* ---------------------------------------- *)
(* ---------------------------------------- *)

let print_test_report(status : t_test_status) : unit =
  let nbok : int = count_all_ok_tests(status) in
  let nbko : int = count_all_ko_tests(status) in
  let total : int = count_all_tests(status) in
    (
    Printf.printf "Test report:\n   - OK: %3d/%3d\n   - KO: %3d/%3d\n" nbok total nbko total ;
    List.iter print_test_report_step !(status.seq)
    )
;;

let create_test_status() : t_test_status =
  {
  seq = ref []
  }
;;

let test_start(status, name : t_test_status * string) : t_test_step =
  let step = { fname = name; cassert = ref 0; 
              cassert_ignored = ref 0; error = ref None } 
  in
    (
    status.seq := add_lst(!(status.seq), step) ;
    step
    )
;;

let test_end(step : t_test_step) : unit =
  ()
;;

let test_error(step : t_test_step) : unit =
  ()
;;

let test_exec(step, fct, arg : t_test_step * ('a -> 'b) * 'a) : 'b t_test_result =
  try 
    (
    let res = (fct arg) in
    (Some(res), Test_exec_success)
    )
  with
    Failure(msg) -> (let r = Test_exec_failure(msg) in (step.error := Some(r); (None, r)))
    | e -> (let r = Test_exec_error(e) in (step.error := Some(r); (None, r))) 
;;

let test_fail_exec(step, fct, arg : t_test_step * ('a -> 'b) * 'a) : 'b t_test_result =
  try (
    let res = (fct arg) in
    (Some(res), Test_fail_failure)
  )
  with
    Failure(msg) -> (let r = Test_fail_success(msg) in  (None, r)) 
  | e -> (let r = Test_fail_error(e) in (step.error := Some(r);  (None, r)))
;;

let test_is_success(tres : 'a t_test_result) : bool =
  let (_, t) : 'a t_test_result = tres in
    match t with
      Test_exec_success -> true
      | Test_fail_success(_) -> true
      | _ -> false
;;

let test_get(t : 'a t_test_result) : 'a =
  let (res, _) : 'a t_test_result = t in
    match res with
      Some(e) -> e
      | None -> failwith("Unexpected error, does your call produce a value?")
;;

let test_fail_get(res : 'a t_test_result) : string =
  let (_, t) : 'a t_test_result = res in
    match t with
      | Test_fail_success(a) -> a
      | _ -> failwith("Unexpected error, do you test success of your test?")
;;

let assert_true(step, msg, b : t_test_step * string * bool) : unit =
  if !(step.error) = None 
  then 
    if b
    then step.cassert := !(step.cassert) + 1
    else step.error := Some(Test_assert_violation(msg))
  else step.cassert_ignored := !(step.cassert_ignored) + 1
;;

let assert_false(step, msg, b : t_test_step * string * bool) : unit =
  if !(step.error) = None 
  then 
    if (not b) 
    then step.cassert := !(step.cassert) + 1
    else step.error := Some(Test_assert_violation(msg))
  else step.cassert_ignored := !(step.cassert_ignored) + 1
;;

let assert_equals(step, msg, a, b : t_test_step * string * 'a * 'a) : unit =
  if !(step.error) = None 
  then 
    if a = b 
    then step.cassert := !(step.cassert) + 1
    else step.error := Some(Test_assert_violation(msg))
  else step.cassert_ignored := !(step.cassert_ignored) + 1
;;

let assert_notequals(step, msg, a, b : t_test_step * string * 'a * 'a) : unit =
  if !(step.error) = None 
  then 
    if a <> b 
    then step.cassert := !(step.cassert) + 1
    else step.error := Some(Test_assert_violation(msg))
  else step.cassert_ignored := !(step.cassert_ignored) + 1
;;

(* ---------------------------------------------- *)
(* fonctions de manipulation de listes / tableaux *)
(* pouvant servir pour les predicats              *)
(* ---------------------------------------------- *)

let rec list_contains_value_aux(l, e : 'a list * 'a) : bool * 'a list =
  if l = []
  then (false, l)
  else
    let (x, r) : 'a * 'a list = (fst(l), rem_fst(l)) in
      if x = e
      then (true, r)
      else
        let (b, newr) : bool * 'a list = list_contains_value_aux(r, e) in
          (b, add_fst(newr, x))
;;

let rec list_contains_value(l, e : 'a list * 'a) : bool =
  let (b, r) : bool * 'a list = list_contains_value_aux(l, e) in
    b
;; 

let rec list_included_in_list(l1,l2 : 'a list * 'a list) : bool =
  if l1 = []
  then true
  else
    let (x, r1) : 'a * 'a list = (fst(l1), rem_fst(l1)) in
    let (b, r2) : bool * 'a list = list_contains_value_aux(l2, x) in
      if b
      then list_included_in_list(r1,r2)
      else false
;;

let list_similar_to_list(l1,l2 : 'a list * 'a list) : bool =
  len(l1) = len(l2) && (list_included_in_list(l1,l2)) && (list_included_in_list(l2,l1))
;;

(* fonction de conversion de tableau en liste *)

let list_of_array(t : 'a array) : 'a list =
  let res : ('a list) ref = ref [] and indmax : int = arr_len(t) - 1 in
    (
    for i = 0 to indmax
    do res := add_lst(!res, t.(i))
    done ;
    !res ;
    )
;;