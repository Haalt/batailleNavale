open CPutil
open Types
open CPbattleship
open Str

(* 5px1, 5py1 *)
(* --ship_index-axis-pos_index value *)
(* "--5px2" *)
(* if string_match r "--5px2" 0 && not(string_match r "--5pz2" 0) then print_string("sah") *)

(* Str.split(Str.regexp("--"))(s);; *)

let get_ship_index(flag : string) : int =
  let index_list : string list = Str.split(Str.regexp("--"))(flag) in
  let index_string : string = fst(index_list) in
  let index_char : char = index_string.[0] in
  int_of_string(Printf.sprintf "%c" index_char)
;;

let get_pos_axis(flag : string) : string =
  let index_list : string list = Str.split(Str.regexp("--[0-5]p"))(flag) in
  let index_string : string = fst(index_list) in
  let index_char : char = index_string.[0] in
  Printf.sprintf "%c" index_char
;;

let get_pos_value(flag : string) : int =
  let index_list : string list = Str.split(Str.regexp("--[0-5]p."))(flag) in
  let index_string : string = fst(index_list) in
  let index_char : char = index_string.[0] in
  int_of_string(Printf.sprintf "%c" index_char)
;;

let place_in_tmp_pos(pos_mat, ship_index, axis, pos_index, pos_value : t_position array array * int * string * int * int) : unit = 
  if axis = "x" 
    then pos_mat.(ship_index).(pos_index) <- {cx = pos_value; cy = pos_mat.(ship_index).(pos_index).cy}
  else
    pos_mat.(ship_index).(pos_index) <- {cx = pos_mat.(ship_index).(pos_index).cx; cy = pos_value}
;;

let find_index(player_ships, ships_size : t_ship_remain_list * int) : int = 
  let index : int ref = ref 0 in
  for i = 0 to len(!player_ships) - 1
  do
    let current : t_ship_remain = nth(!player_ships, i) in
    if current.sh_type = ships_size
    then index := i
  done;
  !index
;;

let insert_ships(tmp_ships, player_ships : t_position array array * t_ship_remain_list) : t_ship_remain_list = 
  let ships : t_ship_remain_list = ref [] in
  for i = 0 to arr_len(tmp_ships) - 1
  do
    let tmp_list : t_remaining_pos = ref [] in
    for y = 0 to arr_len(tmp_ships.(i)) - 1
    do
      tmp_list := add_lst(!tmp_list, tmp_ships.(i).(y));
    done;
    let s : t_ship_remain = nth(!player_ships, i) in
    s.pos := !tmp_list;
    ships := add_lst(!ships, s);
  done;
  ships
;;

let update_matrix(mat, remain : t_matrix * t_ship_remain_list) : unit =
  for i = 0 to len(!remain) - 1
  do
    let s : t_ship_remain = nth(!remain, i) in
    for y = 0 to len(!(s.pos)) - 1
    do
      let c : t_position = nth(!(s.pos), y) in
      mat.(c.cx).(c.cy) <- SHIP
    done;
  done;
;;

let flatten_matrix(mat : t_position array array) : t_position array =
  let total_length : int ref = ref 0 in
  for i = 0 to arr_len(mat) - 1 do total_length := !total_length + arr_len(mat.(i)) done;
  let arr : t_position array = arr_make(!total_length, {cx = 0; cy = 0}) in
  let current_index : int ref = ref 0 in
  for i = 0 to arr_len(mat) - 1
  do
    for y = 0 to arr_len(mat.(i)) - 1
    do
      arr.(!current_index) <- mat.(i).(y);
      current_index := !current_index + 1;
    done;
  done;
  arr
;;

(* flemme de check les positions sont adjascentes *)
let validate_ships(tmp_pos, prm : t_position array array * t_param) : bool =
  let is_valid : bool ref = ref true in
  for i = 0 to arr_len(tmp_pos) - 1
  do
    for y = 0 to arr_len(tmp_pos.(i)) - 1
    do
      if tmp_pos.(i).(y).cx > matrix_dx(prm) || tmp_pos.(i).(y).cx < 0 ||tmp_pos.(i).(y).cy > matrix_dy(prm) || tmp_pos.(i).(y).cy < 0 
        then is_valid := false
    done;
  done;
  let flattened_pos : t_position array = flatten_matrix(tmp_pos) in
  for i = 0 to arr_len(flattened_pos) - 1
  do
    for y = 0 to arr_len(flattened_pos) - 1
    do
      if not(i = y) && flattened_pos.(i).cx = flattened_pos.(y).cx && flattened_pos.(i).cy = flattened_pos.(y).cy then is_valid := false;
    done;
  done;
  !is_valid
;;

let toggle_next_player(gameData : t_play) : t_play =
  if not(gameData.pl2.human) then (
    gameData.nextPlayer := PL1;
    gameData
  ) else gameData
;;
 
let main() = 
  let tmp_pos : t_position array array = mat_make(5, 5, {cx = 0; cy = 0}) in

  tmp_pos.(0) <- arr_make(5, {cx = 0; cy = 0});
  tmp_pos.(1) <- arr_make(4, {cx = 0; cy = 0});
  tmp_pos.(2) <- arr_make(3, {cx = 0; cy = 0});
  tmp_pos.(3) <- arr_make(3, {cx = 0; cy = 0});
  tmp_pos.(4) <- arr_make(2, {cx = 0; cy = 0});

  let r : regexp = regexp "--[0-5]p[(x,y)][0-5]" in
  let gameId : string ref = ref "" and player : string ref = ref "" in

  for i = 1 to Array.length Sys.argv - 1 
  do
    if Sys.argv.(i) = "--gameId" then gameId := Sys.argv.(i + 1)

    else if Sys.argv.(i) = "--player" then player := Sys.argv.(i + 1)

    else if string_match r Sys.argv.(i) 0 then (

      let ship_index : int = get_ship_index(Sys.argv.(i)) - 1 in
      let axis : string = get_pos_axis(Sys.argv.(i)) in
      let pos_index : int = get_pos_value(Sys.argv.(i)) - 1 in
      let pos_value : int = int_of_string(Sys.argv.(i + 1)) in

      place_in_tmp_pos(tmp_pos, ship_index, axis, pos_index, pos_value);
    )
  done;

  
  let game_data : t_play = get_game_info(!gameId) in

  if validate_ships(tmp_pos, game_data.prm)
  then (
    if game_data.pl1.name = !player then (

      game_data.pl1.remain := !(insert_ships(tmp_pos, game_data.pl1.remain));

      update_matrix(game_data.pl1.mymat, game_data.pl1.remain);

      

      save_game_info(!gameId, toggle_next_player(game_data));

      print_game_info(game_data.pl1, game_data.prm, !gameId);

    ) else if game_data.pl2.name = !player then (

      game_data.pl2.remain := !(insert_ships(tmp_pos, game_data.pl2.remain));

      update_matrix(game_data.pl2.mymat, game_data.pl2.remain);

      save_game_info(!gameId, game_data);
      
      print_game_info(game_data.pl2, game_data.prm, !gameId)
    )
  )
;;

let () = main()