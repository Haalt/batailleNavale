open CPutil
open Types

let init_t_proba_matrix(prm : t_param) : t_proba_matrix = 
  mat_make(matrix_dx(prm), matrix_dy(prm), 0)
;;

let find_hits(board, prm : t_matrix * t_param) : t_pos_list = 
  let res : t_pos_list ref = ref [] in
  for x = 0 to matrix_dx(prm) - 1
  do
    for y = 0 to matrix_dy(prm) - 1
    do
      if board.(x).(y) = DAMAGED
      then res := add_lst(!res, {cx = x; cy = y})
    done;
  done;
  !res
;;

let get_z(pos, is_vertical : t_position * bool) : int = 
  if is_vertical then pos.cy
  else pos.cx
;;

let get_position(board, is_vertical, x, y, i : t_matrix * bool * int * int * int) : t_value = 
  if is_vertical then board.(x).(i)
  else board.(i).(y)
;;

let can_ship_occupy_position(opp_board, pos, sh_size, is_vertical, prm : t_matrix * t_position * int * bool * t_param) : bool =
  let z : int = get_z(pos, is_vertical) in
  let stop : int = z + sh_size - 1 in

  if stop > matrix_dx(prm) - 1 then false
  else (
    let res : bool ref = ref true in
    for i = z to stop
    do 
      let p : t_value = get_position(opp_board, is_vertical, pos.cx, pos.cy, i) in
      if p = TRIED
      then res := false
    done;
    !res
  )
;;

let increase_probas(m, pos, sh_size, is_vertical, probas : t_matrix * t_position * int * bool * t_proba_matrix) : unit = 
  let z : int = get_z(pos, is_vertical) in
  let stop : int = z + sh_size - 1 in
  let multi : int ref = ref 1 in
  for i = z to stop
  do
    if (is_vertical && m.(pos.cx).(i) = DAMAGED) || m.(i).(pos.cy) = DAMAGED
      then multi := 50;
    (* else if m.(i).(pos.cy) = HIT
      then multi := 50; *)
  done;

  for i = z to stop
  do
    if is_vertical then probas.(pos.cx).(i) <- (probas.(pos.cx).(i) + 1) * !multi
    else probas.(i).(pos.cy) <- (probas.(i).(pos.cy) + 1) * !multi
  done;
;;

let get_probabilities(opp_board, opp_ships, prm : t_matrix * t_ship_remain_opp_list * t_param) : t_proba_matrix =
  let probas : t_proba_matrix = init_t_proba_matrix(prm) in
  for i = 0 to len(!opp_ships) - 1
  do
    let current_ship : t_ship_remain_opp = nth(!opp_ships, i) in
    for y = 0 to matrix_dy(prm) - 1
    do
      for x = 0 to matrix_dx(prm) - 1
        do
          let current_pos : t_position = {cx=x; cy=y} in
          if can_ship_occupy_position(opp_board, current_pos, current_ship.sh_type, false, prm)
            then (
              increase_probas(opp_board, current_pos, current_ship.sh_type, false, probas);
              if can_ship_occupy_position(opp_board, current_pos, current_ship.sh_type, true, prm)
                then increase_probas(opp_board, current_pos, current_ship.sh_type, true, probas)
            )
          else if can_ship_occupy_position(opp_board, current_pos, current_ship.sh_type, true, prm)
            then increase_probas(opp_board, current_pos, current_ship.sh_type, true, probas)
      done;
    done;
  done;
  probas
;;

let find_best_proba(probas, m, prm : t_proba_matrix * t_matrix * t_param) : t_position = 
  let best_pos : t_position ref = ref {cx = 0; cy = 0} and best_proba : int ref = ref 0 in
  for x = 0 to matrix_dx(prm) - 1
  do
    for y = 0 to matrix_dy(prm) - 1
    do
      if not(m.(x).(y) = DAMAGED) && !best_proba < probas.(x).(y)
      then (
        best_proba := probas.(x).(y);
        best_pos := {cx = x; cy = y};
      ) 
    done;
  done;
  !best_pos
;;

let get_adjacent_pos(pos : t_position) : t_pos_list =
  [
    {cx = pos.cx; cy = pos.cy};
    {cx = pos.cx - 1; cy = pos.cy};
    {cx = pos.cx + 1; cy = pos.cy};
    {cx = pos.cx; cy = pos.cy-1};
    {cx = pos.cx; cy = pos.cy+1}
  ]
;;

let increase_proba_around_hits(hits, probas : t_pos_list * t_proba_matrix) : t_proba_matrix = 
  let complete_pos : t_pos_list ref = ref [] in
  for i=0 to len(hits) - 1
  do
    let pos : t_position = nth(hits, i) in
    complete_pos := concat(!complete_pos, get_adjacent_pos(pos));
  done;

  for i = 0 to len(!complete_pos) - 1
  do
    let pos : t_position = nth(!complete_pos, i) in
    let (is_present, index) : bool * int = is_in_list(!complete_pos, pos, i + 1) in
    if not(is_present)
    then probas.(pos.cx).(pos.cy) <- probas.(pos.cx).(pos.cy) * 50
  done;
  probas
;;




let get_ai_shoot(m, opp_ships, prm : t_matrix * t_ship_remain_opp_list * t_param) : t_position =
  (* let hits : t_pos_list = find_hits(m, prm) in *)
  let probas : t_proba_matrix = get_probabilities(m, opp_ships, prm) in
  (* increase_proba_around_hits(hits, probas);
  find_best_proba(increase_proba_around_hits(hits, probas), m, prm) *)
  find_best_proba(probas, m, prm)
  (* best_pos *)
;;