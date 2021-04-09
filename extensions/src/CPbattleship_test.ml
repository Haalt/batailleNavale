open CPutil
open Types
open CPbattleship

let test_valid_empty_position_a(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "valid_empty_position") in
  (* est ce qu'il y a des choses à préparer pour appeler la fonctio nsous test? *)
  let param : t_param = init_param() in
  let mat : t_matrix = [| [| EMPTY ; EMPTY|] ;[| SHIP; SHIP|] |] in
  let x : int = 20 in
  let y : int = 20 in
  
  let test_result : bool t_test_result = test_exec(test_step, valid_empty_position, (x,y,mat,param)) in
  if test_is_success(test_result)
  then
    assert_false(test_step, "case 20x20 faux", test_get(test_result))
  else
    test_error(test_step);
  test_end(test_step)
;;
  
let test_valid_empty_position_b(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "valid_empty_position") in
  (* est ce qu'il y a des choses à préparer pour appeler la fonctio nsous test? *)
  let param : t_param = init_param() in
  let mat : t_matrix = [| [| EMPTY ; EMPTY|] ;[| SHIP; SHIP|] |] in
  let x : int = 0 in
  let y : int = 0 in
  
  let test_result : bool t_test_result = test_exec(test_step, valid_empty_position, (x,y,mat,param)) in
  if test_is_success(test_result)
  then
    assert_true(test_step, "case 0x0", test_get(test_result))
  else
    test_error(test_step)
  ;
  test_end(test_step)
;;
  
  
let test_valid_empty_position_c(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "valid_empty_position") in
  (* est ce qu'il y a des choses à préparer pour appeler la fonctio nsous test? *)
  let param : t_param = init_param() in
  let mat : t_matrix = [| [| EMPTY ; EMPTY|] ;[| SHIP; SHIP|] |] in
  let x : int = 1 in
  let y : int = 0 in
  
  let test_result : bool t_test_result = test_exec(test_step, valid_empty_position, (x,y,mat,param)) in
  if test_is_success(test_result)
  then
    assert_false(test_step, "case 1x0", test_get(test_result))
  else
    test_error(test_step)
  ;
  test_end(test_step)
;;

let test_is_empty_neighbour_a(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "is_empty_neighbour") in
  let param : t_param = init_param() in
  let mat : t_matrix = [| [| EMPTY ; EMPTY; EMPTY|] ;[| EMPTY ; EMPTY; EMPTY|]; [| EMPTY ; EMPTY; EMPTY|] |] in
  let x : int = 1 in
  let y : int = 1 in

  let test_result : bool t_test_result = test_exec(test_step, is_empty_neighbour, (x, y, mat, param)) in
  if test_is_success(test_result)
  then 
    assert_true(test_step, "case center all empty", test_get(test_result))
  else 
    test_error(test_step);
  test_end(test_step);
;;
  
let test_is_empty_neighbour_b(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "is_empty_neighbour") in
  let param : t_param = init_param() in
  let mat : t_matrix = [| [| EMPTY ; EMPTY; EMPTY|] ;[| EMPTY ; EMPTY; EMPTY|]; [| EMPTY ; EMPTY; EMPTY|] |] in
  let x : int = 12 in
  let y : int = 1 in

  let test_result : bool t_test_result = test_exec(test_step, is_empty_neighbour, (x, y, mat, param)) in
  if test_is_success(test_result)
  then 
    assert_false(test_step, "case outside of board all empty", test_get(test_result))
  else 
    test_error(test_step);
  test_end(test_step);
;;

let test_is_empty_neighbour_c(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "is_empty_neighbour") in
  let param : t_param = init_param() in
  let mat : t_matrix = [| [| EMPTY ; SHIP; EMPTY|] ;[| EMPTY ; EMPTY; EMPTY|]; [| EMPTY ; EMPTY; EMPTY|] |] in
  let x : int = 1 in
  let y : int = 1 in

  let test_result : bool t_test_result = test_exec(test_step, is_empty_neighbour, (x, y, mat, param)) in
  if test_is_success(test_result)
  then 
    assert_false(test_step, "case center of board one ship", test_get(test_result))
  else 
    test_error(test_step);
  test_end(test_step);
;;

let test_insert_ship_matrix_a(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "insert_ship_matrix") in
  let m : t_matrix = [| [| EMPTY ; EMPTY; EMPTY|] ;[| EMPTY ; EMPTY; EMPTY|]; [| EMPTY ; EMPTY; EMPTY|] |] in
  let l : t_pos_list = [{cx = 0; cy = 0}; {cx = 1; cy = 1}; {cx = 2; cy = 2}] in
  
  let test_result : unit t_test_result = test_exec(test_step, insert_ship_matrix, (l, m)) in
  if test_is_success(test_result)
  then (
    assert_equals(test_step, "case valid list",  m.(0).(0), SHIP);
    assert_equals(test_step, "case valid list",  m.(1).(1), SHIP);
    assert_equals(test_step, "case valid list",  m.(2).(2), SHIP);
    assert_equals(test_step, "case valid list", m.(1).(2), EMPTY);
    assert_equals(test_step, "case valid list", m.(0).(1), EMPTY);
  ) 
  else 
    test_error(test_step);
  test_end(test_step);
;;
    
let test_insert_ship_matrix_b(status : t_test_status) : unit =
    let test_step : t_test_step = test_start(status, "insert_ship_matrix") in
    
    let m : t_matrix = [| [| EMPTY ; EMPTY; EMPTY|] ;[| EMPTY ; EMPTY; EMPTY|]; [| EMPTY ; EMPTY; EMPTY|] |] in
    let l : t_pos_list = [] in

    let test_result : unit t_test_result = test_exec(test_step, insert_ship_matrix, (l, m)) in
    if test_is_success(test_result)
    then (
      assert_equals(test_step, "case empty list", m, [| [| EMPTY ; EMPTY; EMPTY|] ;[| EMPTY ; EMPTY; EMPTY|]; [| EMPTY ; EMPTY; EMPTY|] |]);
    )
    else 
      test_error(test_step);
    test_end(test_step);
;;

let test_insert_rand_ship_a(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "insert_rand_ship") in
  let m : t_matrix = init_full_empty_board() in
  let nb_pos : int = 3 in
  let l : t_ship_remain = {pos= ref []; sh_type= 3; name= "sah"} in
  let param : t_param = init_param() in

  let test_result : unit t_test_result = test_exec(test_step, insert_rand_ship, (nb_pos, m, l, param)) in

  if test_is_success(test_result)
  then (
    for i=0 to len(!(l.pos)) - 1
    do 
      let pos : t_position = nth(!(l.pos), i) in
      assert_equals(test_step, "normal case", m.(pos.cx).(pos.cy), SHIP);
    done;
  ) else 
    test_error(test_step);
  test_end(test_step);
;;

let test_insert_rand_ship_b(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "insert_rand_ship") in
  let m : t_matrix = [| 
    [| SHIP ; SHIP; SHIP; SHIP; SHIP; SHIP; SHIP; SHIP; SHIP; SHIP|];
    [| SHIP ; SHIP; SHIP; SHIP; SHIP; SHIP; SHIP; SHIP; SHIP; SHIP|];
    [| SHIP ; SHIP; SHIP; EMPTY; EMPTY; EMPTY; SHIP; SHIP; SHIP; SHIP|];
    [| SHIP ; SHIP; SHIP; EMPTY; EMPTY; EMPTY; SHIP; SHIP; SHIP; SHIP|];
    [| SHIP ; SHIP; SHIP; EMPTY; EMPTY; EMPTY; SHIP; SHIP; SHIP; SHIP|];
    [| SHIP ; SHIP; SHIP; EMPTY; EMPTY; EMPTY; SHIP; SHIP; SHIP; SHIP|];
    [| SHIP ; SHIP; SHIP; EMPTY; EMPTY; EMPTY; SHIP; SHIP; SHIP; SHIP|];
    [| SHIP ; SHIP; SHIP; SHIP; SHIP; SHIP; SHIP; SHIP; SHIP; SHIP|];
    [| SHIP ; SHIP; SHIP; SHIP; SHIP; SHIP; SHIP; SHIP; SHIP; SHIP|];
    [| SHIP ; SHIP; SHIP; SHIP; SHIP; SHIP; SHIP; SHIP; SHIP; SHIP|];
  |] in
  let nb_pos : int = 3 in
  let l : t_ship_remain = {pos= ref []; sh_type= 3; name= "sah"} in
  let param : t_param = init_param() in

  let test_result : unit t_test_result = test_exec(test_step, insert_rand_ship, (nb_pos, m, l, param)) in

  if test_is_success(test_result)
  then (
    assert_equals(test_step, "case only 3 squares available", m.(3).(4), SHIP);
    assert_equals(test_step, "case only 3 squares available", m.(4).(4), SHIP);
    assert_equals(test_step, "case only 3 squares available", m.(5).(4), SHIP);
  ) else 
    test_error(test_step);
  test_end(test_step);
;;

let test_insert_rand_ship_b(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "insert_rand_ship") in
  let m : t_matrix = init_full_empty_board() in
  let nb_pos : int = 0 in
  let l : t_ship_remain = {pos= ref []; sh_type= 3; name= "sah"} in
  let param : t_param = init_param() in

  let test_result : unit t_test_result = test_exec(test_step, insert_rand_ship, (nb_pos, m, l, param)) in

  if test_is_success(test_result)
  then (
    assert_equals(test_step, "case trying to insert 0 length ship", len(!(l.pos)), 0);
  ) else 
    test_error(test_step);
  test_end(test_step);
;;

let test_init_matrix_ships_computer_a(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "init_matrix_ships_computer") in
  let prm = init_param() in

  let test_result : (t_matrix * t_ship_remain_list) t_test_result = test_exec(test_step, init_matrix_ships_computer, (prm)) in

  if test_is_success(test_result)
  then (
    let (m, l) : t_matrix * t_ship_remain_list = test_get(test_result) in
    assert_equals(test_step, "case normal param", len(!l), 5);
    for i=0 to len(!l) - 1
    do
      let s : t_ship_remain = nth(!(l), i) in
      let index : int = rand_int(0, len(!(s.pos)) - 1) in
      let element : t_position = nth(!(s.pos), index) in
      assert_equals(test_step, "case normal param", m.(element.cx).(element.cy), SHIP);
    done;
  ) else 
    test_error(test_step);
  test_end(test_step)
;;

let test_position_in_list_a(status : t_test_status) =
  let test_step : t_test_step = test_start(status, "position_in_list") in
  let p : t_position = {cx=3; cy=3} in
  let l : t_pos_list = [{cx=1; cy=8};{cx=2; cy=9};{cx=3; cy=3};{cx=5; cy=4}] in

  let test_result : bool t_test_result = test_exec(test_step, position_in_list, (p, l)) in

  if test_is_success(test_result)
  then
    assert_true(test_step, "case should find", test_get(test_result))
  else 
    test_error(test_step);
  test_end(test_step)
;;

let test_position_in_list_b(status : t_test_status) =
  let test_step : t_test_step = test_start(status, "position_in_list") in
  let p : t_position = {cx=3; cy=3} in
  let l : t_pos_list = [] in

  let test_result : bool t_test_result = test_exec(test_step, position_in_list, (p, l)) in

  if test_is_success(test_result)
  then
    assert_false(test_step, "case empty list", test_get(test_result))
  else 
    test_error(test_step);
  test_end(test_step)
;;

let test_position_in_list_c(status : t_test_status) =
  let test_step : t_test_step = test_start(status, "position_in_list") in
  let p : t_position = {cx=9; cy=3} in
  let l : t_pos_list = [{cx=1; cy=8};{cx=2; cy=9};{cx=3; cy=3};{cx=5; cy=4}] in

  let test_result : bool t_test_result = test_exec(test_step, position_in_list, (p, l)) in

  if test_is_success(test_result)
  then
    assert_false(test_step, "case not in list", test_get(test_result))
  else 
    test_error(test_step);
  test_end(test_step)
;;

let test_choose_shoots_a(status : t_test_status) =
  let test_step = test_start(status, "choose_shoots") in

  let nb : int = 3 in
  let m : t_matrix = init_full_empty_board() in
  let prm : t_param = init_param() in

  let test_result : t_pos_list t_test_result = test_exec(test_step, choose_shoots, (nb, m, prm)) in

  if test_is_success(test_result)
  then (
    let result : t_pos_list = test_get(test_result) in
    assert_equals(test_step, "case valid input", len(result), nb);
    for i=0 to len(result) - 1
    do
      let pos : t_position = nth(result, i) in
      assert_equals(test_step, "case valid input", m.(pos.cx).(pos.cy), EMPTY);
    done;
  )
  else
    test_error(test_step);
  test_end(test_step)
;;

let test_choose_shoots_b(status : t_test_status) =
  let test_step = test_start(status, "choose_shoots") in

  let nb : int = 0 in
  let m : t_matrix = init_full_empty_board() in
  let prm : t_param = init_param() in

  let test_result : t_pos_list t_test_result = test_exec(test_step, choose_shoots, (nb, m, prm)) in

  if test_is_success(test_result)
  then (
    let result : t_pos_list = test_get(test_result) in
    assert_equals(test_step, "case no shoots", len(result), nb);
  )
  else
    test_error(test_step);
  test_end(test_step)
;;

let test_rem_position_from_list_a(status : t_test_status) = 
  let test_step = test_start(status, "rem_position_from_list") in

  let p : t_position = {cx = 2; cy = 2} in
  let l : t_ship_remain_list = ref [
    {pos= ref [{cx = 8; cy = 2};{cx = 6; cy = 2};{cx = 2; cy = 2};{cx = 4; cy = 2}]; sh_type= 4; name= "sah"
  }] in

  let test_result : int t_test_result = test_exec(test_step, rem_position_from_list, (p, l)) in

  if test_is_success(test_result)
  then (
    let result : int = test_get(test_result) in
    assert_notequals(test_step, "case is in list", result, 2);
  )
  else
    test_error(test_step);
  test_end(test_step)
;;

let test_rem_position_from_list_b(status : t_test_status) = 
  let test_step = test_start(status, "rem_position_from_list") in

  let p : t_position = {cx = 2; cy = 2} in
  let l : t_ship_remain_list = ref [
    {pos= ref [{cx = 8; cy = 2};{cx = 6; cy = 2};{cx = 2; cy = 2};{cx = 4; cy = 2}]; sh_type= 4; name= "sah"}
  ] in

  let test_result : int t_test_result = test_exec(test_step, rem_position_from_list, (p, l)) in

  if test_is_success(test_result)
  then (
    let result : int = test_get(test_result) in
    assert_equals(test_step, "case not in list", result, -1);
  )
  else
    test_error(test_step);
  test_end(test_step)
;;

let test_all_shoots_a(status : t_test_status) = 
  let test_step = test_start(status, "all_shoots") in

  let prm : t_param = init_param() in
  let players : t_play = init_play("sah1", false, "sah2", false, prm) in
  let shoots : t_pos_list = choose_shoots(3, players.pl2.mymat, prm) in

  let expected_result : t_value array = arr_make(len(shoots), TRIED) in
  for i=0 to len(shoots) - 1
  do
    let el : t_position = nth(shoots, i) in
    if players.pl2.mymat.(el.cx).(el.cy) = SHIP
    then expected_result.(i) <- DAMAGED
  done;


  let test_result : int t_test_result = test_exec(test_step, all_shoots, (shoots, players.pl1, players.pl2, prm)) in

  if test_is_success(test_result)
  then (
    assert_equals(test_step, "case 3 valid shots", test_get(test_result), 0);
    for i=0 to len(shoots) - 1
    do
      let el : t_position = nth(shoots, i) in
      assert_equals(test_step, "case 3 valid shots", players.pl2.mymat.(el.cx).(el.cy), expected_result.(i))
    done;
  )
  else
    test_error(test_step);
  test_end(test_step)
;;

let test_all_shoots_b(status : t_test_status) = 
  let test_step = test_start(status, "all_shoots") in

  let prm : t_param = init_param() in
  let players : t_play = init_play("sah1", false, "sah2", false, prm) in
  let shoots : t_pos_list = [] in

  let test_result : int t_test_result = test_exec(test_step, all_shoots, (shoots, players.pl1, players.pl2, prm)) in

  if test_is_success(test_result)
  then (
    assert_equals(test_step, "case empty list", test_get(test_result), 0);
  )
  else
    test_error(test_step);
  test_end(test_step)
;;


let test_all_shoots_c(status : t_test_status) = 
  let test_step = test_start(status, "all_shoots") in

  let prm : t_param = init_param() in
  let players : t_play = init_play("sah1", false, "sah2", false, prm) in
  let shoots : t_pos_list = [{cx=3; cy=3};{cx=13; cy=3};{cx=4; cy=2};] in

  let test_result : int t_test_result = test_exec(test_step, all_shoots, (shoots, players.pl1, players.pl2, prm)) in

  if test_is_success(test_result)
  then (
    assert_equals(test_step, "case 2 valid shots, 1 invalid", test_get(test_result), 1);
  )
  else
    test_error(test_step);
  test_end(test_step)
;;

let test_all_shoots_d(status : t_test_status) = 
  let test_step = test_start(status, "all_shoots") in

  let prm : t_param = init_param() in
  let players : t_play = init_play("sah1", false, "sah2", false, prm) in
  let shoots : t_pos_list = [{cx=0; cy=0}] in

  players.pl2.mymat.(0).(0) <- TRIED;

  let test_result : int t_test_result = test_exec(test_step, all_shoots, (shoots, players.pl1, players.pl2, prm)) in

  if test_is_success(test_result)
  then (
    assert_equals(test_step, "case already shot pos", test_get(test_result), len(shoots));
  )
  else
    test_error(test_step);
  test_end(test_step)
;;


(**
  Fonction du driver de test pour lancer tous les tests précédents.
  @author Etienne & Hakim
*)
let test_run() : unit =
  rand_init();
  let status : t_test_status = create_test_status() in
  (
    test_valid_empty_position_a(status);
    test_valid_empty_position_b(status);
    test_valid_empty_position_c(status);
    test_is_empty_neighbour_a(status);
    test_is_empty_neighbour_b(status);
    test_is_empty_neighbour_c(status);
    test_insert_ship_matrix_a(status);
    test_insert_ship_matrix_b(status);
    test_insert_rand_ship_a(status);
    test_insert_rand_ship_b(status);
    test_init_matrix_ships_computer_a(status);
    test_position_in_list_a(status);
    test_position_in_list_b(status);
    test_position_in_list_c(status);
    test_choose_shoots_a(status);
    test_choose_shoots_b(status);
    test_rem_position_from_list_a(status);
    test_all_shoots_a(status);
    test_all_shoots_b(status);
    test_all_shoots_c(status);
    test_all_shoots_d(status);
  );
  print_test_report(status)
;;
  
test_run();;