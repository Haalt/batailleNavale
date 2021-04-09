open CPutil
open Types
open Ai

let test_increase_probas(status : t_test_status) = 
  let test_step = test_start(status, "increase_probas") in

  let pos : t_position = { cx=0; cy=0 } in
  let sh_size : int = 3 in
  let is_vertical : bool = true in
  let probas : t_proba_matrix = mat_make(10, 10, 0) in
  
  let test_result : unit t_test_result = test_exec(test_step, increase_probas, (pos, sh_size, is_vertical, probas)) in

  if test_is_success(test_result)
  then (
    assert_equals(test_step, "case is_vertical=true", probas.(0).(0), 1);
    assert_equals(test_step, "case is_vertical=true", probas.(1).(0), 1);
    assert_equals(test_step, "case is_vertical=true", probas.(2).(0), 1);
    assert_equals(test_step, "case is_vertical=true", probas.(3).(0), 0);
  )
  else
    test_error(test_step);
  test_end(test_step)
;;

let test_increase_probas_b(status : t_test_status) = 
  let test_step = test_start(status, "increase_probas") in

  let pos : t_position = { cx=0; cy=0 } in
  let sh_size : int = 3 in
  let is_vertical : bool = false in
  let probas : t_proba_matrix = mat_make(10, 10, 0) in
  
  let test_result : unit t_test_result = test_exec(test_step, increase_probas, (pos, sh_size, is_vertical, probas)) in

  if test_is_success(test_result)
  then (
    assert_equals(test_step, "case is_vertical=false 0-0", probas.(0).(0), 1);
    assert_equals(test_step, "case is_vertical=false 0-1", probas.(0).(1), 1);
    assert_equals(test_step, "case is_vertical=false 0-2", probas.(0).(2), 1);
    assert_equals(test_step, "case is_vertical=false 0-3", probas.(0).(3), 0);
    assert_equals(test_step, "case is_vertical=false 1-0", probas.(1).(0), 0);
  )
  else
    test_error(test_step);
  test_end(test_step)
;;

let test_get_probabilities(status : t_test_status) = 
  let test_step = test_start(status, "increase_probas") in

  let opp_board : t_matrix = mat_make(10, 10, EMPTY) in
  let prm : t_param = {
    mat = {
      dx = 10;
      dy = 10
    };
    ships = {
      len = 5;
      sh_type = [|5;4;3;3;2|]
    };
    shoots_nb = 5
  } in

  let opp_ships : t_ship_remain_opp_list = ref [
    { pos_nb = ref 5; sh_type = 5; name = "porte avion" };
    { pos_nb = ref 4; sh_type = 4; name = "croiseur" };
    { pos_nb = ref 3; sh_type = 3; name = "contre torpilleur" };
    { pos_nb = ref 3; sh_type = 3; name = "contre torpilleur" };
    { pos_nb = ref 2; sh_type = 2; name = "torpilleur" };
  ] in
  
  let test_result : t_proba_matrix t_test_result = test_exec(test_step, get_probabilities, (opp_board, opp_ships, prm)) in

  if test_is_success(test_result)
  then (
    let probas : t_proba_matrix = test_get(test_result) in
    assert_equals(test_step, "case 0-0", probas.(0).(0), 10);
    print_string(string_of_int(probas.(0).(0))^"\n");
    assert_equals(test_step, "case 0-1", probas.(0).(1), 15);
    assert_equals(test_step, "case 1-0", probas.(1).(0), 15);
    assert_equals(test_step, "case 1-1", probas.(1).(1), 20);
  )
  else
    test_error(test_step);
  test_end(test_step)
;;

let test_run() : unit =
  rand_init();
  let status : t_test_status = create_test_status() in
  (
    test_increase_probas(status);
    test_increase_probas_b(status);
    test_get_probabilities(status)
  );
  print_test_report(status)
;;
  
test_run();;