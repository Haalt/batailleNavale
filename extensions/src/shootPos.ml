open CPutil
open Types
open CPbattleship

(* argv:
--posX: int
--posY: int
--player: string
--gameId: string
*)

let play_ai(pl : t_play) : int = 
  if not(pl.pl2.human) then 
    play_one_player_computer(pl.pl2, pl.pl1, pl.prm)
  else 0
;;

let update_nextPlayer(pl : t_play) : unit =
  if pl.pl2.human then pl.nextPlayer := PL2;
;;

let main() =
  rand_init();

  let posX : int ref = ref 0 and posY : int ref = ref 0 and playerName : string ref = ref "" and gameId : string ref = ref "" in
  for i = 1 to Array.length Sys.argv - 1 
  do
    if Sys.argv.(i) = "--posX" then posX := int_of_string(Sys.argv.(i + 1))
    else if Sys.argv.(i) = "--posY" then posY := int_of_string(Sys.argv.(i + 1))
    else if Sys.argv.(i) = "--player" then playerName := Sys.argv.(i + 1)
    else if Sys.argv.(i) = "--gameId" then gameId := Sys.argv.(i + 1)
  done;

  let pos : t_position = {cx = !posX; cy = !posY} in
  let play : t_play = get_game_info(!gameId) in

 if !(play.nextPlayer) = PL1 && !playerName = play.pl1.name
 then (
    let is_success : bool = one_shoot(pos, play.pl1, play.pl2, play.prm) in

    if not(is_success) then failwith("SAHHHHH1")
    else (
      save_game_info(!gameId, play);

      let shoots_failed : int = play_ai(play) in
      update_nextPlayer(play);
      save_game_info(!gameId, play);
      
      print_game_info(play.pl1, play.prm, play.gameId)
    ) 
  ) else if !(play.nextPlayer) = PL2 && !playerName = play.pl2.name then (
    let is_success : bool = one_shoot(pos, play.pl2, play.pl1, play.prm) in

    if not(is_success) then failwith("SAHHHHH")
    else (
      play.nextPlayer := PL1;
      save_game_info(!gameId, play);
      
      print_game_info(play.pl2, play.prm, play.gameId)
    )
  )
;;

let () = main();;