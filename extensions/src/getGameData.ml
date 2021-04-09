open CPutil
open Types
open CPbattleship

let main() = 
  let playerName : string ref = ref "" and gameId : string ref = ref "" in
  for i = 1 to Array.length Sys.argv - 1 
  do
    if Sys.argv.(i) = "--player" then playerName := Sys.argv.(i + 1)
    else if Sys.argv.(i) = "--gameId" then gameId := Sys.argv.(i + 1)
  done;

  let p : t_play = get_game_info(!gameId) in
  if p.pl1.name = !playerName
    then print_game_info(p.pl1, p.prm, p.gameId)
  else if p.pl2.name = !playerName
    then print_game_info(p.pl2, p.prm, p.gameId)
;;

let () = main();;