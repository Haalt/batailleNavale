open CPutil
open Types
open CPbattleship


let generate_random_string(l : int) : string =
  let alphabet : string array = [|"A";"B";"C";"D";"E";"F";"G";"H";"I";"J";"K";"L";"M";"N";"O";"P";"Q";"R";"S";"T";"U";"V";"W";"X";"Y";"Z";"a";"b";"c";"d";"e";"f";"g";"h";"i";"j";"k";"l";"m";"n";"o";"p";"q";"r";"s";"t";"u";"v";"w";"x";"y";"z"|] in
  let s : string ref = ref "" in
  for i=0 to l
  do
    let i : int = rand_int(0, 48) in
    s := !s ^ alphabet.(i);
  done;
  !s
;;

(* argv:
--multiplayer: bool
--p1Name: string
--p2Type: string => hum2 = p2Type == "cpu" ? false : true
--p2Name: string => nom : "retardedAi"
*)


let main() =
  rand_init();

  let playerNb : int ref = ref 0 and nm_1 : string ref = ref "" and hum_1 : bool = true and nm_2 : string ref = ref "" and hum_2 : bool ref = ref false and prm : t_param = init_param() in
  for i = 1 to Array.length Sys.argv - 1 
  do
    if Sys.argv.(i) = "--pseudo" then nm_1 := Sys.argv.(i + 1)
    else if Sys.argv.(i) = "--pseudo2" then nm_2 := Sys.argv.(i + 1)
    else if Sys.argv.(i) = "--player" then
      if Sys.argv.(i+1) = "1" then (
        hum_2 := false;
        nm_2 := "retardedAi"
      ) else 
        hum_2 := true;
  done;

  let id : string = generate_random_string(5) in
  let pl : t_play = init_play(!nm_1, hum_1, !nm_2, !hum_2, id, prm) in

  save_game_info(id, pl);

  print_game_info(pl.pl1, pl.prm, pl.gameId)
;;

let () = main();;