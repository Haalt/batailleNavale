open CPutil
open Types
open CPbattleship_display



(* ----------------------- *)
(* ----------------------- *)
(*  fonctions a developper *)
(* ----------------------- *)
(* ----------------------- *)

  
let ships_number(prm : t_param) : int =
  prm.ships.len
;;
  
let ships_type(prm : t_param) : int array =
  prm.ships.sh_type
;;
  
let shoots_number(prm : t_param) : int =
  prm.shoots_nb
;;
  
let init_param() : t_param =
  {
    mat = {
      dx = 10;
      dy = 10
    };
    ships = {
      len = 5;
      sh_type = [|5;4;3;3;2|]
    };
    shoots_nb = 5
  }
;;
  
let is_in_mat(x, y, p : int * int * t_param) : bool =
  let dx = matrix_dx(p) in
  let dy = matrix_dy(p) in
  0<= x && x < dx && 0 <= y && y < dy
;;

(* je vous laisse ajouter vos commentaires aux fonctions précédentes *)

(** @author Max *)
let is_empty(x, y, m : int * int * t_matrix) : bool =
  m.(x).(y)= EMPTY
;;
(** @author Edgar *)
let is_ship(x, y, m : int * int * t_matrix) : bool =
  m.(x).(y)= SHIP
;;

(**
Vérifie que la case est VALIDE ET que dans la matrice il s'agit d'une case vide.

@author Edgar & Noé
@param x ligne dans notre matrice
@param y colonne dans notre matrice
@param m matrice complete
@param prm ensemble des paramètres du jeu
@return Renvoie true si la case est valide et bien dans la matrice et que cette case est vide.
*)
let valid_empty_position(x, y, m, prm : int * int * t_matrix * t_param) : bool =
  is_in_mat(x,y,prm) && is_empty(x,y,m)
;;

(**
Verifie si les 8 cases adjascentes sont vides

@author Edgar
@param x ligne de la case
@param y colone de la case
@param m matrice
@param prm ensemble des paramètres du jeu
@return true si les cases voisines sont vides, false si il y a quelque chose
*)
let is_empty_neighbour(x, y, m, prm : int * int * t_matrix * t_param) : bool =
  if not(is_in_mat(x, y, prm)) then false
  else 
    let i : int ref = ref 0 and result : bool ref = ref true in
    let indexes : int matrix = [| [|-1; -1|]; [|-1; 0|]; [|-1; 1|]; [|0; -1|]; [|0; 1|]; [|1; -1|]; [|1; 0|];[|1; 1|] |] in
    while !i < 8 && !result
    do (
      if is_in_mat(x + indexes.(!i).(0), y + indexes.(!i).(1), prm)
        then result := is_empty(x + indexes.(!i).(0), y + indexes.(!i).(1), m)
      else ();
      i := !i + 1;
    )
    done;
  !result
;;

(**
verifie sur une case est dans la matrice et si les cases adjascentes sont vides

@author Edgar
@param x ligne de la case
@param y colone de la case
@param m matrice
@param prm ensemble des paramètres du jeu
@return true si elle est dans la matrice et qu'elle n'a pas de voisin
*)
let valid_empty_neighbour(x, y, m, prm : int * int * t_matrix * t_param) : bool =
  is_in_mat(x, y, prm) && is_empty_neighbour(x, y, m, prm)
;;

(**
retrouve la position suivante basé sur un point et une direction

@author Edgar
@param pt poisition du point precendant
@param int le numero de la direction
@return la position de la case suivante
*)
let next_position(pt, dir : t_position * int) : t_position =
  let indexes : int matrix = [| [|1; 0|]; [|1; 1|]; [|0; 1|]; [|-1; 1|]; [|-1; 0|]; [|-1; -1|]; [|0; -1|];[|-1; 1|] |] in
  {cx = pt.cx + indexes.(dir).(0); cy = pt.cy + indexes.(dir).(1)}
;;

(**
prend une list de case et ajoute SHIP a ces coordonnées dans la matrice

@author Edgar
@param l liste de position a marquer SHIP dans la matrice
@param m matrice
@return unit
*)
let rec insert_ship_matrix(l, m : t_pos_list * t_matrix) : unit =
  if not(len(l) = 0)
  then (
    let current_case = fst(l) in
    m.(current_case.cx).(current_case.cy) <- SHIP;
    insert_ship_matrix(rem_fst(l), m)
  ) 
;;

(**
insert un bateau de taille n dans la matrice de maniere aléatoire et ajoute ses coordonnées a la liste des position restantes au joueur

@author Edgar
@param nb_pos longueur du bateau a inserer
@param m la matrice
@param l liste des positions restantes au joueur
@param prm ensemble des paramètres du jeu
@return unit
*)
let insert_rand_ship(nb_pos, m, l, prm : int * t_matrix * t_ship_remain * t_param) : unit = 
  let isValid : bool ref = ref false in
  let tmpShipPos : t_remaining_pos = ref [] in
  while not(!isValid) && nb_pos > 0
  do
    let shouldSkip : bool ref = ref false and i : int ref = ref 0 in
    let direction : int = rand_int(0, 7) in
    let currentPos : t_position ref = ref {
      cx = rand_int(0, matrix_dx(prm));
      cy = rand_int(0, matrix_dy(prm))
    } in
    while !i < nb_pos && !shouldSkip = false
    do
      if valid_empty_neighbour(!currentPos.cx, !currentPos.cy, m, prm)
      then (
        tmpShipPos := add_lst(!tmpShipPos, !currentPos);
        currentPos := next_position(!currentPos, direction);
        i := !i + 1;
        if !i = nb_pos then (
          isValid := true;
          shouldSkip := true;
        )
      ) else (
        shouldSkip := true;
        tmpShipPos := []
      ) 
    done;
  done;
  insert_ship_matrix(!tmpShipPos, m);
  l.pos := concat(!(l.pos), !tmpShipPos);
;;


(**
initialise une matrice vide de 10x10

@author Edgar
@return une matrice vide
*)
let init_full_empty_board() : t_matrix =
  [| 
    [| EMPTY ; EMPTY; EMPTY; EMPTY; EMPTY; EMPTY; EMPTY; EMPTY; EMPTY; EMPTY|];
    [| EMPTY ; EMPTY; EMPTY; EMPTY; EMPTY; EMPTY; EMPTY; EMPTY; EMPTY; EMPTY|];
    [| EMPTY ; EMPTY; EMPTY; EMPTY; EMPTY; EMPTY; EMPTY; EMPTY; EMPTY; EMPTY|];
    [| EMPTY ; EMPTY; EMPTY; EMPTY; EMPTY; EMPTY; EMPTY; EMPTY; EMPTY; EMPTY|];
    [| EMPTY ; EMPTY; EMPTY; EMPTY; EMPTY; EMPTY; EMPTY; EMPTY; EMPTY; EMPTY|];
    [| EMPTY ; EMPTY; EMPTY; EMPTY; EMPTY; EMPTY; EMPTY; EMPTY; EMPTY; EMPTY|];
    [| EMPTY ; EMPTY; EMPTY; EMPTY; EMPTY; EMPTY; EMPTY; EMPTY; EMPTY; EMPTY|];
    [| EMPTY ; EMPTY; EMPTY; EMPTY; EMPTY; EMPTY; EMPTY; EMPTY; EMPTY; EMPTY|];
    [| EMPTY ; EMPTY; EMPTY; EMPTY; EMPTY; EMPTY; EMPTY; EMPTY; EMPTY; EMPTY|];
    [| EMPTY ; EMPTY; EMPTY; EMPTY; EMPTY; EMPTY; EMPTY; EMPTY; EMPTY; EMPTY|];
  |]
;;

(**
initialise les bateaux d'un joueur

@author Edgar
@return une liste des 5 bateaux qu'un joueur a au debut de partie
*)
let init_t_ship() : t_ship_remain_list = ref
  [
    { pos = ref []; sh_type = 5; name = "porte avion" };
    { pos = ref []; sh_type = 4; name = "croiseur" };
    { pos = ref []; sh_type = 3; name = "contre torpilleur" };
    { pos = ref []; sh_type = 3; name = "contre torpilleur" };
    { pos = ref []; sh_type = 2; name = "torpilleur" };
  ]
;;

let init_t_ship_remain() : t_ship_remain_opp_list = ref
  [
    { pos_nb = ref 5; sh_type = 5; name = "porte avion" };
    { pos_nb = ref 4; sh_type = 4; name = "croiseur" };
    { pos_nb = ref 3; sh_type = 3; name = "contre torpilleur" };
    { pos_nb = ref 3; sh_type = 3; name = "contre torpilleur" };
    { pos_nb = ref 2; sh_type = 2; name = "torpilleur" };
  ]
;;

(**
initialise la matrice d'un joueur ordinateur avec ses 5 bateaux inserés

@author Edgar
@param prm ensemble des paramètres du jeu
@return la matrice du joueur et la liste de coordonnées de ses bateaux
*)
let init_matrix_ships_computer(prm : t_param) : t_matrix * t_ship_remain_list =
  let m : t_matrix = init_full_empty_board() in
  let l : t_ship_remain_list = init_t_ship() in
  let iter_l : t_ship_remain_list = ref !l in
  for i=0 to len(!l) - 1
  do
    let current : t_ship_remain = fst(!iter_l) in
    insert_rand_ship(current.sh_type, m, current, prm);
    iter_l := rem_fst(!iter_l)
  done;
  (m, l)
;;

(**
initialise la matrice d'un joueur humain

@author Edgar
@param myname nom du joueur humain (pourquoi ...?)
@param prm ensemble des paramètres du jeu
@return une matrice vide et la liste des bateaux disponibles au joueur sans coordonées
*)
let init_matrix_ships_human(myname, prm : string * t_param) : t_matrix * t_ship_remain_list =
  (init_full_empty_board(), init_t_ship())
;;

(**
intialise la matrice d'un joueur

@author Edgar
@param myname nom du joueur
@param hum bool qui indique si le joueur est humain
@param prm ensemble des paramètres du jeu
@return une matrice
*)
let init_matrix_ships(myname, hum, prm : string * bool * t_param) : t_matrix * t_ship_remain_list = 
  if hum then init_matrix_ships_human(myname, prm)
  else init_matrix_ships_computer(prm)
;;

(**
compte le nombre total de case que tous les bateaux d'un joueur prennent

@author Edgar
@param prm ensemble des paramètres du jeu
@return le nombre de case
*)
let count_positions_ship(prm : t_param) : int =
  let counter : int ref = ref 0 in
  for i=0 to prm.ships.len - 1
  do
    counter := !counter + prm.ships.sh_type.(i);
  done;
  !counter
;;

(**
initialise un joueur

@author Edgar
@param myname nom du joueur
@param hum bool qui indique si le joueur est humain
@param prm ensemble des paramètres du jeu
@return un joueur
*)
let init_player(myname, hum, prm : string * bool * t_param) : t_player =
  let (m, l) : t_matrix * t_ship_remain_list = init_matrix_ships(myname, hum, prm) in
  {
    name = myname; 
    human = hum; 
    mymat = m; 
    remain = l; 
    opp_mat = init_full_empty_board(); 
    opp_remain = init_t_ship_remain()
  }
;;

(**
intialise deux joueurs

@author Edgar
@param nm1 nom du joueur 1
@param hum1 bool qui indique sur le joueur 1 est humain
@param nm2 nom du joueur 2
@param hum2 bool qui indique sur le joueur 2 est humain
@param prm ensemble des paramètres du jeu
@return deux joueurs
*)
let init_play(nm1, hum1, nm2, hum2, prm : string * bool * string * bool * t_param) : t_play = 
  { pl1 = init_player(nm1, hum1, prm); pl2 = init_player(nm2, hum2, prm) }
;;

(**
fonction qui cherche si une position est dans une liste de position

@author Edgar
@param p position recherchée
@param l liste de position dans laquelle on cherche
@return true si l element est dans la liste
*)
let rec position_in_list(p, l : t_position * t_pos_list) : bool =
  if len(l) = 0 then false
  else 
    let pos : t_position = fst(l) in
    if pos.cx = p.cx && pos.cy = p.cy 
      then true
    else position_in_list(p, rem_fst(l))
;;

(**
tire une position au hasard dans la matrice

@author Edgar
@param prm ensemble des paramètres du jeu
@return une poistion random
*)
let rand_position(prm : t_param) : t_position =
  {cx = rand_int(0, matrix_dx(prm)-1); cy = rand_int(0, matrix_dy(prm)-1)}
;;

(**
calcule une position aléatoire correspondant à une case vide d’une grille, et qui n’apparaît pas dans la liste l

@author Edgar

@param l liste de positions
@param m matrice
@param prm ensemble des paramètres du jeu
@return la position éligible a un tir
*)
let rec choose_one_shoot(l, m, prm : t_pos_list * t_matrix * t_param) : t_position =
  let rand_pos : t_position = rand_position(prm) in
  if not(position_in_list(rand_pos, l)) && is_empty(rand_pos.cx, rand_pos.cy, m)
    then rand_pos
  else choose_one_shoot(l, m, prm)
;;


(**
calcule une liste de destinations de tirs aléatoires distinctes correspondantes à des cases vides de la grille

@author Edgar
@param nb_shoot nombre de tirs a calculer
@param m matrice
@param prm ensemble des paramètres du jeu
@return une liste de position
*)
let choose_shoots(nb_shoots, m, prm : int * t_matrix * t_param) : t_pos_list =
  let rec choose_shoots_aux(nb, l, m, prm : int * t_pos_list * t_matrix * t_param) : t_pos_list = 
    if nb = 0 then l
    else 
      choose_shoots_aux(nb - 1, add_lst(l, choose_one_shoot(l, m, prm)), m, prm);
  in
  choose_shoots_aux(nb_shoots, [], m, prm)
;;

(* let rem_position_from_list_old(pos, pos_list : t_position * t_pos_list) : t_pos_list =
  let rec rem_position_from_list_aux(p, l, index : t_position * t_pos_list * int) : t_pos_list = 
    if index = len(l) then l
    else 
      let current = nth(l, index) in
      if current.cx = pos.cx && current.cy = pos.cy
      then rem_nth(l, index)
      else rem_position_from_list_aux(p, l, index + 1)
  in
  rem_position_from_list_aux(pos, pos_list, 0)
;; *)

(**
cherche une position dans une liste de position

@author Edgar
@param l liste de position dans laquelle on cherche
@param p la position recherchée
@param index l'index actuel de la recherche
@return (true, index) si l'element est dans la liste, (false, -1) si l'élément n'est pas dans la liste 
*)
let rec is_in_list(l, p, index :  t_pos_list * t_position * int) : bool * int =
  if len(l) = 0 then (false, -1)
  else (
    let current : t_position = fst(l) in
    if current.cx = p.cx && current.cy = p.cy then (true, index)
    else is_in_list(rem_fst(l), p, index+1)
  )
;;

(**
supprime un bateau restant si la longueur de ses positions restantes est 0

@author Edgar
@param l la liste des bateaux restants
@param index l'index du bateau en question
@return unit
*)
let remove_ship_if_empty(l, index : t_ship_remain_list * int) : unit =
  let ship : t_ship_remain = nth(!l, index) in
  if len(!(ship.pos)) = 0
  then l := rem_nth(!l, index)
;;

(**
cherche et supprime une position dans une liste de bateaux, supprime le bateau si c'etait ca derniere position et renvois son index

@author Edgar
@param pos position a supprimer
@param ship_list liste de bateaux
@return l'index du bateau qui a perdu une position
*)
let rem_position_from_list(pos, ship_list : t_position * t_ship_remain_list) : int =
  let rec rem_position_from_list_aux(p, l, index : t_position * t_ship_remain_list * int) : int = 
    if index = len(!l) then -1
    else 
      let current = nth(!l, index) in
      let (isPresent, i) : bool * int = is_in_list(!(current.pos), pos, 0) in
      if isPresent
      then (
        current.pos := rem_nth(!(current.pos), i);
        remove_ship_if_empty(l, index);
        index
      ) else rem_position_from_list_aux(p, l, index + 1)
  in
  rem_position_from_list_aux(pos, ship_list, 0)
;;

(**
renvois DAMAGED si value egal SHIP sinon TRIED

@author Edgar
@param value la valeur a tester
@return DAMAGED si value egal SHIP sinon TRIED
*)
let get_shoot_result(value : t_value) : t_value = 
  if value = SHIP then DAMAGED
  else TRIED
;;

(**
retire 1 au bateau qui a été touché dans la liste des bateaux ennemis restants, retire le bateau si c'était sa derniere position

@author Edgar
@param opp_remain liste des bateaux restant à l'adversaire
@param index index du bateau ayant subis le tir
@return unit
*)
let decrease_opp_remain(opp_remain, index : t_ship_remain_opp_list * int) : unit =
  let rec decrease_opp_remain_aux(opp_r, opp_r2, i : t_ship_remain_opp list * t_ship_remain_opp list * int) : t_ship_remain_opp list = 
    if len(opp_r) = 0 then opp_r2
    else (
      let change : t_ship_remain_opp = fst(opp_r) in
      if i = index
      then (
        change.pos_nb := !(change.pos_nb) - 1;
        concat(add_lst(opp_r2, change), rem_fst(opp_r))
      ) else decrease_opp_remain_aux(rem_fst(opp_r), add_lst(opp_r2, change), i + 1)
    )
  in
  let ship : t_ship_remain_opp = nth(!opp_remain, index) in
  if !(ship.pos_nb) = 1
  then opp_remain := rem_nth(!opp_remain, index)
  else 
    opp_remain := decrease_opp_remain_aux(!opp_remain, [], 0)
;;

(**
effectue un tir sur la position pos du joueur_2, renvois false si la position est hors de la matrice ou si la position a déjà été tirée, sinon prend en compte le tir

@author Edgar
@param pos la position du tir
@param player_1 joueur qui tire
@param player_2 joueur qui se fait rafaler
@param prm ensemble des paramètres du jeu
@return true si le tir est valide, false sinon
*)
let one_shoot(pos, player_1, player_2, prm : t_position * t_player * t_player * t_param) : bool = 
  if not(is_in_mat(pos.cx, pos.cy, prm)) then false
  else (
    let value : t_value = player_2.mymat.(pos.cx).(pos.cy) in
    if not(value = EMPTY) && not(value = SHIP) then false
    else (
      let hit_result : t_value = get_shoot_result(value) in
      player_1.opp_mat.(pos.cx).(pos.cy) <- hit_result;
      player_2.mymat.(pos.cx).(pos.cy) <- hit_result;
      if hit_result = DAMAGED
      then (
        let index : int = rem_position_from_list(pos, player_2.remain) in
        decrease_opp_remain(player_1.opp_remain, index);
        true
      ) else true
    )
  )
;;

let rec all_shoots_aux(shoots, count, player_1, player_2, prm : t_pos_list * int * t_player * t_player * t_param) : int = 
  if len(shoots) = 0 then count
  else if one_shoot(fst(shoots), player_1, player_2, prm)
    then all_shoots_aux(rem_fst(shoots), count - 1, player_1, player_2, prm)
  else all_shoots_aux(rem_fst(shoots), count, player_1, player_2, prm)
;;

(**
effectue le nombre shoots de tirs du joueur_1 sur le joueur_2

@author Edgar
@param shoots le nomrbre de tirs
@param player_1 joueur qui tire
@param player_2 joueur qui se fait rafaler
@param prm ensemble des paramètres du jeu
@return la nombre de tirs manqués
*)
let all_shoots(shoots, player_1, player_2, prm : t_pos_list * t_player * t_player * t_param) : int =
  all_shoots_aux(shoots, len(shoots), player_1, player_2, prm)
;;

(**
fait jouer un joueur ordinateur

@author Edgar
@param player_1 joueur ordinateur dont c'est le tour de jouer
@param player_2 joueur dont ce n'est pas le tour de jouerer
@param prm ensemble des paramètres du jeu
@return le nombre de tirs qui ont échoué
*)
let play_one_player_computer(player_1, player_2, prm : t_player * t_player * t_param) : int =
  all_shoots(choose_shoots(1, player_1.opp_mat ,prm), player_1, player_2, prm)
;;

(**
fait jouer un joueur humain

@author Edgar
@param player_1 joueur humain dont c'est le tour de jouer
@param player_2 joueur dont ce n'est pas le tour de jouerer
@param prm ensemble des paramètres du jeu
@return 0
*)
let play_one_player_human(player_1, player_2, prm : t_player * t_player * t_param) : int =
  0
;;

(**
fait jouer player_1 en testant si il est humain ou ordinateur

@author Edgar
@param player_1 joueur dont c'est le tour de jouer
@param player_2 joueur dont ce n'est pas le tour de jouer
@param prm ensemble des paramètres du jeu
@return le nombre de tir ayant échoué
*)
let play_one_player(player_1, player_2, prm : t_player * t_player * t_param) : int =
  if player_1.human then play_one_player_human(player_1, player_2, prm)
  else play_one_player_computer(player_1, player_2, prm)
;;

(**
renvois 0 ou 1 en si w est pair ou impair

@author Edgar
@param w nombre a tester
@return 0 ou 1
*)
let new_who(w : int) : int =
  1 - (w mod 2)
;;


(* ----------------------- *)
(* ----------------------- *)
(*  fonction principale    *)
(* ----------------------- *)
(* ----------------------- *)


let play(nm_1, hum_1, nm_2, hum_2 : string * bool * string * bool) : unit =
  rand_init();
  let prm : t_param = init_param() and who : int ref = ref 1 
  and the_end : bool ref = ref false and nb_failed : int ref = ref 0
  in
  let g_prm : t_graphic_param = init_graphic_param(prm) in
  let pl : t_play = init_play(nm_1, hum_1, nm_2, hum_2, prm) 
  in
  
  (* print_string("initialisation") ; print_newline() ; *)
  draw_play(pl, g_prm) ;
  (* wait(3) ; *)
  while not(!the_end) 
  do
    draw_mess(0, "joueur "^string_of_int(!who)^" joue :", g_prm);
    if !who = 1
    then nb_failed := play_one_player(pl.pl1, pl.pl2, prm)
    else nb_failed := play_one_player(pl.pl2, pl.pl1, prm);
    draw_play(pl, g_prm) ;
    if !nb_failed > 0
    then draw_mess(!who, string_of_int(!nb_failed) ^" tirs deja tires", g_prm);
    (* wait(1) ; *)
    who := new_who(!who) ;
    the_end := (len(!(pl.pl1.opp_remain)) = 0) || (len(!(pl.pl2.opp_remain)) = 0);
  done;
  wait(20);
;;


(* play("sah1", false, "sah2", false);; *)

    

