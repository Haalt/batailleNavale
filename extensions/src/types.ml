open CPutil

type t_value = EMPTY | SHIP | DAMAGED | TRIED [@@deriving yojson] ;;

type t_play_next = PL1 | PL2 | NULL [@@deriving yojson] ;;

type t_matrix = t_value matrix [@@deriving yojson] ;; 

type t_position = {cx : int ; cy : int} [@@deriving yojson] ;;

type t_pos_list = t_position list [@@deriving yojson] ;;

type t_remaining_pos = t_pos_list ref [@@deriving yojson] ;;

type t_ship_remain = {pos: t_remaining_pos; sh_type: int; name: string} [@@deriving yojson] ;;
type t_ship_remain_opp = {pos_nb: int ref; sh_type: int; name: string} [@@deriving yojson] ;;

type t_ship_remain_list = t_ship_remain list ref [@@deriving yojson] ;;
type t_ship_remain_opp_list = t_ship_remain_opp list ref [@@deriving yojson] ;;


type t_player = {name : string ; human : bool ; 
                 mymat : t_matrix ; remain : t_ship_remain_list;
                 opp_mat : t_matrix ; opp_remain : t_ship_remain_opp_list} [@@deriving yojson] ;;

type t_matrix_param = {dx : int ; dy : int} [@@deriving yojson] ;;
type t_ships_param = {len : int ; sh_type : int array} [@@deriving yojson] ;;

type t_param = {mat : t_matrix_param ; ships : t_ships_param ; shoots_nb : int} [@@deriving yojson] ;;

type t_play = {pl1 : t_player ; pl2 : t_player; prm: t_param; nextPlayer: t_play_next ref; gameId: string} [@@deriving yojson];;

 
type t_proba_matrix = int matrix;;


(**
accesseur a la largeur de la grille extrait du paramètre p.

@author Hakim
@param p instancie les paramètres de notre jeu.
@return Renvoie la largeur de ma grille.
*)

let matrix_dx(p : t_param) : int =
  p.mat.dx
;;
  
(* accesseur a la hauteur de la grille*)
let matrix_dy(p : t_param) : int =
  p.mat.dy
;;

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