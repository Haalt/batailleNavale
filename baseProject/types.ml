open CPutil

type t_value = EMPTY | SHIP | DAMAGED | TRIED;;

type t_matrix = t_value matrix;; 

type t_position = {cx : int ; cy : int};;

type t_pos_list = t_position list;;

type t_remaining_pos = t_pos_list ref;;

type t_ship_remain = {pos: t_remaining_pos; sh_type: int; name: string};;
type t_ship_remain_opp = {pos_nb: int ref; sh_type: int; name: string};;

type t_ship_remain_list = t_ship_remain list ref;;
type t_ship_remain_opp_list = t_ship_remain_opp list ref;;


(* opp_nb *)
type t_player = {name : string ; human : bool ; 
                 mymat : t_matrix ; remain : t_ship_remain_list;
                 opp_mat : t_matrix ; opp_remain : t_ship_remain_opp_list};;

type t_play = {pl1 : t_player ; pl2 : t_player};;

type t_matrix_param = {dx : int ; dy : int};;
type t_ships_param = {len : int ; sh_type : int array};;
type t_param = {mat : t_matrix_param ; ships : t_ships_param ; shoots_nb : int};;
 


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