open CPutil
open Types



type t_point = {x : int ; y : int} ;;

type t_color_param = {empty : t_color ; ship : t_color ; 
                        damaged : t_color ; tried : t_color ;
                        border : t_color ; letter : t_color} ;;

type t_graphic = {ecx : int ; ecy : int ; dil : int} ;;

type t_graphic_param = {par : t_param ; graph : t_graphic ; 
                        color : t_color_param} ;;

let graph_prm(p : t_graphic_param) : t_param =
  p.par
;;

(* ecx = ecart en x pour l'origine du trace *)
(* et ecart entre matrices *)
(* ecy = ecart en y pour l'origine du trace et ecart *)
(* entre matrices et zone de message *)
(* dil = coefficient d'homothetie *)

let init_graphic_param(prm : t_param) : t_graphic_param = 
  {par = prm; graph = {ecx = 100; ecy = 50 ; dil = 20} ; 
      color = {empty = white ; ship = grey ; damaged = red ; 
               tried = green ; border = blue ; letter = red}}
;;

let graph_dil(gprm : t_graphic_param) : int =
  gprm.graph.dil
;;

let graph_ecx(gprm : t_graphic_param) : int =
  gprm.graph.ecx
;;

let graph_ecy(gprm : t_graphic_param) : int =
  gprm.graph.ecy
;;

(* couleurs *)
let color_empty(gprm : t_graphic_param) : t_color =
  gprm.color.empty
;;

let color_ship(gprm : t_graphic_param) : t_color =
  gprm.color.ship
;;

let color_damaged(gprm : t_graphic_param) : t_color =
  gprm.color.damaged
;;

let color_tried(gprm : t_graphic_param) : t_color =
  gprm.color.tried
;;

let color_border(gprm : t_graphic_param) : t_color =
  gprm.color.border
;;

let color_letter(gprm : t_graphic_param) : t_color =
  gprm.color.letter
;;

(* ----------------------- *)



let color_of_value(col, gprm : t_value * t_graphic_param) : t_color =
  if col = EMPTY
  then color_empty(gprm)
  else 
    if col = SHIP
    then color_ship(gprm)
    else
      if col = DAMAGED
      then color_damaged(gprm)
      else 
        if col = TRIED 
        then color_tried(gprm)
        else failwith "erreur squarecol : type de case inconnu"
;;

(* --------------------------- *)

let point_for_matrix(i, gprm : int * t_graphic_param) : t_point =
  let px : int ref = ref (graph_ecx(gprm)) and
      py : int ref = ref (graph_ecy(gprm)) and 
      mx : int = matrix_dx(graph_prm(gprm)) and
      my : int = matrix_dy(graph_prm(gprm)) and
      dil : int = graph_dil(gprm)
  in
  if i = 1
  then {x = !px ; y = !py}
  else
    if i = 2
    then {x = 2 * !px + dil * mx ; y = !py}
    else 
      if i = 3
      then {x = !px ; y = 4 * !py + dil * my}
      else {x = 2 * !px + dil * mx ; y = 4 * !py + dil * my}
;;

let point_for_mess_area(gprm : t_graphic_param) : t_point = 
  {x = graph_ecx(gprm) + (graph_dil(gprm) * matrix_dx(graph_prm(gprm))) / 2 ; 
   y = 2 * graph_ecy(gprm) + graph_dil(gprm) * matrix_dy(graph_prm(gprm))}
;;

let draw_matrix(pmat, gprm : t_point * t_graphic_param) : unit =
  let t : string array = [|"A" ; "B" ; "C" ; "D" ; "E" ; "F" ; "G" ; "H" ; "I" ; "J"|] 
           and mx : int = matrix_dx(graph_prm(gprm)) 
           and my : int = matrix_dy(graph_prm(gprm)) 
           and dil : int = graph_dil(gprm) in
    (
    set_color(color_border(gprm)) ;
    for i = 0 to mx + 1
    do fill_rect(pmat.x + dil * i, pmat.y, 1, dil * (mx + 1) - 1) ;
    done ; 
    for j = 0 to my + 1
    do fill_rect(pmat.x, pmat.y + dil * j, dil * (my + 1) + 1, 1) ;
    done ; 
    set_color(color_empty(gprm)) ;
    fill_rect(pmat.x, pmat.y, dil - 1, dil - 1);
    set_color(color_letter(gprm)) ;
    for i = 1 to mx
    do 
      moveto(pmat.x + i * dil + dil / 2 - 1, pmat.y + dil /2 - 5) ;
      draw_string(string_of_int(i-1)) ;
    done ;  
    for j = 1 to my
    do 
      moveto(pmat.x + dil /2 - 1, pmat.y + j * dil + dil / 2 - 5) ;
      draw_string(t.(j-1)) ;
    done ;
  )
;;

let draw_mess_area(pmat, gprm : t_point * t_graphic_param) : unit =
  let dx : int = graph_ecx(gprm) + graph_dil(gprm) * matrix_dx(graph_prm(gprm)) 
         and dy : int = graph_ecy(gprm)
  in
    (
    set_color(color_border(gprm)) ;
    fill_rect(pmat.x, pmat.y, dx, 1) ;
    fill_rect(pmat.x, pmat.y + dy, dx, 1) ;
    fill_rect(pmat.x, pmat.y, 1, dy) ;
    fill_rect(pmat.x + dx, pmat.y, 1, dy + 1) ;
    )
;;

let clear_mess_area(gprm : t_graphic_param) : unit =
  let pt : t_point = point_for_mess_area(gprm) 
       and dx = graph_ecx(gprm) + graph_dil(gprm) * matrix_dx(graph_prm(gprm))
       and dy = graph_ecy(gprm) 
  in
    (
    set_color(color_empty(gprm)) ; 
    fill_rect(pt.x + 2, pt.y + 2, dx - 5, dy - 5)
    )
;;

let draw_mess(i, s, gprm : int * string * t_graphic_param) : unit =
    let p : t_point = point_for_mess_area(gprm) in
    let dy : int = if i = 1 then 10 else 25 in
      (
      set_color(color_letter(gprm)) ;
      moveto(p.x + 10, p.y + dy) ;
      draw_string(s) ;
      )
;;

let draw_frame(gprm : t_graphic_param) : unit = 
    (
    draw_matrix(point_for_matrix(1, gprm), gprm) ;
    draw_matrix(point_for_matrix(2, gprm), gprm) ; 
    draw_matrix(point_for_matrix(3, gprm), gprm) ;
    draw_matrix(point_for_matrix(4, gprm), gprm) ;
    draw_mess_area(point_for_mess_area(gprm), gprm) ;
    ) 
;;

let plot(pt, pmat, elt, gprm : t_point * t_point * t_value * t_graphic_param) : unit =
  let dil : int = graph_dil(gprm) in
    (
    set_color(color_of_value(elt, gprm)) ;
    fill_rect(pmat.x + dil * (pt.x + 1) + 2, pmat.y + dil * (pt.y + 1) + 2, 
                dil - 3, dil - 3) ;
    )
;;

let plot_matrix_value(i, pt, elt, gprm : int * t_point * t_value * t_graphic_param) : unit =
  let pmat : t_point = point_for_matrix(i, gprm) in
    plot(pt, pmat, elt, gprm)
;;

let draw_matrix(mat_nb, m, gprm : int * t_matrix * t_graphic_param) : unit =
  let maxx : int = matrix_dx(graph_prm(gprm)) - 1  
         and maxy : int = matrix_dy(graph_prm(gprm)) - 1 
  in
    for i = 0 to maxx
    do
      for j = 0 to maxy
      do plot_matrix_value(mat_nb, {x = i; y = j}, m.(i).(j), gprm)
      done
    done
;;

let draw_play(pl, gprm : t_play * t_graphic_param) : unit =
  (
  open_graph(700, 700);
  clear_graph() ;
  draw_frame(gprm) ;
  draw_matrix(1, pl.pl1.mymat, gprm) ; 
  draw_matrix(2, pl.pl1.opp_mat, gprm) ;
  draw_matrix(3, pl.pl2.mymat, gprm) ;
  draw_matrix(4, pl.pl2.opp_mat, gprm) ;
  )
;;