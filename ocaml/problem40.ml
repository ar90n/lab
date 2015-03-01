type bool_expr =
  |Var of string
  |Not of bool_expr
  |And of bool_expr * bool_expr
  |Or of bool_expr * bool_expr;;

let table2 v1 v2 expr = 
  let rec look_up_var var_name = function
  | [] -> raise Not_found
  | [(name,v)] -> if name = var_name then v else raise Not_found
  | (name,v)::t -> if name = var_name then v else look_up_var var_name t in 
  let rec aux var_tab = function
  | Var( v ) -> look_up_var v var_tab
  | Not( expr ) -> not ( aux var_tab expr )
  | And( lh, rh ) -> ( aux var_tab lh ) && ( aux var_tab rh )
  | Or ( lh, rh ) -> ( aux var_tab lh ) || ( aux var_tab rh ) in
  let var_tabs = [[( v1, true ); (v2, true )]; [(v1,true);(v2,false)];[(v1,false);(v2,true)];[(v1,false);(v2,false)]] in
  List.map (fun tab -> let (_,v1_val)::(_,v2_val)::t = tab in ( v1_val, v2_val, aux tab expr ) ) var_tabs;;

table2 "a" "b" (And(Var "a", Or(Var "a", Var "b")));;
