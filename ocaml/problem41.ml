type bool_expr =
  |Var of string
  |Not of bool_expr
  |And of bool_expr * bool_expr
  |Or of bool_expr * bool_expr;;

let table vs expr = 
  let rec look_up_var var_name = function
  | [] -> raise Not_found
  | [(name,v)] -> if name = var_name then v else raise Not_found
  | (name,v)::t -> if name = var_name then v else look_up_var var_name t in 
  let rec aux var_tab = function
  | Var( v ) -> look_up_var v var_tab
  | Not( expr ) -> not ( aux var_tab expr )
  | And( lh, rh ) -> ( aux var_tab lh ) && ( aux var_tab rh )
  | Or ( lh, rh ) -> ( aux var_tab lh ) || ( aux var_tab rh ) in
let gen_tab l = 
let rec aux acc = function
| [] -> [acc]
| h::t -> ( aux ((h,true)::acc) t ) @ ( aux ((h,false)::acc) t ) in
aux [] l in
List.map (fun tab -> ( tab, aux tab expr ) ) ( gen_tab vs );;

table ["a";"b"] (And(Var "a", Or(Var "a", Var "b")));;

let a = Var "a" and b = Var "b" and c = Var "c" in
table ["a"; "b"; "c"] (Or(And(a, Or(b,c)), Or(And(a,b), And(a,c))));;
