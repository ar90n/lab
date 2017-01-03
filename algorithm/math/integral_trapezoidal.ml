let rec integral f a b =
    let dx = 0.1e-0 in
        let trapezoidal_rule f a = ( (  f a  +.  f ( a +. dx ) ) *. dx ) /. 2.0 in
	if b <= a then 0.0
        else trapezoidal_rule f a +. integral f ( a +. dx ) b;;

print_float ( integral (fun x -> x *. x ) 0.0 3.0 );;
