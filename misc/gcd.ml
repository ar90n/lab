let rec gcd x y = match y with
    | 0 -> x
    | _ -> gcd y ( x mod y );;

print_endline ( string_of_int ( gcd 20 16 ) );;
print_endline ( string_of_int ( gcd 1071 1029 ) );;
