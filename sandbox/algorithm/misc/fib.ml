let rec fib_impl v n m = 
    if v = 0 then m 
    else fib_impl ( v - 1 ) ( n + m ) n;;

let fib n = fib_impl n 1 0;;

print_endline( string_of_int ( fib 0 ) );;
print_endline( string_of_int ( fib 1 ) );;
print_endline( string_of_int ( fib 2 ) );;
print_endline( string_of_int ( fib 3 ) );;
print_endline( string_of_int ( fib 4 ) );;
print_endline( string_of_int ( fib 5 ) );;
print_endline( string_of_int ( fib 6 ) );;
print_endline( string_of_int ( fib 7 ) );;
