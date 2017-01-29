let rec duplicate = function
| [] as l -> l
| h :: r -> h :: h :: duplicate r;;

duplicate ["a";"b";"c";"c";"d"];;
