let rec min_nodes h =
    if h <= 0 then 0 
    else if h = 1 then 1
else min_nodes (h - 1) + min_nodes (h - 2) + 1;;
let rec max_height = function
    | 0 -> 0 
    | n ->
      let h = max_height (n - 1) in
      if max_height (n - min_nodes (h - 1) - 1) = h then h + 1 else h;;
