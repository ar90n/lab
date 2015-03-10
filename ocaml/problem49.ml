let rec min_nodes h =
    if h <= 0 then 0 
    else if h = 1 then 1
else min_nodes (h - 1) + min_nodes (h - 2) + 1;;
