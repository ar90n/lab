let rec max_height = function
    | 0 -> 0 
    | n ->
      let h = max_height (n - 1) in
      if max_height (n - min_nodes (h - 1) - 1) = h then h + 1 else h;;
