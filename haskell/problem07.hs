data NestedList a = Elem a | List [NestedList a]
flatten:: (NestedList a) -> [a]
flatten (Elem x) = [x]
flatten (List x) = foldr (\a b -> ( flatten a ) ++ b ) [] x
