let BinarySearch (haystack : 'a array) (needle : 'a) : int option =
    let (lo,hi) = (0, Array.length haystack - 1)
    let rec BinarySearch' (lo : int) (hi : int) =
        if hi < lo then None
        else
            let mid = lo + (hi - lo) / 2
            let pivot = fst  (haystack.[mid])
            if pivot > needle then BinarySearch' lo (mid - 1)
            else if pivot < needle then BinarySearch' (mid+1) hi
            else Some mid
    BinarySearch' lo hi
