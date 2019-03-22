let find: (array(int), int) => option(int) = (lst, v) => {
    let rec aux = (lhi, rhi) => {
        if(rhi < lhi) {
            None
        } else {
            let mid = (lhi + rhi) / 2;
            switch(lst[mid]) {
            | mv when v < mv => aux(lhi, mid-1)
            | mv when mv < v => aux(mid+1, rhi)
            | _ => Some(mid)
            }
        }
    }

    aux(0, Array.length(lst) - 1)
}
