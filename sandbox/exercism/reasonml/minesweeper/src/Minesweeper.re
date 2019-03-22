let rec range = (b, e, s) => if(b < e) [b, ...range(b + s, e, s)] else []
let cross = (lh, rh) => List.map(l => List.map(r => (l, r), rh), lh) |> List.flatten

let annotate : array(string) => array(string) = level => {
    let r = Array.length(level);
    if(r == 0) {
        level
    } else {
        let c = String.length(level[0])
        let m = level
            |> Array.map(x => x
                |> Js.String.split("")
                |> Array.map(x => switch(x){
                    |"*" => -1
                    |" " => 0
                    })
               )

        cross(range(0, r, 1), range(0, c, 1))
        |> Array.of_list
        |> Array.iter(((i, j)) => if(m[i][j] == -1) {
                    if((i < r - 1) && (0 <= m[i+1][j])) {
                        m[i+1][j] = m[i+1][j] + 1
                    }
                    if((j < c - 1) && (0 <= m[i][j+1])) {
                        m[i][j+1] = m[i][j+1] + 1
                    }
                    if((i < r - 1) && (j < c - 1) && (0 <= m[i+1][j+1])) {
                        m[i+1][j+1] = m[i+1][j+1] + 1
                    }
                    if((i < r - 1) && (0 <= j - 1) && (0 <= m[i+1][j-1])) {
                        m[i+1][j-1] = m[i+1][j-1] + 1
                    }
                } else if(0 <= m[i][j]) {
                    if((i < r - 1) && (-1 == m[i+1][j])) {
                        m[i][j] = m[i][j] + 1
                    }
                    if((j < c - 1) && (-1 == m[i][j+1])) {
                        m[i][j] = m[i][j] + 1
                    }
                    if((i < r - 1) && (j < c - 1) && (-1 == m[i+1][j+1])) {
                        m[i][j] = m[i][j] + 1
                    }
                    if((i < r - 1) && (0 <= j - 1) && (-1 == m[i+1][j-1])) {
                        m[i][j] = m[i][j] + 1
                    }
                }
           )

        m
        |> Array.map(x => x
            |> Array.map(x => switch(x) {
                |(-1) => "*"
                |0 => " "
                |_ => string_of_int(x)
              })
            |> Js.Array.joinWith("")
           )
    }
}
