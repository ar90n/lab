let Lcs (xs : 'a array) =
    let n = Array.length xs
    let dp = Array2D.create (n+1) (n+1) -1
    for i = 0 to n do
        dp.[i,0] <- 0
        dp.[0,i] <- 0
    done
    for i = 1 to n do
        for j = 1 to n do
            let c = if xs.[i-1] = xs.[n - j] then 1 else 0
            dp.[i,j] <- max (max (dp.[i-1,j-1] + c) dp.[i-1,j]) dp.[i,j-1]
        done
    done
    dp.[n,n]
