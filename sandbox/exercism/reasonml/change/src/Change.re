type coins = list(int);
let makeChange: (int, coins) => option(coins) = (v, lst) => {
    if(v < 0) {
        None
    } else {
        let cs = Array.of_list(lst)
        let numOfCoins = List.length(lst)
        let dp = Array.make_matrix(v + 1, numOfCoins+1, 100000000)
        let parent = Array.make_matrix(v + 1, numOfCoins+1, -1)

        for(i in 1 to numOfCoins) {
            dp[0][i] = 0
            parent[0][i] = 0
        }

        for(i in 1 to numOfCoins) {
            let c = cs[i-1]
            for(j in 1 to (v + 0)) {
                let v0 = if(0 <= (j - c)) (dp[j-c][i] + 1) else 100000000
                let v1 = dp[j][i-1]
                if(v0 < v1) {
                    dp[j][i] = v0
                    parent[j][i] = c
                } else {
                    dp[j][i] = v1
                    parent[j][i] = 0
                }
            }
        }

        if(dp[v][numOfCoins] == 100000000) {
            None
        } else {
            let result = ref([])
            let j = ref(v)
            let i = ref(numOfCoins)
            while(parent[j^][i^] != -1) {
                if(parent[j^][i^] == 0) {
                    i := i^ - 1
                } else {
                    let cc = cs[i^ -1]
                    result := [cc, ...(result^)]
                    j := j^ - cs[i^ -1]
                }
            }
            Some(result^)
        }
    }
}
