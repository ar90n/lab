module Primes =
    let mutable primes = [7; 5; 3; 2]
    let mutable upper_bound = 7

    let rec create (n : int) : int list =
        for i = upper_bound + 1 to n do
            if isPrime i then
                primes <- i :: primes
            upper_bound <- i
        done
        List.filter ((>=) n) primes

    and isPrime (n : int) : bool = 
        n / 2
        |> create
        |> List.forall (fun p -> n % p <> 0)

    let factorize (n : int) : int list =
        let rec doit (n : int) (primes : int list) (factors : int list) : int list =
            match primes with
            | hp::tp when n % hp = 0 -> doit (n / hp) primes (hp::factors)
            | hp::tp -> doit n tp factors
            | [] -> factors
        let primes = create n
        doit n primes []
