let isAsk: string => bool = s => s != "" && s.[String.length(s) - 1] == '?';
let isYell: string => bool = s => s != "" && String.uppercase(s) == s && String.lowercase(s) != s;
let isWithoutSaying: string => bool = s => String.trim(s) == ""

let hey: string => string = s => {
    let trimed = String.trim(s)
    switch((isAsk(trimed), isYell(trimed), isWithoutSaying(trimed))) {
    | (true, true, _) => "Calm down, I know what I'm doing!"
    | (true, false, _) => "Sure."
    | (false, true, _) => "Whoa, chill out!"
    | (false, false, true) => "Fine. Be that way!"
    | _ => "Whatever."
    }
}
