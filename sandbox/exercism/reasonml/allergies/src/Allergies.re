let toList: int => list(string) = v => {
    [
        if((v land 0x01) != 0) "eggs" else "",
        if((v land 0x02) != 0) "peanuts" else "",
        if((v land 0x04) != 0) "shellfish" else "",
        if((v land 0x08) != 0) "strawberries" else "",
        if((v land 0x10) != 0) "tomatoes" else "",
        if((v land 0x20) != 0) "chocolate" else "",
        if((v land 0x40) != 0) "pollen" else "",
        if((v land 0x80) != 0) "cats" else ""
    ]
    |> List.filter(x => x != "")
}

let isAllergicTo: (string, int) => bool = (s, v) => {
    toList(v) |> List.exists(x => x == s)
}

