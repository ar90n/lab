module []

fruitDict : Dict Str U64
fruitDict =
    Dict.empty {}
    |> Dict.insert "Apple" 3
    |> Dict.insert "Banana" 2

expect
    Dict.len fruitDict == 2

expect
    Dict.get fruitDict "Apple" == (Ok 3)

expect
    Inspect.toStr fruitDict == "{\"Apple\": 3, \"Banana\": 2}"

expect
    Dict.keys fruitDict == ["Apple", "Banana"]

expect
    Dict.values fruitDict == [3, 2]

expect
    Dict.toList fruitDict == [("Apple", 3), ("Banana", 2)]

expect
    Dict.remove fruitDict "Apple"
    |> Dict.remove "Banana"
    |> Dict.isEmpty

expect
    updatedDict =
        Dict.update fruitDict "Apple" addFruit

    addFruit : [Present U64, Missing] -> [Present U64, Missing]
    addFruit = \valueTag ->
        when valueTag is
            Missing -> Present 1
            Present count -> Present (count + 1)

    Dict.get updatedDict "Apple" == (Ok 4)
