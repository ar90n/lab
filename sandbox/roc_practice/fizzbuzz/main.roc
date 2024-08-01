app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.12.0/Lb8EgiejTUzbggO2HVVuPJFkwvvsfW6LojkLR20kTVE.tar.br" }

import pf.Stdout
import pf.Task

main =
    List.range { start: At 1, end: At 100}
    |> List.map  fizzBuzz
    |> Str.joinWith ","
    |> Stdout.line

fizzBuzz : I32 -> Str
fizzBuzz = \n ->
    fizz = n % 3 == 0
    buzz = n % 5 == 0

    if fizz && buzz then
        "FizzBuzz"
    else if fizz then
        "Fizz"
    else if buzz then
        "Buzz"
    else
        Num.toStr n

expect fizzBuzz 1 == "1"
expect fizzBuzz 7 == "7"

expect fizzBuzz 3 == "Fizz"
expect fizzBuzz 9 == "Fizz"

expect fizzBuzz 5 == "Buzz"
expect fizzBuzz 20 == "Buzz"

expect fizzBuzz 15 == "FizzBuzz"