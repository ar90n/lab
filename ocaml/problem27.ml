let test_func1 list ns =
  let rec aux acc emit ns = function
  | [] -> acc
  | [x] as l -> ( match ns with
                    | [1] -> (
                        emit l acc
                      )
                    | _ -> (
                        emit [] acc
                      ) )
  | h::t -> ( let new_emit x = emit ( h :: x ) in
              match ns with
              | [] as l -> acc
              | [0] -> (
                  emit [] acc
                )
              | [hn] -> (
                  aux ( aux acc emit [hn] t ) new_emit [( hn - 1 )] t
                )
              | 0::tn -> (
                  aux ( emit [] acc ) ( fun x acc -> (x::acc)) tn (h::t)
                )
              | hn::tn -> (
                  aux ( aux acc emit (hn::tn) t ) new_emit (( hn - 1 )::tn) t
                )
            ) in
  let emit x acc = ( x :: acc ) in
  aux [] emit ns list ;;

test_func1 [ "a"; "b"; "c"; "d" ] [2;1];;
