module BinaryTree =
    type 'a bintree =
        | Empty
        | Node of 'a * 'a bintree * 'a bintree

    let init =
        Empty
