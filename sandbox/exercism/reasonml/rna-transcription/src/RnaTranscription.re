type dna =
  | A
  | C
  | G
  | T;

type rna =
  | A
  | C
  | G
  | U;

let toRna: list(dna) => list(rna) = src => {
    let aux: dna => rna = d => switch(d) {
    | A => U
    | C => G
    | G => C
    | T => A
    }
    List.map(aux, src)
}
