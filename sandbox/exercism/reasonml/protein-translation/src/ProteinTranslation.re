let proteins: string => list(string) =
  s => {
    let rec aux = s =>
      switch (s) {
      | ["A", "U", "G", ...tail] => ["Methionine", ...aux(tail)]
      | ["U", "U", "U", ...tail]
      | ["U", "U", "C", ...tail] => ["Phenylalanine", ...aux(tail)]
      | ["U", "U", "A", ...tail]
      | ["U", "U", "G", ...tail] => ["Leucine", ...aux(tail)]
      | ["U", "C", "U", ...tail]
      | ["U", "C", "C", ...tail]
      | ["U", "C", "A", ...tail]
      | ["U", "C", "G", ...tail] => ["Serine", ...aux(tail)]
      | ["U", "A", "U", ...tail]
      | ["U", "A", "C", ...tail] => ["Tyrosine", ...aux(tail)]
      | ["U", "G", "U", ...tail]
      | ["U", "G", "C", ...tail] => ["Cysteine", ...aux(tail)]
      | ["U", "G", "G", ...tail] => ["Tryptophan", ...aux(tail)]
      | _ => []
      };
    Js.String.split("", s) |> Array.to_list |> aux;
  };
