const dnas = ['A', 'C', 'G', 'T'] as const;
type DNA = typeof dnas[number];

const isDna = (c: string): c is DNA => dnas.map(x => c === x).reduce((r, x) => r || x, false)


const rnas = ['A', 'C', 'G', 'U'] as const;
type RNA = typeof rnas[number];

const dnaToRna = new Map<DNA, RNA>([
  ['G', 'C'],
  ['C', 'G'],
  ['T', 'A'],
  ['A', 'U'],
]);

class Transcriptor {
  toRna(dnaStr: string) {
    const rnaStr = dnaStr.split("")
      .filter(isDna)
      .map(x => dnaToRna.get(x))
      .reduce((rnaStr, x) => rnaStr + x, "");

    if(rnaStr.length !== dnaStr.length) {
      throw('Invalid input DNA.');
    }

    return rnaStr;
  }
}

export default Transcriptor
