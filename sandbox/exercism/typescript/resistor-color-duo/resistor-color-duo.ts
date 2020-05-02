const codes = {
  'black': 0,
  'brown': 1,
  'red': 2,
  'orange': 3,
  'yellow': 4,
  'green': 5,
  'blue': 6,
  'violet': 7,
  'grey': 8,
  'white': 9
} as const;
type Color = keyof typeof codes;

export class ResistorColor {
  private colors: Color[];

  constructor(colors: string[]) {
    if (colors.length < 2) {
      throw "At least two colors need to be present"
    }
    this.colors = colors as Color[];
  }

  value = (): number => this.colors
    .map(x => codes[x])
    .slice(0, 2)
    .reduce((a: number, b) => 10 * a + b, 0)
}
