class Pangram {
  private static isAlpha = /[a-zA-Z]/;

  private s: string;

  constructor(s: string) {
    this.s = s;
  }

  public isPangram(): boolean {
    const alphabets = this.s.split("")
      .filter(x => Pangram.isAlpha.test(x))
      .map(x => x.toUpperCase());
    return (new Set(alphabets)).size === 26;
  }
}

export default Pangram;
