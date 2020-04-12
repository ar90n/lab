type Planet = 'Earth' | 'Mercury' | 'Venus' | 'Mars' | 'Jupiter' | 'Saturn' | 'Uranus' | 'Neptune';

class SpaceAge {
  private static oneEarthYear: number = 365.25 * 24 * 3600;

  private _seconds: number;

  get seconds(): number {
    return this._seconds;
  }

  constructor(seconds: number) {
    this._seconds = seconds;
  }

  private getYearRatio(planet: Planet): number {
    switch (planet) {
      case 'Earth':
        return 1.0;
      case 'Mercury':
        return 0.2408467;
      case 'Venus':
        return 0.61519726;
      case 'Mars':
        return 1.8808158;
      case 'Jupiter':
        return 11.862615;
      case 'Saturn':
        return 29.447498;
      case 'Uranus':
        return 84.016846;
      case 'Neptune':
        return 164.79132;
      default:
        const _exhaustiveCheck: never = planet;
    }

    return 0.0;
  }

  private calcSpageAge(planet: Planet): number {
    const spaceAge = this.seconds / (this.getYearRatio(planet) * SpaceAge.oneEarthYear);
    const roundedSpaceAge = Math.round(100 * spaceAge) / 100;
    return roundedSpaceAge;
  }

  public onEarth(): number {
    return this.calcSpageAge('Earth');
  }

  public onMercury(): number {
    return this.calcSpageAge('Mercury');
  }

  public onVenus(): number {
    return this.calcSpageAge('Venus');
  }

  public onMars(): number {
    return this.calcSpageAge('Mars');
  }

  public onJupiter(): number {
    return this.calcSpageAge('Jupiter');
  }

  public onSaturn(): number {
    return this.calcSpageAge('Saturn');
  }

  public onUranus(): number {
    return this.calcSpageAge('Uranus');
  }

  public onNeptune(): number {
    return this.calcSpageAge('Neptune');
  }
}

export default SpaceAge;
