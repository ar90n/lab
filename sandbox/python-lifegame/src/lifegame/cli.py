from . import Grid, ALIVE, simulate, live_a_generation

def main():
    grid = Grid(5 , 9)
    grid.assign(0, 3, ALIVE)
    grid.assign(1, 4, ALIVE)
    grid.assign(2, 2, ALIVE)
    grid.assign(2, 3, ALIVE)
    grid.assign(2, 4, ALIVE)
    print(str(grid))
    print('-' * 30)

    for i in range(10):
        grid = live_a_generation(grid, simulate(5, 9))
        print(str(grid))
        print('-' * 30)


if __name__  == '__main__':
    main()