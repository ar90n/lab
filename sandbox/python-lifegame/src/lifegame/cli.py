from contextlib import contextmanager
import time
import os
import tty
import termios

from . import Grid, ALIVE, simulate, live_a_generation


def write(s):
    if isinstance(s, str):
        s = bytes(s, "utf-8")
    os.write(1, s)


def goto(row, col):
    write("\x1b[%d;%dH" % (row + 1, col + 1))


def clear():
    write(b"\x1b[2J")


def cursor(enable):
    code = b"\x1b[?25h" if enable else b"\x1b[?25l"
    write(code)

def draw(grid):
    clear()
    goto(0, 0)
    print(str(grid))
    time.sleep(0.1)

def has_interrupt():
    buf = os.read(0, 1)
    return buf is not None and 0 < len(buf)

@contextmanager
def fullscreen():
    org_termios0 = termios.tcgetattr(0)
    org_termios1 = termios.tcgetattr(1)

    tty.setraw(0)
    new_termios1 = termios.tcgetattr(1)
    new_termios1[3] &= ~(termios.ECHO | termios.ICANON) # lflags
    new_termios1[6][termios.VMIN] = 0  # cc
    new_termios1[6][termios.VTIME] = 0 # cc
    termios.tcsetattr(1, termios.TCSADRAIN, new_termios1)

    goto(0, 0)
    clear()
    cursor(False)

    yield

    cursor(True)
    clear()
    goto(0, 0)

    termios.tcsetattr(1, termios.TCSADRAIN, org_termios1)
    termios.tcsetattr(0, termios.TCSANOW, org_termios0)


def main():
    with fullscreen():
        grid = Grid(20, 40)
        grid.assign(0, 3, ALIVE)
        grid.assign(1, 4, ALIVE)
        grid.assign(2, 2, ALIVE)
        grid.assign(2, 3, ALIVE)
        grid.assign(2, 4, ALIVE)
        draw(grid)

        while True:
            if has_interrupt():
                break
            grid = live_a_generation(grid, simulate(20, 40))
            draw(grid)


if __name__ == '__main__':
    main()
