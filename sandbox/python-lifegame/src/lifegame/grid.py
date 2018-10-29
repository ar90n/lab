from .common import EMPTY

class Grid:
    def __init__(self, height, width):
        self.height = height
        self.width = width
        self.rows = [[EMPTY] * self.width for _ in range(height)]

    def __str__(self):
        return '\r\n'.join(map(''.join, self.rows))

    def query(self, y, x):
        return self.rows[y % self.height][x % self.width]

    def assign(self, y, x, state):
        self.rows[y % self.height][x % self.width] = state
