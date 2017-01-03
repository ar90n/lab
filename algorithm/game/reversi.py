#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import sys
import enum
import itertools
import copy

class player( object ):
    def __init__( self, s ):
        self.stone = s

class human_player( player ):
    def __init__( self, s ):
        super().__init__( s )

    def play( self, board ):
        while True:
            try:
                x,y = [int(x) for x in input( '( x,y ) =>' ).split(',')]
            except:
                continue

            if not board.is_puttable( x, y, self.stone ):
                continue

            return (x,y)

class simple_ai_player( player ):
    def __init__( self, s ):
        super().__init__(s)

        self.eval_values = [ [ 100, -50, 35, 30, 30, 35, -50, 100 ],
                             [ -50, -70, 10, 15, 15, 10, -70, -50 ],
                             [ 35, 10, 20, 25, 25, 20, 10, 35 ],
                             [ 30, 15, 25, 50, 50, 25, 15, 30 ],
                             [ 30, 15, 25, 50, 50, 25, 15, 30 ],
                             [ 35, 10, 20, 25, 25, 20, 10, 35 ],
                             [ -50, -70, 10, 15, 15, 10, -70, -50 ],
                             [ 100, -50, 35, 30, 30, 35, -50, 100 ] ]

    def play( self, board ):
        max_pos = None
        max_score = -sys.float_info.max
        for p in board.find_puttable_hands( self.stone ):
            score = self.eval_values[ p[1] - 1 ][ p[0] - 1]
            if max_score < score:
                max_pos = p
                max_score = score

        return max_pos

class minmax_ai_player( simple_ai_player ):
    def __init__( self, s, max_depth ):
        super().__init__( s )
        self.max_depth = max_depth

    def play( self, board ):
        p, s = self.eval_player( board, self.max_depth, 0 )
        return p

    def eval_player( self, board, depth, score ):
        if depth == 0:
            return ( None, score )

        puttable_hands = board.find_puttable_hands( self.stone )
        if len( puttable_hands ) == 0:
            score = self.eval_enemy( board, depth - 1, score )[1]
            return ( None, score )

        max_score = -sys.float_info.max
        max_pos = None
        for p in puttable_hands:
            b = copy.deepcopy( board )
            b.put( p[0], p[1], self.stone )
            s = self.eval_enemy( b, depth - 1, score + self.eval_values[ p[1] - 1 ][ p[0] - 1 ] )[1]

            if max_score < s:
                max_score = s
                max_pos = p

        return ( max_pos, max_score )

    def eval_enemy( self, board, depth, score ):
        if depth == 0:
            return ( None, score )

        puttable_hands = board.find_puttable_hands( stone( self.stone.back() ) )
        if len( puttable_hands ) == 0:
            s = self.eval_player( board, depth - 1, score )[1]
            return ( None, s )

        min_score = sys.float_info.max
        min_pos = None
        for p in puttable_hands:
            b = copy.deepcopy( board )
            b.put( p[0], p[1], self.stone )
            s = self.eval_player( b, depth - 1, score - self.eval_values[ p[1] - 1 ][ p[0] - 1 ] )[1]

            if s < min_score:
                min_score = s
                min_pos = p

        return ( min_pos, min_score )

class negamax_ai_player( simple_ai_player ):
    def __init__( self, s, max_depth ):
        super().__init__( s )
        self.max_depth = max_depth

    def play( self, board ):
        p,s = self.eval( board, self.max_depth, stone( self.stone.state ), 0 )
        return p

    def eval( self, board, depth, s, score ):
        if depth == 0:
            next_score = score
            if s.state != self.stone.state:
                next_score = -next_score
            return ( None, next_score )

        puttable_hands = board.find_puttable_hands( s )
        if len( puttable_hands ) == 0:
            next_score = -self.eval( board, depth - 1, stone( s.back() ), score )[1]
            return ( None, next_score )

        max_score = -sys.float_info.max
        max_pos = None
        for p in puttable_hands:
            b = copy.deepcopy( board )
            b.put ( p[0], p[1], s )

            tmp_score = score
            if s.state == self.stone.state:
                tmp_score += self.eval_values[ p[1] - 1 ][ p[0] - 1]
            else:
                tmp_score -= self.eval_values[ p[1] - 1 ][ p[0] - 1]
            next_score = -self.eval( b, depth - 1, stone( s.back() ), tmp_score )[1]
            if max_score < next_score:
                max_score = next_score
                max_pos = p

        return ( max_pos, max_score )

class stone( object ):
    STATE = enum.Enum( "STONE", "EMPTY WHITE BLACK WALL")
    state = STATE.EMPTY

    def __init__(self, s ):
        self.state = s
        pass

    def back(self):
        back_states = { self.STATE.EMPTY : self.STATE.EMPTY,
                        self.STATE.WHITE : self.STATE.BLACK,
                        self.STATE.BLACK : self.STATE.WHITE,
                        self.STATE.WALL  : self.STATE.WALL }
        return back_states[ self.state ]

    def flip(self):
        self.state = self.back()
        pass

    def __eq__(self, other):
        return self.state == other.state

    def __str__(self):
        str_states = { self.STATE.EMPTY : '.',
                       self.STATE.WHITE : 'W',
                       self.STATE.BLACK : 'B',
                       self.STATE.WALL  : 'X' }
        return str_states[ self.state ]

class position( object ):
    def __init__(self, x, y ):
        self.x = x
        self.y = y
        pass

    def __eq__(self, other):
        if not isinstance( other, position ):
            return False
        return self.x == other.x and self.y == other.y

    def __str__(self):
        return str(self.x) + " " + str( self.y )

class board( object ):
    WIDTH = 8
    HEIGHT = 8
    MAP_WIDTH = WIDTH + 2
    MAP_HEIGHT = HEIGHT + 2

    DX = [-1,0,1,-1,1,-1,0,1]
    DY = [-1,-1,-1,0,0,1,1,1]

    def __init__(self):
        self.__board = [[ stone(stone.STATE.EMPTY ) for x in range(self.MAP_WIDTH) ] for y in range(self.MAP_HEIGHT)]
        for x in range(self.MAP_WIDTH):
            self.__board[0][x] = stone( stone.STATE.WALL )
            self.__board[self.MAP_HEIGHT-1][x] = stone( stone.STATE.WALL )
        for y in range(self.MAP_HEIGHT):
            self.__board[y][0] = stone( stone.STATE.WALL )
            self.__board[y][self.MAP_WIDTH-1] = stone( stone.STATE.WALL )

        self.__board[4][4] = stone( stone.STATE.WHITE )
        self.__board[5][5] = stone( stone.STATE.WHITE )
        self.__board[4][5] = stone( stone.STATE.BLACK )
        self.__board[5][4] = stone( stone.STATE.BLACK )

        self.__board[3][4] = stone( stone.STATE.WHITE )

        self.__rest_cell = self.WIDTH * self.HEIGHT - 4
        pass

    def is_puttable(self, x, y, s ):
        if x < 1 or self.WIDTH < x or y < 1 or self.MAP_HEIGHT < x:
            return False

        if self.__board[y][x].state != stone.STATE.EMPTY:
            return False

        for dx,dy in zip( self.DX, self.DY ):
            if 0 < self.count_flippable( x, y, s, dx, dy ):
                return True

        return False

    def is_puttable_somewhere( self, s ):
        for x,y in itertools.product( range(1,self.WIDTH + 1), range(1,self.HEIGHT + 1 ) ):
            if self.is_puttable( x, y, s ):
                return True
        return False

    def find_puttable_hands( self, s ):
        puttable_hands = []
        for x,y in itertools.product( range(1,self.WIDTH + 1), range(1,self.HEIGHT + 1 ) ):
            if self.is_puttable( x, y, s ):
                puttable_hands.append( ( x, y ) )

        return puttable_hands

    def count_flippable(self, x, y, s, dx, dy):
        c = 0
        xx = x + dx
        yy = y + dy

        while self.__board[yy][xx].state == s.back():
            c += 1
            xx += dx
            yy += dy

        if self.__board[yy][xx] == s:
            return c
        return 0

    def put(self, x, y, s ):
        if not self.is_puttable( x, y, s ):
            return

        self.__board[y][x] = copy.deepcopy( s )
        for dx,dy in zip( self.DX, self.DY ):
            c = self.count_flippable( x, y, s, dx, dy )
            for ddx, ddy in [ ( dx * i, dy * i )  for i in range( 1, c + 1 )]:
                self.__board[ y + ddy ][ x + ddx ].flip()

        self.__rest_cell -= 1
        pass

    def is_end(self):
        return self.__rest_cell == 0

    def show_result(self):
        black_num = 0
        white_num = 0
        for x,y in itertools.product( range(1,self.WIDTH + 1), range(1,self.HEIGHT + 1 ) ):
            if self.__board[ y ][ x ].state == stone.STATE.BLACK:
                black_num += 1
            elif self.__board[ y ][ x ].state == stone.STATE.WHITE:
                white_num += 1

        res = "B:" + str( black_num ) + " W:" + str( white_num )
        print( res )

    def __getitem__(self, item):
        return self.__board[item]

    def __str__(self):
        s = ""
        for line in self.__board:
            for cell in line:
                s += str(cell)
            s += "\n"
        return s

def main_loop():
    b = board()

    has_passed = False
    #black_player = human_player( stone( stone.STATE.BLACK ) )
    #black_player = simple_ai_player( stone( stone.STATE.BLACK ) )
    black_player = negamax_ai_player( stone( stone.STATE.BLACK ), 3 )
    #black_player = minmax_ai_player( stone( stone.STATE.BLACK ), 3 )
    #white_player = human_player( stone( stone.STATE.WHITE ) )
    white_player = simple_ai_player( stone( stone.STATE.WHITE ) )
    flip_player = { black_player : white_player, white_player : black_player }

    current_player = black_player
    while not b.is_end():
        print( b )

        if not b.is_puttable_somewhere( current_player.stone ):
            if has_passed:
                break

            has_passed = True
            current_player = flip_player[ current_player ]

        has_passed = False
        p = current_player.play( b )
        if p is None:
            break
        b.put( p[0], p[1], current_player.stone )

        current_player = flip_player[ current_player ]

    b.show_result()

def main( ):
    main_loop()
    return

if __name__ == '__main__':
    main( )
