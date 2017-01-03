import os, sys, re, math

class Paintball:
    # String[] getLeaderboard(String[] players, String[] messages)
    def getLeaderboard(self, players, messages):
        dic_players = {}
        for player in players:
            name,team = player.split(' ' )
            dic_players[ name ] = { 'team' : team, 'score' : 0 }

        for message in messages:
            src,_,dst = message.split( ' ' )
            if dic_players[ src ][ 'team' ] != dic_players[ dst ][ 'team' ]:
                dic_players[ src ]['score'] += 1
                dic_players[ dst ]['score'] -= 1
            else:
                dic_players[ src ]['score'] -= 1

        team_member = {}
        team_score = {}
        for key,val in zip( dic_players.keys(), dic_players.values() ):
            player_name = key
            player_score = val['score']
            player_team = val['team']

            if not team_member.has_key( player_team ):
                team_member[ player_team ] = {}
                team_score[ player_team ] = 0
            if not team_member[ player_team ].has_key( player_score ):
                team_member[ player_team ][ player_score ] = []
            team_member[ player_team ][ player_score ].append( player_name )
            team_score[ player_team ] += player_score

        tmp_score = {}
        for name,score in team_score.items():
            if not tmp_score.has_key( score ):
                tmp_score[ score ] = []
            tmp_score[ score ].append( name )

        result = []
        for score,teams in sorted( tmp_score.items(), reverse=True ):
            for team in sorted( teams ):
                result.append( "%s %s" % ( team, score ) )
                for member_score,members in sorted( team_member[ team ].items(), reverse = True ):
                    for member in sorted( members ):
                        result.append( "  %s %s" % ( member, member_score ) )

        return result
        pass



# BEGIN CUT HERE

def eq(n, have, need):
    if isinstance(have, list) and isinstance(need, list):
        if len(have) != len(need):
            sys.stdout.write("Test Case #%d...FAILED: returned %d elements; expected %d elements." % (n, len(have), len(need)))
            printerror(have, need)
        else:
            for i in range(0, len(have)):
                if not eqval(have[i], need[i]):
                    sys.stdout.write("Test Case #%d...FAILED: Expected and returned array differ in position %d.\n" % (n, i))
                    printerror(have, need)
                    return
            sys.stdout.write("Test Case #%d...PASSED\n" % n)
    else:
        if eqval(have, need):
            sys.stdout.write("Test Case #%d...PASSED\n" % n)
        else:
            sys.stdout.write("Test Case #%d...FAILED\n" % n)
            printerror(have, need)

def printerror(have, need):
    sys.stdout.write("\tExpected: " + printval(need) + "\n")
    sys.stdout.write("\tRecieved: " + printval(have) + "\n")

def printval(a):
    if isinstance(a, str):
        return '"' + a + '"'
    elif isinstance(a, long):
        return str(a) + 'L'
    else:
        return str(a)

def eqval(a, b):
    if isinstance(a, float) or isinstance(b, float):
        return (abs(a - b) < 1e-9 or (abs(a) >= 1 and abs((a - b) / a) < 1e-9))
    else:
        return (a != None and b != None and a == b)

if __name__ == "__main__":
    eq(0, Paintball().getLeaderboard(["A RED", "B BLUE"], ["A SPLATTERED B"]), ["RED 1", "  A 1", "BLUE -1", "  B -1"])
    eq(1, Paintball().getLeaderboard(["LISA RED", "BART RED", "HOMER BLUE", "MARGE BLUE", "MAGGIE GREEN"], ["MAGGIE SPLATTERED HOMER", "MAGGIE SPLATTERED MARGE"]), ["GREEN 2", "  MAGGIE 2", "RED 0", "  BART 0", "  LISA 0", "BLUE -2", "  HOMER -1", "  MARGE -1"])
    eq(2, Paintball().getLeaderboard(["TODD STRIKEFORCE", "BART OMEGA", "DATA STRIKEFORCE", "MILHOUSE OMEGA", "NELSON DISCOVERYCHANNEL", "MARTIN DISCOVERYCHANNEL"], ["BART SPLATTERED MARTIN", "TODD SPLATTERED MARTIN"]), ["OMEGA 1", "  BART 1", "  MILHOUSE 0", "STRIKEFORCE 1", "  TODD 1", "  DATA 0", "DISCOVERYCHANNEL -2", "  NELSON 0", "  MARTIN -2"])
    eq(3, Paintball().getLeaderboard(["DR COHO", "ST COHO", "PE COHO"], ["DR SPLATTERED ST", "ST SPLATTERED PE"]), ["COHO -2", "  PE 0", "  DR -1", "  ST -1"])
    eq(4, Paintball().getLeaderboard(["A B", "AA AA", "AAA AAA"], ["A SPLATTERED AAA", "A SPLATTERED AAA", "A SPLATTERED AAA", "AA SPLATTERED AAA", "AA SPLATTERED AAA"]), ["B 3", "  A 3", "AA 2", "  AA 2", "AAA -5", "  AAA -5"])

# END CUT HERE
