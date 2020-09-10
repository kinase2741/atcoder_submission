#!/usr/bin/env python3

import sys
sys.setrecursionlimit(10**7)

INF = 10**7
DIRECTION = [[0,1],[1,0]]

def solve(h,w,board):
    dp = [[0]*w for _ in range(h)]
    def isSharp(y,x):
        return board[y][x] == '#'
    for y in range(h):
        if y == 0:
            dp[0][0] = isSharp(0,0)
        else:
            dp[y][0] = dp[y-1][0] + isSharp(y,0)
        for x in range(w-1):
            if y == 0:
                dp[y][x+1] = dp[y][x] + isSharp(y,x+1)
            else:
                dp[y][x+1] = min(dp[y][x], dp[y-1][x+1]) + isSharp(y,x+1)

    
    return dp[-1][-1]

def main():
    H,W = map(int,input().split())
    board = [input() for _ in range(H)]
    print('{}\n'.format(solve(H,W,board)))
    # write c


if __name__ == '__main__':
    main()
