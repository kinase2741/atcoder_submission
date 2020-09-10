#!/usr/bin/env python3
from collections import Counter
MOD = 998244353


def solve(n,d):
    if d[0] != 0:
        return 0
    dc = Counter(d)
    if dc[0] > 1:
        return 0
    res = 1
    k = 1
    dmax = max(d)
    while k <= dmax:
        res *= dc[k-1]**dc[k]
        res %= MOD
        k += 1
    return res


def main():
    N = int(input())
    d = list(map(int,input().split()))
    # N,M = map(int,input().split())
    print(solve(N,d))


if __name__ == '__main__':
  main()
