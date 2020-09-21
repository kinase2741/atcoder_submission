#!/usr/bin/env python3
# import sys
# sys.recursionlimit(10**7)
from functools import reduce


def solve(n,a):
    if a <= 8 and b <= 8:
        return "Yay!"
    else:
        return ":("

def main():
    N = int(input())
    a = list(map(int,input().split()))
    print('{}'.format(solve(N,a)))




if __name__ == '__main__':
  main()
