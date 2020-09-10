#!/usr/bin/env python3

def f(n,memo={}):
    if n == 0 :
        memo[0] = 2
        return memo[0]
    elif n == 1:
        memo[1] = 1
        return memo[1]
    elif n in memo.keys():
        return memo[n]
    else:
        memo[n] = f(n-1) + f(n-2)
        return memo[n]

def main():
    N = int(input())
    print(f(N))

if __name__ == '__main__':
    main()