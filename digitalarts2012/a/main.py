#!/usr/bin/env python3
# import sys
# sys.recursionlimit(10**7)

def judge(word,ngLst):
    for ngWord in ngLst:
        if len(word) != len(ngWord):
            continue
        flag = True
        for i in range(len(word)):
            if word[i] != ngWord[i] and ngWord[i] != "*":
                flag = False
                break
        if flag:
            return True

    return False



def solve(s,t):
    return ["*"* len(word) if judge(word,t) else word for word in s]


def main():
    s = list(input().split())
    N = int(input())
    t = [input() for _ in range(N)]
    print(*solve(s,t),end="\n")
    return




if __name__ == '__main__':
  main()
