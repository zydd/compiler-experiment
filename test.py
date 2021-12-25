import sys
sys.setrecursionlimit(int(10e6) + 10)

def sum(acc, count, max):
    if count < max:
        return sum ((acc+ count), (count+ 1), max)
    else:
        return acc

print(sum(0,0, int(10e6) + 1))
