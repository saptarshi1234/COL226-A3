import sys
sys.setrecursionlimit(100000)
from math import sqrt

def mod(x,y):
    return x if x < y else mod(x-y, y)

def isPrime(n):
    for i in range(2, int(sqrt(n))+1):
        if mod(n,i) == 0:
            return False
    return True

def kth(k):
    count = 0
    n = 1
    while count != k:
        n += 1
        if isPrime(n):
            count += 1
    return n

k = int(sys.argv[1])
print(kth(k))
