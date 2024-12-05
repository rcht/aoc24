import sys
from collections import defaultdict

f = open(sys.argv[1])
lines = [s.strip() for s in f.readlines()]
f.close()

edges = defaultdict(list)

midsm = 0

for s in lines:
    if s == "":
        pass

    elif "|" in s:
        a, b = [int(j) for j in s.split('|')]
        edges[a].append(b)

    else:
        nums = [int(i) for i in s.split(',')]
        bruh = True
        for i,n1 in enumerate(nums):
            for j,n2 in enumerate(nums):
                if i < j and n2 not in edges[n1]:
                    bruh = False
        if bruh:
            midsm += nums[len(nums)//2]

print(midsm)
