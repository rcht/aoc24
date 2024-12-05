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
        hehe = nums[0:]
        for i in range(len(nums)):
            for j in range(len(nums)):
                if i < j and nums[i] in edges[nums[j]]:
                    nums[i], nums[j] = nums[j], nums[i]
        if hehe != nums:
            midsm += nums[len(nums)//2]
                        

print(midsm)
