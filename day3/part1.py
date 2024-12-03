print(sum(map(lambda a: a[0] * a[1], map(lambda t: [int(i) for i in t], __import__('re').findall(r'mul\((0|[1-9]\d*),(0|[1-9]\d*)\)', open('input').read())))))
