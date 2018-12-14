# https://adventofcode.com/2018/day/14 in Python
i, j = 0, 1
a = [3, 7]
k = 640441

recipes = where = None
while not (recipes and where):
    t = a[i] + a[j]
    a += [1, t%10] if t > 9 else [t]
    i = (i + a[i] + 1) % len(a)
    j = (j + a[j] + 1) % len(a)

    if len(a) >= k + 9:
        recipes = a[k:k+10]

    last = ''.join(map(str, a[-7:]))
    if str(k) in last:
        where = last.index(str(k)) + len(a) - 7

print(recipes, where)
