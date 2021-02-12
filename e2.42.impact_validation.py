import math
al = lambda c, n: n ** (c-1) * math.factorial(n) * n / math.factorial(c)
bl = lambda c, n: math.factorial(n) * n / math.factorial(c)
calc = lambda n: sum(al(c, n) for c in range(1, n+1)) / sum(bl(c, n) for c in range(1, n+1))
print(calc(8))
