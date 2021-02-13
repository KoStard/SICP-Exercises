import sys
from matplotlib import pyplot as plt
import numpy as np

lines = []
for line in sys.stdin:
    l = line.strip()
    if " : " not in l:
        continue
    f, t = l.split(" : ")
    fx, fy = f[1:-1].split(" ")
    tx, ty = t[1:-1].split(" ")
    xs = []
    ys = []
    xs.append(float(fx))
    xs.append(float(tx))
    ys.append(float(fy))
    ys.append(float(ty))
    plt.plot(xs, ys, linewidth=0.5, color='black')
    # lines.append([[float(fx), float(fy)], [float(tx), float(ty)]])

plt.show()
