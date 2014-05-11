import matplotlib as mpl
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.ticker as plticker
import pandas as pd
from scipy.special import binom
from math import ceil, floor, sqrt, log

grid_color = '#b17100'
mew = 1.5

def grid_pr(r, c, k):
    m = r + c - 1
    if k < m:
        return 0.0
    else:
        p = c * binom(r*c-m, k-m) / binom(r*c, k)
        return min(p, 1.0)

def grid_min(n):
    r = int(ceil(sqrt(n)))
    c = int(floor(sqrt(n)))
    if r*c < n:
        c = r
    return (r, c)

def tree_min(d, n):
    if d == 2:
        return ceil(log(n+1, 2) - 1)
    return ceil(log(d-1, d) + log(n, d) - 1)

def plot_pr(r, c, fig, ax):
    x = range(1, r*c)
    y_grid = map(lambda k: grid_pr(r, c, k), x)
    y_maj = map(lambda k: 1.0 if k > r*c/2 else 0.0, x)
    plt.plot(
            list(x), list(y_grid), '+', markeredgewidth=mew, label='grid')
    plt.plot(
            list(x), list(y_maj), 'x', markeredgewidth=mew, label='maj')

    ax.set_xlabel('Number of "Yes" replies')
    ax.set_ylabel('Probability of getting a quorum')
    ax.set_title('Probability of forming a quorum in a {} x {} grid'.format(r,c))

    xloc = plticker.MultipleLocator(base=1.0)
  # ax.xaxis.set_major_locator(xloc)
  # yloc = plticker.MultipleLocator(base=1.0)
  # ax.yaxis.set_major_locator(yloc)
    plt.grid(True)
    plt.ylim(-0.05, 1.05)
    plt.legend(loc='upper left')

def plot_min(r, c, fig, ax):
    x = range(1, r*c)
    tuple_sum = lambda x: x[0] + x[1]
    y_grid = map(lambda k: tuple_sum(grid_min(k)) - 1, x)
    y_maj = map(lambda k: floor((k+1.0)/2), x)
    y_tree2 = map(lambda k: tree_min(2, k), x)
    y_tree3 = map(lambda k: tree_min(3, k), x)
    plt.plot(
            list(x), list(y_maj), '.', markeredgewidth=mew, label='maj')
    plt.plot(
            list(x), list(y_grid), '+', markeredgewidth=mew, label='grid')
    plt.plot(
            list(x), list(y_tree2), '.', markeredgewidth=mew, label='tree2')
    plt.plot(
            list(x), list(y_tree3), 'x', markeredgewidth=mew, label='tree3')

    ax.set_xlabel('Cluster size')
    ax.set_ylabel('Minimum number of processes per quorum')
    ax.set_title('Minimum quorum cardinality for different cluster sizes')

    xloc = plticker.MultipleLocator(base=1.0)
  # ax.xaxis.set_major_locator(xloc)
  # yloc = plticker.MultipleLocator(base=1.0)
  # ax.yaxis.set_major_locator(yloc)
    plt.grid(True)
    plt.legend(loc='upper left')

if __name__ == '__main__':
    fig, ax = plt.subplots()
  # plot_pr(4, 5, fig, ax)
  # plt.figure()
    plot_min(5, 6, fig, ax)
    plt.show()
