import matplotlib as mpl
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.ticker as plticker
import pandas as pd
from scipy.special import binom
from math import ceil, floor, sqrt, log

colors = ['#5600b1', '#b17100', '#009fb1', '#00b137', '#b10028']
# colors = ['#b17100', '#009fb1', '#00b137', '#b10028']
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
  # plt.ylim(-0.05, 1.05)
    plt.legend(loc='upper left')

def plot_min(r, c, fig, ax):
    x = range(1, r*c)
    tuple_sum = lambda x: x[0] + x[1]
    y_grid = map(lambda k: tuple_sum(grid_min(k)) - 1, x)
    plt.plot(
            list(x), list(y_grid), '+', markeredgewidth=mew)

    ax.set_xlabel('Cluster size')
    ax.set_ylabel('Minimum number of processes per quorum')
  # ax.set_title('Minimum quorum cardinality for different cluster sizes')

    xloc = plticker.MultipleLocator(base=1.0)
  # ax.xaxis.set_major_locator(xloc)
  # yloc = plticker.MultipleLocator(base=1.0)
  # ax.yaxis.set_major_locator(yloc)
    plt.grid(True)
    plt.legend(loc='upper left')

def percentile(n):
    def percentile_(x):
        return np.percentile(x, n)
    percentile_.__name__ = 'percentile_%s' % n
    return percentile_

def quantiles(df):
    return np.percentile(df.values, [0.25, 0.5, 0.75])

def q75(df):
    return np.percentile(df.values, 0.75)

def plot_errorbars(writes, fig, ax):

    grouped = writes.groupby(['Experiment', 'Cluster'])
  # aggregated = grouped.aggregate(
  #         [percentile(25), percentile(50), percentile(75)])
    q25 = grouped.aggregate(percentile(25))['TTC']
    median = grouped.aggregate(percentile(50))['TTC']
    q75 = grouped.aggregate(percentile(75))['TTC']

  # experiments = ['majority', 'tree3', 'grid']
    experiments = ['grid']
    bar_width = 1.0 / 9
  # offset = -1.0 * bar_width

    for experiment, color in zip(experiments, colors):
        y = median.ix[experiment]
        x = y.index
        yerr=[
            y - q25.ix[experiment],
            q75.ix[experiment] - y]

        plt.errorbar(
                x, y,
              # bar_width,
                color=color,
                linewidth=2,
                elinewidth=1,
                marker='o',
              # alpha=0.7,
              # log=1,
                yerr=yerr)

    ax.set_xlabel('Cluster size')
    ax.set_ylabel('Time to consensus [ms]')
  # ax.set_title('Time to consensus the Grid Protocol with different uptimes')

    xloc = plticker.MultipleLocator(base=1.0)
    ax.xaxis.set_major_locator(xloc)
  # yloc = plticker.MultipleLocator(base=1.0)
  # ax.yaxis.set_major_locator(yloc)
    plt.grid(True)
    plt.legend(loc='upper left')

def plot_cdf(data, col, vals, title, fig, ax):

    for val, color in zip(vals, colors):
        ttc = data[data[col] == val]['TTC']
        n, bins, patches = plt.hist(
                ttc.values, 1000,
                normed=1, histtype='step',
                label=val, color=color, cumulative=True)

    ax.set_xlabel('Time to consensus [ms]')
    ax.set_ylabel('Cumulative density function')
  # ax.set_title('Cumulative density function for ' + title)

    yloc = plticker.MultipleLocator(base=0.25)
    ax.yaxis.set_major_locator(yloc)

    plt.grid(True)
    plt.ylim(0, 1.05)
    plt.xlim(0, 105)
    plt.legend(loc='lower right')

if __name__ == '__main__':
    from glob import glob
    percentile = lambda q: lambda a: np.percentile(a, q)

    input_files = glob('/home/leo/Temp/lowrate.ttc')

    data = pd.concat([pd.read_csv(f) for f in input_files])
    data['TTC'] = data['TTC'] / 1000 # convert to milliseconds
    writes = data[data['Operation'] == 'Write']

    fig, ax = plt.subplots()

    plot_errorbars(writes, fig, ax)
    ax2 = ax.twinx()
    plot_min(4, 5, fig, ax2)
    ax.set_title('TTC and quorum cardinality for the Grid Protocol')
  # plt.figure()
  # plot_cdf(
  #         writes[writes['Cluster'] == 11],
  #       # 'Experiment', ['plain', 'majority', 'tree2', 'tree3', 'grid'],
  #         'Experiment', ['majority', 'tree3', 'grid'],
  #         'cluster size 11', fig, ax)
  # plt.figure()
  # plot_cdf(writes, 16, fig, ax)
    plt.show()
