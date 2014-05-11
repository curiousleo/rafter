import matplotlib as mpl
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.ticker as plticker
import pandas as pd

colors = ['#5600b1', '#b17100', '#009fb1', '#00b137', '#b10028']
# colors = ['#b17100', '#009fb1', '#00b137', '#b10028']

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
    experiments = ['maj-100', 'maj-70', 'maj-40']
    bar_width = 1.0 / 9
    offset = -0.5 * bar_width

    for experiment, color in zip(experiments, colors):
        y = median.ix[experiment]
        x = y.index
        yerr=[
            y - q25.ix[experiment],
            q75.ix[experiment] - y]

        plt.errorbar(
                x + offset,
                y,
              # bar_width,
                label=experiment,
                color=color,
                linewidth=2,
                elinewidth=1,
                marker='o',
              # alpha=0.7,
              # log=1,
                yerr=yerr)
        offset = offset + bar_width

    ax.set_xlabel('Cluster size')
    ax.set_ylabel('Time to consensus [ms]')
    ax.set_title('Time to consensus the Grid Protocol with different uptimes')

    xloc = plticker.MultipleLocator(base=1.0)
    ax.xaxis.set_major_locator(xloc)
  # yloc = plticker.MultipleLocator(base=1.0)
  # ax.yaxis.set_major_locator(yloc)
    plt.grid(True)
    plt.legend(loc='upper left')

def plot_cdf(data, col, vals, title, fig, ax):

    for val in vals:
        ttc = data[data[col] == val]['TTC']
        n, bins, patches = plt.hist(
                ttc.values, 1000,
                normed=1, histtype='step',
                label=val, cumulative=True)

    ax.set_xlabel('Time to consensus [ms]')
    ax.set_ylabel('Cumulative density function')
    ax.set_title('Cumulative density function for ' + title)

    yloc = plticker.MultipleLocator(base=0.25)
    ax.yaxis.set_major_locator(yloc)

    plt.grid(True)
    plt.ylim(0, 1.05)
  # plt.xlim(0, 105)
    plt.legend(loc='lower right')

if __name__ == '__main__':
    from glob import glob
    percentile = lambda q: lambda a: np.percentile(a, q)

    input_files = glob('/home/leo/Temp/local/all.ttc')

    data = pd.concat([pd.read_csv(f) for f in input_files])
    data['TTC'] = data['TTC'] / 1000 # convert to milliseconds
    writes = data[data['Operation'] == 'Write']

    fig, ax = plt.subplots()

  # plot_errorbars(writes, fig, ax)
  # plt.figure()
    plot_cdf(
            writes[writes['Cluster'] == 6],
          # 'Experiment', ['plain', 'majority', 'tree2', 'tree3', 'grid'],
            'Experiment', ['grid-100', 'grid-70', 'grid-40', 'maj-100', 'maj-70', 'maj-40'],
            'cluster size 6', fig, ax)
  # plt.figure()
  # plot_cdf(writes, 16, fig, ax)
    plt.show()
