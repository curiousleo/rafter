import matplotlib as mpl
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.ticker as plticker
import pandas as pd

colors = {'plain': '#5600b1', 'majority': '#b17100', 'tree2': '#009fb1', 'tree3': '#00b137', 'grid' :'#b10028'}

data = pd.read_csv('/home/leo/Temp/lowrate.memcached')
fig, ax = plt.subplots()
sorted = data.sort(['Experiment', 'Cluster'])
grouped = sorted.groupby('Experiment')

for experiment, g in grouped:
    x = g['Cluster']
    y = g['Throughput']
    fit = np.poly1d(np.polyfit(x, y, 2))
    plt.plot(x, y, 'o', label=experiment, color=colors[experiment])
    plt.plot(x, fit(x), '--', color=colors[experiment])

ax.set_xlabel('Cluster size')
ax.set_ylabel('Operations per second (average)')
ax.set_title('Throughput for different voting schemes')

xloc = plticker.MultipleLocator(base=1.0)
ax.xaxis.set_major_locator(xloc)

plt.grid(True)
plt.legend(loc='upper right')

plt.show()
