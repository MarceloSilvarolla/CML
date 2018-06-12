import pandas as pd
import numpy as np
import matplotlib.pylab as plt

dataset_file = 'classification1.csv'

dataset = pd.read_csv(dataset_file)

plt.scatter(dataset['x1'], dataset['x2'], color='black')
plt.show()

pos = dataset.iloc[np.where(dataset['y'] > 0)]
neg = dataset.iloc[np.where(dataset['y'] < 0)]

plt.scatter(pos['x1'], pos['x2'], color='black')
plt.scatter(neg['x1'], neg['x2'], color='black')

plt.xlabel('x1')
plt.ylabel('x2')

plt.show()
