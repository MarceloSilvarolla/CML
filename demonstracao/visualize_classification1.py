import pandas as pd
import numpy as np
import matplotlib.pylab as plt
import sys

predictions_file = sys.argv[1]
dataset_file = 'classification1.csv'

dataset = pd.read_csv(dataset_file)
predictions = pd.read_csv(predictions_file)

plt.xlabel('x1')
plt.ylabel('x2')

plt.scatter(dataset['x1'], dataset['x2'], color='black')
plt.show()

pos = dataset.iloc[np.where(predictions['y'] > 0)]
neg = dataset.iloc[np.where(predictions['y'] < 0)]

plt.scatter(pos['x1'], pos['x2'], color='green')
plt.scatter(neg['x1'], neg['x2'], color='blue')

plt.show()
