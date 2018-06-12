import pandas as pd
import numpy as np
import matplotlib.pylab as plt
import sys

predictions_file = sys.argv[1]
dataset_file = 'classification2.csv'

dataset = pd.read_csv(dataset_file)
predictions = pd.read_csv(predictions_file)

plt.xlabel('x1')
plt.ylabel('x2')

blue = dataset.iloc[np.where(dataset['color'] == 'blue')]
green = dataset.iloc[np.where(dataset['color'] == 'green')]

plt.scatter(blue['x1'], blue['x2'], color='blue')
plt.scatter(green['x1'], green['x2'], color='green')
plt.show()

blue = dataset.iloc[np.where(predictions['color'] == 'blue')]
green = dataset.iloc[np.where(predictions['color'] == 'green')]

plt.xlabel('x1')
plt.ylabel('x2')

plt.scatter(blue['x1'], blue['x2'], color='blue')
plt.scatter(green['x1'], green['x2'], color='green')
plt.show()
