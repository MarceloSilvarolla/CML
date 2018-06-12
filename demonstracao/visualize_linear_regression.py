import pandas as pd
import numpy as np
import matplotlib.pylab as plt
import sys

predictions_file = sys.argv[1]
dataset_file = 'linear_regression.csv'

dataset = pd.read_csv(dataset_file)
predictions = pd.read_csv(predictions_file)

plt.xlabel('m2')
plt.ylabel('price')

plt.scatter(dataset['m2'], dataset['price'], color='green')
plt.show()

plt.xlabel('m2')
plt.ylabel('price')

plt.scatter(dataset['m2'], dataset['price'], color='green')
plt.plot(dataset['m2'], predictions['price'], color="blue", linewidth=2.0)
plt.show()
