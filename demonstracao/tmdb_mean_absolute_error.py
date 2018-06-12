from __future__ import print_function
import numpy as np
import pandas as pd
import sys

df1 = pd.read_csv(sys.argv[1])
df2 = pd.read_csv(sys.argv[2])

print("Mean Absolute Error: ", np.mean(np.abs((df1 - df2)))['vote_average'])
