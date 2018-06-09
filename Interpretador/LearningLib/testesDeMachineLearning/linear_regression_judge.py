import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

original_data = pd.read_csv("examples/linear_regression_example2.csv")

m2 = original_data["m2"]
correct_price = original_data["price"]
predicted_price = pd.read_csv("linear_output.csv")

print(correct_price.head())
print(predicted_price.head())

plt.xlabel("m2")
plt.ylabel("price")

plt.scatter(m2, correct_price)
plt.plot(m2, predicted_price, color="green")

plt.show()
