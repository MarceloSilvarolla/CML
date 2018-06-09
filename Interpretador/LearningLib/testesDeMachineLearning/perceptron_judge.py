import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

original_data = pd.read_csv("examples/perceptron_example.csv")
predicted_data = pd.read_csv("perceptron_output.csv")

print(original_data.head())
print(predicted_data.head())

x = original_data[["x_1", "x_2"]]
y_true = original_data["y"]
y_pred = predicted_data["y"]

x_pos = x[y_pred > 0.0]
x_neg = x[y_pred < 0.0]

plt.xlabel("x_1")
plt.ylabel("x_2")
plt.scatter(x_pos["x_1"], x_pos["x_2"], color="green")
plt.scatter(x_neg["x_1"], x_neg["x_2"], color="red")
#plt.scatter(x_pos["x_1"], x_pos["x_2"])
#plt.scatter(x_neg["x_1"], x_neg["x_2"])

plt.show()
