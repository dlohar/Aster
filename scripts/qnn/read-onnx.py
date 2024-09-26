# Uncomment if not installed
# !pip install onnx


import onnx
from onnx import numpy_helper
import numpy as np
np.set_printoptions(threshold=np.inf)


# Load ONNX model
FILE_NAME = 'benchmark1.onnx'
onnx_model = onnx.load(f'{FILE_NAME}')

INTIALIZERS=onnx_model.graph.initializer

weights_collect, bias_collect = [], []

for initializer in INTIALIZERS:
    W= numpy_helper.to_array(initializer)
    if len(W.shape) == 1:
      bias_collect.append(W)
    else:
      weights_collect.append(W)

w_str = ''
for w in weights_collect:
  print(w.shape)
  w_str += str(w.tolist()) + "\n"

b_str = ''
for b in bias_collect:
  print(b.shape)
  b_str += str(b.tolist()) + "\n"

# Save values as text
with open(f'{FILE_NAME.split(".")[0]}_weights_vals.txt', 'w') as f:
  f.write(w_str)

with open(f'{FILE_NAME.split(".")[0]}_bias_vals.txt', 'w') as f:
  f.write(b_str)

# Print the math operations. Gives idea of activation functions as well!
for node in onnx_model.graph.node:
  print(node)

