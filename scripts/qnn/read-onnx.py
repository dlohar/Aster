# Uncomment if not installed
# !pip install onnx

import onnx
from onnx import numpy_helper
import numpy as np
np.set_printoptions(threshold=np.inf)


# Load ONNX model
FILE_NAME = 'benchmark2.onnx'
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

code_weights_biases = ""
for i, w in enumerate(weights_collect):
  code_weights_biases += f"val weights{i + 1} = Matrix(List(\n"
  list_w = w.tolist()
  for j, row in enumerate(list_w):
    str_row = ', '.join([str(r) for r in row])
    if j == len(list_w) - 1:
      code_weights_biases += f"\tList({str_row})))\n\n\n"
    else:
      code_weights_biases += f"\tList({str_row}),\n"

for i, b in enumerate(bias_collect):
  code_weights_biases += f"val bias{i + 1} = Vector(List({', '.join([str(bs) for bs in b.tolist()])}))\n\n"

# Write scala code
with open(f'{FILE_NAME.split(".")[0]}.scala', 'w') as f:
  f.write('import daisy.lang._\nimport Vector._\n\n')
  f.write(code_weights_biases)
  f.write('} ensuring(res => res +/- 1e-3)}\n')
