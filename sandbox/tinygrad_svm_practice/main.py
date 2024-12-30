#%%
from tinygrad import Tensor, nn
import numpy as np
from sklearn.datasets._samples_generator import make_blobs
import matplotlib.pyplot as plt
# %%
class SVM:
    def __init__(self, alpha=0.01):
        self.w = Tensor.randn(2, 1, requires_grad=True).to('python')
        self.b = Tensor.randn(1, requires_grad=True).to('python')    
        self.alpha = alpha

    def __call__(self, x):
        return (x @ self.w + self.b).flatten()
    
    def loss(self, y_pred, y_true):
        t1 = Tensor.relu(1 - y_pred * y_true).mean()
        t2 = self.w.T @ self.w
        total = t1 + self.alpha * t2
        return total.flatten()[0]

# %%
x, y = make_blobs(n_samples=1024, centers=2, cluster_std=1.2, random_state=1)
x = (x- x.mean()) / x.std()
y[np.where(y == 0)] = -1
x, y = Tensor(x).to('python'), Tensor(y).to('python')
# %%
model = SVM()
optim = nn.optim.SGD([model.w, model.b], lr=0.01, weight_decay=0.999)

# %%
epochs = 50
batch_size = 32
with Tensor.train():
    for i in range(epochs):
        for j in range(0, len(x), batch_size):
            x_batch = x[j:j+batch_size]
            y_batch = y[j:j+batch_size]

            y_pred = model(x_batch)
            optim.zero_grad()
            loss = model.loss(y_pred, y_batch).backward()
            optim.step()

            if j == 0:
                print(loss.item())
# %%
plt.scatter(np.asarray(x.tolist())[:,0], np.asarray(x.tolist())[:,1], c=y.tolist())
x1 = np.linspace(-3, 3, 100)
x2 = (-model.b.item() - model.w[0].item() * x1) / model.w[1].item()
plt.plot(x1, x2, color='red')