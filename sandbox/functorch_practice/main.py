# %%
import torch
import torch.nn as nn
import torch.nn.functional as F
from functools import partial
from functorch import make_functional, vmap, grad
import torchvision
import torchvision.transforms as transforms
import torchopt
from torchopt import FuncOptimizer
import numpy as np

torch.manual_seed(0);

class Net(nn.Module):
    def __init__(self):
        super(Net, self).__init__()
        self.conv1 = nn.Conv2d(1, 32, 3, 1)
        self.conv2 = nn.Conv2d(32, 64, 3, 1)
        self.dropout1 = nn.Dropout(0.25)
        self.dropout2 = nn.Dropout(0.5)
        self.fc1 = nn.Linear(9216, 128)
        self.fc2 = nn.Linear(128, 10)

    def forward(self, x):
        x = self.conv1(x)
        x = F.relu(x)
        x = self.conv2(x)
        x = F.relu(x)
        x = F.max_pool2d(x, 2)
        #x = self.dropout1(x)
        x = torch.flatten(x, 1)
        x = self.fc1(x)
        x = F.relu(x)
        #x = self.dropout2(x)
        x = self.fc2(x)
        output = F.log_softmax(x, dim=1)
        return output

def loss_fn(predictions, targets):
    return F.cross_entropy(predictions, targets)

# %%
device = "cuda:0" if torch.cuda.is_available() else "cpu"
batch_size = 4
# %%
model = Net().to(device=device)
fmodel, params = make_functional(model)
# %%
def compute_loss_stateless_model (params, sample, target):
    batch = sample.unsqueeze(0)
    targets = target.unsqueeze(0)

    predictions = fmodel(params, batch) 
    loss = loss_fn(predictions, targets)
    return loss
ft_compute_sample_grad = vmap(grad(compute_loss_stateless_model), in_dims=(None, 0, 0))

# %%
transform = transforms.Compose(
    [transforms.ToTensor(),
     transforms.Normalize((0.5, ), (0.5, ))])
trainset = torchvision.datasets.MNIST(root='./data', 
                                        train=True,
                                        download=True,
                                        transform=transform)
trainloader = torch.utils.data.DataLoader(trainset,
                                            batch_size=100,
                                            shuffle=True,
                                            num_workers=2)

testset = torchvision.datasets.MNIST(root='./data', 
                                        train=False, 
                                        download=True, 
                                        transform=transform)
testloader = torch.utils.data.DataLoader(testset, 
                                            batch_size=100,
                                            shuffle=False, 
                                            num_workers=2)

classes = tuple(np.linspace(0, 9, 10, dtype=np.uint8))

# %%
dataiter = iter(trainloader)
optimizer = torchopt.adam()
opt_state = optimizer.init(params)
import gc
for i, (images, labels) in enumerate(dataiter):
    torch.cuda.empty_cache()
    gc.collect()
    images = images.to(device)
    labels = labels.to(device)
    grads = ft_compute_sample_grad(params, images, labels)
    del images
    del labels
    images = None
    labels = None
    grads_mean = tuple(t.mean(axis=0) for t in grads)
    del grads
    grads = None
    updates, new_opt_state = optimizer.update(grads_mean, opt_state)  # get updates
    del grads_mean
    del opt_state
    grads_mean = None
    opt_state = None
    opt_state = new_opt_state
    new_params = torchopt.apply_updates(params, updates)         # update network parameters
    del params
    params = new_params
    del updates
    updates = None

# %%