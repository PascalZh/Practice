# a simple bp network algorithm
A network is actually a network of weights.
A node is referred to a list of weight that connects to a neural node.
A layer is a list of list, contains all nodes of that layer.
A w is a weight, determined by two nodes.

All the things ared coded from 1.

# Training Analysis
## Step 1: nr1.log nr2.log
nr1.log is the log file of number_recognition_by_bp.rkt of first try.
I had planned to run the train for 14 hours. But I found some abnormal data.
The total error (suppose loss function is $\frac{1}{2} (y - t)^2$) is calculated by simply add up the error of every output neuron.
The mean total error as shown in the nr1.log file has decreased rapidly, but after 2 or 3 loops it decreases very slowly.
The abnormality is that after about ten thousands it increases slowly to my surprise.
So I stopped the training, and just loops for 200 times so I can see the result. The result is in the nr2.log file.

I think it is oscillating, maybe it is too few of the training samples, or incorrect of the learning rate.
Maybe I could use some improved algorithm of back propagation neural network (adaptive learning rate).

## Step 2: all kinds of trial...
I have tried a lot combination of node number and learning rate, but the result almost converges to 0.4456...
And they all oscillate after some loops.

## Step 3: apply inertia (a0.0.log ~ a1.0.log)
The massive numbers in the file refer to a w of two nodes, I just pick it with caaar.

I compared different inertia term (momentum) α, but it seems make no difference. I think is must be the problem of samples.

# TODO
- [x]  `apply-network` is ok.
- [x]  there are some bugs with `train`.
- [x]  attaining big samples.
