# a simple bp network algorithm
A network is actually a network of weights.
A node is referred to a list of weight that connects to a neural node.
A layer is a list of list, contains all nodes of that layer.
A w is a weight, determined by two nodes.
All the things ared coded from 1.

# Training Analysis
## nr1.log nr2.log
nr1.log is the log file of number_recognition_by_bp.rkt of first try.
I had planned to run the train for 14 hours. But I found some abnormal data.
The total error (suppose loss function is $\frac{1}{2} (y - t)^2$) is calculated by simply add up the error of every output neuron.
The mean total error as shown in the nr1.log file has decreased rapidly, but after 2 or 3 loops it decreases very slowly.
The abnormality is that after about ten thousands it increases slowly to my surprise.
So I stopped the training, and just loops for 200 times so I can see the result. The result is in the nr2.log file.

I think it is oscillating, maybe it is too few of the training samples, or incorrect of the learning rate.
Maybe I could use some improved algorithm of back propagation neural network (adaptive learning rate).


# TODO
- [x]  `apply-network` is ok.
- [x]  some bugs with `train`
