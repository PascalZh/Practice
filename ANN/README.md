# Artificial Neural Network
This created for practicing something about ANN.

## Table of Contents

[**BP Neural Network with Racket**](#a-simple-bp-network)
* [Training Analysis](#bp-ntw-racket-analysis)
* [Summary](#bp-ntw-racket-summary)
* [TODO](#bp-ntw-racket-todo)

<a name="bp-ntw-racket"></a>
## A simple bp network algorithm by Racket

This network is used to recognize handwritten numbers.

A network is actually a network of weights.
A node is referred to a list of weight that connects to a neural node.
A layer is a list of list, contains all nodes of that layer.
A w is a weight, determined by two nodes.

<a name="bp-ntw-racket-analysis"></a>
### Training Analysis
#### Step 1: nr1.log nr2.log
*nr1.log* is the log file of *number_recognition_by_bp.rkt* of first try.
I had planned to run the train for 14 hours. But I found some abnormal data.
The total error (suppose loss function is <img src="http://www.sciweavers.org/tex2img.php?eq=%5Cfrac%7B1%7D%7B2%7D%20%28y%20-%20t%29%5E2&bc=White&fc=Black&im=jpg&fs=12&ff=arev&edit=0" align="center" border="0" alt="\frac{1}{2} (y - t)^2" width="76" height="43" /> ) is calculated by simply add up the error of every output neuron.
The mean total error as shown in the nr1.log file has decreased rapidly, but after 2 or 3 loops it decreases very slowly.
The abnormality is that after about ten thousands it increases slowly to my surprise.
So I stopped the training, and just loops for 200 times so I can see the result. The result is in the nr2.log file.

I think it is oscillating, maybe it is too few of the training samples, or incorrect of the learning rate.
Maybe I could use some improved algorithm of back propagation neural network (adaptive learning rate).

#### Step 2: all kinds of trial...
I have tried a lot combination of node number and learning rate, but the result almost converges to 0.4456...
And they all oscillate after some loops.

#### Step 3: apply inertia (a0.0.log ~ a1.0.log)
The massive numbers in the file refer to a w of two nodes, I just pick it with `caaar`.

I compared different inertia term (momentum) α, but it seems make no difference. I think is must be the problem of samples.

#### Final Step: train the network with huge size dataset (1.log, 2.log)
I try to use this simple bp neural network to recognize handwritten numbers. Every pixel is counted as a input neuron of the network. The number of output neuron is ten, since I encoded the tag by 10 bits (e.g. 10000 00000 represents 0; 01000 00000 represents 1; 00100 00000 represents 2).

I choose **SGD** (stochastic gradient decreasing) to update the weight values. So the network will converge very soon but perhaps converge to local optimum solution (this is true for 2.log).

<a name="bp-ntw-racket-summary"></a>
### Summary
Choose list to calculate the network is not wise. And my algorithm does really cost memory. Maybe I should write the program with C++.

<a name="bp-ntw-racket-todo"></a>
### TODO
- [x]  `apply-network` is ok.
- [x]  there are some bugs with `train`.
- [x]  attaining big samples.
