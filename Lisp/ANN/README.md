# a simple bp network algorithm
A network is actually a network of weights.
A node is refered to a list of weight that connects to a neural node.
A layer is a list of list, contains all nodes of that layer.
A w is a weight, determined by two nodes.
All the things ared coded from 1.

~~Bias is set to 1 by default, modify it in `apply-network` function.

If you create a network of n nodes of some layer, you actually get n+1 nodes.
Bias will be added automatically.
Be aware of it when access layer, node or w through indexing.~~

# TODO
- [x]   `apply-network` is ok.
- []    some bugs with `train`
