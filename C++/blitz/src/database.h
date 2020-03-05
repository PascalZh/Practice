// terminology explanation
// lexicon指词库
// The lexicon is parsed into a data structure, such as AVL tree, splay tree.
// To define a abstract tree, we call a node as a Block, Block class must implement the following functions:
// Block<T>::Scope Block<T>::has_key(T);
// enum class Block<T>::Scope = { LeftSide, In, RightSide };
// Block can be a block of data, or a pointer pointing to a block of data.


template <class T>
class Block;

template <class T>
template <class BlockT = Block<T> >
class SplayTree {
    private:
        struct Node {
            BlockT block;
            Node * left = nullptr;
            Node * right = nullptr;
        };
    private:
        Node * root;
    public:
        SplayTree() {
            root = new Node();
        }
        ~SplayTree() {
            delete root;
        }
    private:
        Node * splay_search(T i, Node * t) {
            Node N, *l, *r, *y;
            if (t == nullptr) return t;
            l = r = &N;
            using BS = BlockT::Scope;
            auto lt = [](T x, BlockT b) {return b.has_key(i) == BS::LeftSide};
            auto gt = [](T x, BlockT b) {return b.has_key(i) == BS::RightSide};
            auto eq = [](T x, BlockT b) {return b.has_key(i) == BS::In};
            while (true) {
                if (lt(i, t->block)) {
                    if (t->left != nullptr && lt(i, t->left->block)) {
                        y = t->left;
                        t->left = y->right;
                        y->right = t;
                        t = y;
                    }
                    if (t->left == nullptr) break;
                    r->left = t;
                    r = t;
                    t = t->left;
                } else if (gt(i, t->block)) {
                    if (t->right != nullptr && gt(i, t->right->block)) {
                        y = t->right;
                        t->right = y->left;
                        y->left = t;
                        t = y;
                    }
                    if (t->right == nullptr) break;
                    r->right = t;
                    r = t;
                    t = t->right;
                } else {
                    break;
                }
            }
            l->right = t->left;
            r->left = t->right;
            t->left = N.right;
            t->right = N.left;
            return t;
        }
};
