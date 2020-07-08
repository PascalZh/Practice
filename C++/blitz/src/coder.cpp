#include "coder.h"
namespace blitz {

HuffmanCoder::HuffmanCoder(const string& text)
{
    if (text.empty()) throw;
    // generate the intial queue
    vector<Tree> vec;
    for (char ch : text) {
        auto it = find_if(vec.begin(), vec.end(),
                [ch](Tree& t) { return t.ch == ch; });
        if (it == vec.end()) {
            Tree t; t.freq = 1; t.ch = ch;
            vec.push_back(t);
        } else {
            it -> freq += 1;
        }
    }
    priority_queue<Tree, vector<Tree>, greater<Tree>>
        queue(greater<Tree>(), vec);

    // update the queue by repeatedly joining two trees.
    while(queue.size() > 1) {
        Tree* tree1 = new Tree(queue.top()); queue.pop();
        Tree* tree2 = new Tree(queue.top()); queue.pop();

        Tree new_tree;
        new_tree.freq = tree1 -> freq + tree2 -> freq;
        new_tree.left = tree1; new_tree.right = tree2;
        queue.push(new_tree);
    }

    m_root = queue.top();
    generate_code(m_root);
}
void HuffmanCoder::delete_tree(Tree* t)
{
    if (!t) return;
    Tree* right = t->right;
    Tree* left = t->left;
    delete t;
    if (right) delete_tree(right);
    if (left) delete_tree(left);
}

string HuffmanCoder::encode(const string& str)
{
    constexpr size_t bit_width = sizeof(data_type) * 8;
    vector<data_type> output;
    size_t padding = 0;
    for (char ch : str) {
        auto search = m_dict.find(ch);
        if (search == m_dict.end())
            throw;
        else {
            Code c = search -> second;
            if (padding == 0) {
                padding = bit_width - c.length;
                output.push_back(c.code << padding);
            } else if (padding < c.length) {
                data_type redundancy = c.length - padding;
                padding = padding + bit_width - c.length;
                output.back() += c.code >> redundancy;
                output.push_back(c.code << padding);
            } else {
                padding = padding - c.length;
                output.back() += c.code << padding;
            }
        }
    }
    m_padding = padding;
    string output_(reinterpret_cast<const char*>(output.data()),
            reinterpret_cast<const char*>(output.data() + output.size()));
    return output_;
}

string HuffmanCoder::decode(const string& data_)
{
    constexpr size_t bit_width = sizeof(data_type) * 8;
    string output;
    vector<data_type> data(reinterpret_cast<const data_type*>(data_.data()),
            reinterpret_cast<const data_type*>(data_.data() + data_.size()));
    Tree* current = &m_root;
    for (int i = 0; i < data.size(); ++i) {
        int n_bits = i == data.size() - 1 ? bit_width - m_padding : bit_width;
        for (int j = 0; j < n_bits; ++j) {
            data_type bit = (data[i] & (1 << (bit_width - j - 1))) >> (bit_width - j - 1);
            current = bit == 0 ? current -> left : current -> right;
            if (!current) throw;
            if (is_leaf(*current)) {
                output.push_back(current -> ch);
                current = &m_root;
            }
        }
    }
    return output;
}

void HuffmanCoder::generate_code(Tree& t)
{
    cout << t << endl;
    if (t.left) {
        t.left -> code   = t.code << 1;
        t.left -> length = t.length + 1;
        generate_code(*t.left);
    }
    if (t.right) {
        t.right -> code   = (t.code << 1) + 1;
        t.right -> length = t.length + 1;
        generate_code(*t.right);
    }
    if (is_leaf(t)) {
        m_dict.insert({t.ch, Code{t.code, t.length}});
    }
}

} /* namespace blitz */ 
