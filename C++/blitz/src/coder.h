#pragma once
#include <string>
#include <vector>
#include <algorithm>
#include <functional>
#include <map>
#include "utils.h"

namespace blitz
{

using namespace std;

class Coder
{
public:
    using data_type = uint32_t;
    virtual vector<data_type> encode(const string& str) = 0;
    virtual string decode(const vector<data_type>& data) = 0;
};

class HuffmanCoder : public Coder
{
public:
    HuffmanCoder(const string& text)
    {
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

    virtual vector<data_type> encode(const string& str)
    {
        vector<data_type> output;
        size_t padding = 0;
        constexpr size_t bit_width = sizeof(data_type) * 8;
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
        return output;
    }
    virtual string decode(const vector<data_type>& data)
    {
        string output;
        Tree* current = &m_root;
        for (int i = 0; i < data.size(); ++i) {
            constexpr size_t bit_width = sizeof(data_type) * 8;
            int n_bits = i == data.size() - 1 ? bit_width - m_padding : bit_width;
            for (int j = n_bits - 1; j >= 0; --j) {
                data_type bit = (data[i] & (1 << j)) >> j;
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

private:
    struct Tree {
          unsigned   freq   = 0;
          char       ch     = ' ';
          data_type  code   = 0;
          data_type  length = 0;
          Tree*      left   = nullptr;
          Tree*      right  = nullptr;
        friend bool operator>(const Tree& lhs, const Tree& rhs)
        { return lhs.freq > rhs.freq; }
        friend ostream& operator<<(ostream& out, const Tree& t)
        {
            return out << t.freq << "\t" << t.ch << " " << t.code << "\t{"
                << to_binary(t.code) << "} " << t.length << " "
                << long(t.left) << " " << long(t.right);
        }
    };
    struct Code {
        data_type code   = 0;
        data_type length = 0;
    };

    Tree m_root; // could be used to decode the data.
    map<char, Code> m_dict; // could be used to encode the string.
    size_t m_padding;

    void generate_code(Tree& t)
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

    bool is_leaf(const Tree& t) const { return !t.left && !t.right; }
};

} /* blitz */ 
