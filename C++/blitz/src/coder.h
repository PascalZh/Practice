#pragma once
#include <string>
#include <vector>
#include <algorithm>
#include <functional>
#include <map>
#include <queue>
#include "utils.h"

namespace blitz {

using namespace std;

class Coder
{
public:
    virtual string encode(const string& str) const = 0;
    virtual string decode(const string& data) const = 0;
};

class IdentityCoder : public Coder
{
public:
    IdentityCoder(const string& text) {};
    string encode(const string& str) const { return str; }
    string decode(const string& str) const { return str; }
};

class HuffmanCoder : public Coder
{
public:
    using data_type = uint32_t;
    HuffmanCoder(const string& text);
    ~HuffmanCoder()
    {
        if (m_root.left) delete_tree(m_root.left);
        if (m_root.right) delete_tree(m_root.right);
    }

    string encode(const string& str) const;
    string decode(const string& data_) const;

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
    mutable size_t m_padding;

    void generate_code(Tree& t);

    bool is_leaf(const Tree& t) const { return !t.left && !t.right; }
    void delete_tree(Tree* t);
};

} /* blitz */ 
