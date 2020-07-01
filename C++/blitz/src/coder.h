#pragma once
#include <string>
#include <vector>
#include <algorithm>
#include <functional>
#include "utils.h"

namespace blitz
{

using namespace std;

class Coder
{
public:
    virtual string encode(const string& str) = 0;
    virtual string decode(const string& str) = 0;
};

class HuffmanCoder : public Coder
{
public:
    HuffmanCoder(string text)
    {
        // generate the intial queue
        vector<Tree> vec;
        for (char ch : text) {
            auto it = find_if(vec.begin(), vec.end(),
                    [ch](Tree& t) { return t.ch == ch; });
            if (it == vec.end()) {
                Tree t;
                t.freq = 1;
                t.ch = ch;
                vec.push_back(t);
            } else {
                it -> freq += 1;
            }
        }

        priority_queue<Tree, vector<Tree>, greater<Tree>> _queue(greater<Tree>(), vec);

        // update the queue by repeatedly joining two trees.
        while(_queue.size() > 1) {
            Tree* tree1 = new Tree(_queue.top());
            _queue.pop();

            Tree* tree2 = new Tree(_queue.top());
            _queue.pop();

            Tree new_tree;
            new_tree.freq = tree1->freq + tree2->freq;
            new_tree.left = tree1;
            new_tree.right = tree2;
            _queue.push(new_tree);
        }

        function<void(Tree&)> generate_code = [&generate_code](Tree& t)
        {
            cout << t << endl;
            if (t.left) {
                t.left->code = t.code + "0";
                generate_code(*t.left);
            }
            if (t.right) {
                t.right->code = t.code + "1";
                generate_code(*t.right);
            }
        };
        auto top = _queue.top();
        top.code = "";
        generate_code(top);

    }

    virtual string encode(const string& str) { return string("fjeiw"); }
    virtual string decode(const string& str) { return string("fjeiw"); }

private:
    struct Tree {
        int freq = 0;
        char ch = ' ';
        string code;
        Tree* left = nullptr;
        Tree* right = nullptr;
        friend bool operator>(const Tree& lhs, const Tree& rhs) { return lhs.freq > rhs.freq; }
        friend ostream& operator<<(ostream& out, const Tree& t)
        {
            out << t.freq << "\t" << t.ch << "\t" << t.code << "\t\t"
                << long(t.left) << " " << long(t.right);
            return out;
        }
    };
};

} /* blitz */ 
