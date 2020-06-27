#pragma once
#include <string>
#include <vector>
#include <algorithm>

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
                vec.push_back(Tree{1, ch});
            } else {
                it -> freq += 1;
            }
        }
        _queue = priority_queue<Tree, vector<Tree>, greater<Tree>>(greater<Tree>(), vec);
        //while(!_queue.empty()) {
            //auto p = _queue.top();
            //cout << p.freq << " " << p.ch << endl;
            //_queue.pop();
        //}

        // update the queue by repeatedly joining two trees.
        vector<Tree> cont; // container for children of trees.
        while(_queue.size() > 1) {
            cont.push_back(_queue.top());
            Tree* tree1 = &cont.back();
            _queue.pop();

            cont.push_back(_queue.top());
            Tree* tree2 = &cont.back();
            _queue.pop();

            Tree new_tree = {tree1 -> freq + tree2 -> freq, 0, tree1, tree2};
            cout << new_tree << endl;
            _queue.push(new_tree);
        }
    }

    virtual string encode(const string& str) { return string("fjeiw"); }
    virtual string decode(const string& str) { return string("fjeiw"); }

private:
    struct Tree {
        int freq = 0;
        char ch = 0;
        Tree* left = nullptr;
        Tree* right = nullptr;
        friend bool operator>(const Tree& lhs, const Tree& rhs) { return lhs.freq > rhs.freq; }
        friend ostream& operator<<(ostream& out, const Tree& t) { out << t.freq << " " << int(t.ch) << " " << long(t.left) << " " << long(t.right);}
    };
    priority_queue<Tree, vector<Tree>, greater<Tree>> _queue;
};

} /* blitz */ 
