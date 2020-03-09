#ifndef _TREE_H_
#define _TREE_H_
#include <iostream>
#include <fstream>
#include <sstream>
#include <exception>
#include <map>
#include <initializer_list>

template <class Key, class Value>
class SearchTree {
    using ios = std::ios; using string = std::string; using stringstream = std::stringstream;
    using runtime_error = std::runtime_error;

    private:
    unsigned long long total_freq_acc;
    unsigned long long total_freq;
    std::map<Key, Value> cache;
    string path;

    public:
    SearchTree() {}
    SearchTree(const string &path)
        : path(path)
    {
        std::ifstream f(path);
        string buf;
        if (getline(f, buf)) {
            stringstream ss(buf);
            ss >> this -> total_freq;
        } else {
            throw runtime_error("parse " + path + " error");
        }
        while (getline(f, buf)) {
            stringstream ss(buf);
            Key k; Value v;
            ss >> k; ss >> v;
            cache.insert({std::move(k), std::move(v)});
        }
    }
    const Value *find(const Key &key)
    {
    }
    void insert(Key &&k, Value &&v)
    {
    }
    void save_data()
    {
    }
};

#endif /* _TREE_H_ */
