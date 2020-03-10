#ifndef _TREE_H_
#define _TREE_H_
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <exception>
#include <map>
#include <cassert>

template <class Key, class Value>
class SearchTree {
    using ios = std::ios; using string = std::string; using stringstream = std::stringstream;
    using runtime_error = std::runtime_error;

    private:
    unsigned long long total_freq_acc;
    unsigned long long total_freq;
    std::map<Key, std::vector<Value>> cache;
    std::map<Key, std::vector<int>> index;
    std::vector<int> loaded;
    string dir; // e.g. db, db has files: index.db, 0.db, 1.db, 2.db, ...

    public:
    SearchTree() {}
    SearchTree(const string &dir)
        : dir(dir)
    {
        system(("mkdir -p " + dir).c_str());

        // read index.db
        std::ifstream f(dir + "/index.db");
        if (!f) return;
        string buf;
        while (getline(f, buf)) {
            stringstream ss(buf);
            Key k; std::vector<int> indices;
            ss >> k;
            int idx;
            while (ss >> idx) {
                indices.push_back(idx);
            }
            index.insert({k, indices});
        }
        load(0);
    }
    auto find(const Key &k)
    {
        auto it_ind = index.find(k);
        for (int & idx : it_ind -> second)
            load(idx);

        return cache.find(k);
    }
    auto begin() { return cache.begin(); }
    auto end() { return cache.end(); }

    // find, begin, end are just wrapper of map
    // but insert has different meaning rather than map
    void insert(Key &&k, Value &&v)
    {
    }
    private:
    void serialize()
    {
    }
    void load(int n)
    {
        assert(n > 0);

        for (auto &l : loaded)
            if (l == n) return;
        loaded.push_back(n);

        string tmp; stringstream ss; ss << n; ss >> tmp;
        std::ifstream f(dir + "/" + tmp + ".db");
        if (!f) return;
        Key k; Value v;
        while (f >> k >> v) {
            cache.insert({k, v});
        }
    }
};

#endif /* _TREE_H_ */
