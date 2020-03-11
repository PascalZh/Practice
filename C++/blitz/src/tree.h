#ifndef _TREE_H_
#define _TREE_H_
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <exception>
#include <map>
#include <cassert>
#include <list>
#include <forward_list>

template <class Key, class Value>
class SearchTree {
    private:
    using ios = std::ios; using string = std::string; using stringstream = std::stringstream;
    using runtime_error = std::runtime_error;

    unsigned long long total_freq_acc;
    unsigned long long total_freq;

    using Container = std::vector<Value>;
    using Tree = std::map<Key, Container>;
    Tree cache;
    std::vector<Key> index;
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
        Key v;
        while (f >> v) {
            index.push_back(Key(v));
        }
        load(1);
    }
    auto find(const Key &k)
    {
        int i = 1;
        for (auto ind = index.begin(); ind != index.end(); ind++, i++) {
            if (k <= *ind) {
                load(i);
                return cache.find(k);
            }
        }
        return cache.end();
    }
    auto begin() { return cache.begin(); }
    auto end() { return cache.end(); }

    // find, begin, end are just wrapper of map
    // but insert has different meaning rather than map
    void insert(Key &k, Value &v)
    {
        auto it = cache.find(k);
        if (it == cache.end()) {
            Container vs(1, v);
            cache.insert({k, std::move(vs)});
        } else {
            it -> second.push_back(v);
        }
    }
    void print(std::pair<const Key, Container> &i) const
    {
        std::cout << "pinyin: " <<  i.first << std::endl;
        std::cout << "word:   ";
        for (const auto &x : i.second) {
            std::cout << "|" << x << "|";
        }
        std::cout << std::endl;
    }
    unsigned long capacity() const
    {
        unsigned long ret = 0;
        //for (const auto & x: cache) {
            //ret += 8 * 2;
            //ret += x.first.capacity();
            //for (const auto &word : x.second) {
                //ret += word.capacity();
            //}
        //}
        return ret;
    }
    unsigned long size() const { return cache.size(); }
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
#ifndef NDEBUG
        std::cout << tmp + ".db loaded" << std::endl;
#endif
        if (!f) return;

        Key k; Value v;
        while (f >> k >> v) {
            insert(k, v);
        }
    }
};

#endif /* _TREE_H_ */
