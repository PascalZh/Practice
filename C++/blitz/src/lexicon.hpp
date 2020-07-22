#pragma once
#include <algorithm>
#include <sstream>
#include <string>

#include <vector>
#include <map>
#include <list>
#include <memory>

#include <iostream>
#include <cassert>

#include "coder.hpp"

namespace blitz
{

using namespace std;

struct Record {
    string pinyin;
    string word;
    int freq = 0;
    friend ostream& operator<<(ostream& out, const Record& e)
    {
        return out << e.pinyin << ' ' << e.word << ' ' << e.freq << '\n';
    }
    friend istream& operator>>(istream& in, Record& e)
    {
        return in >> e.pinyin >> e.word >> e.freq;
    }
    friend ostream& operator<<(ostream& out, const vector<Record>& records)
    {
        for (auto& e: records)
            out << e;
        return out;
    }
    friend istream& operator>>(istream& in, vector<Record>& records)
    {
        for(Record e; in >> e; )
            records.push_back(e);
        in.clear();
        return in; 
    }

    weak_ordering operator<=>(const Record& rhs) const
    {
        if (auto cmp = pinyin <=> rhs.pinyin; cmp != 0)
            return cmp;
        return word <=> rhs.word;
    }
    bool operator==(const Record& rhs) const
    {
        if (pinyin.size() != rhs.pinyin.size() or word.size() != rhs.word.size())
            return false;
        return pinyin == rhs.pinyin and word == rhs.word;
    }
};

class Lexicon
{
public:
    virtual bool           insert(Record e) = 0;
    virtual bool           erase(const string& pinyin, const string& word) = 0;
    virtual vector<std::reference_wrapper<Record>> find_all(const string& pinyin) = 0;

    virtual void           show_map_node() const = 0;

    static unique_ptr<Lexicon> create();

    virtual ~Lexicon() {}
};

} /* namespace blitz */ 
