#pragma once
#include <string>
#include <vector>
#include <map>
#include <iostream>

#include "coder.h"

namespace blitz
{

using namespace std;

struct Entry {
    string pinyin;
    string word;
    int freq;
    friend ostream& operator<<(ostream& out, const Entry& e)
    {
        return out << e.pinyin << ' ' << e.word << ' ' << e.freq << '\n';
    }
    friend istream& operator>>(istream& in, Entry& e)
    {
        return in >> e.pinyin >> e.word >> e.freq;
    }
};

class Lexicon
{
public:
    virtual vector<Entry> search_pinyin(const string& pinyin) const = 0;
    virtual void insert_word(const string& pinyin, const string& word, int freq) = 0;
};

class LexiconMMU : public Lexicon
{
public:
    virtual vector<Entry> search_pinyin(const string& pinyin) const
    {
        ;
    }
private:
    struct DataBlock {
        // The lexicon is encoded in `data` by `encoder`.
        Coder *coder;
        string data;
        string min_pinyin;
        string max_pinyin;
    };
};

} /* blitz */ 
