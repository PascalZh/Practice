#pragma once
#include <sstream>
#include <string>
#include <vector>
#include <map>
#include <iostream>
#include <memory>
#include <cassert>

#include "coder.h"

namespace blitz
{

using namespace std;

struct Entry {
    string pinyin;
    string word;
    int freq;
    friend ostream& operator<<(ostream& out, const Entry& e)
    { return out << e.pinyin << '|' << e.word << '|' << e.freq << '\n'; }
    friend istream& operator>>(istream& in, Entry& e)
    {
        string output;
        in >> output;
        if (output.empty()) return in;
        vector<string> v = split(output, "|");
        if (v.size() != 3) throw runtime_error("v.size() != 3");
        e.pinyin = v[0];
        e.word = v[1];
        e.freq = lexical_cast<int>(v[2]);
        return in;
    }
    friend ostream& operator<<(ostream& out, const vector<Entry>& entrys)
    { for (auto& e: entrys) out << e; return out; }
    friend istream& operator>>(istream& in, vector<Entry>& entrys)
    { for(Entry e; in >> e; ) entrys.push_back(e); return in; }

    friend bool operator<(const Entry& lhs, const Entry& rhs)
    { return lhs.pinyin < rhs.pinyin || (lhs.pinyin == rhs.pinyin && lhs.word < rhs.word); }
    friend bool operator>(const Entry& lhs, const Entry& rhs)
    { return lhs.pinyin > rhs.pinyin || (lhs.pinyin == rhs.pinyin && lhs.word > rhs.word); }
    friend bool operator==(const Entry& lhs , const Entry& rhs)
    { return lhs.pinyin == rhs.pinyin && lhs.pinyin == rhs.pinyin; }
};

class Lexicon
{
public:
    virtual vector<Entry> search_pinyin(const string& pinyin) const = 0;
    virtual bool insert_word(const string& pinyin, const string& word, int freq) = 0;
};

class LexiconMMU : public Lexicon
{
public:
    virtual vector<Entry> search_pinyin(const string& pinyin) const;
    virtual bool insert_word(const string& pinyin, const string& word, int freq);
private:
    struct DataBlock {
        // The lexicon is encoded in `data` by `encoder`.
        // Be careful that the unique_ptr coder will disable the copy assignment
        // So it can only be moved.
        unique_ptr<Coder> coder;
        string data;

        explicit DataBlock(const string& data_) {
            coder = make_unique<HuffmanCoder>(data_);
            data = coder->encode(data_);
            assert(data_ == original_data());
        }
        DataBlock(DataBlock&& db) : coder(move(db.coder)), data(move(db.data)) {}
        DataBlock(const DataBlock&) = delete;
        DataBlock& operator=(const DataBlock&) = delete;

        bool insert_entry(const Entry& entry);
        string original_data() const { return coder->decode(data); }
    };
    struct PinyinRange {
        string min;
        string max;
        // According to the ordering defined below, two ranges are equal if they
        // intersect each other. In this case, map::insert will fail because the
        // 'same' key is being inserted, so we can choose to deal with this case
        // more conveniently.
        inline bool is_in_range(string str) { return min <= str and str <= max; }
        friend bool operator<(const PinyinRange& lhs, const PinyinRange& rhs)
        { return lhs.max < rhs.min; }
        friend bool operator>(const PinyinRange& lhs, const PinyinRange& rhs)
        { return lhs.min > rhs.max; }
    };

    void split_into_two_nodes(map<PinyinRange, DataBlock>::iterator it);

    map<PinyinRange, DataBlock> m_dict;
};

} /* namespace blitz */ 
