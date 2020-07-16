#pragma once
#include <algorithm>
#include <sstream>
#include <string>

#include <vector>
#include <map>
#include <list>

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

    friend bool operator<(const Record& lhs, const Record& rhs)
    {
        return lhs.pinyin < rhs.pinyin || (lhs.pinyin == rhs.pinyin && lhs.word < rhs.word);
    }
    friend bool operator>(const Record& lhs, const Record& rhs)
    {
        return lhs.pinyin > rhs.pinyin || (lhs.pinyin == rhs.pinyin && lhs.word > rhs.word);
    }
    friend bool operator==(const Record& lhs , const Record& rhs)
    {
        return lhs.pinyin == rhs.pinyin && lhs.pinyin == rhs.pinyin;
    }
};

struct DataBlock {
    // The lexicon is encoded in `data` by `coder`.
private:
    using coder_type = IdentityCoder;
    coder_type coder;
    string data;
    mutable vector<Record> buf;
    static list<DataBlock*> buffered_ptrs;

public:
    using iterator = vector<Record>::iterator;
    explicit DataBlock(const string& data_) : coder(data_), data(coder.encode(data_))
    {
        assert(data_ == coder.decode(data));
    }

    DataBlock(DataBlock&& db) : coder(move(db.coder)), data(move(db.data)), buf(move(db.buf))
    {
        if (!db.buf.empty()) {
            buffered_ptrs.remove(&db);
            buffered_ptrs.remove(this);
            buffered_ptrs.push_front(this);
        }
    }

    DataBlock(const DataBlock&) = delete;
    DataBlock& operator=(const DataBlock&) = delete;

    bool insert(Record record);

    vector<Record> find_all(const string& pinyin);

    // The following functions will not call `commit_buf` because these functions
    // return a iterator which can be invalidated by `commit_buf`.
    iterator find_record(const string& pinyin, const string& word) const
    {
        auto records_ = this->records();
        return find_if(records_.begin(), records_.end(),
                [&pinyin, &word](const Record& r) { return r.pinyin == pinyin && r.word == word; });
    }
    iterator begin() const { return records().begin(); }
    iterator end() const { return records().end(); }
    size_t size() const { return records().size(); }

    vector<Record>& records() const
    {
        if (buf.empty()) {
            buf = lexical_cast<vector<Record>>(coder.decode(data));
            buffered_ptrs.remove(const_cast<DataBlock*>(this));
            buffered_ptrs.push_front(const_cast<DataBlock*>(this));
        }
        return buf;
    }

    void commit_buf()
    {
        if (buffered_ptrs.size() > 8) {
            auto& last_db = *buffered_ptrs.back();
            assert(last_db.buf.size() != 0);
            string d = lexical_cast<string>(last_db.buf);
            last_db.coder = coder_type(d);
            last_db.data = coder.encode(d);
            last_db.buf.clear();
            last_db.buf.shrink_to_fit(); 
        }
    }
};

struct PinyinRange {
    string min;
    string max;
    // According to the ordering defined below, two ranges are equal if they
    // intersect each other. In this case, map::insert will fail because the
    // 'same' key is being inserted, so we can choose to deal with this case
    // more conveniently.
    friend bool operator<(const PinyinRange& lhs, const PinyinRange& rhs)
    {
        return lhs.max < rhs.min;
    }
    friend bool operator>(const PinyinRange& lhs, const PinyinRange& rhs)
    {
        return lhs.min > rhs.max;
    }
};

class LexiconBase
{
public:
    virtual vector<Record> find_all(const string& pinyin) const = 0;
    virtual bool insert(Record e) = 0;
    virtual bool set_freq(const string& pinyin, const string& word, function<int(int)> op) = 0;
};

class Lexicon : LexiconBase
{
private:
    map<PinyinRange, DataBlock> m_dict;

    void split_into_two_nodes(decltype(m_dict)::iterator it);

    static constexpr int MAX_RECORDS = 256;

public:
    vector<Record> find_all(const string& pinyin) const
    {
        // see struct PinyinRange, two ranges are equivalent if they intersect.
        auto it = m_dict.find(PinyinRange{pinyin, pinyin});
        auto ret = it == m_dict.end() ? vector<Record>()
            : const_cast<DataBlock&>(it->second).find_all(pinyin);

        return ret;
    }


    bool insert(Record e);
    bool insert(string pinyin, string word, int freq) { return insert(Record{pinyin, word, freq}); }

    bool set_freq(const string& pinyin, const string& word, int freq)
    {
        return set_freq(pinyin, word, [freq](int i) { return freq; });
    }
    bool set_freq(const string& pinyin, const string& word, function<int(int)> op);

    void init_lexicon(const string& data);

    // some auxiliary functions for debug
    void show_map_node() const;

};

} /* namespace blitz */ 
