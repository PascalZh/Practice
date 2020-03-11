#ifndef __CORE_H__
#define __CORE_H__

#include <vector>
#include <queue>
#include <list>
#include <string>
#include <string.h>
#include <map>
#include <array>
#include <fstream>
#include <iostream>
#include <sstream>
#include <memory>
#include <cassert>

class pinyin_t {
    private:
        std::string code;
    public:
        pinyin_t() {}
        pinyin_t(pinyin_t &&s) : code(std::move(s.code)) {}
        pinyin_t(const pinyin_t &s) : code(s.code) {}
        explicit pinyin_t(const std::string &s) : code(s) {}

        bool is_valid() const { return !code.empty(); }
        bool operator<(const pinyin_t & rhs) const {
            assert(is_valid());
            const auto &c1 = this -> code;
            const auto &c2 = rhs.code;
            size_t n1 = c1.size();
            size_t n2 = c2.size();
            size_t m = n1 < n2 ? n1 : n2;
            for (size_t i = 0; i < m; i++) {
                if (c1[i] < c2[i]) {
                    return true;
                } else if (c1[i] > c2[i]) {
                    return false;
                }
            }
            // now any of c1, c2 is a substr of each other
            // this says "ni" < "ni'hao"
            return n1 < n2;
        }
        bool operator>(const pinyin_t &rhs) const { return rhs < *this; }
        bool operator<=(const pinyin_t &rhs) const { return !(*this > rhs); }
        bool operator>=(const pinyin_t &rhs) const { return !(*this < rhs); }
        bool operator==(const pinyin_t &rhs) const { return !(*this < rhs) && !(*this > rhs); }

        friend std::ostream &operator<<(std::ostream &out, const pinyin_t &py)
        {
            assert(py.is_valid());
            out << py.code;
            return out;
        }

        friend std::istream &operator>>(std::istream &in, pinyin_t &py)
        {
            in >> py.code;
            if (!py.is_valid()) { throw std::runtime_error("in >> pinyin: not valid input!"); }
            return in;
        }
        size_t capacity() const { return code.capacity(); }
};

class word_t {
    private:
        unsigned int freq;
        std::string word;
    public:
        word_t() :word() {}
        word_t(const word_t &w) : freq(w.freq), word(w.word) {}
        word_t(word_t &&w) : freq(w.freq), word(std::move(w.word)) {}
        word_t(const unsigned int &f, const std::string &w) : freq(f), word(w) {}
        friend std::ostream &operator<<(std::ostream &out, const word_t &w)
        {
            assert(w.is_valid());
            out << w.freq << " " << w.word;
            return out;
        }
        friend std::istream &operator>>(std::istream &in, word_t &w)
        {
            in >> w.freq >> w.word;
            if (!w.is_valid()) { throw std::runtime_error("in >> word: not valid input!"); }
            return in;
        }
        bool is_valid() const { return freq >= 0 && !word.empty(); }
        size_t capacity() const { return sizeof(freq) + word.capacity(); }
};

struct query_record_t {
    std::string pinyin;
    std::vector<std::string> candidates;
};

class WordQueryBase
{
    public:
        virtual void query(const std::string &pinyin, size_t n_candidates) = 0;
        virtual const query_record_t * get_last_query() const = 0;
};

using SearchTreeSimple = std::map<pinyin_t, std::vector<std::string>>;

class WordQuerySimple : public WordQueryBase
{
    public:

    private:
        std::vector<query_record_t> records;
        SearchTreeSimple cache;

        size_t max_records;

    public:
        WordQuerySimple();

        void query(const std::string &, size_t);

        const query_record_t * get_last_query() const {
            return &records.back();
        }
};

#if defined if_
#error "some macro defined before!"
#endif
#define if_(x, y) x; if(y)

#endif /* __CORE_H__ */
