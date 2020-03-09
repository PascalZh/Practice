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
        static unsigned long long total_size;
        static unsigned long long total_capacity;
        static unsigned long long total_n;
        pinyin_t(pinyin_t &&s) : code(std::move(s.code)) {}
        pinyin_t(const pinyin_t &s) : code(s.code) {
            total_size += code.size();
            total_capacity += code.capacity();
            total_n++;
        }
        explicit pinyin_t(const std::string &s) : code(s) {
            total_size += code.size();
            total_capacity += code.capacity();
            total_n++;
        }
        bool operator<(const pinyin_t & rhs) const {
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
        ~pinyin_t() {
            if (!code.empty()) {
                assert(total_size >= code.size() &&
                        total_capacity >= code.capacity() &&
                        total_n >= 1);
                total_size -= code.size();
                total_capacity -= code.capacity();
                total_n--;
            }
        }
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
