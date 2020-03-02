#ifndef __CORE_H__
#define __CORE_H__

#include <vector>
#include <queue>
#include <list>
#include <string>
#include <map>
#include <array>
#include <fstream>
#include <iostream>
#include <sstream>
#include <memory>

using pinyin_t = std::string;

struct query_record_t {
    pinyin_t pinyin;
    std::vector<std::string> candidates;
};

class WordQueryBase
{
    public:
        virtual void query(pinyin_t pinyin, size_t n_candidates) = 0;
        virtual const query_record_t * get_last_query() const = 0;
};


using SearchTreeSimple = std::map<pinyin_t, std::vector<std::string>>;

class WordQuerySimple : public WordQueryBase
{
    public:

    private:
        std::queue<query_record_t, std::list<query_record_t>> records;
        const std::unique_ptr<SearchTreeSimple> cache;

        size_t max_records;

    public:
        WordQuerySimple();

        void query(pinyin_t, size_t);

        const query_record_t * get_last_query() const {
            return &records.back();
        }
};

#if defined if_
#error "some macro defined before!"
#endif
#define if_(x, y) x; if(y)

#endif
