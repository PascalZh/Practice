#include <vector>
#include <string>
#include <map>
#include <array>
#include <fstream>
#include <iostream>
#include <sstream>
#include <memory>

using pinyin_t = std::string;


class WordQueryBase
{
public:
    virtual void query(pinyin_t pinyin, size_t n_candidates) = 0;
};


using SearchTreeBase = std::map<pinyin_t, std::vector<std::string>>;

class WordQuerySimple : public WordQueryBase
{
public:
    struct query_record_t {
        pinyin_t pinyin;
        std::vector<std::string> candidates;
    };
    
    std::vector<query_record_t> records;
    std::unique_ptr<SearchTreeBase> cache;
    WordQuerySimple();
    
    void query(pinyin_t, size_t);

};

#if defined if_
#error "some macro defined before!"
#endif
#define if_(x, y) x; if(y)
