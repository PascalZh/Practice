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
private:
    struct query_record_t {
        pinyin_t pinyin;
        std::vector<std::string> candidates;
    };
    
    std::vector<query_record_t> records;
    std::unique_ptr<SearchTreeBase> cache;
    
public:
    void query(pinyin_t, size_t);

    WordQuerySimple();
};
