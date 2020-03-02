#include "core.h"
#include "utils.h"
#include <exception>
using std::vector; using std::map; using std::string; using std::wstring; using std::cout; using std::cin; using std::endl;

WordQuerySimple::WordQuerySimple()
    : cache(new SearchTreeSimple()), max_records(1000)
{
    string buf;
    string nGBK = "GBK.txt";
    string nSogou = "sogou_lexicon.txt";
    auto fGBK = std::ifstream(nGBK);
    auto fSogou = std::ifstream(nSogou);
    if (!fGBK.is_open()) {
        throw std::runtime_error(string()+ "open " + nGBK + " failed!");
    }

    while (getline(fGBK, buf)) {
        //std::cout << buf << std::endl;
        
        vector<string> w;
        split(buf, w, "=");
        if (w.size() != 2) {
            throw std::runtime_error(nGBK + "file parse error:");
        }
        //std::cout << w[0] << std::endl;
        //std::cout << string(w[1].begin(), w[1].end()) << std::endl;

        if_(auto it = cache -> find(w[0]), it == cache -> end()) {
            vector<string> words{w[1]};
            cache -> insert({w[0], std::move(words)});
        } else {
            it -> second.push_back(w[1]);
        }
    }
    //for ( auto&[pinyin, words] : *cache) {
        //std::cout << pinyin << std::endl;
        //for ( auto& word : words ) {
            //std::cout << word;
        //}
        //std::cout << std::endl;
    //}
}


void WordQuerySimple::query(
          pinyin_t pinyin
        , size_t n_candidates
        )
{
    // single word query
    query_record_t q;
    q.pinyin = pinyin;
    if_(auto it = cache -> find(pinyin), it == cache -> end()) {
        ;
    } else {
        vector<string> tmp(n_candidates);
        auto first = it -> second.begin();
        auto last = it -> second.end();
        for (size_t i = 0; i < n_candidates && first != last; i++) {
            first++;
        }
        tmp.assign(it -> second.begin(), first);
        q.candidates = std::move(tmp);
    }
    
    records.push(std::move(q));
    if (records.size() > this -> max_records && !records.empty())
        records.pop();
    //for (auto& c : records.back().candidates) {
        //cout << c;
    //}
    //cout << endl;
}
