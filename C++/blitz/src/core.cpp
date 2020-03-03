#include "core.h"
#include "utils.h"
#include <exception>
using std::vector; using std::map; using std::string; using std::wstring; using std::cout; using std::cin; using std::endl;

extern char _binary_gbk_txt_start;

WordQuerySimple::WordQuerySimple()
    : cache(new SearchTreeSimple()), max_records(1000)
{
    string nSogou = "sogou_lexicon.txt";
    auto fSogou = std::ifstream(nSogou);

    string basic_lexicon_raw(&_binary_gbk_txt_start);
    vector<string> basic_lexicon;
    split(basic_lexicon_raw, basic_lexicon, "\n");
    for (auto& line : basic_lexicon) {
        vector<string> w;
        split(line, w, "=");
        if (w.size() != 2) throw std::runtime_error("There are some problems with gbk.o file.");
        if_(auto it = cache -> find(w[0]), it == cache -> end()) {
            vector<string> words{std::move(w[1])};
            cache -> insert({std::move(w[0]), std::move(words)});
        } else {
            it -> second.push_back(std::move(w[1]));
        }
    }
}


void WordQuerySimple::query(pinyin_t pinyin, size_t n_candidates)
{
    // single word query
    query_record_t q;
    q.pinyin = pinyin;
    if_(auto it = cache -> find(pinyin), it == cache -> end()) {
        ;
    } else {
        vector<string> tmp(n_candidates);
        auto first = it -> second.begin(); auto last = it -> second.end();
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
