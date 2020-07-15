#include "core.h"
#include "utils.h"
#include <exception>
#include <chrono>
using std::vector; using std::map; using std::string; using std::wstring; using std::cout; using std::cin; using std::endl;

extern char _binary_gbk_txt_start;

WordQuerySimple::WordQuerySimple()
    : cache(), max_records(1000)
{
    // parse gbk.txt
    string gbk_raw(&_binary_gbk_txt_start);
    vector<string> gbk;
    split(gbk_raw, gbk, "\n");
    for (auto& line : gbk) {
        vector<string> w;
        split(line, w, "=");
        if (w.size() != 2)
            throw std::runtime_error("There are some problems with gbk.o file.");
        pinyin_t py(w[0]);
        auto it = cache.find(py);
        if(it == cache.end()) {
            vector<string> words{std::move(w[1])};
            cache.insert({std::move(py), std::move(words)});
        } else {
            it -> second.push_back(std::move(w[1]));
        }
    }

    // parse sogou_lexicon.txt, it takes about 1.6s
    auto t1 = std::chrono::high_resolution_clock::now();
    string path_sogou = "sogou_lexicon.txt";
    std::ifstream in_sogou(path_sogou);
    string buf1, buf2;
    while (in_sogou >> buf1 >> buf2) {
        pinyin_t py(buf1.substr(1));
        vector<string> words({std::move(buf2)});
        cache.insert({std::move(py), std::move(words)});
    }
    auto t2 = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double, std::milli> ms = t2 - t1;
	cout << "parse sogou_lexicon.txt: "  << ms.count() << "ms" << endl;
}


void WordQuerySimple::query(const string &pinyin, size_t n_candidates)
{
    // single word query
    query_record_t q = {
        .pinyin = pinyin
    };
    pinyin_t py(pinyin);
    auto it = cache.find(py);
    if(it == cache.end()) {
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

    this -> records.push_back(std::move(q));
    //if (records.size() > this -> max_records && !records.empty())
        //records.pop();
    //for (auto& c : records.back().candidates) {
        //cout << c;
    //}
    //cout << endl;
}
