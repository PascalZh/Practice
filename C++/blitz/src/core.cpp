#include "core.h"
#include <exception>
using std::vector, std::map, std::string, std::wstring, std::cout, std::cin, std::endl;

void split(const string& s, vector<string>& tokens, const string& delimiters);

WordQuerySimple::WordQuerySimple()
    : cache(new SearchTreeBase())
{
    string buf;
    string nGBK = "GBK.txt";
    string nSogou = "sogou_lexicon.txt";
    auto fGBK = std::ifstream(nGBK);
    auto fSogou = std::ifstream(nSogou);

    while (getline(fGBK, buf)) {
        //std::cout << buf << std::endl;
        
        vector<string> w;
        split(buf, w, "=");
        if (w.size() != 2) {
            throw std::runtime_error(nGBK + "file parse error:");
        }
        //std::cout << w[0] << std::endl;
        //std::cout << string(w[1].begin(), w[1].end()) << std::endl;
        
        if (auto it = cache -> find(w[0]); it == cache -> end()) {
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
    if (auto it = cache -> find(pinyin); it == cache -> end()) {
        ;
    } else {
        vector<string> tmp(n_candidates);
        auto start_it = it -> second.begin();
        auto traverse_it = it -> second.begin();
        auto end_it = it -> second.end();
        for (size_t i = 0; i < n_candidates && traverse_it != end_it; i++) {
            traverse_it++;
        }
        tmp.assign(start_it, traverse_it);
        q.candidates = std::move(tmp);
    }
    records.push_back(std::move(q));
    for (auto& c : records.back().candidates) {
        cout << c;
    }
    cout << endl;
}

void split(const string& s, vector<string>& tokens, const string& delimiters=" ")
{
    string::size_type lastPos = s.find_first_not_of(delimiters, 0);
    string::size_type pos = s.find_first_of(delimiters, lastPos);
    while (string::npos != pos || string::npos != lastPos) {
        tokens.push_back(s.substr(lastPos, pos - lastPos));
        lastPos = s.find_first_not_of(delimiters, pos);
        pos = s.find_first_of(delimiters, lastPos);
    }
}
