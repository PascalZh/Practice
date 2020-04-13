#pragma once
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <exception>
#include <map>
#include <cassert>
#include <list>
#include <forward_list>
#include <cstring>

#include "compressor.h"
#include "utils.h"

class LexiconNode {
public:
    using string = std::string;
    using Result = std::vector<std::tuple<string, string, int>>;

    static Compressor compressor;

    LexiconNode(const string& d) : data(d) {}
    
    Result search_pinyin(const string& pinyin) const
    {
        // e.g. d = "a'a'a,阿阿阿,777\na'a'a'a,阿阿阿阿,666"
        string d = compressor.decode(this -> data);

        Result ret;
        for (auto record : split(d, "\n")) {
            auto r = split(record, ",");
            if (r[0] == pinyin) {
                // parse the frequence
                int freq = str2int(r[2]);
                ret.push_back(std::make_tuple(r[0], r[1], freq));
            }
        }
        
        return ret;
    }
    void insert_word(const string& pinyin, const string& word, int freq)
    {
        // if there is a record matching (pinyin, word), it will add up freq to the origin one.
        string d = compressor.decode(this -> data);
        auto records = split(d, "\n");
        
        bool finded = false;
        for (auto& record : records) {
            auto r = split(record, ",");
            if (r[0] == pinyin && r[1] == word) {
                r[2] = int2str(str2int(r[2]) + freq);
                record = join(r, ",");
                finded = true;
                break;
            }
        } if (!finded) {
            records.push_back(join({pinyin, word, int2str(freq)}, ","));
        }
        d = join(records, "\n");
        this -> data = compressor.encode(d);
    }

private:
    string a, b; // denote the span pinyin of data
    string data;
};
