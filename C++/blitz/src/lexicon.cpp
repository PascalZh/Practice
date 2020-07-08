#include "lexicon.h"

namespace blitz {

vector<Entry> LexiconMMU::search_pinyin(const string& pinyin) const
{
    vector<Entry> result;
    // see struct PinyinRange, two ranges are equivalent if they intersect.
    PinyinRange range{pinyin, pinyin};
    auto it = m_dict.find(range);
    if (it == m_dict.end())
        return result;

    string data = it->second.original_data();
    stringstream ss(data);
    for (Entry entry; ss >> entry; )
        if (entry.pinyin == pinyin)
            result.push_back(entry);
    return result;
}

bool LexiconMMU::insert_word(const string& pinyin, const string& word, int freq)
{
    bool success;
    Entry entry{pinyin, word, freq};
    PinyinRange range{pinyin, pinyin};

    if (m_dict.empty()) {
        m_dict.emplace(move(range), DataBlock(lexical_cast<string>(entry)));
        return true;
    }

    auto it = m_dict.find(range);
    if (it == m_dict.end()) {
        auto ub = m_dict.upper_bound(range);
        it = ub == m_dict.end() ? --m_dict.end() : ub;

        success = it->second.insert_entry(entry);
        assert(success);
        auto nh = m_dict.extract(it);
        nh.key().min = pinyin;
        m_dict.insert(move(nh));
    } else {
        success = it->second.insert_entry(entry);
    }

    // if the `it->second` is too big, split it into two nodes.
    if (it->second.data.size() > 4 * 1024)
        split_into_two_nodes(it);
    return success;
}
// TODO: add more api for CURD

void LexiconMMU::split_into_two_nodes(map<PinyinRange, DataBlock>::iterator it)
{
    // TODO: to be tested.
    vector<Entry> entrys = lexical_cast<vector<Entry>>(it->second.original_data());

    auto middle = entrys.begin() + entrys.size() / 2;
    while (middle + 1 != entrys.end() && !(*middle < *(middle + 1)))
        ++middle;
    if (middle + 1 == entrys.end()) // fail in splitting.
        return;
    else {
        m_dict.erase(it);
        string data1, data2;
        for (auto it_ = entrys.begin(); it_ != middle + 1; ++it_)
            data1 += lexical_cast<string>(*it_);
        for (auto it_ = middle + 1; it_ != entrys.end(); ++it_)
            data2 += lexical_cast<string>(*it_);
        m_dict.emplace(PinyinRange{entrys.begin()->pinyin, middle->pinyin}, DataBlock(data1));
        m_dict.emplace(PinyinRange{(middle + 1)->pinyin, (--entrys.end())->pinyin}, DataBlock(data2));
    }
}

bool LexiconMMU::DataBlock::insert_entry(const Entry& entry)
{
    auto entrys = lexical_cast<vector<Entry>>(this->original_data());
    if (binary_search(entrys.begin(), entrys.end(), entry))
        return false;
    entrys.push_back(entry);

    sort(entrys.begin(), entrys.end());

    string data = lexical_cast<string>(entrys);
    this->coder = make_unique<HuffmanCoder>(data);
    this->data = this->coder->encode(data);
    return true;
}

} /* namespace blitz */ 
