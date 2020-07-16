#include "lexicon.hpp"

namespace blitz {

list<DataBlock*> DataBlock::buffered_ptrs;

bool DataBlock::insert(Record record)
{
    auto records_ = this->records();

    if (binary_search(records_.begin(), records_.end(), record))
        return false;

    records_.push_back(record);
    sort(records_.begin(), records_.end());
    commit_buf();
    return true;
}

vector<Record> DataBlock::find_all(const string& pinyin)
{
    const auto records_ = this->records();

    vector<Record> result;
    for (auto& r : records_)
        if (r.pinyin == pinyin)
            result.push_back(r);
    commit_buf();
    return result;
}

bool Lexicon::insert(Record record)
{
    bool success;
    PinyinRange range{record.pinyin, record.pinyin};

    if (m_dict.empty()) {
        m_dict.emplace(move(range), DataBlock(lexical_cast<string>(record)));
        return true;
    }

    auto it = m_dict.find(range);
    if (it == m_dict.end()) {
        auto ub = m_dict.upper_bound(range);
        it = ub == m_dict.end() ? --m_dict.end() : ub;

        success = it->second.insert(move(record));
        assert(success);

        auto nh = m_dict.extract(it);
        nh.key().min = move(range.min);
        m_dict.insert(move(nh));
    } else {
        success = it->second.insert(move(record));
    }

    // if the `it->second` is too big, split it into two nodes.
    if (it->second.size() > MAX_RECORDS)
        split_into_two_nodes(it);
    return success;
}

bool Lexicon::set_freq(const string& pinyin, const string& word, function<int(int)> op)
{
    auto it_dict = m_dict.find(PinyinRange{pinyin, pinyin});
    if (it_dict == m_dict.end())
        return false;

    auto& [range, datablock] = *it_dict;
    DataBlock::iterator it = datablock.find_record(pinyin, word);
    if (it == datablock.end()) {
        datablock.commit_buf();
        return false;
    }
    
    it->freq = op(it->freq);
    datablock.commit_buf();
    return true;
}

void Lexicon::init_lexicon(const string& data)
{
    string::size_type last_pos = 0;
    string::size_type pos = data.find_first_of('\n', 0);
    while (pos != string::npos && pos + 1 < data.size()) {
        for (int cnt_newline = 0; pos != string::npos && cnt_newline < MAX_RECORDS; ++cnt_newline)
            pos = data.find_first_of('\n', pos + 1);

        auto space1 = data.find_first_of(' ', last_pos);
        auto space3 = data.find_last_of(' ', pos);
        space3 = data.find_last_of(' ', space3 - 1);
        auto space2 = data.find_last_of('\n', space3 - 1);
        m_dict.emplace<PinyinRange, DataBlock>(
                {
                    data.substr(last_pos, space1 - last_pos),
                    data.substr(space2 + 1, space3 - space2 - 1)
                },
                DataBlock(data.substr(last_pos, pos + 1 - last_pos))
                );
        last_pos = pos + 1;
    }
}

void Lexicon::split_into_two_nodes(decltype(m_dict)::iterator it)
{
    const auto records_ = it->second.records();

    auto middle = records_.begin() + records_.size() / 2;
    while (middle + 1 != records_.end() && !(*middle < *(middle + 1)))
        ++middle;
    if (middle + 1 == records_.end()) // fail in splitting.
        return;
    else {
        m_dict.erase(it);
        string data1, data2;
        for (auto it_ = records_.begin(); it_ != middle + 1; ++it_)
            data1 += lexical_cast<string>(*it_);
        for (auto it_ = middle + 1; it_ != records_.end(); ++it_)
            data2 += lexical_cast<string>(*it_);
        m_dict.emplace(PinyinRange{records_.begin()->pinyin, middle->pinyin}, DataBlock(data1));
        m_dict.emplace(PinyinRange{(middle + 1)->pinyin, (--records_.end())->pinyin}, DataBlock(data2));
    }
}

void Lexicon::show_map_node() const
{
    for(auto& [range, datablock] : m_dict) {
        cout << "data.size() = " << datablock.size();
        cout << " [" << range.min << ", " << range.max << "]" << endl;
        auto vec = datablock.records();
        cout << vec[0];
        cout << vec.back() << endl;
    }
}

} /* namespace blitz */ 
