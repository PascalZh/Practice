#include "lexicon.hpp"
namespace blitz {

class LexiconImpl;

struct DataBlock {
    // The lexicon is encoded in `data` by `coder`.
private:
    unique_ptr<Coder> coder;
    string data;
    mutable vector<Record> buf;
    static inline list<DataBlock*> pbuffered;
    static inline list<const DataBlock*> pcbuffered;

public:
    using iterator = vector<Record>::iterator;
    using const_iterator = vector<Record>::const_iterator;
    explicit DataBlock(const string& data_)
        : coder(Coder::create(data_)), data(coder->encode(data_))
    {
        assert(data_ == coder->decode(data));
    }

    DataBlock(DataBlock&& db) : coder(move(db.coder)), data(move(db.data)), buf(move(db.buf))
    {
        if (!db.buf.empty()) {
            pbuffered.remove(&db);
            pbuffered.remove(this);
            pbuffered.push_front(this);
        }
    }

    DataBlock(const DataBlock&) = delete;
    DataBlock& operator=(const DataBlock&) = delete;

    bool insert(Record record);

    vector<Record> find_all(const string& pinyin) const;

    // The following functions will not call `commit_buf` because these functions
    // return a iterator which can be invalidated by `commit_buf`.
    iterator find_record(const string& pinyin, const string& word)
    {
        auto records_ = this->records();
        return find_if(records_.begin(), records_.end(),
                [&pinyin, &word](const Record& r) { return r.pinyin == pinyin && r.word == word; });
    }
    iterator begin() { return records().begin(); }
    iterator end() { return records().end(); }
    size_t size() const { return records().size(); }

    vector<Record>& records()
    {
        if (buf.empty()) {
            buf = lexical_cast<vector<Record>>(coder->decode(data));
            pbuffered.remove(this);
            pbuffered.push_front(this);
        }
        return buf;
    }

    vector<Record>& records() const
    {
        if (buf.empty()) {
            buf = lexical_cast<vector<Record>>(coder->decode(data));
            pcbuffered.remove(this);
            pcbuffered.push_front(this);
        }
        return buf;
    }

    void commit_buf()
    {
        if (pbuffered.size() > 8) {
            auto& last_db = *pbuffered.back();
            assert(last_db.buf.size() != 0);
            string d = lexical_cast<string>(last_db.buf);
            last_db.coder = Coder::create(d);
            last_db.data = coder->encode(d);
            last_db.buf.clear();
            last_db.buf.shrink_to_fit(); 
        }
    }
    void commit_buf() const
    {
        if (pcbuffered.size() > 8) {
            auto& last_db = *pcbuffered.back();
            last_db.buf.clear();
            last_db.buf.shrink_to_fit(); 
        }
    }
};

struct PinyinRange {
    string min;
    string max;
    // According to the ordering defined below, two ranges are equal if they
    // intersect each other. In this case, map::insert will fail because the
    // 'same' key is being inserted, so we can choose to deal with this case
    // more conveniently.
    bool operator<(const PinyinRange& rhs) const { return max < rhs.min; }
    bool operator>(const PinyinRange& rhs) const { return min > rhs.max; }
};

class LexiconImpl : public Lexicon
{
private:
    map<PinyinRange, DataBlock> m_dict;

    void split_into_two_nodes(decltype(m_dict)::iterator it);

    static constexpr int MAX_RECORDS = 256;

public:
    bool insert(Record e);
    bool erase(const string& pinyin, const string& word) { return true; }
    vector<Record> find_all(const string& pinyin) const
    {
        // see struct PinyinRange, two ranges are equivalent if they intersect.
        auto it = m_dict.find(PinyinRange{pinyin, pinyin});
        auto ret = it == m_dict.end() ? vector<Record>()
            : it->second.find_all(pinyin);

        return ret;
    }

    bool set_freq(const string& pinyin, const string& word, function<int(int)> op);

    void init_lexicon(const string& data);
    // some auxiliary functions for debug
    void show_map_node() const;
};

//┌───────────────────────────────────────────────────────────────────────────┐
//│                               functions                                   │
//└───────────────────────────────────────────────────────────────────────────┘

unique_ptr<Lexicon> Lexicon::create()
{
    return make_unique<LexiconImpl>();
}

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

vector<Record> DataBlock::find_all(const string& pinyin) const
{
    const auto records_ = this->records();

    vector<Record> result;
    for (auto& r : records_)
        if (r.pinyin == pinyin)
            result.push_back(r);
    commit_buf();
    return result;
}

bool LexiconImpl::insert(Record record)
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

bool LexiconImpl::set_freq(const string& pinyin, const string& word, function<int(int)> op)
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

void LexiconImpl::init_lexicon(const string& data)
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

void LexiconImpl::split_into_two_nodes(decltype(m_dict)::iterator it)
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

void LexiconImpl::show_map_node() const
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
