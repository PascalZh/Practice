#include "core.hpp"

namespace blitz {

void InputMethod::set_candidates()
{
    assert(std::all_of(m_input_seq.begin(), m_input_seq.end(), check_input));
    switch (m_mode) {
        case Mode::Normal:
            set_candidates_normal();
            return;
    }
}

void InputMethod::sort_candidates()
{

}

const vector<Record>& InputMethod::get_candidates() const
{

}

void InputMethod::set_candidates_normal()
{
    vector<string> tokens = tokenize(m_input_seq);

    vector<vector<string>> possible_pinyins_;
    for (auto& token : tokens) {
        vector<string> pinyins = valid_pinyin_dict.at(token);
        possible_pinyins_.push_back(pinyins);
    }

    auto cartesian_product = [](vector<string> lhs, vector<string> rhs)
    {
        if (lhs.empty()) return rhs;
        if (rhs.empty()) return lhs;

        vector<string> ret;
        for (auto& l : lhs)
            for (auto& r : rhs)
                ret.emplace_back(l + "'" + r);
        return ret;
    };
    vector<string> possible_pinyins = std::accumulate(
            possible_pinyins_.begin(),
            possible_pinyins_.end(),
            vector<string>{},
            cartesian_product
            );
    cout << possible_pinyins << endl;
}

vector<string> InputMethod::tokenize(const string& input)
{
    vector<string> ret;
    string::size_type begin = 0;
    for (int n = 1; begin < input.size(); n = 1) {
        while (std::binary_search(
                    valid_pinyin_tokens.begin(),
                    valid_pinyin_tokens.end(),
                    input.substr(begin, n)
                    ) && begin + n - 1 < input.size())
            ++n;
        --n;
        ret.emplace_back(input.substr(begin, n));
        begin += n;
    }
    return ret;
}

bool InputMethod::choose_the_candidate(unsigned idx)
{

}

const map<string, vector<string>> InputMethod::init_valid_pinyin_dict()
{
    map<string, vector<string>> ret;
    for(auto& pinyin : InputMethod::valid_pinyin) {
        for(int i = 0; i < pinyin.size(); ++i) {
            string token = pinyin.substr(0, i + 1);
            auto it = ret.find(token);
            if (it == ret.end())
                ret[token] = vector<string>{pinyin};
            else {
                auto& [_, pinyins] = *it;
                if (find(pinyins.begin(), pinyins.end(), pinyin) == pinyins.end())
                    pinyins.push_back(pinyin);
            }
        }
    }
    return ret;
}

const vector<string> InputMethod::init_valid_pinyin_tokens()
{
    vector<string> ret;
    for (auto& [token, _] : InputMethod::valid_pinyin_dict)
        ret.push_back(token);
    std::sort(ret.begin(), ret.end());
    return ret;
}

} // namespace blitz
