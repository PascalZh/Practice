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
    std::sort(m_candidates.begin(), m_candidates.end(),
            [](const Record& lhs, const Record& rhs) { return lhs.freq < rhs.freq; } );
}

const vector<Record>& InputMethod::get_candidates() const
{
    return m_candidates;
}

void InputMethod::set_candidates_normal()
{
    this->tokenize();

    vector<vector<string>> pp; // a collection of sets of possible pinyin
    std::transform(m_tokens.begin(), m_tokens.end(), std::back_inserter(pp),
            [](const string& token) { return valid_pinyin_dict.at(token); });

    auto cartesian_product = [](vector<string> lhs, vector<string> rhs)
    {
        if (lhs.empty()) return rhs; if (rhs.empty()) return lhs;

        vector<string> ret;
        for (auto& l : lhs) for (auto& r : rhs)
                ret.emplace_back(l + "'" + r);
        return ret;
    };
    vector<string> possible_pinyins = std::accumulate(pp.begin(), pp.end(),
            vector<string>{}, cartesian_product);
    for (auto& pinyin : possible_pinyins) {
        auto records = m_lex->find_all(pinyin);
        m_candidates.insert(m_candidates.end(), records.begin(), records.end());
    }
}

void InputMethod::tokenize()
{
    TimeIt it("tokenize");
    assert(join(m_tokens, "") == m_input_seq.substr(0, m_input_seq.size() - 1));
    if (m_tokens.empty()) {
        m_tokens.push_back(m_input_seq);
        return;
    }
    string new_ch = m_input_seq.substr(m_input_seq.size() - 1, 1);
    string maybe_token = m_tokens.back() + new_ch;
    if (std::binary_search(valid_pinyin_tokens.begin(), valid_pinyin_tokens.end(), maybe_token))
        m_tokens.back() = maybe_token;
    else
        m_tokens.push_back(new_ch);
}

bool InputMethod::choose_the_candidate(size_t idx)
{
    if (not idx < m_candidates.size())
        return false;
    auto& cand = m_candidates[idx];
    m_lex->set_freq(cand.pinyin, cand.word, [](int x) { return x + 1; });
    return true;
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
