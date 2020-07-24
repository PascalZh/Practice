#include "core.hpp"

namespace blitz {

InputMethod::InputMethod()
  : m_dl(make_unique<DataLoader>()), m_lex(Lexicon::create()),
  m_mode(Mode::Normal)
{
  stringstream ss(m_dl->read_data());
  for (Record r; ss >> r;)
    m_lex->insert(move(r));
}

void InputMethod::set_candidates()
{
  Expects(std::all_of(m_input_seq.begin(), m_input_seq.end(), check_input));
  switch (m_mode) {
    case Mode::Normal:
      set_candidates_normal();
      return;
  }
}

void InputMethod::sort_candidates()
{
  std::sort(m_candidates.begin(), m_candidates.end(),
      [](const Record* lhs, const Record* rhs)
      { return lhs->freq < rhs->freq; } );
}

const vector<Record> InputMethod::get_candidates() const
{
  vector<Record> result;
  result.reserve(m_candidates.size());
  for (auto& v : m_candidates) {
    if (std::count(v->pinyin.begin(), v->pinyin.end(), '\'') + 1
        == m_tokens.size())
      result.push_back(*v);
  }
  return result;
}

void InputMethod::set_candidates_normal()
{
  this->tokenize();

  auto cartesian_product =
    [](const vector<string>& lhs, const vector<string>& rhs)
    -> const vector<string>
  {
      if (lhs.empty()) return rhs; if (rhs.empty()) return lhs;

      vector<string> ret;
      for (auto& l : lhs) for (auto& r : rhs) ret.emplace_back(l + "'" + r);
      return ret;
  };

  auto find_candidates =
    [this](const vector<string>& pp)
    {
      if (m_candidates.empty()) {
        set<Record*> candidates;
        for (auto& pinyin : pp) {
          auto cand = m_lex->find_all(pinyin);
          candidates.insert(cand.begin(), cand.end());
        }
        std::copy(candidates.begin(), candidates.end(), back_inserter(m_candidates));

      } else {
        m_candidates.erase(
            std::remove_if(
              m_candidates.begin(),
              m_candidates.end(),
              [&pp](const Record* r)
              {
                return std::all_of(
                    pp.begin(),
                    pp.end(),
                    [&r](const string& pinyin)
                    { return not r->pinyin.starts_with(pinyin); }
                    );
              }
              ),
            m_candidates.end()
            );
      }

      m_pp.clear();
      for (auto record : m_candidates) 
        m_pp.push_back(record->pinyin);
    };

  if (m_tokens.size() == 1) {
    // TODO set candidates when there is only one token.
  } else if (m_tokens.size() == 2) {
    m_pp = cartesian_product(
        syllable_map.at(m_tokens[0]), syllable_map.at(m_tokens[1]));
    find_candidates(m_pp);

  } else {
    find_candidates(cartesian_product(m_pp, vector<string>(1, m_tokens.back())));
  }
}

void InputMethod::tokenize()
{
  TimeIt ti("tokenize");
  Expects(join(m_tokens, "") == m_input_seq.substr(0, m_input_seq.size() - 1));
  if (m_tokens.empty()) {
    m_tokens.push_back(m_input_seq);
    return;
  }
  string new_ch = m_input_seq.substr(m_input_seq.size() - 1, 1);
  string maybe_token = m_tokens.back() + new_ch;
  if (std::binary_search(valid_token.begin(), valid_token.end(), maybe_token))
    m_tokens.back() = maybe_token;
  else
    m_tokens.push_back(new_ch);
}

bool InputMethod::choose_the_candidate(size_t idx)
{
  if (not idx < m_candidates.size())
    return false;
  //auto& cand = m_candidates[idx];
  //m_lex->set_freq(cand.pinyin, cand.word, [](int x) { return x + 1; });
  return true;
}

//┌────────────────────────────────────────────────────────────────────────────┐
//│                   init static variables                                    │
//└────────────────────────────────────────────────────────────────────────────┘

const map<string, vector<string>> InputMethod::init_syllable_map()
{
  map<string, vector<string>> ret;
  for(auto& pinyin : InputMethod::valid_syllable) {
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

const vector<string> InputMethod::init_valid_token()
{
  vector<string> ret;
  for (auto& [token, _] : InputMethod::syllable_map)
    ret.push_back(token);
  std::sort(ret.begin(), ret.end());
  return ret;
}

} // namespace blitz
