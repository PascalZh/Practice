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
  std::sort(m_pc.begin(), m_pc.end(),
      [](const Record* lhs, const Record* rhs)
      { return lhs->freq < rhs->freq; } );
}

const vector<Record> InputMethod::get_candidates() const
{
  vector<Record> result;
  result.reserve(m_candidates.size());
  for (auto& v : m_candidates) {
    if (std::count(v->pinyin.begin(), v->pinyin.end(), '\'') + 1
        == m_tokens.size()) {
      result.push_back(*v);
    }
  }
  return result;
}

void InputMethod::set_candidates_normal()
{
  auto append_token = [](const vector<string>& lhs, const string& token)
    -> const vector<string>
  {
      vector<string> ret;
      for (auto& l : lhs)
        ret.emplace_back(l + "'" + token);
      return ret;
  };

  auto find_candidates = [this](const vector<string>& pp)
  {
    if (m_pc.empty()) {
      set<Record*> candidates;
      for (auto& pinyin : pp) {
        auto cand = m_lex->find_all(pinyin);
        candidates.insert(cand.begin(), cand.end());
      }
      std::copy(candidates.begin(), candidates.end(), back_inserter(m_pc));

    } else {
      vector<Record*> new_candidates;
      for (auto r : m_pc) {
        if (
            std::count(r->pinyin.begin(), r->pinyin.end(), '\'') + 1 >= m_tokens.size() and
            std::any_of(pp.begin(), pp.end(),
              [&r](const string& pinyin)
              { return r->pinyin.starts_with(pinyin); })
           )
          new_candidates.push_back(r);
      }
      m_pc = move(new_candidates);
    }

    for (auto& v : m_pc) {
      if (std::count(v->pinyin.begin(), v->pinyin.end(), '\'') + 1
          == m_tokens.size()) {
        m_candidates.push_back(v);
      }
    }

    m_pp.clear();
    for (auto record : m_pc) 
      m_pp.push_back(record->pinyin);
  };

  this->tokenize();

  if (m_tokens.size() == 1) {
    // TODO set candidates when there is only one token.
    m_pp = syllable_map.at(m_tokens[0]);
  } else {
    find_candidates(append_token(m_pp, m_tokens.back()));
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

bool InputMethod::choose_candidate(size_t idx)
{
  if (not idx < m_pc.size())
    return false;
  ++(m_candidates[idx]->freq);
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
