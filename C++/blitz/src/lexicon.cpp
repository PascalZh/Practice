#include "lexicon.hpp"
namespace blitz {

class LexiconImpl : public Lexicon
{
private:
  using Block = vector<Record>;
  vector<Block> m_data;

  static constexpr int MAX_RECORDS = 256;

public:
  bool insert(Record e);
  bool erase(const string& pinyin, const string& word) { return true; }
  vector<std::reference_wrapper<Record>> find_all(const string& pinyin);

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

bool LexiconImpl::insert(Record record)
{
  auto ub = std::upper_bound(m_data.begin(), m_data.end(), record,
      [](const Record& r, const Block& block) {
      return r.pinyin < block.front().pinyin;
      });
  // TODO
}

vector<std::reference_wrapper<Record>> LexiconImpl::find_all(const string& pinyin)
{
  // see struct PYRange, two ranges are equivalent if they intersect.
  vector<std::reference_wrapper<Record>> result;

  return result;
}

void LexiconImpl::show_map_node() const
{
}

} /* namespace blitz */ 
