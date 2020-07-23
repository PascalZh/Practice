#include "lexicon.hpp"
namespace blitz {

class LexiconImpl : public Lexicon
{
private:
  using Block = vector<Record>;
  vector<Block> m_data;

  static constexpr int MAX_RECORDS = 256;
  bool insert_into(decltype(m_data)::iterator it, const Record& record);

public:
  bool insert(const Record& e);
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

bool LexiconImpl::insert_into(decltype(m_data)::iterator it, const Record& record)
{
#ifdef INSERT_NEW_BLOCK
#error "INSERT_NEW_BLOCK defined!"
#endif
#define INSERT_NEW_BLOCK(code)\
  Block new_block;\
  new_block.reserve(MAX_RECORDS);\
  new_block.push_back(record);\
  code\
  m_data.insert(++it, move(new_block));

  if (it->back() < record) {
    if (it->size() < MAX_RECORDS) {
      it->push_back(record);
    } else {
      INSERT_NEW_BLOCK(;);
    }
    return true;
  }

  auto needle = std::lower_bound(it->begin(), it->end(), record,
      [](const Record& lhs, const Record& rhs) { return lhs.pinyin < rhs.pinyin; } );

  if (std::binary_search(needle, it->end(), record))
    return false;

  if (it->size() < MAX_RECORDS) {
    it->insert(needle, record);
  } else {
    // if some records in *it have the same pinyin as the record, they must be
    // moved into the new block.

    if (needle == it->end()) {
      INSERT_NEW_BLOCK(;);
    } else {
      INSERT_NEW_BLOCK(
          new_block.insert(new_block.end(), needle, it->end());
      );
      it->erase(needle, it->end());
    }
  }
  return true;
}

bool LexiconImpl::insert(const Record& record)
{
  if (m_data.empty()) {
    Block b;
    b.push_back(record);
    m_data.emplace_back(move(b));
    return true;
  } else {
    auto ub = m_data.back().back() < record ? m_data.end()
      : std::upper_bound(m_data.begin(), m_data.end(), record,
          [](const Record& r, const Block& block) {
          return r.pinyin < block.front().pinyin;
          });
    auto it = ub == m_data.begin() ? ub : --ub;
    return insert_into(it, record);
  }
}

vector<std::reference_wrapper<Record>> LexiconImpl::find_all(const string& pinyin)
{
  // see struct PYRange, two ranges are equivalent if they intersect.
  vector<std::reference_wrapper<Record>> result;
  auto it_block = std::lower_bound(m_data.begin(), m_data.end(), pinyin,
      [](const string& pinyin, const Block& block) {
      return pinyin < block.front().pinyin;
      });
  auto it = std::lower_bound(it_block->begin(), it_block->end(), pinyin,
      [](const string& pinyin, const Record& record) {
      return pinyin < record.pinyin;
      });
  while (it_block != m_data.end()) {
    while (it != it_block->end() and is_substr_of(pinyin, it->pinyin)) {
      result.push_back(std::ref(*it));
      ++it;
    }
    if (it == it_block->end()) {
      ++it_block;
      it = it_block == m_data.end() ? it : it_block->begin();
      continue;
    } else {
      break;
    }
  }

  return result;
}

void LexiconImpl::show_map_node() const
{
}

} /* namespace blitz */ 
