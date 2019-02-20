#include "AGo.h"
#include <iostream>

bool check_valid(const Action & a, const Board & b)
{
  return true;
}

Move check_eye(const Action & a, const Board & b)
{
  return Move(); 
}

bool move(Tree * cur_node, Tree * next_node, Board b)
{
  auto a = next_node -> a;
  if ( check_valid(a, b) ) {
    b[a] = static_cast<std::string>(a)[0] == 'b' ?
      BoardFlag::black : BoardFlag::white;
  }
  else return false;
}

unsigned get_seed()
{
  typedef std::chrono::high_resolution_clock hclock;
  unsigned seed = hclock::now().time_since_epoch().count();
  seed ^= std::hash<std::thread::id>()(std::this_thread::get_id());
  return seed;
}

std::tuple<col_t, row_t> str2coord(const std::string &ind)
{
  col_t col_ind = COLUMN_INDICES.find( ind[0] );
  row_t row_ind;
  std::stringstream ss(ind.substr(1)); // eg. turn "19"(string) to 19(int)
  ss >> row_ind;

  return std::make_tuple(col_ind, row_ind - 1);
}

// ************** class Action ************** {{{
#define THROW_ACTION_CONVERSION_ERR throw \
  std::runtime_error("Action conversion error.")

Action::Action(const std::string &action)
{
  if (action.size() < 3)      THROW_ACTION_CONVERSION_ERR;
  if (action[0] == 'b')       this->code = 0;
  else if (action[0] == 'w')  this->code = MAX_STONE_NUM;
  else                        THROW_ACTION_CONVERSION_ERR;
  col_t col; row_t row;
  std::tie(col, row) = str2coord(action.substr(1));
  if (col > 18 || row > 18)  THROW_ACTION_CONVERSION_ERR;
  this->code += col + row * 19;
}

Action::operator std::string()
{
  col_t col = this->code % MAX_STONE_NUM % 19;
  row_t row = this->code % MAX_STONE_NUM / 19;
  std::string ret;
  std::stringstream ss;
  ss << row + 1; ss >> ret;            // now ret should be e.g. 2, 9, 15
  ret = COLUMN_INDICES[col] + ret; // now ret should be e.g. A2, B9, T15
  ret = (this->code < MAX_STONE_NUM?'b':'w') + ret;
                                   // now ret should be e.g. bA2, wB9, bT15
  return ret;
}

#undef THROW_ACTION_CONVERSION_ERR
// }}}

// ************** MonteCarloTree ************** {{{

void MonteCarloTree::expand()
{
  std::vector<Move> valid_moves;

  // enumerate moves that are valid
  bool is_next_step_black = static_cast<std::string>
    (_cur_node->a)[0] == 'r' || 'w' ?
    true : false;
  char a[5] = "xxxx";
  for ( auto col : "ABCDEFGHJKLMNOPQRST" )
    for ( auto row : {1,2,3,4,5,6,7,8,9,10,
        11,12,13,14,15,16,17,18,19} ) {
      a[0] = is_next_step_black ? 'b' : 'w';
      a[1] = col;
      if (row >= 10) {
        a[2] = '1'; a[3] = row % 10 + 48; a[4] = 0;
      } else { a[2]= row + 48; a[3] = 0; }

      Action tmp(a);
      if ( check_valid(tmp, _cur_board) ) {
        valid_moves.push_back(check_eye(tmp, _cur_board));
      }
    }

  _pruner(valid_moves, _cur_board);

  for ( auto &move : valid_moves ) {
    _cur_node -> append_child(move);
  }
}

void MonteCarloTree::simulate()
{
  auto children = _cur_node -> children;
  for ( auto iter = children.begin(); iter != children.end(); iter++ ) {

  }
}

void MonteCarloTree::back_propagation()
{

}

// }}}

// *************** class Tree *************** {{{
void Tree::add_dirichlet_noise(float epsilon = 0.25, float alpha = 0.03)
{ // epsilon and alpha could be found in the paper.
  auto child_cnt = children.size();

  auto dirichlet_vector = std::vector<float>{};
  std::gamma_distribution<float> gamma(alpha, 1.0f);
  std::mt19937_64 gen(get_seed());
  for (size_t i = 0; i < child_cnt; i++) {
    dirichlet_vector.emplace_back(gamma(gen));
  }

  auto sample_sum = std::accumulate(begin(dirichlet_vector),
      end(dirichlet_vector), 0.0f);

  // If the noise vector sums to 0 or a denormal, then don't try to
  // normalize.
  if (sample_sum < std::numeric_limits<float>::min()) {
    return;
  }

  for (auto& v : dirichlet_vector) {
    v /= sample_sum;
  }

  child_cnt = 0;
  for (auto& child : children) {
    auto eta_a = dirichlet_vector[child_cnt++];
    child->p = child->p * (1 - epsilon) + epsilon * eta_a;
  }
}
void Tree::append_child(const Move & m)
{
  Action a_; Prisoners prisoners_;
  std::tie(a_, prisoners_) = m;
  Tree * child = new Tree(a_, prisoners_);
  child->parent = this;
  children.push_back(child);
}

// }}}
