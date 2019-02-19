#include "AGo.h"
#include <iostream>

bool check_valid(const Action & a, const Board & b)
{
  return true;
}

Move check_eye(const Action & a, const Board & b)
{
  return ret; 
}

bool move(Tree * cur_node, Tree * next_node, Board b)
{
  auto a = next_node -> a;
  if ( check_valid(a, b) ) {
    b[a] = static_cast<std::string>(a)[0] == 'b' ? BoardFlag::black : BoardFlag::white;
  }
  else return false;
}

inline bool is_node_root(const Tree *node) { return node->a == Action::root; }

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
unsigned get_seed()
{
  typedef std::chrono::high_resolution_clock hclock;
  unsigned seed = hclock::now().time_since_epoch().count();
  seed ^= std::hash<std::thread::id>()(std::this_thread::get_id());
  return seed;
}

void Tree::add_dirichlet_noise(float epsilon = 0.25, float alpha = 0.03) // epsilon and alpha could be found in the paper.
{
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
