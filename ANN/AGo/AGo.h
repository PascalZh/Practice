#ifndef __AGO_H__
#define __AGO_H__

#include <functional>
#include <vector>
#include <tuple>
#include <forward_list>
#include <string>
#include <sstream>
#include <cstring>
#include <algorithm>
#include <stdexcept>

struct Tree;
struct Board;
class MonteCarloTree;
enum class BoardFlag : char { none, white, black };

std::string column_indices = "ABCDEFGHJKLMNOPQRST";

using Action = std::string;
using Position = Action;
using Prisoners = std::forward_list<Position>;
using Move = std::tuple<Action, Prisoners>; // Move is light version of Tree removing children and parent;
using State = std::tuple<Tree *, Board>;

using Selector = std::function<State (State)>; // param: current node; return: node
using Pruner = std::function<void (std::vector<Move>, Board)>;
using Simulator = std::function<void ()>;
//using GoRule = std::function<std::vector<Move> (Board, bool)>; // return all possible positions (Tree data structure contains info of prisoners if action be taken); the second param defines either next stone is black or white.

extern bool check_valid(const Action &, const Board &);
extern Move check_eye(const Action &, const Board &); // check whether there exists an eye once action taken.
extern bool move(Tree *, Tree *, Board);


struct Tree {
  Tree * parent;
  Action a; // eg: a = "bA12", "wP2"   b is black, w is white
  Prisoners prisoners; // once this action has captured opponent's stones, these stones will be recorded in this variable.
  std::vector<Tree *> children;

  void append_child(const Move & m)
  {
    Action a_; Prisoners prisoners_;
    std::tie(a_, prisoners_) = m;
    Tree * child = new Tree(a_, prisoners_);
    child->parent = this;
    children.push_back(child);
  }
  bool is_leaf() { return children.empty(); }
  explicit Tree( const Action & a_ ) : a(a_) {}
  Tree( const Action & a_, const Prisoners & prisoners_) : a(std::move(a_)), prisoners(std::move(prisoners_)) {}
  ~Tree()
  {
    std::for_each(children.begin(), children.end(),
        [](Tree * node) { delete node; });
  }
};

struct Board {
  BoardFlag _board[19][19];
  Board() {
    for (int i = 0; i < 19; i++)
      for (int j = 0; j < 19; j++)
        _board[i][j] = BoardFlag::none;
  }

  char column_index = '\0';
  BoardFlag & operator[] (const std::string & ind)
  {
    auto col_ind = column_indices.find( ind[0] );
    int row_ind;
    std::stringstream ss(ind.substr(1));
    ss >> row_ind;
    if ( col_ind == std::string::npos || row_ind < 1 || row_ind > 19  )
      throw std::range_error("AGo::Board index out of range!");
    return _board[col_ind][row_ind];
  }
};

class MonteCarloTree
{
  public:
    MonteCarloTree(Selector selector_)
      :_cur_root(new Tree(Action("root"))), _selector(selector_)
    {}
  private:
    inline void select();
    void expand();
    inline void simulate();
    inline void back_propagation();

    inline void search() { select(); expand(); simulate(); back_propagation(); }

  public:
    void start_search_loop();
    ~MonteCarloTree ()
    {
      while ( strcmp(_cur_root -> a.c_str(), "root") != 0)
        _cur_root = _cur_root -> parent;
      delete _cur_root;
    }

  private:
    Tree * _cur_root; // it refers to current state of play during the game.
    Tree * _cur_node; // it refers to current state of play during the MCTS.
    Board _cur_board; // corresponds to _cur_node.
    std::vector<Move> _valid_moves;

    Selector _selector;
    Pruner _pruner, _pre_pruner;
    // _pre_pruner could be a CNN or other things, it works during expansion.
    // _pruner will work during simulate? I'm not sure.
};

#endif /* ifndef __AGO_H__ */
