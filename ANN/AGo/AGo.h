#ifndef __AGO_H__
#define __AGO_H__

#include <functional>
#include <vector>
#include <memory>
#include <tuple>
#include <forward_list>
#include <array>
#include <map>

#include <iostream>
#include <string>

#include <algorithm>
#include <stdexcept>
#include <chrono>

#include <thread>
#include <mutex>
#include <condition_variable>
#include <atomic>

#include <limits>
#include <random>

#include <fmt/format.h>
#include <fmt/ostream.h>

#include <boost/lexical_cast.hpp>
#include <boost/process.hpp>
#include <boost/asio.hpp>

//#include <Python.h>

class Action;
struct Tree;
struct Board;
class MonteCarloTree;
enum class BoardFlag : char { none, white, black };

using Position = Action;
using Prisoners = std::forward_list<Position>;
using Move = std::tuple<Action, Prisoners>;
// Move is light version of Tree removing children and parent;
using State = std::tuple<Tree *, Board>;

// param: current node; return: node
using Pruner = std::function<void (std::vector<Move>, Board)>;
using Simulator = std::function<void ()>;
using col_t = size_t;
using row_t = size_t;

extern bool check_valid(const Action &, const Board &);
extern Move check_eye(const Action &, const Board &);
// check whether there exists an eye once action taken.
extern bool move(Tree *, Tree *, Board);
extern unsigned get_seed();
extern std::tuple<col_t, row_t> str2coord(const std::string &s);

// interface with python
//extern unique_ptr<vector<float>> py_list2array(PyObject *list);

class Action {
  private:
    using code_t = uint16_t;
    code_t code;
    static constexpr code_t MAX_STONE_NUM = 19 * 19;
    // 0 ~ MAX_STONE_NUM - 1             : bA1 ~ bT19
    // MAX_STONE_NUM ~ 2 * MAX_STONE_NUM - 1 : wA1 ~ wT19
    // 2 * MAX_STONE_NUM                 : none

  public:
    static const Action none; // Action is default to be none.
    Action() : code(2 * MAX_STONE_NUM) {}
    explicit Action(const std::string &action);
    operator std::string();
    bool operator ==(const Action &rhs) const { return code == rhs.code; }
    bool operator !=(const Action &rhs) const { return code != rhs.code; }
};

struct Tree {
  Action a; // eg: a = "bA12", "wP2"   b is black, w is white
  Tree * parent;
  Prisoners prisoners; // once this action has captured opponent's stones,
  // these stones will be recorded in this variable.
  std::vector<Tree *> children;

  float w; unsigned n;
  float p;
  thread_local static unsigned seed;

  explicit Tree(const Action & a_, Tree *parent_)
    : a(std::move(a_)), parent(parent_) {}
  Tree(const Action & a_, Tree *parent_, const Prisoners & prisoners_)
    : a(std::move(a_)), parent(parent_), prisoners(std::move(prisoners_)) {}
  ~Tree() {
    std::for_each(children.begin(), children.end(),
        [](Tree * node) { delete node; });
  }

  void append_child(const Move & m);
  bool is_leaf() { return children.empty(); }

  void add_dirichlet_noise(float epsilon, float alpha);
};

struct Board {
  BoardFlag _board[19][19];
  Board() {
    for (int i = 0; i < 19; i++)
      for (int j = 0; j < 19; j++)
        _board[i][j] = BoardFlag::none;
  }

  BoardFlag & operator [](const std::string & ind)
  {
    col_t col_ind; row_t row_ind;
    std::tie(col_ind, row_ind) = str2coord(ind);
    if ( col_ind > 18 || row_ind > 18  ) // unsigned variable is always >= 0
      throw std::range_error("AGo::Board index out of range!");
    return _board[col_ind][row_ind];
  }
};

class MonteCarloTree
{
  public:
    MonteCarloTree(Pruner pruner_ = nullptr)
      :_cur_root(new Tree(Action::none, nullptr)),
      _pruner(pruner_) {}
  private:
    void select();
    void expand();
    void simulate();
    void back_propagation();

    void search() { select(); expand(); simulate(); back_propagation(); }

  public:
    void start_search_loop();
    ~MonteCarloTree ()
    {
      while (_cur_root->a != Action::none)
        _cur_root = _cur_root -> parent;
      delete _cur_root;
    }

  private:
    Tree * _cur_root; // it refers to current state of play during the game.
    Tree * _cur_node; // it refers to current state of play during the MCTS.
    Board _cur_board; // corresponds to _cur_node.
    std::vector<Move> _valid_moves;

    Pruner _pruner, _pre_pruner;
    // _pre_pruner could be a CNN or other things, it works during expansion.
    // _pruner will work during simulate? I'm not sure.
};

class AGoTree : MonteCarloTree {
  public:
    AGoTree()
    {
      init_nn();
      if (ref_count > 0)
        throw std::runtime_error("AGoTree created more than 1 objects!");
      else ++ref_count;
    }
    ~AGoTree() { stop_nn(); }
    void start_search_loop();
  private:
    static const std::function<float ()> PUCT;
    static int ref_count;
    static std::vector<std::tuple<unsigned long,std::array<float,362>>> pvs;

    // Only one object is allowed to exist at the same time
    bool is_nn_ready;
    void init_nn();
    void stop_nn();

    // threaded search
    static void search();
    static void retrieve_pv();
    // io
    static std::string buf;
    static boost::process::ipstream task_in;
    static boost::process::opstream task_out;
    static boost::process::child *p_c;

};

#endif /* ifndef __AGO_H__ */
