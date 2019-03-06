#ifndef AGO_H_
#define AGO_H_

#include <functional>
#include <vector>
#include <memory>
#include <tuple>
#include <forward_list>
#include <array>
#include <map>

#include <iostream>
#include <fstream>
#include <string>
#include <regex>

#include <algorithm>
#include <cassert>
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
#include <boost/filesystem.hpp>

//#include <Python.h>

namespace ago {

  using namespace fmt::literals; using fmt::print;
  namespace bp = boost::process;
  namespace fs = boost::filesystem;
  using boost::lexical_cast;
  using std::cout; using std::cin; using std::endl;
  using std::string; using std::vector; using std::map; using std::array; using std::tuple;
  using std::make_tuple; using std::function; using std::for_each;
  using std::unique_ptr; using std::shared_ptr; using std::make_shared;
  using std::thread; using std::mutex; using std::atomic;
  using std::condition_variable; using std::unique_lock;
  using std::numeric_limits; using std::runtime_error;

  class Action;
  struct Tree;
  struct Board;
  class MonteCarloTree;

  enum class board_flag : char { empty=0, black, white, eye_b, eye_w };
  board_flag &operator+=(board_flag &lhs, const int &rhs);
  board_flag &operator-=(board_flag &lhs, const int &rhs);
  using bf = board_flag;

  using Stone = Action;
  using Prisoners = std::forward_list<Stone>;
  using Move = tuple<Action, Prisoners>;
  // Move is light version of Tree removing children and parent;
  using State = tuple<Tree *, Board>;

  // param: current node; return: node
  using Pruner = function<void (vector<Move>, Board)>;
  using Simulator = function<void ()>;
  using col_t = size_t;
  using row_t = size_t;

  namespace utils {

    extern unsigned get_seed();
    extern tuple<col_t, row_t> str2coord(const string &s);

  }

  // Other:

  // interface with python
  //extern unique_ptr<vector<float>> py_list2array(PyObject *list);

  class Action {
    // 0 ~ MAX_STONE_NUM - 1                 : bA1 ~ bT19
    // MAX_STONE_NUM ~ 2 * MAX_STONE_NUM - 1 : wA1 ~ wT19
    // 2 * MAX_STONE_NUM                     : bpass
    // 2 * MAX_STONE_NUM + 1                 : wpass
    // numeric_limits<code_t>J::max()        : root

    private:
      using code_t = uint16_t;
      code_t code;
      static constexpr code_t MAX_STONE_NUM = 19 * 19;

    public:
      static const string column_indices;
      static const Action root; // Action is default to be root.
      Action() : code(numeric_limits<code_t>::max()) {}
      Action(bf color, col_t col, row_t row);
      explicit Action(const string &action);
      operator string() const;

      bf color() const; // root's color is bf::empty
      tuple<col_t,row_t> coord() const;
      inline bool is_pass() const {
        return code == 2 * MAX_STONE_NUM || code == 2 * MAX_STONE_NUM + 1; }
      inline bool operator ==(const Action &rhs) const { return code == rhs.code; }
      inline bool operator !=(const Action &rhs) const { return code != rhs.code; }
  };

  struct Tree {
    Action a; // eg: a = "bA12", "wP2"   b is black, w is white
    Tree *parent;
    vector<Tree *> children;
    Prisoners prisoners; // once this action has captured opponent's stones,
    // these stones will be recorded in this variable.

    float w; unsigned n; // notation is consistent with the paper
    float p;
    unique_ptr<float[]> dist; // size: 1+19*19+1, first: value, last: probability of pass
    thread_local static unsigned seed;

    Tree(const Action & a_ = Action::root)
      : a(a_), parent(nullptr), w(0.0f), n(0), p(0), dist{} {}
    ~Tree() {for_each(children.begin(), children.end(), [](Tree *node){delete node;});}

    inline bool is_leaf() const { return children.empty(); }
    void push_back_child(const Action &a, Board &b);
    inline void erase_children(vector<Tree *>::iterator itr);
    inline void clear_children();

    void add_dirichlet_noise(float epsilon, float alpha);

    static bool is_game_end(Tree *cur_node, Board &board);
    static bool check_valid_move(const Action &action, Board &board);
    static tuple<int,int> calc_score(Board &board);

    static void move(Tree *, Tree *, Board &);
    static void undo_move(Tree *, Board &);

    private:
    Tree(const Action &a_, Tree *parent_, Prisoners && p)
      : a(a_), parent(parent_), prisoners(p), w(0.0f), n(0), p(0), dist{} {}

    Move action2move(const Action &a, Board &b);
  };

  struct Board {
    static const tuple<col_t,row_t> no_stone;

    bf _board[19][19];
    tuple<col_t, row_t> _cur_stone = no_stone;
    Board();

    inline bf &operator [](const string & ind) { return (*this)[utils::str2coord(ind)]; }
    inline bf &operator [](const tuple<col_t,row_t> &coord);
    inline bf *operator [](int i) { assert(i < 19); return _board[i]; }

    friend std::ostream &operator <<(std::ostream &out, Board &board);
  };

  class MonteCarloTree
  {
    public:
      MonteCarloTree(Pruner);
    protected:
      using F = function<void ()>;
      // you can pass any arg through lambda's capture feature
      F select;
      F expand;
      F simulate;
      F back_propagation;

      void search() { select(); expand(); simulate(); back_propagation(); }

    public:
      void genmove();
      ~MonteCarloTree ();

    protected:
      Tree *_cur_node; // it refers to current state of play during the game.
      Board _cur_board; // corresponds to _cur_node.
      vector<Move> _valid_moves;
      Pruner _pruner, _pre_pruner;
  };

  class AGoTree : MonteCarloTree {
    public:
      AGoTree();
      ~AGoTree();
      void genmove();
      void start_selfplay();
    private:
      function<float (Tree *)> PUCT;
      static int ref_count; // Only one object is allowed to exist at the same time

      bool is_nn_ready;
      void init_nn();
      void stop_nn();

      // threaded search
      void search();
      void retrieve_pv();

      // io
      boost::process::ipstream task_in;
      boost::process::opstream task_out;
      boost::process::child *p_c;

      size_t core_num;
      map<unsigned long,unique_ptr<float[]>> pvs;
      // buffer that store (p, v) result of nn.
      vector<unique_ptr<Tree>> trees;

      // other

      // hyperparameters
      float c_puct;
      size_t num_simulate;
  };

}

#include "AGo.inl"
#endif /* ifndef AGO_H_ */
