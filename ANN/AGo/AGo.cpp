#include "AGo.h"

namespace ago {

  // ********* namespace utils *******{{{
  namespace utils {

    unsigned get_seed()
    {
      typedef std::chrono::high_resolution_clock hclock;
      uint32_t seed = hclock::now().time_since_epoch().count();
      seed ^= std::hash<std::thread::id>()(std::this_thread::get_id());
      return static_cast<unsigned>(seed);
    }

    string get_game_state(Tree *node, Board b)
    {
#define bf2str(x) (x == bf::black?char(int(bf::black)+int('0')):\
    char(int(bf::white)+int('0')))
      string ret = " "; ret[0] = bf2str(reverse_bf(node->a.color()));
      for (int k = 0; k < 8; k++) {
        string s(19*19,' ');
        for (int i = 0; i < 19; i++) {
          for (int j = 0; j < 19; j++) {
            if (node->a != Action::root) {
              s[i*19+j] = char(int(b[i][j]) + int('0'));
            } else {
              s[i*19+j] = '0';
            }
          }
        }
        ret += ":" + std::move(s);
        if (node->a != Action::root) {
          Tree::undo_move(node, b);
          node = node->parent;
        }
      }
      return ret;
#undef bf2str
    }
  }
  // }}}

  // ************** class Action ************** {{{

  const Action Action::root;
  const string Action::column_indices = "ABCDEFGHJKLMNOPQRST";

  Action::Action(const string &action)
  {
    if (action == "bpass") {
      this->code = 2 * MAX_STONE_NUM; return;
    } else if (action == "wpass") {
      this->code = 2 * MAX_STONE_NUM + 1; return;
    }
    if (action[0] == 'b') this->code = 0;
    else if (action[0] == 'w') this->code = MAX_STONE_NUM;


    col_t col; row_t row; std::tie(col, row) = utils::str2coord(action.substr(1));
    this->code += col * 19+ row;
  }

  Action::Action(bf color, col_t col, row_t row)
  {
    assert(color == bf::black || color == bf::white);
    assert(col < 19 && row < 19);
    if (color == bf::black) code = 0;
    else if (color == bf::white) code = MAX_STONE_NUM;
    code += col * 19 + row;
  }

  // }}}

  // ************** class MonteCarloTree ************** {{{

  MonteCarloTree::MonteCarloTree(Pruner pruner_ = nullptr)
    :_cur_node(new Tree), _pruner(pruner_)
  {

  }

  MonteCarloTree::~MonteCarloTree ()
  {
    while (_cur_node->a != Action::root)
      _cur_node = _cur_node->parent;
    delete _cur_node;
  }
  // }}}

  // *************** struct Tree *************** {{{
  thread_local unsigned Tree::seed=utils::get_seed();

  void Tree::add_dirichlet_noise(float epsilon = 0.25, float alpha = 0.03)
  { // epsilon and alpha could be found task_in the paper.
    auto child_cnt = children.size();

    auto dirichlet_vector = vector<float>{};
    std::gamma_distribution<float> gamma(alpha, 1.0f);
    std::mt19937_64 gen(this->seed);
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

  bool Tree::check_valid_move(const Action &a, Board &b)
  {
    if (!a.is_pass() && b[a.coord()] != bf::empty) { return false; }
    if (!a.is_pass()) {
      int i, j;
      std::tie(i, j) = a.coord();
      auto b_ = b;
      b_[i][j] = a.color();
      function<bool (int, int, const bf &)> check_Qi =
        [&b_, &check_Qi] (int col, int row, bf color) -> bool
        { // 返回值表示这个位子的棋子是否有气

          b_[col][row] += 2;
          if (col+1 < 19 && b_[col+1][row] == bf::empty ||
              col-1 >= 0 && b_[col-1][row] == bf::empty ||
              row+1 < 19 && b_[col][row+1] == bf::empty ||
              row-1 >= 0 && b_[col][row-1] == bf::empty) {
            return true;
          }
          if (col+1 < 19 && b_[col+1][row] == color
              && check_Qi(col+1, row, color) ||
              col-1 >= 0 && b_[col-1][row] == color
              && check_Qi(col-1, row, color) ||
              row+1 < 19 && b_[col][row+1] == color
              && check_Qi(col, row+1, color) ||
              row-1 >= 0 && b_[col][row-1] == color
              && check_Qi(col, row-1, color)) {
            return true;
          }
          return false;
        };
      return check_Qi(i, j, a.color());
    }
    return true;
  }

  tuple<int,int> Tree::calc_score(Board &board)
  {
#define TEST_PRINT \
    for (int m = 18; m >= 0; m--) {\
      for (int n = 0; n < 19; n++) {\
        if (b[n][m] == searched) cout << '+';\
        else if (b[n][m] == marker) cout << '*';\
        else cout << b[n][m];\
        cout << ' ';\
      }\
      cout << endl;\
    }\
    cout << endl;
    float white_score{}; float black_score{};
    int b[19][19];
    for (int i = 0; i < 19; i++) {
      for (int j = 0; j < 19; j++) {
        b[i][j] = int(board[i][j]);
        if (board[i][j] == bf::white) {
          ++white_score;
        }
        if (board[i][j] == bf::black) {
          ++black_score;
        }
      }
    }

    const int &searched = 8888;
    const int &marker = 8848;
    function<int (int, int)> mark_dipan =
      [&b, &mark_dipan, &searched, &marker] (int col, int row) -> int
      { // 返回值用来标记是白子或者黑子的底盘
        if (b[col][row] == int(bf::white)) {
          return int(bf::white);
        }
        if (b[col][row] == int(bf::black)) {
          return int(bf::black);
        }
        b[col][row] = marker;
        vector<int> ret;
        if (col+1 < 19 && b[col+1][row] != searched &&
            b[col+1][row] != marker) {
          ret.push_back(mark_dipan(col+1, row));
        }
        if (col-1 >= 0 && b[col-1][row] != searched &&
            b[col-1][row] != marker) {
          ret.push_back(mark_dipan(col-1, row));
        }
        if (row+1 < 19 && b[col][row+1] != searched &&
            b[col][row+1] != marker) {
          ret.push_back(mark_dipan(col, row+1));
        }
        if (row-1 >= 0 && b[col][row-1] != searched &&
            b[col][row-1] != marker) {
          ret.push_back(mark_dipan(col, row-1));
        }
        auto begin = ret.begin();
        auto end = std::remove_if(ret.begin(), ret.end(), [](int &x) { return x == 0; });
        if (std::any_of(begin, end, [](int &x){ return x==-1; })) {
          return -1;
        } else if (begin == end) {
          return 0; // 0 表示无法判断
        } else if (std::all_of(begin, end, [&ret](int &x){ return ret[0] == x; })) {
          return ret[0];
        } else {
          return -1; // -1用来表示这块地盘既不是白子的也不是黑子的
        }
      };
    for (int i = 0; i < 19; i++) {
      for (int j = 0; j < 19; j++) {
        if (b[i][j] == int(bf::empty)) {
          int res = mark_dipan(i, j);
          if (res == -1) {
            for (int i_ = 0; i_ < 19; i_++) {
              for (int j_ = 0; j_ < 19; j_++) {
                if (b[i_][j_] == marker) {
                  b[i_][j_] = searched;
                }
              }
            }
          } else {
            for (int i_ = 0; i_ < 19; i_++) {
              for (int j_ = 0; j_ < 19; j_++) {
                if (b[i_][j_] == marker) {
                  b[i_][j_] = searched;
                  if (res == int(bf::white)) {
                    ++white_score;
                  }
                  if (res == int(bf::black)) {
                    ++black_score;
                  }
                }
              }
            }
          }
          //TEST_PRINT;
          //cout << black_score << endl;
          //cout << white_score << endl;
        }
      }
    }
    return make_tuple(black_score, white_score);
  }

  Move Tree::action2move(const Action &a, Board &b)
  {
    if (!check_valid_move(a, b)) {
      cout << endl << "!!!!!!!!!!!!!!!!!!!!" << endl;
      cout << string(a) << b;
      assert(false);
    }

    if (a.is_pass()) {
      return make_tuple(a, Prisoners());
    }

    vector<size_t> pris_coord;
    function<bool (int, int, const bf &)> check_Qi =
      [&b, &check_Qi, &pris_coord] (int col, int row, bf color) -> bool
      { // 返回值表示这个位子的棋子是否有气

        b[col][row] += 2;
        pris_coord.push_back(col*19+row);
        if (col+1 < 19 && b[col+1][row] == bf::empty ||
            col-1 >= 0 && b[col-1][row] == bf::empty ||
            row+1 < 19 && b[col][row+1] == bf::empty ||
            row-1 >= 0 && b[col][row-1] == bf::empty) {
          return true;
        }
        if (col+1 < 19 && b[col+1][row] == color
            && check_Qi(col+1, row, color) ||
            col-1 >= 0 && b[col-1][row] == color
            && check_Qi(col-1, row, color) ||
            row+1 < 19 && b[col][row+1] == color
            && check_Qi(col, row+1, color) ||
            row-1 >= 0 && b[col][row-1] == color
            && check_Qi(col, row-1, color)) {
          return true;
        }
        return false;
      };

    Prisoners p;

    auto restore_board = [&b, &p, &pris_coord] (bool add_to_p) {
      for (auto &x : pris_coord) {
        b[x/19][x%19] -= 2;
        if (add_to_p) {
          p.push_front(Action(b[x/19][x%19], x/19, x%19));
        }
      }
      pris_coord.clear();
    };

    // put action in the board
    const auto &coord = a.coord();
    const auto &color = a.color();
    b[coord] = color;

    // check whether there exist different stones
    // in the directions: <>^V(left, right, up, down)
    const auto &col = std::get<0>(coord);
    const auto &row = std::get<1>(coord);

    for (int i : {1, 2, 3, 4}) {
      int _col_(col), _row_(row);
      switch(i) {
        case 1: _col_++; break; case 2: _col_--; break;
        case 3: _row_++; break; case 4: _row_--; break;
      }
      if (_col_ >= 0 && _col_ < 19 && _row_ >= 0 && _row_ < 19 &&
          b[_col_][_row_] != b[col][row] && b[_col_][_row_] != bf::empty) {
        restore_board(!check_Qi(_col_, _row_, b[_col_][_row_]));
      }
    }

    b[coord] = bf::empty;
    return make_tuple(a, std::move(p));
  }

  void Tree::move(Tree *cur_node, Tree *next_node, Board &b)
  {
    const Action &a = next_node->a;
    if (a.is_pass()) {
      b._cur_stone = Board::no_stone;
      return;
    }
    assert(b[a.coord()] == bf::empty);
    assert(next_node->parent == cur_node);


    // update b
    b._cur_stone = a.coord(); b[a.coord()] = a.color();
    for (auto &stone : next_node->prisoners) { b[stone.coord()] = bf::empty; }
  }

  void Tree::undo_move(Tree * cur_node, Board &b)
  {
    const Action &a = cur_node->a;
    if (a.is_pass()) {
      return;
    }
    assert(a != Action::root);
    assert(b[a.coord()] == a.color());

    // update _cur_stone and _board
    if (cur_node->parent->a == Action::root || cur_node->parent->a.is_pass()) {
      b._cur_stone = Board::no_stone;
    } else {
      b._cur_stone = cur_node->parent->a.coord();
    }

    b[a.coord()] = bf::empty;

    // update _board with prisoners
    for_each(cur_node->prisoners.begin(), cur_node->prisoners.end(),
        [&b] (Stone &stone) { b[stone.coord()] = stone.color(); });
  }

  // }}}

  // *************** class AGoTree *************** {{{

  // initialize static members
  int AGoTree::ref_count = 0;

  Tree * AGoTree::search()
  {
    // Initialize...
    assert(_cur_node->children.empty());
    // create a tree for every thread
    auto root = new Tree;
    root->parent = _cur_node->parent; root->a = _cur_node->a;
    root->prisoners = _cur_node->prisoners;
    // it fakes a node through copying _cur_code
    Tree *cur_node; Board cur_board;

    // {{{ subfunction
    auto select = [this, &cur_node, &cur_board, &root] ()
    { // return 1 if black win, return 2 if white win, return 0 if not over
      cur_node = root; cur_board = _cur_board;
      while(!cur_node->is_leaf()) {
        auto first = cur_node->children.begin();
        auto last = cur_node->children.end();
        auto largest = first;
        ++first;
        for (; first != last; ++first) {
          if (PUCT(*largest) < PUCT(*first)) { largest = first; }
        }
        cur_node = *largest;
        Tree::move(cur_node->parent, cur_node, cur_board);
      }

      if (Tree::is_game_end(cur_node, cur_board)) {
        // when game ends, let cur_node be a special leaf node, and do not expand
        float b; float w;
        std::tie(b, w) = Tree::calc_score(cur_board);
        return b > w?int(bf::black):(b < w?int(bf::white):0); // 0 for draw
      } else { return -1; }
    };

    auto expand = [&cur_node, &cur_board, &root] (float pv[]) {
      const auto &color_to_play = utils::reverse_bf(cur_node->a.color());
      cur_node->w = pv[0]; // value
      cur_node->n += 1;

      // build nodes with the probability of 19*19 moves
      for (int i = 1; i < 19*19+1; i++) {
        auto col = (i - 1) / 19; auto row = (i - 1) % 19; 
        Action a(color_to_play, col, row);
        if (Tree::check_valid_move(a, cur_board)) {
          cur_node->push_back_child(a, cur_board);
          cur_node->children.back()->p = pv[i];
        }
      }
      // build a node of pass
      string tmp_a = color_to_play == bf::black?"bpass":"wpass";
      cur_node->push_back_child(Action(std::move(tmp_a)), cur_board);
      cur_node->children.back()->p = pv[19*19+1];

      cur_node->add_dirichlet_noise();
    };

    // }}}

    for (int i = 0; i < num_simulate ; i++) {
      static float pv[19*19+1+1];
      // 1. Select
      int winner = select();
      // 2. Expand and Evaluate
      if (winner == -1) { // game is not over, continue evaluating
        string game_state = utils::get_game_state(cur_node, cur_board);

        if ( i != num_simulate - 1 )
          task_out << "send {}"_format(game_state) << endl;
        else
          task_out << "finish" << endl;

        string buf;
        task_in >> buf;
        if (buf == "receive") {
          for (int i = 0; i < 19*19+1+1; ++i) {
            task_in >> pv[i];
          }
        } else if (buf == "finish") {
          break;
        }
        expand(pv);
      } else { // the case that one wins
        int c = int(cur_node->a.color());
        if (winner == 0)
          cur_node->w = 0.5;
        else if (c == winner)
          cur_node->w = 1;
        else
          cur_node->w = 0;
      }

      // 3. Backpropagation
      float v{1.0f}; // v value
      bf v_color; // v_color means the color of 'v'
      if (winner == 0) { // draw
        while (cur_node != root) {
          auto &p = cur_node->parent;
          p->w += 0.5; p->n += 1;
          cur_node = p;
        }
        continue;
      } else if (winner == int(bf::black)) {
        v_color = bf::black;
      } else if (winner == int(bf::white)) {
        v_color = bf::white;
      } else if (winner == -1) {
        v = pv[0];
        v_color = cur_node->a.color();
      }

      while (cur_node != root) { // backup to cur_node's parents
        auto &p = cur_node->parent;
        if(p->a.color() == v_color) p->w += v; else p->w -= v;
        p->n += 1;
        cur_node = p;
      }
    }

    return root;
  }

  void AGoTree::genmove()
  {
    assert(std::numeric_limits<float>::has_infinity);
    assert(!Tree::is_game_end(_cur_node, _cur_board));
    if (!is_nn_ready)
      throw runtime_error("Neural Network(pytorch) is not ready!");

    auto root = search();

    int sum{}; unique_ptr<float[]> p(new float[19*19+1]{}); // p : move probability dist
    for (auto &c : root->children) {
      if (c->a.is_pass()) {
        p[19*19] += c->n;
      } else {
        const auto &ind = c->a.coord();
        p[std::get<0>(ind)*19+std::get<1>(ind)] += c->n;
      }
      sum += c->n;
    }
    if (sum != 0) for (int i = 0; i < 19*19+1; ++i) { p[i] /= sum; }

    // choose one position to put on stone
    size_t ind{999};
    int discrete_dist[19*19+1];
    for (int i = 0; i < 19*19+1; ++i) {discrete_dist[i] = p[i]*1000 + 1;}
    if (tau == std::numeric_limits<float>::infinity()) {
      float max = -1;
      for (int i = 0; i < 19*19+1; i++) {
        if (p[i] > max) { max = p[i]; ind = i; }
      }
    } else {
      if ( tau != 1.0f )
        for (int i = 0; i < 19*19+1; ++i) {
          discrete_dist[i] = std::pow(discrete_dist[i], 1 / tau);
        }
    }

    std::random_device rd;
    std::mt19937 gen(rd());
    std::discrete_distribution<> d(discrete_dist, discrete_dist+19*19+1);
    ind = ind != 999 ? ind : d(gen);
    const auto &color_to_play = utils::reverse_bf(_cur_node->a.color());

    Action action_to_gen;
    do {
      if (ind == 19*19) { // choose pass
        string tmp_a = color_to_play == bf::black?"bpass":"wpass";
        action_to_gen = Action(std::move(tmp_a));
      } else {
        action_to_gen = Action(color_to_play, ind/19, ind%19);
      }
    } while ( !Tree::check_valid_move(action_to_gen, _cur_board)
        && ((ind = d(gen)) || true) );
    _cur_node->push_back_child(action_to_gen, _cur_board);

    Tree::move(_cur_node, _cur_node->children[0], _cur_board);
    _cur_node->dist = std::move(p);
    _cur_node = _cur_node->children[0];
    cout << _cur_board;
  }

  void AGoTree::start_selfplay()
  {
    while(!Tree::is_game_end(_cur_node, _cur_board)) {
      genmove();
    }
    float b; float w; std::tie(b, w) = Tree::calc_score(_cur_board);
    int winner;
    if (b > w) winner = int(bf::black); // int(bf::black) = 1
    if (b < w) winner = int(bf::white);
    if (b == w) winner = 0;

    // serializing the data
    static const string tmp = "data/{}/tmp"_format(std::this_thread::get_id());
    auto file = std::ofstream(tmp);
    while (_cur_node->a != Action::root) {
      auto &p = _cur_node->parent;
      Tree::undo_move(_cur_node, _cur_board);
      s = utils::get_game_state(p, _cur_board);

      file << s << "=";

      for (int i = 0; i < 19*19+1; ++i)
        file << p->dist[i] << ":";
      if (winner == 0 || p->a == Action::root)
        file << "d";
      else if (winner == int(p->a.color()))
        file << "w ";
      else
        file << "l";

      file << "=" << string(p->a) << endl;
      _cur_node = p;
    }
    while (_cur_node->a != Action::root)
      _cur_node = _cur_node->parent;
    _cur_node->clear_children();
  }

  AGoTree::AGoTree()
    : num_simulate(1600), c_puct(4.0f), tau(1.0f)
  {
    if ( ref_count != 0 )
      throw runtime_error("AGoTree object is constructed in multiple times.");
    ++ref_count;
    init_nn();
    PUCT = [this] (Tree *node)
    {
      assert(node->p > 0.0f);
      float Q = node->n == 0?0:node->w / node->n;
      auto &p = node->parent;
      assert(std::accumulate(p->children.begin(), p->children.end(), 0,
            [] (int n, Tree *rhs) { return n + rhs->n; }) == int(p->n - 1)
          || p->children.empty());
      int sum_n = p->n;
      float U = c_puct * node->p * std::sqrt(sum_n) / float(1 + node->n);
      return Q + U;
    };

    cout << "Please input c_puct:";
    cin >> c_puct;
    cout << "Please input num_simulate:";
    cin >> num_simulate;
  }

  AGoTree::~AGoTree() { stop_nn(); --ref_count; }

  // }}}

  // *************** struct Board *************** {{{
  const tuple<col_t,row_t> Board::no_stone = make_tuple(19, 19);
  Board::Board() {
    for (int i = 0; i < 19; i++)
      for (int j = 0; j < 19; j++)
        _board[i][j] = bf::empty;
  }
  std::ostream &operator <<(std::ostream &out, Board &board)
  {
    constexpr auto ban = "   a b c d e f g h j k l m n o p q r s t";

    col_t cur_i; col_t cur_j;
    std::tie(cur_i, cur_j) = board._cur_stone;

    cout << ban << endl;
    for (int j = 18; j >= 0; --j) {
      string line = " . . . . . . . . . . . . . . . . . . . ";

      if (j == 3 || j == 9 || j == 15) {
        line[2*3+1] = '+';
        line[2*9+1] = '+';
        line[2*15+1] = '+';
      }

      const auto &b = board._board;
      for (int i = 18; i >= 0; --i) {
        assert(b[i][j] == bf::black || b[i][j] == bf::white || b[i][j] == bf::empty);
        if (b[i][j] == bf::black)
          line[2*i+1] = 'X';
        else if (b[i][j] == bf::white)
          line[2*i+1] = 'O';
      }

      // mark _cur_stone
      if (j == int(cur_j) && board._cur_stone != Board::no_stone) {
        line[2*cur_i] = '('; line[2*cur_i+2] = ')';
      }

      auto row_num = "{:>2}"_format(j+1);
      line = row_num + line + row_num;
      cout << line << endl;
    }
    cout << ban << endl;
    return out;
  }
  // }}}

}
