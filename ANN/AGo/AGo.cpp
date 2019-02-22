#include "AGo.h"

using namespace fmt::literals;
using boost::lexical_cast;
const std::string COLUMN_INDICES = "ABCDEFGHJKLMNOPQRST";

using std::cout; using std::cin; using std::endl;
using std::vector; using std::unique_ptr; using std::array;
using std::tuple;

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
  uint32_t seed = hclock::now().time_since_epoch().count();
  seed ^= std::hash<std::thread::id>()(std::this_thread::get_id());
  return static_cast<unsigned>(seed);
}

tuple<col_t, row_t> str2coord(const std::string &ind)
{
  col_t col_ind = COLUMN_INDICES.find( ind[0] );
  row_t row_ind = lexical_cast<row_t>(ind.substr(1));
  // eg. turn "19"(string) to 19(int)

  return std::make_tuple(col_ind, row_ind - 1);
}

/*
unique_ptr<vector<float>> py_list2array(PyObject *list)
{
  auto ret = std::make_unique<vector<float>>();
  PyObject *iter = PyObject_GetIter(list);

  if (!iter)
    cout << "iter error!" << endl;

  while (true) {
    PyObject *next = PyIter_Next(iter);
    if (!next) break;
    ret->push_back(PyFloat_AsDouble(next));
    cout << ret->back() << endl;
  } 
  return ret;
}
*/

// ************** class Action ************** {{{
#define THROW_ACTION_CONVERSION_ERR throw \
  std::runtime_error("Action conversion error.")

const Action Action::none;
Action::Action(const std::string &action)
{
  if (action.size() < 3)      THROW_ACTION_CONVERSION_ERR;
  if (action[0] == 'b')       this->code = 0;
  else if (action[0] == 'w')  this->code = MAX_STONE_NUM;
  else                        THROW_ACTION_CONVERSION_ERR;
  col_t col; row_t row;
  std::tie(col, row) = str2coord(action.substr(1));
  if (col > 18 || row > 18)   THROW_ACTION_CONVERSION_ERR;
  this->code += col + row * 19;
}

Action::operator std::string()
{
  col_t col = this->code % MAX_STONE_NUM % 19;
  row_t row = this->code % MAX_STONE_NUM / 19;
  std::string ret = fmt::to_string(row + 1);
  // now ret should be e.g. 2, 9, 15
  ret = COLUMN_INDICES[col] + ret;
  // now ret should be e.g. A2, B9, T15
  ret = (this->code < MAX_STONE_NUM?'b':'w') + ret;
  // now ret should be e.g. bA2, wB9, bT15
  return ret;
}

#undef THROW_ACTION_CONVERSION_ERR
// }}}

// ************** class MonteCarloTree ************** {{{

void MonteCarloTree::select() {}
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

void MonteCarloTree::start_search_loop()
{
}

// }}}

// *************** struct Tree *************** {{{
thread_local unsigned Tree::seed=get_seed();
void Tree::add_dirichlet_noise(float epsilon = 0.25, float alpha = 0.03)
{ // epsilon and alpha could be found task_in the paper.
  auto child_cnt = children.size();

  auto dirichlet_vector = std::vector<float>{};
  std::gamma_distribution<float> gamma(alpha, 1.0f);
  std::mt19937_64 gen(seed);
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
  Tree * child = new Tree(a_, this, prisoners_);
  children.push_back(child);
}

// }}}

// *************** class AGoTree *************** {{{
using std::thread; using std::mutex; using std::atomic;
using std::condition_variable; using std::unique_lock;
using fmt::print;
namespace bp = boost::process;

// initialize static members
int AGoTree::ref_count = 0;
std::string AGoTree::buf;
bp::ipstream AGoTree::task_in;
bp::opstream AGoTree::task_out;
bp::child *AGoTree::p_c = nullptr;

// globals for thread
mutex mtx;
condition_variable cv;
size_t core_num = thread::hardware_concurrency();
atomic<size_t> cnt_search_thd(core_num);
// count how many threads that run at AGoTree::search
atomic<size_t> remaining_retrieval(0);
// count how many data returned from python that retrieve_pv need to get

std::map<thread::id, bool> forbid_access_pvs;
// once retrieve_pv has retrieved data, it give every search thread
// a chance to find whether the data is its own data.

vector<tuple<unsigned long,array<float,19*19+1>>> AGoTree::pvs(core_num);
// buffer that store (p, v) result of nn.

atomic<size_t> code_color(36); // print with different color in threads
void AGoTree::search()
{
  auto color = "\033[{}m"_format(code_color--);
  std::string reset = "\033[0m";

  auto id = std::this_thread::get_id();
  unsigned long task_id;
  std::stringstream ss;
  ss << id; ss >> task_id;

  print(color + "Thread {0} init! seed({1})\n"_format(id, Tree::seed)
      + reset);

  mtx.lock();
  forbid_access_pvs[id] = true;
  // this operation actual modifies a tree, and it is not thread-safe.
  mtx.unlock();

  for (int i = 0; i < 2; i++) {
    // 1. Select
    // 2. Expand and Evaluate
    // Get current state.

    task_id = task_id + i;
    // put task into queue
    mtx.lock();
    cout << color << "Thread " + "{id}({i}) "_format("id"_a=id, "i"_a=i)
      << reset << "has put task into python\n";
    task_out << "task_id {}"_format(task_id) << endl;
    mtx.unlock();
    ++remaining_retrieval;

    bool is_pv_gotten = false; // get data from python
    while (true) {
      unique_lock<mutex> lck(mtx);

      while (forbid_access_pvs[id])
        cv.wait(lck); // this will unlock the mtx
      print(color + "<**Thread {0} acquired "_format(id)
          + reset);
      forbid_access_pvs[id] = true;
      for (auto itr = pvs.begin(); itr != pvs.end(); ++itr) {
        if (std::get<0>(*itr) == task_id) {
          // if *itr is the result of this search
          // save or apply...
          auto &pv = std::get<1>(*itr);
          cout << "{:*^15}"_format(pv[0]);
          pvs.erase(itr);
          is_pv_gotten = true; break;
        }
      }
      if (!is_pv_gotten) cout << "{:*^15}"_format("");
      print(color + " released**>\n" + reset);
      if (is_pv_gotten) break;
    }

    // 3. Backpropagation
  }
  --cnt_search_thd;
  print("Thread {} exited normally...\n"_format(id));
}

void AGoTree::retrieve_pv()
{
  print("Thread retrieve_pv init.\n");
  size_t cnt_idle = 0;
  while (true) {
    if (remaining_retrieval > 0) {
      cnt_idle = 0;
      // retrieve... and save in the pvs
      task_in >> buf;
      if (buf == "task_done") {
        task_in >> buf;
        auto task_id = lexical_cast<unsigned long>(buf);
        array<float, 19*19+1> pv;
        for (int i = 0; i < 19*19+1; ++i) {
          task_in >> buf;
          pv[i] = lexical_cast<float>(buf);
        }
        pvs.push_back(std::make_tuple(task_id, std::move(pv)));
        // lexical_cast will check the data, so
        // succeed in receiving data
        --remaining_retrieval;
        unique_lock<mutex> lck(mtx);
        for (auto &p : forbid_access_pvs) {
          p.second = false;
        }
        cv.notify_all();
        print("\033[31mThread retrieve_pv: notify_all: data ready\033[0m\n");
      }
    } else {
      ++cnt_idle;
      std::this_thread::sleep_for(std::chrono::milliseconds(100));
      if (cnt_idle > 5 || cnt_search_thd == 0) {
        break;
      }
      cout << cnt_idle << endl;
    }
  }
  print("retrieve_pv Thread exited normally\n");
}

void AGoTree::start_search_loop()
{
  if (!is_nn_ready) {
    throw std::runtime_error("Neural Network(pytorch) is not ready!");
  } else {
    vector<thread> tasks;

    for (unsigned i = 0; i < core_num; i++)
      tasks.push_back(thread(search));

    thread retrieve_task(retrieve_pv);

    for (auto &task : tasks)
      task.join();

    retrieve_task.join();
  }
}

void AGoTree::init_nn()
{
  is_nn_ready = true;
  p_c = new bp::child("./AGo.py", bp::std_in < task_out,
      bp::std_out > task_in);
}

void AGoTree::stop_nn()
{
  delete p_c;
}

// }}}
