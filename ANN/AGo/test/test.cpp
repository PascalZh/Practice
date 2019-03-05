#include "../AGo.h"
using namespace ago;
#define PRT_TST(exp) cout << #exp "\t= " << exp << endl

class MarkTest {
  public:
    MarkTest(std::string s) : _s(s){cout << "<<<<<<<<<<<<<<<< " << s << ":" << endl;}
    std::string _s;
    ~MarkTest() {cout << ">>>>>>>>>>>>>>>> " << _s << " finish" << endl << endl;}
};
#define MARKTEST(s) MarkTest marktest(s)

class TimeBenchMark {
  typedef std::chrono::high_resolution_clock hclock;
  std::chrono::time_point<hclock> t;
  public:
    TimeBenchMark() { t = hclock::now(); }
    ~TimeBenchMark() { cout << std::chrono::duration_cast<std::chrono::microseconds>(hclock::now() - t).count() / 1000000.0f << "s" << endl; }
};
#define TIMETEST TimeBenchMark time_bench_mark

void testRandom()
{
  MARKTEST("testRandom");
  for (int i = 0; i < 1000; i++) {
    cout << utils::get_seed() << endl;
  }

}

void testTree()
{
  MARKTEST("testTree");
  PRT_TST(sizeof(Tree));

  cout << "Tree contains:" << endl << endl;
  PRT_TST(sizeof(Tree *));
  PRT_TST(sizeof(Action));
  PRT_TST(sizeof(Prisoners));
  PRT_TST(sizeof(std::vector<Tree *>));
  cout << endl;

  PRT_TST(sizeof(Move));
  PRT_TST(sizeof(State));
  PRT_TST(sizeof(Board));

  // test move ... 
//#define CHECK_MEM_LEAK
#ifdef CHECK_MEM_LEAK
  for (int i = 0; i < 1000000; i++) { // check whether has memory leak
#endif
    Board b;
    Tree *root = new Tree;
    Tree *p = root;
    for (auto a : {"bT1", "bT2", "wS1"}) {
      p->push_back_child(Action(a), b);
      Tree::move(p, p->children[0], b);
      cout << b;
      p = p->children[0];
    }
    //while (p != root) {
      //Tree::undo_move(p, b);
      //cout << b;
      //p = p->parent;
    //}
    p->clear_children();
#ifndef CHECK_MEM_LEAK
    //for (string a; cin >> a; ) {
      //p->push_back_child(Action(a), b);
      //p->move(p, p->children[0], b);
      //p = p->children[0];
      //cout << b;
    //}
#endif
    delete root;
#ifdef CHECK_MEM_LEAK
  }
  //cin.get();
#endif
}

void testAction()
{
  MARKTEST("testAction");
  Action a("wA1"), a1("bB1"), a2("wT19");
  cout << "Action a(\"wA1\");" << endl;
  PRT_TST(static_cast<string>(a));
  Board b;
  PRT_TST(int(b["T19"]));
  PRT_TST(int(b["T19"] = bf::white));
  PRT_TST(int(b["A1"] = bf::black));

}

void testcalc_score()
{
  MARKTEST("testcalc_score");
  Board b;
  b[0][0] = bf::black;
  b[1][2] = bf::black;
  b[1][1] = bf::black;
  b[0][3] = bf::black;
  b[17][0] = bf::white;
  b[17][1] = bf::white;
  b[17][2] = bf::white;
  b[17][3] = bf::white;
  b[17][4] = bf::white;
  b[16][0] = bf::black;
  b[16][1] = bf::black;
  b[16][2] = bf::black;
  b[16][3] = bf::black;
  b[16][4] = bf::black;
  b[18][5] = bf::white;
  b[17][5] = bf::black;
  b[18][6] = bf::black;
  Tree::calc_score(b);
}
/*
   void show_id(PyObject * pFunc)
   {
   auto thread_id = std::this_thread::get_id();
   cout << "thread id: {}"_format(thread_id) << endl;

   PyGILState_STATE gstate;
   gstate = PyGILState_Ensure();

   auto args = Py_BuildValue("(s)", "{}"_format(thread_id).c_str());
// can't call python function at the same time!
auto ret = PyObject_CallObject(pFunc, args);
Py_DECREF(ret);

PyGILState_Release(gstate);
}

void testPython(char * argv0)
{
MARKTEST("testPython");
auto thread_id = std::this_thread::get_id();
cout << "main thread: {}"_format(thread_id) << endl;

size_t len_argv0 = std::strlen(argv0);
Py_SetProgramName(Py_DecodeLocale(argv0, &len_argv0));
Py_Initialize();

std::string pwd_ = std::getenv("PWD");
std::wstring pwd; pwd.assign(pwd_.begin(), pwd_.end());
auto pythonhome = Py_GetPythonHome();
std::wstring path = pythonhome?pythonhome:L"";
path = path.size()?path + L":" + pwd:pwd;
Py_SetPythonHome(const_cast<wchar_t *>(path.c_str()));
std::wcout << "PYTHONHOME:" << Py_GetPythonHome() << endl;

auto pModule = PyImport_Import(PyUnicode_FromString("test.test_python"));
if (!pModule) {
cout << "module import fail!" << endl;
return;
}
cout << "module import succeed!" << endl;

auto pFunc = PyObject_GetAttrString(pModule, "hello_world");
if (!pFunc || !PyCallable_Check(pFunc))
{
cout << "function import fail!" << endl;
return;
}
cout << "function import succeed!" << endl;

PyEval_InitThreads(); 
PyEval_ReleaseThread(PyThreadState_Get());  

auto args = Py_BuildValue("(s)", "{}"_format(thread_id).c_str());
auto pRet = PyObject_CallObject(pFunc, args);

auto ret = py_list2array(pRet);
for (auto &m : *ret) {
cout << m << endl;
}

std::vector<std::thread> tasks;
for (unsigned i = 0; i < std::thread::hardware_concurrency(); i++)
tasks.push_back(std::thread(show_id, pFunc));
for (auto &task : tasks)
task.join();

Py_DECREF(pModule); Py_DECREF(pFunc);
Py_Finalize();
}
*/
int main(int argc, char *argv[])
{
  //testRandom();
  //testTree();
  //testAction();
  //testcalc_score();
  //PRT_TST(sizeof(int));
  //PRT_TST(sizeof(int32_t));
  //PRT_TST(int(bf::none));

  TIMETEST;
  int i;
  cout << "Input how many games you want to play" << endl;
  cin >> i;
  AGoTree t;
  while(i--) {

    t.start_selfplay();
  }
  cout << "test finish..." << endl;

  return 0;
}
