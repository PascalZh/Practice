#include <cstdlib>
#include <memory>
#include "../AGo.h"


using std::cout; using std::cin; using std::endl;
#define PRT_TST(exp) cout << #exp "\t= " << exp << endl
class MarkTest { public: MarkTest(std::string s) : _s(s){cout << "<<<<<<<<<<<<<<<< " << s << ":" << endl;}
  std::string _s; ~MarkTest() {cout << ">>>>>>>>>>>>>>>> " << _s << " finish" << endl << endl;} };
#define MARKTEST(s) MarkTest marktest(s)

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
}

void testAction()
{
  MARKTEST("testAction");
  Action a("wA1"), a1("bB1"), a2("wT19");
  cout << "Action a(\"wA1\");" << endl;
  PRT_TST(std::string(a));
  Board b;
  PRT_TST(int(b["T19"]));
  PRT_TST(int(b["T19"] = BoardFlag::white));
  PRT_TST(int(b["A1"] = BoardFlag::black));

}

void show_id()
{
  auto thread_id = std::this_thread::get_id();
  cout << "thread id:" << thread_id << endl;
}

void testPython()
{
  MARKTEST("testPython");
  auto thread_id = std::this_thread::get_id();
  cout << "main thread: " << thread_id << endl;

  Py_Initialize();
  std::system("export PYTHONPATH=.:$PYTHONPATH");
  // This is very important!!!

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

  std::string str_thread_id;
  std::stringstream ss; ss << thread_id;
  ss >> str_thread_id;

  auto args = Py_BuildValue("(s)", str_thread_id.c_str());
  auto pRet = PyObject_CallObject(pFunc, args);

  auto ret = py_list2array(pRet);
  for (auto &m : *ret) {
    cout << m << endl;
  }

  std::vector<std::thread> tasks;
  for (unsigned i = 0; i < std::thread::hardware_concurrency(); i++)
    tasks.push_back(std::thread(show_id));
  for (auto &task : tasks)
    task.join();

  Py_Finalize();
}
int main(int argc, char *argv[])
{
  testTree();
  testAction();
  testPython();
  PRT_TST(sizeof(unsigned));
  PRT_TST(sizeof(unsigned int));
  PRT_TST(sizeof(short));
  PRT_TST(sizeof(int32_t));
  return 0;
}
