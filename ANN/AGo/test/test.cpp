#include "../AGo.h"

using std::cout; using std::cin; using std::endl;
#define PRT_TST(exp) cout << #exp "\t= " << exp << endl
class MarkTest { public: MarkTest(std::string s) : _s(s){cout << "<<<<<<<<<<<<<<<< " << s << ":" << endl;}
  std::string _s; ~MarkTest() {cout << ">>>>>>>>>>>>>>>> " << _s << " finish" << endl << endl;} };
#define MARKTEST(s) MarkTest marktest(s)

void testRandom()
{
  MARKTEST("testRandom");
  for (int i = 0; i < 1000; i++) {
    cout << get_seed() << endl;
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
  testRandom();
  testTree();
  testAction();
  PRT_TST(sizeof(unsigned));
  PRT_TST(sizeof(int32_t));

  AGoTree t;
  t.start_search_loop();
  cout << "test finish..." << endl;

  return 0;
}
