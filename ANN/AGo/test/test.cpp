#include "../AGo.h"

#include <iostream>

using std::cout; using std::cin; using std::endl;
#define PRT_TST(exp) cout << #exp "\t= " << exp << endl
#define AGO_TST_START cout << "<<<<<<<<<<<<<<<<" << endl
#define AGO_TST_STOP cout << ">>>>>>>>>>>>>>>>" << endl << endl

void testTree()
{
  AGO_TST_START;
  cout << "testTree:" << endl;
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
  AGO_TST_STOP;
}

void testAction()
{
  AGO_TST_START;
  cout << "testAction:" << endl << endl;
  Action a("wA1"), a1("bB1"), a2("wT19");
  cout << "Action a(\"wA1\");" << endl;
  PRT_TST(std::string(a));
  Board b;
  PRT_TST(int(b["T19"]));
  PRT_TST(int(b["T19"] = BoardFlag::white));
  PRT_TST(int(b["A1"] = BoardFlag::black));

  AGO_TST_STOP;
}
int main(int argc, char *argv[])
{
  testTree();
  testAction();
  PRT_TST(sizeof(unsigned));
  PRT_TST(sizeof(unsigned int));
  PRT_TST(sizeof(short));
  PRT_TST(sizeof(int32_t));
  return 0;
}
