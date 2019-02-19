#include "../AGo.h"

#include <iostream>

using std::cout; using std::cin; using std::endl;
#define PRT_TST(exp) cout << #exp "\t= " << exp << endl;

void testTree()
{
  cout << "testTree:" << endl;
  PRT_TST(sizeof(Tree))

  cout << "Tree contains:" << endl;
  PRT_TST(sizeof(Tree *))
  PRT_TST(sizeof(Action))
  PRT_TST(sizeof(Prisoners))
  PRT_TST(sizeof(std::vector<Tree *>))
  cout << endl;

  PRT_TST(sizeof(Move))
  PRT_TST(sizeof(State))
  PRT_TST(sizeof(Board))
  cout << "finish testTree" << endl;
}

void testAction()
{
  Action a("wA1");
  cout << std::string(a) << endl;
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
