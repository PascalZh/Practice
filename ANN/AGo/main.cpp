#include "AGo.h"
using namespace ago;

int main(int argc, char *argv[])
{
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
