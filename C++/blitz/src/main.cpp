#include "core.h"
#include <memory>
using std::unique_ptr;

int main()
{
    unique_ptr<WordQueryBase> w(new WordQuerySimple());
    w -> query("ni", 100);
    return 0;
}
