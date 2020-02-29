#include "core.h"
#include <memory>
using std::unique_ptr; using std::cin; using std::make_unique;

int main()
{
    auto w = make_unique<WordQuerySimple>();
    w -> query("ni", 100);
    cin.get();
    return 0;
}
