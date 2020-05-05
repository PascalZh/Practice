#include <iostream>

#include "test.h"

namespace i2a {

class Counter {
public:
    Counter(size_t m) : _max(m) {}
    size_t max() { return this -> _max; }
    int value() { return _value(count); }
    void increase()
    {
        int toss = randint(0, _value(count) - _value(count + 1) - 1);
        if (toss == 0)
            ++count;
        if (count > _max)
            throw;
    }
private:
    int _value(size_t cnt) { return cnt * 10; }
    size_t _max;
    size_t count = 0;
};

} /* namespace i2a */

using namespace i2a;
int main(int argc, char *argv[])
{
    using std::cout; using std::endl;
    // must call srand before use of Counter.
    srand(time(NULL));
    for (size_t k = 0; k < 100; ++k) {
        Counter c(10000);
        for (size_t i = 0; i < 10000; ++i)
            c.increase();
        cout << c.value() << " ";
    }
    cout << endl;
    return 0;
}
