#pragma once

#include <iostream>
#include <vector>
#include <array>
#include <limits>
#include <cstdlib>
#include <ctime> 
#include <chrono>

namespace i2a {

using namespace std;

template<typename T> using Array = vector<T>;
using Key = int;

int randint(int a, int b)
{
    return (rand() % (b - a + 1)) + a;
}

Array<Array<Key>> test_gen_uniform(size_t len, size_t n)
{
    srand(time(NULL));
    Array<Array<Key>> ret;
    for (size_t i = 0; i < n; ++i) {
        Array<Key> a;
        for (size_t j = 0; j < len; ++j) {
            Key x = randint(-1000, 1000);  
            a.push_back(x);
        }
        ret.push_back(a);
    }
    return ret;
}

class Profile {
public:
    Profile(string p = "", string s = "\n") : prefix(p), suffix(s) {}
    ~Profile()
    {
        auto end = chrono::high_resolution_clock::now();
        chrono::duration<double, milli> ms = end - start;
        cout << prefix <<  ms.count() << "ms" << suffix;
    }
private:
    chrono::time_point<chrono::high_resolution_clock> start = chrono::high_resolution_clock::now();
    string prefix;
    string suffix;
};

} /* namespace i2a */ 
