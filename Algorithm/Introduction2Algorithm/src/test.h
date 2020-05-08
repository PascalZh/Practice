#pragma once

#include <iostream>
#include <vector>
#include <array>
#include <memory>
#include <limits>
#include <cstdlib>
#include <ctime> 

#include "utils.h"

namespace i2a {

template<typename T> using Array = std::vector<T>;
using Key = int;

int randint(int a, int b)
{
    return (std::rand() % (b - a + 1)) + a;
}

Array<Array<Key>> test_gen_uniform(size_t len, size_t n)
{
    Array<Array<Key>> ret;
    for (size_t i = 0; i < n; ++i) {
        Array<Key> a;
        for (size_t j = 0; j < len; ++j) {
            Key x = randint(-50000, 50000);  
            a.push_back(x);
        }
        ret.push_back(a);
    }
    return ret;
}

template<typename T>
void test(T sort_func, std::vector<int> sizes={10, 100, 1000, 10000, 100000},
        int times=100, bool print=false, bool profile=true)
{
    using std::cout; using std::endl;
    for (int i : sizes) {
        std::unique_ptr<Profile> p;
        std::cout << i << ":";
        if (profile)
            p = print ? std::make_unique<Profile>("", "\n") : std::make_unique<Profile>("", "\t");
        for (auto& arr : test_gen_uniform(i, times)) {
            if (print) { cout << "before:"; print_list(arr); }
            sort_func(arr);
            if (print) { cout << "after: "; print_list(arr); }
        }
    }
    cout << endl;
}

#define TEST(func) cout << #func << ":" << endl; \
    test(func); cout << endl;

} /* namespace i2a */ 
