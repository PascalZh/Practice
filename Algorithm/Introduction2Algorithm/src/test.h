#pragma once

#include <iostream>
#include <vector>
#include <array>
#include <memory>
#include <limits>
#include <cstdlib>
#include <functional>
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

struct TestConfig {
    std::function<Array<Array<int>> (size_t, size_t)> gen = test_gen_uniform;
    std::vector<int> sizes = {10, 100, 1000, 10000, 20000};
    int times = 100;
    bool print = false;
    bool profile = true;
};

template<typename T>
void test(T sort_func, TestConfig cfg = TestConfig())
{
    using std::cout;
    using std::endl;
    for (int i : cfg.sizes) {
        std::unique_ptr<Profile> p;
        cout << i << ": ";
        if (cfg.profile)
            p = cfg.print ? std::make_unique<Profile>("", "\n")
                      : std::make_unique<Profile>("", "\t");

        for (auto& arr : cfg.gen(i, cfg.times)) {
            if (cfg.print) { cout << "before:"; print_list(arr); }
            sort_func(arr);
            if (cfg.print) { cout << "after: "; print_list(arr); cout << endl; }
        }
    }
    cout << endl;
}

#define TEST(func) cout << #func << ":" << endl; \
    test(func); cout << endl;

} /* namespace i2a */ 
