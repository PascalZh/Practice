#pragma once
#include <iostream>
#include <chrono>
#include <string>

template <class T>
void print_list(const T& lst)
{
    for (auto& x : lst) {
        std::cout << x << " ";
    }
    std::cout << std::endl;
}

class Profile {
public:
    Profile(std::string p = "", std::string s = " ") : prefix(p), suffix(s) {}
    ~Profile()
    {
        auto end = std::chrono::high_resolution_clock::now();
        std::chrono::duration<double, std::milli> ms = end - start;
        std::cout << prefix <<  ms.count() << "ms" << suffix;
    }
private:
    std::chrono::time_point<std::chrono::high_resolution_clock> start = std::chrono::high_resolution_clock::now();
    std::string prefix;
    std::string suffix;
};
