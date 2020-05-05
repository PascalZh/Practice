#pragma once
#include <iostream>
#include <chrono>
#include <string>
#include <sstream>

template <class T>
void print_list(const T& lst)
{
    for (auto& x : lst) {
        std::cout << x << " ";
    }
    std::cout << std::endl;
}

template <class T>
std::string show_list(const T& lst)
{
    std::stringstream ss;
    std::string s;
    std::string ret;
    for (auto& x : lst) {
        ss << x;
        ss >> s;
        ret += s + " ";
    }
    return ret.substr(0, ret.size() - 1);
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
