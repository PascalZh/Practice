#pragma once
#include <iostream>

template <class T>
void print_list(const T& lst)
{
    for (auto& x : lst) {
        std::cout << x << " ";
    }
    std::cout << std::endl;
}
