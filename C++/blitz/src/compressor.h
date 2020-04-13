#pragma once
#include <iostream>

class Compressor {
public:
    using string = std::string;
    Compressor() {}

    string encode(string str)
    {
        return str;
    }
    string decode(string str)
    {
        return str;
    }
};
