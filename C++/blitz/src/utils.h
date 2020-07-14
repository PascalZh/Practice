#pragma once
#include <string>
#include <vector>
#include <sstream>
#include <iostream>
#include <exception>

void split(const std::string& s, std::vector<std::string>& tokens, const std::string& delimiters);
std::vector<std::string> split(const std::string& s, const std::string& delimiters);

std::string join(std::vector<std::string> v, const std::string& delimiters);

int str2int(const std::string& s);

std::string int2str(int i);

template <class T>
std::string to_binary(T x)
{
    constexpr size_t bit_width = sizeof(T) * 8;
    std::string ret(bit_width, '0');
    for (int i = 0; i < bit_width; ++i) {
        ret[bit_width - i - 1] = ((x & (1 << i)) >> i) == 0 ? '0' : '1';
    }
    return ret;
}

template<class Target, class Source>
typename std::enable_if<not std::is_same<Target, std::string>::value, Target>::type
lexical_cast(const Source& arg)
{
    Target result;
    std::stringstream interpreter;
    if (!(interpreter << arg && interpreter >> result))
        throw std::runtime_error("bad_cast");

    return result;
}

template<class Target, class Source>
typename std::enable_if<std::is_same<Target, std::string>::value, Target>::type
lexical_cast(const Source& arg)
{
    std::stringstream interpreter;
    if (!(interpreter << arg))
        throw std::runtime_error("bad_cast");

    return interpreter.str();
}
