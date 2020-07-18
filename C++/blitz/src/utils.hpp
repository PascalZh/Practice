#pragma once
#include <string>
#include <vector>
#include <sstream>
#include <iostream>
#include <exception>
#include <chrono>
#include <map>
#include <functional>

void split(const std::string& s, std::vector<std::string>& tokens, const std::string& delimiters);
std::vector<std::string> split(const std::string& s, const std::string& delimiters);

std::string join(const std::vector<std::string>& v, const std::string& delimiters);

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

namespace termcolor {

inline auto wrap_with(std::string code, std::string text)
{
    return "\033[" + code + "m" + text +"\033[0m";
}

inline std::string red(std::string text)     { return wrap_with("31", text); }
inline std::string green(std::string text)   { return wrap_with("32", text); }
inline std::string yellow(std::string text)  { return wrap_with("33", text); }
inline std::string blue(std::string text)    { return wrap_with("34", text); }
inline std::string magenta(std::string text) { return wrap_with("35", text); }
inline std::string cyan(std::string text)    { return wrap_with("36", text); }
inline std::string white(std::string text)   { return wrap_with("37", text); }

} // namespace termcolor

class TimeIt
{
    static constexpr auto now = std::chrono::high_resolution_clock::now;

    struct Info {
        double time;
        long count;
    };
public:
    static inline std::map<std::string, Info> dict;

    explicit TimeIt (std::string name = "default")
        : m_name(name)
        , m_start(now()) {}
    ~TimeIt ()
    {
        std::chrono::duration<double, std::milli> ms = now() - m_start;

        auto it = dict.find(m_name);
        if (it == dict.end())
            dict[m_name] = {ms.count(), 1};
        else {
            dict[m_name].time += ms.count();
            dict[m_name].count += 1;
        }
    }

    static void show();

private:
    std::string m_name;
    std::chrono::time_point<std::chrono::high_resolution_clock> m_start;
};

#ifndef UTILS_DISABLE_STREAM_OPERATOR_87
template <class T>
std::ostream& operator<<(std::ostream& out, const std::vector<T>& vec)
{
    for (auto& v : vec)
        out << v << " ";
    return out;
}

template <class Key, class Value>
std::ostream& operator<<(std::ostream& out, const std::map<Key, Value>& dict)
{
    for (auto& [key, value] : dict)
        out << key << ": " << value << std::endl;
    return out;
}
#endif
