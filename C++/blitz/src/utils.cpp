#include "utils.hpp"
    
using std::vector; using std::string;
void split(const string& s, vector<string>& tokens, const string& delimiters)
{
    string::size_type lastPos = s.find_first_not_of(delimiters, 0);
    string::size_type pos = s.find_first_of(delimiters, lastPos);
    while (string::npos != pos || string::npos != lastPos) {
        tokens.push_back(s.substr(lastPos, pos - lastPos));
        lastPos = s.find_first_not_of(delimiters, pos);
        pos = s.find_first_of(delimiters, lastPos);
    }
}

std::vector<std::string> split(const std::string& s, const std::string& delimiters)
{
    vector<string> tokens;
    split(s, tokens, delimiters);
    return tokens;
}

std::string join(const std::vector<std::string>& v, const std::string& delimiters)
{
    if (v.size() < 1) {return "";}
    std::string ret(v[0]);
    for (auto it = ++v.begin(); it != v.end(); ++it)
        ret += delimiters + *it;
    return ret;
}

void TimeIt::show()
{
    using namespace termcolor;
    for (auto& [name, info] : dict) {
        std::cout << cyan(name) << ": \ttime="
            << red(lexical_cast<std::string>(info.time)) << "ms, \tcount="
            << red(lexical_cast<std::string>(info.count)) << "." << std::endl;
    }
}
