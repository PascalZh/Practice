#pragma once
#include <string>
#include <vector>
#include <memory>
#include <algorithm>
#include <functional>
#include <map>
#include <queue>
#include "utils.hpp"

namespace blitz {

using namespace std;

class Coder
{
public:
    virtual string encode(const string& data) = 0;
    virtual string decode(const string& data) = 0;

    static unique_ptr<Coder> create(const string& text);

    virtual ~Coder() {}
};
} // namespace blitz
