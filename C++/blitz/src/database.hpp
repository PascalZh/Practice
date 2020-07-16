#pragma once
#include "lexicon.hpp"
#include "utils.hpp"
#include <string>
#include <sstream>
#include <fstream>
#include <vector>

namespace blitz {

using namespace std;
    
class DataLoader
{
private:
    string m_path;
    fstream m_file;

public:
    DataLoader() : DataLoader("./lexicon.txt") {}
    explicit DataLoader(const string& path)
        : m_path(path), m_file(path, ios::in) {}

    bool good() { return m_file.good(); }

    vector<Record> read_records() { return lexical_cast<vector<Record>>(this->read_data()); }
    
    string read_data()
    {
        std::stringstream buffer;
        buffer << m_file.rdbuf();
        return buffer.str();
    }
};

} /* namespace blitz */ 
